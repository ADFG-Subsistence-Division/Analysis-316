#
# Author: D. Koster
# Script: F99_ADHOC_WORKSPACE.R
#
# Purpose: Workspace for adhoc summaries and simple analysis 
#          necessary for updating the Port Graham figures associated with
#          the PWS RCAC Recovery paper.


library(tidyverse)

harvData <- read.csv('./CSV/03 - Main/harvData_HH_finalPrepped.csv')
selectHarvData <- read.csv('./CSV/03 - Main/PortGraham_Select_CSIS_Species.csv')
historicalHHData <- read.csv('./CSV/03 - Main/historical_HH_DB.csv')

# Pull down HH-level data


# Just sockeye
sockeyeData <- filter(harvData, resource == 115000000)

sockeyeData$useNoHarv = 0
sockeyeData <- sockeyeData %>%
  mutate(useNoHarv = if_else(used == 1 & harvestq == 0, 1, 0))

sockeyeData <- sockeyeData %>%
  group_by(projID, studyear, commname, resName) %>%
  summarise(used = mean(used, na.rm=TRUE),
            attempt = mean(attempt, na.rm=TRUE),
            harvestq = mean(harvestq, na.rm=TRUE),
            received = mean(received, na.rm=TRUE),
            giveaway = mean(giveaway, na.rm=TRUE),
            useNoHarv = mean(useNoHarv, na.rm=TRUE),
            estimatedHarvest = sum(estHarvestLbs),
            percapita = sum(estHarvestLbs/commPop))

# #################################################################################
# Pivot and filter the selected species for use figure.
# #################################################################################


selectUseData <- selectHarvData %>%
  filter(projid %in% c(17,65,81,91,92,93,117,171,235)) %>%
  select(year, commname, rescode, resource, used) %>%
  pivot_wider(values_from = used, names_from = year)

newUseData <- harvData %>%
  filter(resource %in% selectUseData$rescode) %>%
  group_by(resource) %>%
  summarize(used = mean(used, na.rm=TRUE)) %>%
  ungroup() %>%
  rename("2023" = used,
         rescode = resource) 

newUseData <- selectUseData %>%
  left_join(newUseData, by=c("rescode"))

write.csv(newUseData, "./CSV/05 - Final Analysis Output/historicalUseCompare.csv")

# #################################################################################
# Mean species used figure.
# #################################################################################

# Remove vegetation for proper comparison.

usedHxData <- historicalHHData %>%
  filter(specList == 1 & resource < 600000000) %>%
  group_by(studyear, HHID) %>%
  summarize(used = sum(used, na.rm=TRUE))

thisUsedData <- harvData %>%
  filter(specList == 1 & resource < 600000000) %>%
  group_by(studyear, HHID) %>%
  summarise(used = sum(used, na.rm=TRUE))

usedSpeciesData <- dplyr::bind_rows(usedHxData, thisUsedData)


# Compute group statistics (whisker max and mean)
stats <- usedSpeciesData %>%
  group_by(studyear) %>%
  summarise(
    #upper_whisker = quantile(used, 0.75, na.rm = TRUE) + 1.5 * IQR(used, na.rm = TRUE),  # Upper whisker
    upper_whisker = max(used),  # Upper bound
    mean_use = mean(used, na.rm = TRUE)
  )

# Create the boxplot
ggplot(usedSpeciesData, aes(x = as.factor(studyear), y = used)) +
  geom_boxplot(fill = "lightgray", color = "black") +  # Boxplot without outliers
  stat_boxplot(geom = "errorbar", width = 0.2) +  # Whiskers
  theme_minimal(base_family = "serif") + # Apply minimal theme with serif font
  #theme_minimal(base_family = "serif", base_size = 10) + # Apply minimal theme with serif font
  #geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +  # Jittered points for visibility
  geom_text(data = stats, aes(x = as.factor(studyear), y = upper_whisker + 3, label = round(mean_use, 1)), 
            family = "serif", size = 10 / .pt) +  # Adjusted font size (10pt)
  theme(axis.title.y=element_text(size=10, family="serif", face="plain"),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  labs(x = "Study year", y = "Resources used by households") +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "solid"))

