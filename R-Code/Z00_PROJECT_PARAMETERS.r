# ##############################################################################
# Programmer: D. Koster
# Date: 11/2022
# Purpose: load all libraries and set all variables for use with
#         standard analysis syntax. This file contains necessary variables for
#         most or all R processing code. If you need variables added to be
#         used throughout, this is where those are set up.
#
#    THIS FILE CANNOT BE RUN ON ITS OWN, IT IS A HELPER FILE FOR ANALYSIS.
#
# Special update Notes: 
#     - When updating paths, use the / slash instead of the \
#     - Paths starting with the letter drive must end in a /
#     - references to folders do NOT start with a /
#
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
# Programmer: D. Koster 07/05/2023 - modified templates to operate with 
#             project files - This is a template update.
# Programmer: D. Koster 08/14/2023 - added a color-palette function & 
#             'colorsafe' colors for use with figures. A maximum of 11 classes
#             are available.
# Programmer: D. Koster 05/21/2024 - Modified templates to operate in a more 
#             tightly controlled environment. The idea is to have the bulk of the
#             project R code stored in the project folder so that we can develop
#             a workflow that permits code sharing as part of publication.
#             Publishing of R code is not expected to occur this year.
#
# ##############################################################################

# Standard libraries
library(tidyverse)
library(rio)
library(adfgSubs)

# ##############################################################################
# 0.0 - SETUP - Establish user and project specific paths, variables and file names.
# ##############################################################################

# Change this if you're attempting to run code using a drive letter other than S.
sharedResPath = '//dfg.alaska.local/SUB/Anchorage'
preliminaryResultsFolder = 'GitHub/Preliminary Analysis Results/'

server = '//dfg.alaska.local/SUB/Anchorage/'
#server = '//dfg.alaska.local/SUB/Fairbanks/'
#server = 'C:/R-Subist-Data-Local/'

#What project are we working on?
studyear = 2024
projID = 316
projectName = "NPS Ambler Comprehensive"

#projID = 999
#projectName = "MODIFY Z00_PROJECT_PARAMETERS.r BEFORE CONTINUING"

# Resources that shouldn't be expanded in the form of a logic statement.
#  the logic statement MUST be in parenthesis. By convention
#  Polar Bear is never expanded and Muskoxen is not expanded by default, but
#  needs to be evaluated. Other species must be evaluated one by one.
noExpansionCriteria = "(resource == 212000000)"

# Don't change the code below - it works with the above to prevent expansion.
noExpansionCriteriaNot = paste0("!(", noExpansionCriteria, ")", sep="")

# Standard economics bootstrapping resamples (Change based on project requirements)
econBoot_k = 1000
bootSampPath = str_interp("C:/R-Subist-Data-Local/Bootstrapping Data/${projID} - ${studyear}/")

preliminaryResultsPath = str_interp('${server}/')

# We generally don't want scientific notation for our output, it's impractical 
#   for evaluating results or producing tables and figures.
options(scipen = 999)

# ##############################################################################
# 1.0 - harvest column names.
# ##############################################################################

standardCFHarvColumns = c("amtGavetoCrew", "amtGaveOthers", "removeOwnUse")

standardGearColumns = c("amtDriftGillNet",
                        "amtSetGillNet",
                        "amtUnspecifiedGillNet",
                        "amtSeine",
                        "amtFishTrap", 
                        "amtDipNet",
                        "amtRodReel",
                        "amtJigHandLine", 
                        "amtHookUnderIce",
                        "amtNetUnderIce",
                        "amtTrotLine",
                        "amtFishTrapUnderIce", 
                        "amtUnspecifiedIceFishing",
                        "amtLongLineSkate",
                        "amtTrolling",
                        "amtSpearGaff", 
                        "amtCastNet",
                        "amtOtherGear",
                        "amtFishWheel",
                        "amtUnspecifiedSubsistenceNet")
standardFishOtherColumns = c("amtDogFood")

standardMammalColumns = c("harvJan", "harvFeb",
                        "harvMar", "harvApr", "harvMay", "harvJun", "harvJul", "harvAug",
                        "harvSep", "harvOct", "harvNov", "harvDec", "harvUnkn")

standardBirdColumns = c("harvWin", "harvSpr", "harvSum", "harvFal", "harvUnkn")

standardActivityColumns = c("used", "harvestq","attempt", "giveaway", "received")

# 'Key' column list (for harvests); this will allow us to standardize some automation
#   so that template rmd files will run even if not fully configured.
keyHarvColList <- c("projID", "studyear", "communty", "commname", "commhh", 
                    "samphh","sampPop", "NHouseholds","strata", "HHID", 
                    "resource", "specList", "strataWt", "commPop",
                    "NPopulation")

# Standard HH Harvest file columns - adjust if necessary.
fullHarvColList <- c(keyHarvColList,
                     "convFact", 
                     standardActivityColumns,
                     "mRepl",                     
                     standardMammalColumns,
                     standardBirdColumns[!standardBirdColumns == 'harvUnkn'],
                     "amtFurOnly", 
                     standardCFHarvColumns, 
                     "reptHarvestAmt", "reptHarvestLbs","harvestAmt_MR", 
                     "harvestLbs_MR", "estHarvestAmt", "estHarvestLbs",
                     standardFishOtherColumns, 
                     standardGearColumns,
                     "usePrevious", "roadkill")


# ##############################################################################
# 2.0 - Project-specific helper functions.
# ##############################################################################

# Helper function for color palettes. Because this is integral to projects, the
#   helper function will live in the project parameters, it provides a way for
#   analysts to do a better job of customizing colors per-project, while starting with the
#   a default scheme.
getColors <- function(palName = "Spectral", nColors = 1)
{
  pal=c('#000000')
  if(palName == "Rb")
  {
    pal = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7', '#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a')
  }
  if(palName == "RylG")
  {
    pal = c('#a50026', '#006837','#d73027','#1a9850','#f46d43',
      '#66bd63','#fdae61','#a6d96a','#fee08b','#d9ef8b',
      '#000000')
  }
  if(palName == "Spectral")
  {
    pal = c('#3288bd','#f46d43','#66c2a5','#d53e4f','#abdda4',
       '#fdae61', '#e6f598', '#FFD36E', '#5e4fa2', '#9e0142', '#000000')
    
  }
  
  if(nColors > 11)
    return(c('#000000'))
  else
    return(pal[1:nColors])
}


# ##############################################################################
# End of file.
# ##############################################################################
