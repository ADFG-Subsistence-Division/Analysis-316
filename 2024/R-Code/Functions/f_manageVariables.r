# ##############################################################################
# Programmer: D. Koster
# Date: 02/2021
# Purpose: Replicate SPSS functions for:
#   'DELETE VARIABLES'
#    /KEEP (when saving)
#   'RENAME VARIABLES'
#   'RECODE x .. y (a = b).'
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
# Programmer:
#
# Requires: Tidyverse
#
# ##############################################################################

delete_variables <- function(dataSetD,variablesToDelete){
  dataSetD <- dataSetD[,!(names(dataSetD) %in% variablesToDelete)]
  
  return(dataSetD)
}

keep_variables <- function(dataSetK, variablesToKeep){
  dataSetK <- dataSetK[ , (names(dataSetK) %in% variablesToKeep)]
  
  return(dataSetK)
}

inf_to_NA <- function(dataSetI, variableList)
{

  dataSetI <- dataSetI %>% 
    mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(is.infinite(.), NA, .)))

  return(dataSetI)
}

nan_to_NA <- function(datasetN, variableList){
  
  datasetN <- datasetN %>% 
    mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(is.nan(.), NA, .)))
  
  return(datasetN)
}

recode_variables <- function(dataSetR, variableList, oldValue, newValue) {
  # First, we have to establish whether or not the first value is NA
  if(is.na(oldValue)) {
    dataSetR <- dataSetR %>% 
      mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(is.na(.), newValue, .)))
  } else {
    if(is.na(newValue)) {
      dataSetR <- dataSetR %>% 
        mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(. == oldValue, NA, .)))
    } else {
        dataSetR <- dataSetR %>% 
        mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(. == oldValue, newValue, .)))
    }
  }
  
  return(dataSetR)
}

# Just replace whatever is in a column with the assigned value, without regard
#    for the current contents.
assign_value_variables <- function(dataSetA, variableList, newValue) {
  dataSetA <- dataSetA %>% mutate (. , across(.cols = all_of(variableList), ~ newValue))

    return(dataSetA)
}


get_code_labels <- function(codesToFind, codeColumnName, valueColumnName, codeList)
{
  sNamesList = c(codeColumnName, valueColumnName)
  for(kk in sNamesList)
  {
    if(!(kk %in% names(codeList)))
    { 
      print("ERROR,data not present")
      error = c(str_interp("required column ${kk} not present in codeList"))
      return(data.frame(error))
    }
  }
  
  codeList <- rename(codeList, "codeLabel"=valueColumnName)
  
  newCode <- left_join(data.frame(codesToFind), codeList, by=c("codesToFind"=codeColumnName))
  
  return(newCode$codeLabel)
  
}


normalizeColumn <- function(vec)
{
  return ((vec - min(vec)) / (max(vec) - min(vec)))
}
  
#Example using tidyverse; Core R syntax maintained to ensure long-term stability.
#data_set = source_data %>%
#  select(., -ends_with("_drop"))


