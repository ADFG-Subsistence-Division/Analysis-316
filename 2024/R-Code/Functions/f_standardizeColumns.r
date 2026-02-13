# ##############################################################################
# Programmer: D. Koster
# Date: 02/2022
# file: f_standardizeColumns.r
# Purpose: Given a list of columns and an input dataframe this function
#          columns specified in the list to the input dataframe. The default
#          replacement value is 0, but it can be overridden as needd.
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
# Programmer:
#
#
#
# Requires: Tidyverse
# 
#
# ##############################################################################

standardizeColumns <- function(sourceData, colList, replaceValue=0){
  
  # ##############################################################################
  # 1.0 Look through each of the columns and if the column isn't present,
  #     give it the value of replaceValue.
  # ##############################################################################
  
  for(kk in colList)
  {
    if(!(kk %in% names(sourceData)))
    { 
      ks = enquo(kk)
      sourceData$newCol = replaceValue
      sourceData <- rename(sourceData, !!ks := "newCol")
    }
  }
  
  return(sourceData)
}
  
#Example using tidyverse; Core R syntax maintained to ensure long-term stability.
#data_set = source_data %>%
#  select(., -ends_with("_drop"))


