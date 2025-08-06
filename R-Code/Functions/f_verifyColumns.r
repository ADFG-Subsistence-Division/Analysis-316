# ##############################################################################
# Programmer: D. Koster
# Date: 02/2022
# file: f_standardizeColumns.r
# Purpose: Given a list of columns and an input dataframe this function
#          verifies that all columns exist. If one does not, then the data frame
#          will be populated with an error message - this is intended to assist
#          with debugging errors..
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

verifyColumns <- function(sourceData, colList){
  
  # ##############################################################################
  # 1.0 Look through each of the columns and if the column isn't present,
  #     Return an error message.
  # ##############################################################################
  
  for(kk in colList)
  {
    if(!(kk %in% names(sourceData)))
    { 
      #print("ERROR,data not present")
      error = c(str_interp("required column ${kk} not present in source data file"))
      return(data.frame(error))
    }
  }
  
  return(sourceData)
}

  
#Example using tidyverse; Core R syntax maintained to ensure long-term stability.
#data_set = source_data %>%
#  select(., -ends_with("_drop"))


