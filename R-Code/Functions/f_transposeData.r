# ##############################################################################
# Programmer: D. Koster
# Date: 05/2023
# file: f_transposeData.r
# Purpose: Given a data frame, the name of a header column, and rounding precision
#          (which defaults at 2), this code transposes rows of data into columns.
#          the data is converted into character values to satisfy R data-requirements.
#          This code should only be used for organization of display data for
#          R-markdown or sometimes for output of data files that are presented
#          in a transposed format, such as the sample tables.
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

# Transpose; use for small datasets only and for production of tables.
transposeTable <- function(ttData, headerCol = NA, roundPrecision=2) {
  
  # Start by determining which column is going to be the header OR
  #   using a sequence from 1:n
  #   Note that if we've determined that the selected header column is 
  #   not unique, then we will also assign a number from 1:n.
  
  if(is.na(headerCol)){
    hdrName <- as.character(seq(1:nrow(ttData)))
  } else {
    
    ctHeaders <- group_by(sampData, commname) %>%
      summarize(nh = n())
    maxCTHeaders <- max(ctHeaders$nh)
    if(maxCTHeaders > 1) {
      hdrName <- as.character(seq(1:nrow(ttData)))
    } else
    {
      namesVec <- names(ttData)
      idxHdr <- which(namesVec == headerCol)
      hdrName <- ttData[,idxHdr]
      ttData <- ttData[,-idxHdr]
    }
    
  }
  
  namesVec <- names(ttData)
  
  # Round and convert to character.
  for(ii in 1:length(namesVec))
  {
    if(class(ttData[,ii])=="numeric") {
      ttData[,ii] = base::round(ttData[,ii], digits=roundPrecision)
    }
    ttData[,ii] = as.character(ttData[,ii])
  }
  
  ttDF = data.frame("col" = namesVec)
  
  #Now pivot.
  for(jj in 1:nrow(ttData)){
    rVec <- unlist(unname(ttData[jj,]))
    ttDF[,hdrName[jj]] = rVec
  }
  
  return(ttDF)
}
