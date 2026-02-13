# ##############################################################################
# Programmer: [NAME]
# Date: MM/YYYY
# File: Z02_SAMPLE_STARTING_CODE.R
# Purpose: Template to be used as the starting point for analysis scripts.
#
#
# Annual notes / adjustments (Not to be repeated annually)
#  -----
#
#
#
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
# Programmer:
#
# Required libraries:
#   tidyVerse
#   rio
#   haven (for SPSS Sav)
#
# ##############################################################################

# ##############################################################################
# 0.0 - Prep the environment.
# ##############################################################################

# Clear out all existing variables & datasets.
rm(list=ls(all=TRUE))

# Project location
projectPath = '/IM/Temp/Analysis/'

# Set the file-system; adjust comments as needed.

#  Anchorage projects folder
ntwkPath = '//dfg.alaska.local/sub/Anchorage'

#  Fairbanks projects folder
#ntwkPath = '//dfg.alaska.local/sub/Fairbanks'

# Set your working path.
dPath = paste0(ntwkPath, projectPath, sep='')

# Include the project parameters file - this needs to be updated for all 
#   projects.
source(paste(dPath, 'Z00_PROJECT_PARAMETERS.r', sep=''))


# ##############################################################################
# 1.0 - Load all files necessary for data processing steps here.
# ##############################################################################

inputData <- read.csv(str_interp('${dPath}xls-raw/RECXX_raw.csv'), na = '', header = TRUE, strip.white = TRUE)

# ##############################################################################
# 2.0 - BEGIN THE ANALYSIS!
# ##############################################################################

# 2.01 - Take an action.
outputData <- inputData

# ##############################################################################
# N.0-1.0 - WRITE OUT SPSS AND CSV FILES
# ##############################################################################

saveToSPSS(outputData,str_interp('${dPath}SAV/processedData.sav'))
saveToCSV(outputData,str_interp('${dPath}CSV/03 - main/processedData.csv'))

# ##############################################################################
# N.0 END OF SCRIPT
# ##############################################################################
