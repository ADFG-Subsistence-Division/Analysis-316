# ##############################################################################
# Programmer: D. Koster
# Date: 01/2026
# File: U00_AUTO_CSV_ARCHIVER.R
# Purpose: Archive CSV files into SQLite databases for easier network transfer
#   and portability. The Z00_PROJECT_PARAMETERS.R file MUST be
#   configured properly, this file is intended to be automatic once 
#   Z00 has been properly configured.
#
# Annual notes / adjustments (Not to be repeated annually)
#  -----
#
# Implementation notes:
#
# Update as needed annually. Saves data into SPSS .SAV files.
#  Update the Z00_PROJECT_PARAMETERS.r annually to set the pYr value.
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
# Programmer:
#
# Required libraries:
#   tidyVerse
#   rio
#
# ##############################################################################


source('./R-Code/Z00_PROJECT_PARAMETERS.r')


# Utility: Safely test if network dir is online
dir_online <- function(path) {
  tryCatch({ dir_exists(path) && file.access(path, 0) == 0 }, error = function(e) FALSE)
}

# ----- Archive data function.

archive_csvs_to_sqlite <- function(source_dir, db_path) {
  # Find all CSV files recursively
  files <- dir_ls(source_dir, recurse = TRUE, glob="*.csv")
  # Return message:
  msg = "\n Archiving files ..."
  # Setup DB
  con <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS csv_files (
    id INTEGER PRIMARY KEY,
    rel_path TEXT,
    file_name TEXT,
    modified_time TEXT,
    data TEXT
  )")
  
  # Delete all contents; we will refresh from scratch below
  dbExecute(con, "DELETE FROM csv_files")
  
  for (file in files) {
    rel_path <- path_rel(path_dir(file), start = source_dir)
    file_name <- path_file(file)
    modified_time <- file_info(file)$modification_time
    #type <- as.character(file_info(file)$type)
    data <- read_file(file)
    dbExecute(con, "INSERT INTO csv_files (rel_path, file_name, modified_time, data) VALUES (?, ?, ?, ?)",
              params = list(rel_path, file_name, as.character(modified_time), data))
    #print(path_dir(file))
    msg = paste(msg, "\n", rel_path, "/", file_name, sep="")
  }
  dbDisconnect(con)
  
  return(msg)
}

# Archive to sqlite.
if(!is.na(archivePath) & !is.na(localSQLiteDB))
{
  resultMsg = archive_csvs_to_sqlite(archivePath, localSQLiteDB)
  print(resultMsg)
} else {
  print("Z00_PROJECT_PARAMETERS.R may not be properly configured, please check.")
}


if(dir_online(remoteArchivePath))
{
  tryCatch({
    
    # Original line below this is a single line, works fine.
    file.copy(localSQLiteDB, remoteSQLiteDB, overwrite = TRUE)
    
    print("Copy to network -- success.")
  
  }, error = function(e) {
  
    msg = paste("Error with archiving:", e$message, sep=" ")
    print(msg)
    
  })
} else {
  print("Unable to archive, network folder not available.")
}