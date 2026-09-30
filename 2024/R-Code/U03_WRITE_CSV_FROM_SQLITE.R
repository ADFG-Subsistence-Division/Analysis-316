# ##############################################################################
# Programmer: Jesse Coleman & CoPilot (Claude Sonnet 4.5)
# Date: 5/11/2026
# Purpose: Extract all project data as csv files from SQLite database.
# 
#
# Special update Notes: 
#     - When updating paths, use the / slash instead of the \
#     - Paths starting with the letter drive must end in a /
#     - references to folders do NOT start with a /
#     - The SQLite database path is set in Z00_PROJECT_PARAMETERS; 
#       specify remoteSQLiteDB, localSQLiteDB, or a different file path if necessary.
#
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
#
# ##############################################################################

# Standard libraries
library(DBI)
library(dplyr)
library(readr)

# Set project parameters
source("./Z00_PROJECT_PARAMETERS.r")


# Connect to database
con <- DBI::dbConnect(RSQLite::SQLite(), remoteSQLiteDB)
# con <- DBI::dbConnect(RSQLite::SQLite(), "your_sqlite_path") # <- Uncomment & update if need be


# Get the table
db_table <- tbl(con, "csv_files") %>% collect()

# Create output directory
output_dir <- "./extracted_csvs" # <-- Creating a separate directory prevents overwriting any existing data
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Extract each CSV
for (i in 1:nrow(db_table)) {
  # Get the folder/file info
  folder_name <- db_table$rel_path[i]
  file_name <- db_table$file_name[i]
  
  # Parse CSV from character string
  df <- read_csv(db_table$data[i], 
                 show_col_types = FALSE,
                 na = c("", "NA", "N/A", "null"))
  
  # Create subdirectory if needed
  folder_path <- file.path(output_dir, folder_name)
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  
  # Full file path
  full_path <- file.path(folder_path, file_name)
  
  # Write CSV
  write_csv(df, 
            full_path,
            na = "")
  
  message(sprintf("[%d/%d] Written: %s", i, nrow(db_table), full_path))
}

# Disconnect
DBI::dbDisconnect(con)

message(sprintf("\nExtracted %d CSV files to '%s'", nrow(db_table), output_dir))

