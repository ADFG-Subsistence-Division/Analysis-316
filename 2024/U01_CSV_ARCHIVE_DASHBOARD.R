# ##############################################################################
# Programmer: D. Koster
# Date: 01/2026
# File: U01_CSV_ARCHIVE_DASHBOARD.R
# Purpose: Archive CSV files into SQLite databases for easier network transfer
#   and portability. Enables both archive and extract of arbitrary CSV folders
#   into named databases. If the Z00_PROJECT_PARAMETERS.R file has been
#   configured properly, this file is largely automatic.
#
# Annual notes / adjustments (Not to be repeated annually)
#  -----
#
# AI Notes; Note: ChatGPT (Github.com/copilot) was used to develop this dashboard
#   This involved several queries and refinements over the course of several days.
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

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyTree)
library(DBI)
library(RSQLite)
library(fs)

source('./R-Code/Z00_PROJECT_PARAMETERS.r')

# Utility: Read config from file
read_config <- function(path) {
  if (!file.exists(path)) return(list())
  conf <- readLines(path)
  conf <- conf[grepl("=", conf)]
  vals <- str_split_fixed(conf, "=", 2)
  setNames(as.list(trimws(vals[,2])), trimws(vals[,1]))
}

# Utility: Safely test if network dir is online
dir_online <- function(path) {
  tryCatch({ dir_exists(path) && file.access(path, 0) == 0 }, error = function(e) FALSE)
}

# Utility: Generate tree for SQLite
sqlite_tree <- function(db_path) {
  if (!file.exists(db_path)) return(list("Database not found" = ""))
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  tbls <- dbListTables(con)
  tree <- list()
  for (tbl in tbls) {
    rows <- dbGetQuery(con, sprintf("SELECT rel_path, file_name FROM [%s]", tbl))

    if (nrow(rows) == 0) {
      tree[[tbl]] <- list()
      next
    }
  }
  
  rows <- rows %>%
    arrange(rel_path) %>%
    mutate(
      fullPath = paste(rel_path, file_name, sep="/"),
      fullPath = if_else(rel_path != ".", paste("./", fullPath, sep=""), fullPath),
      parsedPath = strsplit(fullPath, .Platform$file.sep),
    )
  
  tree = list()
  for(ii in 1:nrow(rows))
  {
    treePart <- list()
    tmpVec <- unlist(rows$parsedPath[ii])
    thisMaxDepth = length(tmpVec)
    updateStack = list() #Reset update stack.
    stackSize = 0 # Set stack size for popping and adding in a later step.
    logList = ""
    
    updatePath = tree
    for(kk in 2:thisMaxDepth)
    {
      # I am a leaf; leaves always get added.
      if(kk == thisMaxDepth)
      {
        stackSize = stackSize + 1
        # Added update Path, in place of ""
        updateStack[[stackSize]] = structure(updatePath, elmName=tmpVec[kk], elmType="file", insertUpdate="insert")
        logList = paste(logList,'++INS(f):', tmpVec[kk], "@stack:",stackSize,sep="")        
      }
      # I am a folder
      if(kk < thisMaxDepth)
      {
        if(!is.list(updatePath[[tmpVec[kk]]]))
        {  
          #print("folder not in")
          stackSize = stackSize + 1
          updateStack[[stackSize]] = structure(updatePath, elmName=tmpVec[kk], elmType="folder", insertUpdate='insert')
          updatePath = list() # Nothing below it;
          logList = paste(logList,'++INS(d):', tmpVec[kk], "@stack:",stackSize,sep="")        
        } else {
          stackSize = stackSize + 1
          updateStack[[stackSize]] = structure(updatePath, elmName=tmpVec[kk], elmType="folder", insertUpdate='update')
          updatePath <- updatePath[[tmpVec[kk]]] #advance pointer to next node.
          #print("Folder is in.")
        } 
      }
    }
    #print(updateStack)
    #print("----------------------------------------------------")
    logList = ""
    for(kk in stackSize:1)
    {
      currentNodeType = attr(updateStack[[kk]], 'elmType')
      currentNodeName = attr(updateStack[[kk]], 'elmName')
      if(currentNodeType == 'file')
      {
        treePart = updateStack[[kk]]
        treePart[[currentNodeName]] = structure("", sticon="fa fa-file")
      } else {
        if(attr(updateStack[[kk]], 'insertUpdate') == 'update')
        {  
          currentNode = updateStack[[kk]]
          currentNode[[currentNodeName]] = structure(treePart, sticon="fa fa-folder")
          treePart = list()
          treePart = currentNode
        } else {
          currentNode <- updateStack[[kk]] #list()
          currentNode[[currentNodeName]] = structure(treePart, sticon="fa fa-folder")
          treePart = list()
          treePart = currentNode
        }
      }
    }
    #print(logList)
    #  print(paste("Last node:", currentNodeName))
    tree[[currentNodeName]] = treePart[[currentNodeName]]
  }
  
  tree
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

# ---- Extract data function.

extract_csvs_from_sqlite <- function(db_path, target_dir, overwrite = c("all", "older"))
{
  msg <- "\n Extracting files..."
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  overwrite <- match.arg(overwrite)
  df <- dbReadTable(con, "csv_files")
  for (i in seq_len(nrow(df))) {
    row <- df[i,]
    out_dir <- path(target_dir, row$rel_path)
    dir_create(out_dir)
    out_file <- path(out_dir, row$file_name)
    if (file_exists(out_file)) {
      if (overwrite == "older") {
        orig_time <- file_info(out_file)$modification_time
        if (as.POSIXct(row$modified_time) <= orig_time) next
      }
    }
    write_file(row$data, out_file)
    file_chmod(out_file, "0644")
    msg=paste(msg, "\n", out_file, sep=" ")
  }
  dbDisconnect(con)
  
  return(msg)
}


ui <- dashboardPage(
  dashboardHeader(
    title = span("CSV/SQLite Archiver",
                 style = "font-family: 'Segoe UI', 'Arial', sans-serif; font-weight: 700; letter-spacing: 1px; color: #fbb623;"
    ),
    dropdownMenuOutput("notificationMenu")
  ),
  dashboardSidebar(
    #style = "background-color: #003367; color: #fff;",
    tags$label("- Archive data to SQLite -", style = "color: #fbb623; font-weight: 600; padding-left: 32px;"),
    tags$br(),
    tags$label("CSV data folder:", style = "color:#fff; padding-left: 8px;"),
    verbatimTextOutput("archivePath_disp", placeholder = TRUE),
    shinyDirButton("archivePath", "Choose CSV folder", "Select the root folder containing CSV files"),
    tags$label("Local archive DB:", style = "color:#fff; padding-left: 8px;"),
    verbatimTextOutput("localSQLiteDB_disp", placeholder = TRUE),
    shinySaveButton("localSQLiteDB", "Choose Local SQLite DB", "SQLite DB file", localSQLiteDB),
    actionButton("archive_local", "Archive CSV data folder", class = "adfg-btn"),
    actionButton("extract_local", "Extract to CSV data folder", class = "adfg-btn"),
    radioButtons("overwrite_mode", "Overwrite on Extract", c("All" = "all", "Older Only" = "older")),
    tags$label("- Remote actions -", style = "color: #fbb623; font-weight: 600; padding-left: 48px;"),
    tags$br(),
    tags$label("Remote archive DB:", style = "color:#fff; padding-left: 8px;"),
    verbatimTextOutput("remoteSQLiteDB_disp", placeholder = TRUE),
    shinySaveButton("remoteSQLiteDB", "Choose Remote SQLite DB", "Remote DB", remoteSQLiteDB),
    actionButton("copy_to_network", "Copy Archive to Network", class = "adfg-btn")
  ),
  dashboardBody(
    # Custom CSS for ADF&G styling
    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-header, .main-sidebar, .skin-blue .main-header .navbar, .skin-blue .main-header .logo {
        font-family: 'Segoe UI', 'Arial', sans-serif !important;
      }
      .main-header .logo {
        width: 245px !important;
        background-color: #003366 !important;
        color: #fbb623 !important;
        overflow: visible !important;
        font-weight: 700;
        font-size: 20px;
      }
      .skin-blue .main-header .navbar {
        background-color: #003366 !important;
      }
      .skin-blue .main-sidebar {
        background-color: #003366 !important;
      }
      .skin-blue .sidebar a {
        color: #fff !important;
      }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background: #2c76b1 !important;
        color: #fbb623 !important;
        font-weight: bold;
      }
      .adfg-btn {
        background-color: #fbb623 !important;
        color: #003366 !important;
        border: none !important;
        font-weight: 600 !important;
        margin-bottom: 8px;
      }
      .adfg-btn:hover {
        background-color: #ffda79 !important;
        color: #2c76b1 !important;
      }
      .content-wrapper, .box, .tab-content, .tab-pane {
        background-color: #f7f7f7 !important;
      }
      .adfg-info-block {
        background-color: #003366 !important;
        color: white !important;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
        font-size: 15px;
      }
      .box-header {
        color: #003366 !important;
        font-weight: 700 !important;
      }
      label, .control-label, .radio label, .checkbox label {
        color: white !important;
      }
      h4 {
        color: #003366;
      }
      "))),
    tabBox(width = 12,
           tabPanel("SQLite Archives",
                    # Informational block
                    fluidRow(
                      column(12,
                             div(
                               class = "adfg-info-block",
                               tags$strong("How to use:"),
                               tags$hr(style = "border-top: 2px solid #fbb623; margin: 8px 0 12px 0;"),
                               tags$ul(
                                 style = "margin-bottom: 0; padding-left: 20px;",
                                 tags$li("Set project-specific paths in the Z00_PROJECT_PARAMETERS.R file."),
                                 tags$li("Use U00_AUTO_CSV_ARCHIVER.R for auto archive & network copy."),
                                 tags$li("Use over VPN is SLOW; be patient."),
                                 tags$li("CSV data folder is the location of CSV files."),
                                 tags$li("'Archive CSV data folder' will store all CSVs in the selected 'Local archive DB'."),
                                 tags$li("'Extract to CSV data folder' will extract data from 'Local archive DB' and store CSVs in the selected CSV data folder."),
                               )
                             )
                      )
                    ),
                    fluidRow(
                      column(6,
                             tags$h4("Local SQLite Archive:"),
                             shinyTree("local_sqlite_tree", search = FALSE)
                      ),
                      column(6,
                             tags$h4("Remote SQLite Archive:"),
                             shinyTree("remote_sqlite_tree", search = FALSE)
                      )
                    )
           ),
           tabPanel("Status/Logs",
                    verbatimTextOutput("status")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Status values
  notifyBadgeStatus <- reactiveVal("success")
  notifyValues <- reactiveVal(list())

  # Set roots for shinyFiles
  roots = c(
    "Home" = normalizePath("~"),
    "Computer" = "/",
    "Anchorage" = "//dfg.alaska.local/SUB/Anchorage/",
    "Fairbanks" = "//dfg.alaska.local/SUB/Fairbanks/",
    "Local project folder" = getwd()
  )
  
  # ------------ Dir/file picker logic
  shinyDirChoose(input, "archivePath", roots = roots,
                 defaultRoot='Local project folder')
  
  shinyFileSave(input, "localSQLiteDB", roots = roots,
                filetypes = c("", "sqlite", "db"),
                defaultRoot='Local project folder')
  
  shinyFileSave(input, "remoteSQLiteDB", roots = roots,
                filetypes = c("", "sqlite", "db"),
                defaultRoot = 'Anchorage')

  # ------------ Reactive values in app
  state <- reactiveValues(
    archivePath = archivePath,
    localSQLiteDB = localSQLiteDB,
    remoteArchivePath = remoteArchivePath,
    remoteSQLiteDB = remoteSQLiteDB,
    net_online = FALSE,
    status_msg = "Welcome!"
  )
  
  # ------------ Dir/file choose observers
  observeEvent(input$archivePath, {
    sel <- parseDirPath(roots, input$archivePath)
    if (length(sel)) state$archivePath <- sel
  })
  
  observeEvent(input$localSQLiteDB, {
    sel <- parseSavePath(roots, input$localSQLiteDB)
    if (nrow(sel) > 0) {
      state$localSQLiteDB <- as.character(sel$datapath[1])
    }
  })

  observeEvent(input$remoteArchivePath, {
    sel <- parseDirPath(roots, input$remoteArchivePath)
    if (length(sel)) state$remoteArchivePath <- sel
  })
  
  observeEvent(input$remoteSQLiteDB, {
    sel <- parseSavePath(roots, input$remoteSQLiteDB)
    if (nrow(sel) > 0) {
      state$remoteSQLiteDB <- as.character(sel$datapath[1]) 
      #print(state$remoteSQLiteDB)
    }
  })
  
  # ------------ Display chosen dirs/files -- Base names where appropriate.
  output$archivePath_disp <- renderText(state$archivePath)
  output$localSQLiteDB_disp <- renderText(state$localSQLiteDB)
  output$remoteArchivePath_disp <- renderText(state$remoteArchivePath)
  output$remoteSQLiteDB_disp <- renderText(basename(state$remoteSQLiteDB))
  
  # ------------ Network Drive Status - connected/not connected.
  check_network <- reactive({
    invalidateLater(30000, session) # Re-check every 30 seconds
    dir_online(state$remoteArchivePath)
  })
  
  check_status <- function() # Status messages & handling.
  {
    # Check online status.
    file_system_status <- check_network() # Reactive component.
    # If there's been a change, enable notifications. 
    if(state$net_online != file_system_status)
    {networkStatusChange = TRUE}
    else {networkStatusChange = FALSE}
    # Set the state to current file system status.
    state$net_online = file_system_status
    
    badgeStat = "success" # Default badge status for notification area.
    
    notifications <- list()
    if (!file_system_status) {
      if(networkStatusChange)
        showNotification("Unable to connect to ANC -- OFFLINE mode", 
                         type = "message")
      
      notifications <- append(notifications, list(notificationItem(text = "Offline", 
                                                                   icon = icon("circle-xmark"),
                                                                   status = "warning"))
      )
      connectStatus = FALSE
      badgeStat = "warning"
    } else {
      if(networkStatusChange)
        showNotification("Connection tests success - online mode", 
                         type = "message")
      notifications <- append(notifications, list(notificationItem(text = "Connected",
                                                                   icon = icon("circle-check"),
                                                                   status = "success"))
      )
      connectStatus = TRUE
      badgeStat = "success"
    }
    
    notifyBadgeStatus(badgeStat)
    notifyValues(notifications)
  }
  
  # Set the initial notification information.
  output$notificationMenu <- renderMenu({
    check_status()
    dropdownMenu(type = "notifications", 
                 badgeStatus = notifyBadgeStatus(), 
                 .list = notifyValues())
  })
  
  # Basic status message; super-ceded by above.
  #output$net_status <- renderText({
  #  if (check_network()) {
  #    state$net_online <- TRUE
  #    "Network Status: ONLINE"
  #  } else {
  #    state$net_online <- FALSE
  #    "Network Status: OFFLINE"
  #  }
  #})
  
  # ------------ Archive/copy/extract logic
  # Insert your archive_csvs_to_sqlite() and extract_csvs_from_sqlite() implementations here
  
  observeEvent(input$archive_local, {
    req(state$archivePath, state$localSQLiteDB)
    tryCatch({
      msg = archive_csvs_to_sqlite(state$archivePath, state$localSQLiteDB)
      state$status_msg <- paste(state$status_msg, msg, "\n", sep="")
      state$status_msg <- paste(state$status_msg, "Archive complete!", sep="\n")
      showNotification("CSV archive success", 
                       type = "message")
    }, error = function(e) {
      state$status_msg <- paste(state$status_msg, "\n", "Error archiving:", e$message, sep=" ")
      showNotification("Local archive FAILED, see status log for details", 
                       type = "message")
    })
  })
  
  observeEvent(input$copy_to_network, {
    
    req(state$localSQLiteDB, state$remoteSQLiteDB)
    out_path <- state$remoteSQLiteDB
    #out_path <- file.path(state$remoteArchivePath, basename(state$localSQLiteDB))
    #print(out_path)
    state$status_msg = paste(state$status_msg, "Copy local to:", out_path, sep="\n")
    
    tryCatch({
      # Original line below this is a single line, works fine.
      #file.copy(state$localSQLiteDB, out_path, overwrite = TRUE)
      # Get file size for progress reporting (Copilot generated.)
      file_size <- file.info(state$localSQLiteDB)$size
      block_size <- 1024 * 100 #* 1024  # 100KB chunks
      blocks <- ceiling(file_size / block_size)
      withProgress(message = "Copying file to network...", value = 0, {
        # Open file connections
        in_con <- file(state$localSQLiteDB, "rb")
        on.exit(close(in_con), add = TRUE)
        out_con <- file(out_path, "wb")
        on.exit(close(out_con), add = TRUE)
        
        # Copy with progress bar
        for (i in seq_len(blocks)) {
          # Read a block from the input
          bytes <- readBin(in_con, what = "raw", n = block_size)
          if (length(bytes) == 0) break  # End of file
          # Write block to output file
          writeBin(bytes, out_con)
          # Update progress bar
          incProgress(1 / blocks)
        }
      })      
      # Status messages, original code.
      showNotification("Copy success.", 
                       type = "message")
      state$status_msg = paste(state$status_msg, "Copy to network success.", sep="\n")
    }, error = function(e) {
      showNotification("Copy faiure, see status log.", 
                       type = "message")
      state$status_msg <- paste(state$status_msg, "\n", "Error archiving:", e$message, sep=" ")
      })
  })
  
  observeEvent(input$extract_local, {
    req(state$localSQLiteDB, state$archivePath)
    tryCatch({
      msg = extract_csvs_from_sqlite(state$localSQLiteDB, state$archivePath, overwrite = input$overwrite_mode)
      showNotification("Extract success.", 
                       type = "message")
      state$status_msg <- paste(state$status_msg, "\n", "Copy files from:", state$localSQLiteDB, "\n", sep=" ") 
      state$status_msg <- paste(state$status_msg, "\n", "into:", state$archivePath, "\n", sep=" ") 
      state$status_msg <- paste(state$status_msg, "\n", msg, "\n", sep="")
      state$status_msg <- paste(state$status_msg, "Extraction complete!", sep="\n")
    }, error = function(e) {
      showNotification("Extract Faild - see status log.", 
                       type = "message")
      state$status_msg <- paste(state$status_msg, "Error extracting:", e$message, sep=" ")
    })
  })
  
  output$status <- renderText(state$status_msg)
  
  # ------------ SQLite File Trees (shinyTree)
  output$local_sqlite_tree <- renderTree({
    sqlite_tree(state$localSQLiteDB)
  })
  
  output$remote_sqlite_tree <- renderTree({
    sqlite_tree(state$remoteSQLiteDB)
  })
}

shinyApp(ui, server)