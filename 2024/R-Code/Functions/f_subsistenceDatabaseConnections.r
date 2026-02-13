# ##############################################################################
# Programmer: D. Koster
# Date: 02/2021
# File: f_subsistenceDatabaseConnections.r
# Purpose: Standardized place for connect to the SDS - includes necessary
#   connection parameters. Connection code refers here so that global changes
#   are reflected across all projects.
#
# Requires the odbc libarary to be imported.
#
#
# Changelog: [Programmer | Date | Change Decription | Template Update [Y|N] | [One-off]]
# Programmer:
#
#
# ##############################################################################

loadODBCLibrary = function(){
  library(odbc)
}

connectToSDS = function()
{
  connectSDS = odbc::dbConnect(odbc::odbc()
                  , driver = 'SQL Server'
                  , server = 'dfgjnusql-db71p'
                  , database = 'Sub_SDS'
                  , .connection_string = "Trusted_Connection=True;"
                  , timeout = 10
                  , encoding = "UTF-8")
  
  return(connectSDS)
}

connectToASFDB = function()
{
  connectASFDB = odbc::dbConnect(odbc::odbc()
                         , driver = 'SQL Server'
                         , server = 'dfgjnusql-db71p'
                         , database = 'Sub_ASFDB'
                         , .connection_string = "Trusted_Connection=True;"
                         , timeout = 10
                         , encoding = "UTF-8")
  
  return(connectASFDB)
}

connectToCSIS = function()
{
  connectCSIS = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = 'dfgjnusql-db71p'
                           , database = 'Sub_CSIS'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8")
  
  return(connectCSIS)
}

connectToKuskokwim = function()
{
  connectKusko = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = 'dfgjnusql-db71p'
                           , database = 'Sub_Kuskokwim'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8") 
    
  return(connectKusko)
}

connectToHalibut = function()
{
  connectHalibut = odbc::dbConnect(odbc::odbc()
                             , driver = 'SQL Server'
                             , server = 'dfgjnusql-db71p'
                             , database = 'Sub_Halibut'
                             , .connection_string = "Trusted_Connection=True;"
                             , timeout=10
                             , encoding = "UTF-8")
  return(connectHalibut)
}

connectToHHDB = function()
{
  connectKusko = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = 'dfgjnusql-db71p'
                           , database = 'Sub_Survey_Household'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8") 
  
  return(connectKusko)
}


connectToCSHDB = function()
{
  connectCSH = odbc::dbConnect(odbc::odbc()
                           , driver = 'SQL Server'
                           , server = 'dfgjnusql-db71p'
                           , database = 'Sub_CSH'
                           , .connection_string = "Trusted_Connection=True;"
                           , timeout = 10
                           , encoding = "UTF-8") 
  
  return(connectCSH)
}

connectToBBPermit = function()
{
  connectBBPerm = odbc::dbConnect(odbc::odbc()
                               , driver = 'SQL Server'
                               , server = 'dfgjnusql-db71p'
                               , database = 'Sub_Permit_BB'
                               , .connection_string = "Trusted_Connection=True;"
                               , timeout = 10
                               , encoding = "UTF-8") 
  
  return(connectBBPerm)
}