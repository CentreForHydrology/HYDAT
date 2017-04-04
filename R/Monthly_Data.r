
#' @title Get mean monthly hydrometric data for selected station(s) 
#' @description Gets the mean monthly hydrometric series (flow or levels) for 
#'    one or more stations. Flow units in m3/s; water level units in metres.
#' @param con An open SQLite database connection to the HYDAT database
#' @param get_flow Logical. If TRUE the function returns mean annual discharge,
#'    otherwise the function returns mean annual water level
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, year, month, monthly_mean, monthly_total)
#' @seealso \code{\link{AnnualHydrometricData}}
#' @seealso \code{\link{DailyHydrometricData}}
#' @note monthly_total is the total of the mean daily flow for each month that has 
#'    complete data whereas monthly_mean is the average of the mean daily flow for 
#'    each month that has complete data.
#' @importFrom DBI dbGetQuery
#' @export
MonthlyHydrometricData <- function(con, get_flow=TRUE, station_number) {
  
  table <- ifelse(get_flow, "DLY_FLOWS", "DLY_LEVELS")
  
  # prepare SQL command
  sqlString <- c("SELECT STATION_NUMBER, YEAR, MONTH, MONTHLY_MEAN, MONTHLY_TOTAL ",
                 "FROM %s ", 
                 "WHERE STATION_NUMBER in (%s);")                         
  sqlString[2] <- sprintf(sqlString[2], table)
  sqlString[3] <- sprintf(sqlString[3],
                          paste(sprintf("\'%s\'", station_number), collapse=","))
  sqlString <- paste(sqlString, collapse="")
  
  # execute query
  data <- dbGetQuery(con, sqlString)
  
  colnames(data) <- tolower(colnames(data))
  return(data)
}

#' @title Get mean monthly sediment data for selected station(s) 
#' @description Gets the mean monthly sediment (suspended or bedload) for one or 
#'    more stations within the Water Survey of Canada hydrometric database (HYDAT).
#'    Suspended load measured in units of mg/l. Bedload measured in units of tonnes. 
#' @param con An open SQLite database connection to the HYDAT database 
#' @param get_load logical; TRUE to get bedload, FALSE to get suspended load
#' @param station_number a vector of Water Survey of Canada station identifiers
#' @importFrom DBI dbGetQuery
#' @export
MonthlySediment <- function(con, get_load=TRUE, station_number) {
  
  # concatenate SQL string based on parameters
  sqlString <- c("SELECT STATION_NUMBER, YEAR, MONTH,",
                 ifelse(get_load, "MONTHLY_MEAN,",""),
                 "MONTHLY_TOTAL",
                 "FROM", 
                 ifelse(get_load, "SED_DLY_LOADS", "SED_DLY_SUSCON"),
                 "WHERE STATION_NUMBER in (%s)")
  sqlString <- paste(sqlString, collapse=" ")
  sqlString <- sprintf(sqlString, paste(sprintf("\'%s\'", station_number), collapse=","))
  
  # Execute query
  data <- dbGetQuery(con, sqlString)
  
  colnames(data) <- tolower(colnames(data))
  
  return(data)
  
}
