
#' @title Get mean annual hydrometric data for selected station(s)  
#' @description  Gets the mean annual hydrometric series (flow or level) for 
#'    one or more stations. Flow units in m3/s; water level units in metres.
#' @param con An open SQLite database connection to the HYDAT database
#' @param get_flow Logical. If TRUE the function returns mean annual discharge,
#'    otherwise the function returns mean annual water level
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, year, mean)
#' @seealso \code{\link{MonthlyHydrometricData}}
#' @seealso \code{\link{DailyHydrometricData}}
#' @importFrom DBI dbGetQuery
#' @export
AnnualHydrometricData <- function(con, get_flow=TRUE, station_number) {
  type = ifelse(get_flow, "Q", "H")
  sqlString <- c("SELECT STATION_NUMBER, YEAR, MEAN ",
                 "FROM ANNUAL_STATISTICS WHERE DATA_TYPE=\'%s\' AND ",
                 "STATION_NUMBER in (%s)")
  sqlString[2] <- sprintf(sqlString[2], type)
  sqlString[3] <- sprintf(sqlString[3], 
						  paste(
							sprintf("\'%s\'", station_number), 
							collapse=","))
  sqlString <- paste(sqlString, collapse="")
  
  data <- dbGetQuery(con, sqlString)
  colnames(data) <- tolower(colnames(data))
  
  return(data)
}
