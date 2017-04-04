

#' ReportedWaterLevelPrecision
#' @description Returns stated precision of water level (cm or mm) for each year of
#'  published record for a given hydrometric station.
#' @param con an open SQLite database connection
#' @param stationID A vector of Water Survey of Canada station identifiers
#' @return A data frame
#' @importFrom DBI dbGetQuery
#' @export
ReportedWaterLevelPrecision <- function(con, stationID) {
  sqlString <- c("SELECT STATION_NUMBER, YEAR, PRECISION_EN FROM",
                 "(DLY_LEVELS LEFT JOIN PRECISION_CODES ON",
                 "DLY_LEVELS.PRECISION_CODE=PRECISION_CODES.PRECISION_CODE)",
                 "WHERE STATION_NUMBER in (%s)",
                 "GROUP BY STATION_NUMBER, YEAR, PRECISION_EN")
  sqlString <- paste(sqlString, collapse=" ")
  sqlString <- sprintf(sqlString, paste(sprintf("\'%s\'", stationID), collapse=","))
  
  # execute query
  precision <- dbGetQuery(con, sqlString)
  
  return(precision)
}

#' DataSymbols
#' @description Gets the description of data symbols applied to publication of daily hydrometric
#'  (flow,level) and suspended sediment concentrations
#' @param con An open SQLite database connection
#' @return A data frame  (SYMBOL_ID, DESCRIPTION)
#' @importFrom DBI dbGetQuery
#' @export
DataSymbols <- function(con) {
  sqlString <- "SELECT SYMBOL_ID, SYMBOL_EN as DESCRIPTION FROM DATA_SYMBOLS"
  symb <- dbGetQuery(con, sqlString)
  return(symb)
}

#' PrecisionCodes
#' @description Gets the description of precision codes of water levels.
#' @param con An open SQLite database connection
#' @return A data frame (PRECISION_CODE, DESCRIPTION)
#' @importFrom DBI dbGetQuery
#' @export
PrecisionCodes <- function(con) {
	sqlString <- "SELECT PRECISION_CODE, PRECISION_EN as DESCRIPTION FROM PRECISION_CODES"
	prec <- dbGetQuery(con, sqlString)
	return(prec)
}

