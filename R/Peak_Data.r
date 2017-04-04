
#' @title Gets the annual max/min daily data for selected station(s)
#' @description Gets the annual maximum and minimum daily mean hydrometric 
#'    series (flow or level). Flow units in m3/s; water level units in metres.
#' @param con An open SQLite database connection to the HYDAT database
#' @param get_flow Logical. If TRUE the function returns the series of annual 
#'    max/min daily flow, otherwise the function return the series of annual
#'    max/min daily level for each station
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, peak, year, month day, value, flag)
#' @seealso \code{\link{AnnualInstantaneousPeakData}} for annual instantaneous
#'    records, see \code{\link{DataSymbols}} for information on symbols associated
#'    with peak values.
#' @importFrom DBI dbGetQuery
#' @export
AnnualPeakData <- function(con, get_flow=TRUE, station_number){
  
  type = ifelse(get_flow, "Q", "H")
  
  # prepare SQL command
  sqlString <- c("SELECT * FROM ANNUAL_STATISTICS WHERE ",
                 "DATA_TYPE=\'%s\' ",
                 "AND STATION_NUMBER in (%s)")                         
  
  sqlString[2] <- sprintf(sqlString[2], type)
  sqlString[3] <- sprintf(sqlString[3],
                          paste(sprintf("\'%s\'", station_number), collapse=","))
  sqlString <- paste(sqlString, collapse="")
  data <- dbGetQuery(con, sqlString)
  
  peaks <- rbind(
    data.frame(
      STATION_NUMBER=data$STATION_NUMBER,
      PEAK="MINIMUM",
      YEAR=data$YEAR,
      MONTH=data$MIN_MONTH,
      DAY=data$MIN_DAY,
      VALUE=data$MIN,
      SYMBOL=data$MIN_SYMBOL,
	  stringsAsFactors=FALSE
      ),
    data.frame(
      STATION_NUMBER=data$STATION_NUMBER,
      PEAK="MAXIMUM",
      YEAR=data$YEAR,
      MONTH=data$MAX_MONTH,
      DAY=data$MAX_DAY,
      VALUE=data$MAX,
      SYMBOL=data$MAX_SYMBOL,
	  stringsAsFactors=FALSE
      )
  )
  
  colnames(peaks) <- tolower(colnames(peaks))
  return(peaks)
  
}


#' @title Gets the annual max/min instantantaneous data for selected station(s)
#' @description Gets the annual max/min instantaneous hydrometric series 
#'  (flow or level). Data are usually defined to nearest minute in local standard time
#' 	  (see note below).
#' @param con An open SQLite database connection to the HYDAT database
#' @param get_flow Logical. If TRUE the function returns annual max/min
#'    instantaneous flow, otherwise the function return annual max/min 
#'    instantaneous level for each station
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @note In some cases, the timing of the instantaneous max/min may be
#'  uncertain due to equipment failure (e.g., recorder failure). Event 
#'  timing may be censored by the hydrographer to reflect this uncertainty. This
#'  may lead to NA values for day or hour of occurrence. The precision_code is
#'  only valid for water level series and refers to the precision (mm or m) of
#'  the water level measurement (see \code{\link{ReportedWaterLevelPrecision}}).
#' @return A data frame (station_number, year, peak_code, precision_code, month,
#'  day, hour, minute, timezone, peak). 
#' @seealso \code{\link{AnnualPeakData}} for annual maximum/minimum hydrometric 
#'  series. See \code{\link{DataSymbols}} for information on symbols associated
#'  with instantaneous peak values.
#' @importFrom DBI dbGetQuery
#' @export
AnnualInstantaneousPeakData <- function(con, get_flow=TRUE, station_number) {
  type = ifelse(get_flow, "Q", "H")
  
  # prepare SQL command
  sqlString <- c("SELECT * FROM ANNUAL_INSTANT_PEAKS WHERE ",
                 "DATA_TYPE=\'%s\' ",
                 "AND STATION_NUMBER in (%s)")                         
  
  sqlString[2] <- sprintf(sqlString[2], type)
  sqlString[3] <- sprintf(sqlString[3],
                          paste(sprintf("\'%s\'", station_number), collapse=","))
  sqlString <- paste(sqlString, collapse="")
  data <- dbGetQuery(con, sqlString)
  p <- which(colnames(data) == "DATA_TYPE")
  data <- data[,-p]
  
  p <- which(data$PEAK_CODE=="H")
  data$PEAK_CODE[p] <- "MAXIMUM"
  data$PEAK_CODE[-p] <- "MINIMUM"
  
  colnames(data) <- tolower(colnames(data))
  return(data)
}

