#' @title Gets mean daily hydrometric data for selected station(s)
#' @description  Gets the mean daily hydrometric series (flow or level) for 
#'    one or more stations. Flow units in m3/s; water level units in metres.
#' @param con An open SQLite database connection to the HYDAT database
#' @param get_flow Logical. If TRUE the function returns mean daily discharge,
#'    otherwise the function returns mean daily water level
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, date, value, symbol)
#' @note Symbols associated with mean daily hydrometric series are as follows
#'  "A" - manual gauge
#'  "B" - ice affected 
#'  "D" - dry 
#'  "E" - estimated
#' @seealso \code{\link{AnnualHydrometricData}} for mean annual data, 
#' \code{\link{MonthlyHydrometricData}} for mean monthly data, 
#' \code{\link{DailySedimentData}} for mean daily sediment load/concentration,
#' \code{\link{DataSymbols}} for information on symbols associated with mean daily data, and
#' \code{\link{ReportedWaterLevelPrecision}} for information on water level precision
#' @importFrom reshape2 melt
#' @importFrom lubridate leap_year
#' @importFrom DBI dbGetQuery
#' @export
DailyHydrometricData <- function(con, get_flow=TRUE, station_number) {
  
  # determine database table to pull data from (flows or levels)
  tableName <- ifelse(get_flow, "DLY_FLOWS", "DLY_LEVELS")
  
  # prepare SQL command
  sqlString <- c("SELECT * FROM %s ",
                 "WHERE STATION_NUMBER in (%s)")
  sqlString[1] <- sprintf(sqlString[1], tableName)
  sqlString[2] <- sprintf(sqlString[2], 
                          paste(sprintf("\'%s\'", station_number), collapse=","))
  sqlString <- paste(sqlString, collapse="")
  
  data <- dbGetQuery(con, sqlString) 
  
  if(nrow(data)==0){
    warning("No daily hydrometric records found!")
    return(NULL)
  }
  
  # find appropriate columns for values
  header <- ifelse(get_flow, "^FLOW\\d+", "^LEVEL\\d+")
  value.cols <- colnames(data)[grep(header, colnames(data))]
  header <- ifelse(get_flow, "^FLOW_SYMBOL\\d+", "^LEVEL_SYMBOL\\d+")
  flag.cols <- colnames(data)[grep(header, colnames(data))]
  
  # now melt the data frame for data and flags
  dly.data <- melt(data, id.vars=c("STATION_NUMBER", "YEAR", "MONTH"), 
                   measure.vars=value.cols)
  dly.flags <- melt(data, id.vars=c("STATION_NUMBER", "YEAR", "MONTH"), 
                    measure.vars=flag.cols)
  
  # extract flag 'value' and attach to data
  dly.data$FLAG <- dly.flags$value
  
  # we now need to extract the day from the 'variable' column in the data frame
  if(get_flow) {
      dly.data$DAY <- as.numeric(substr(dly.data$variable, 5, 6))
  } else {
    dly.data$DAY <- as.numeric(substr(dly.data$variable, 6, 7))
  }
 
  # censor ambiguous dates (e.g., 31st day for Apr, Jun, Sept, Nov)
  d <- which((dly.data$MONTH %in% c(4,6,9,11)) & (dly.data$DAY > 30))
  if(length(d) > 0) {
    dly.data <- dly.data[-d,]
    dly.flags <- dly.flags[-d,]
  } 
  
  # censor ambiguous dates in February
  d <- which((dly.data$MONTH == 2) & leap_year(dly.data$YEAR) & (dly.data$DAY > 29))
  if(length(d)> 0) {
    dly.data <- dly.data[-d,]
  }
  
  d <- which((dly.data$MONTH == 2) & !leap_year(dly.data$YEAR) & (dly.data$DAY > 28))
  if(length(d)>0){
    dly.data <- dly.data[-d,]
  }
  
  # coerce date
  dly.data$DATE <- as.Date(sprintf("%04d-%02d-%02d", 
                           dly.data$YEAR, dly.data$MONTH, 
                           dly.data$DAY))


  # sort the data frame by ascending DATE
  dly.data <- dly.data[order(dly.data$DATE),]
  
  return(
    data.frame(
      station_number = dly.data$STATION_NUMBER, #as.character(dly.data$STATION_NUMBER),
      date = dly.data$DATE,
      value = dly.data$value,
      symbol = as.character(dly.data$FLAG),
      stringsAsFactors = FALSE
      )
    )
  
}


#' @title Gets mean daily sediment data for selected station(s)
#' @description Gets the mean daily suspended sediment (concentration or load)
#'  for one or more stations. Concentrations are measured in units of mg/L. Load 
#'  are measured in tonnes (mean daily conc * mean daily flow * 0.0864). 
#' @param con An open SQLite database connection to the HYDAT database
#' @param get_load Logical. If TRUE the function returns mean daily load,
#'    otherwise the function returns mean daily concentration.
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, date, value). 
#' @note Symbols associated with mean daily sediment series are as follows:
#' <<need further work here>>
#' @seealso \code{\link{DailyHydrometricData}} for mean daily hydrometric data,
#' @seealso \code{\link{DataSymbols}} for information on symbols associated with mean daily data
#' @importFrom reshape2 melt
#' @importFrom lubridate leap_year
#' @importFrom DBI dbGetQuery
#' @export
DailySedimentData <- function(con, get_load=TRUE, station_number) {

  tableName <- ifelse(get_load, "SED_DLY_LOADS", "SED_DLY_SUSCON")
  
  # prepare SQL command
  sqlString <- c("SELECT * FROM %s ",
                 "WHERE STATION_NUMBER in (%s)")
  sqlString[1] <- sprintf(sqlString[1], tableName)
  sqlString[2] <- sprintf(sqlString[2], 
                          paste(sprintf("\'%s\'", station_number), collapse=","))
  sqlString <- paste(sqlString, collapse="")
  
  data <- dbGetQuery(con, sqlString)
  
  # find appropriate columns for values
  header <- ifelse(get_load, "^LOAD\\d+", "^SUSCON\\d+")
  value.cols <- colnames(data)[grep(header, colnames(data))]
  
  # now melt the data frame for data values
  dly.data <- melt(data, id.vars=c("STATION_NUMBER", "YEAR", "MONTH"), 
                   measure.vars=value.cols)
  
  # symbols/flags only exist for suspended sediment
  if(!get_load) {
    flag.cols <- colnames(data)[grep("^SUSCON_SYMBOL\\d+", colnames(data))]
    dly.flags <- melt(data, id.vars=c("STATION_NUMBER", "YEAR", "MONTH"), 
                      measure.vars=flag.cols)
    # extract flag 'value' and attach to data
    dly.data$FLAG <- dly.flags$value
  }
  
  # we now need to extract the day from the 'variable' column in the data frame
  dly.data$DAY <- as.numeric(gsub(ifelse(get_load,"LOAD","SUSCON"), 
                                  "", dly.data$variable))
  
  # censor ambiguous dates (e.g., 31st day for Apr, Jun, Sept, Nov)
  d <- which((dly.data$MONTH %in% c(4,6,9,11)) & (dly.data$DAY > 30))
  dly.data <- dly.data[-d,]
  
  # censor ambiguous dates in February
  d <- which((dly.data$MONTH == 2) & leap_year(dly.data$YEAR) & (dly.data$DAY > 29))
  if(length(d)> 0) {
    dly.data <- dly.data[-d,]
  }
  
  d <- which((dly.data$MONTH == 2) & !leap_year(dly.data$YEAR) & (dly.data$DAY > 28))
  if(length(d)>0){
    dly.data <- dly.data[-d,]
  }
  
  # coerce date
  dly.data$DATE <- as.Date(sprintf("%04d-%02d-%02d", 
                                   dly.data$YEAR, dly.data$MONTH, 
                                   dly.data$DAY))
  
  # sort the data frame by ascending DATE
  dly.data <- dly.data[order(dly.data$DATE),]
  
  # create output data frame
  output <- data.frame(
              station_number = dly.data$STATION_NUMBER,
              date = dly.data$DATE,
              value = dly.data$value,
              stringsAsFactors = FALSE
            )
  
  # add flag if suspended sediment
  if(!get_load) {
    output$symbol = dly.data$FLAG
  }

  return(output)
  
}



