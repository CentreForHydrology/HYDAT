

#' @title Get list of available hydrometric stations with near real-time data
#' @description Gets the current list of hydrometric gauges available in 
#'  near real-time (within 6 hours) from the National Hydrological Service, 
#'  Environment and Climate Change Canada.
#' @param base_url Base URL to access data mart documentation
#' @param master_file Filename containing the list of available real-time stations 
#' @return A data frame (station_number, name, latitude, longitude, prov_terr_loc, timezone)
#' @examples
#' network <- RealTimeNetwork()
#' # show top 50 stations 
#' head(network, 50)
#' @seealso \code{\link{RealTimeData}}
#' @export
RealTimeNetwork <- function(base_url = "http://dd.weather.gc.ca/hydrometric/doc", 
                            master_file = "hydrometric_StationList.csv") {
  infile <- sprintf("%s/%s", base_url, master_file)
  data <- read.table(infile, sep=",", header=TRUE, stringsAsFactors=FALSE)
  colnames(data) <- c("station_number", "name", "latitude", "longitude",
                      "prov_terr_loc", "timezone")
  return(data)
}

#' @title Gets current available data for hydrometric station  
#' @description Gets near real-time (within 6 hours) hydrometric data for the 
#'  past 30 days for a particular hydrometric station operated by the National 
#'  Hydrological Service of Canada, Water Survey Division.
#' @param base_url Base URL to access the data mart
#' @param prov_terr_loc Two-digit provincial or territorial abbreviation in which 
#' the station is located
#'  (e.g., "BC")
#' @param station_number A seven-digit station identifier (e.g., "08MF005")
#' @seealso \code{\link{RealTimeNetwork}} 
#' @return A data frame (station_number, date_time, hg, hg_grade, hg_sym, hg_code,
#'  qr, qr_grade, qr_sym, qr_code)
#' @note Returned date/times are in Universal Time Coordinated (UTC). 
#' @examples
#' # get realtime data for Fraser River at Hope (08MF005)
#' data <- RealTimeData(prov_terr_loc="BC", station_number="08MF005")
#' plot(
#'   data$date_time, data$qr,
#'	 type="l", col="blue",
#'   xlab="DateTime [UTC]", ylab="Discharge")
#' @export
RealTimeData <- function(base_url = "http://dd.weather.gc.ca/hydrometric", prov_terr_loc, 
                         station_number) {
  # build URL
  type <- c("hourly", "daily")
  url <- sprintf("%s/csv/%s/%s", base_url, prov_terr_loc, type)
  infile <- sprintf("%s/%s_%s_%s_hydrometric.csv", url, prov_terr_loc, station_number, type)
  
  # define data frame column names - they are not returned
  # nicely from the data mart files
  colHeaders <- c("station_number", "date_time", "hg", "hg_grade", "hg_sym", "hg_code",
                  "qr", "qr_grade", "qr_sym", "qr_code")
  
  # get hourly file
  h <- try(read.table(infile[1], header=TRUE, sep=",", stringsAsFactors=FALSE))
  
  if(class(h)=="try-error") {
    stop(sprintf("Station [%s] cannot be found within Province/Territory [%s]...url not located %s",
                 station_number, prov_terr_loc, infile[1]))
  }
  colnames(h) <- colHeaders
  h$date_time <- gsub("([+-]\\d\\d)(:)","\\1", h$date_time)
  h$date_time <- as.POSIXct(strptime(h$date_time, "%FT%T%z", tz="UTC"))
  
  # get daily file
  d <- try(read.table(infile[2], header=TRUE, sep=",", stringsAsFactors=FALSE))
  colnames(d) <- colHeaders
  d$date_time <- gsub("([+-]\\d\\d)(:)","\\1", d$date_time)
  d$date_time <- as.POSIXct(strptime(d$date_time, "%FT%T%z", tz="UTC"))
  
  # now merge the hourly + daily (hourly data overwrites daily where dates are the same)
  p <- which(d$date_time < min(h$date_time))
  output <- rbind(d[p,], h)
  return(output)
}
