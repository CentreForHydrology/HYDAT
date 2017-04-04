
#' @title Get miscellaneous sediment data 
#' @description Gets sediment samples collected by Water Survey of Canada staff at
#' various station locations. Sediment samples are categorized based on the sampling
#' method (Environment Canada, 1988).\cr\cr
#' Depth-Integrating (DI) Method: Suspended sediment load estimated from 'n'
#' equal-discharge segments in which sediment is sampled vertically through the water
#' column at a uniform rate.\cr\cr
#' Point-Integating (PI) Method: Suspended sediment load estimated from graphical methods.\cr
#' Bed Material (BM): Samples of bed material often taken in same sampling verticals as
#' suspended sediment or at waters edge. The total   
#' 
#' obtained by either (1) Depth-Integrating
#' Method or (2) Single Suspended 
#' @param con an open SQLite database connection
#' @param stationID a vector of Water Survey of Canada station identifiers
#' @return A data frame 
#' @references Environment Canada, 1988. Miscellaneous Sediment Data 1966-1983, Inland 
#'  Waters Directorate, Water Resources Branch, Water Survey of Canada, Ottawa, Canada. 248 p.
#' @importFrom DBI dbGetQuery
#' @export
SedimentSamples <- function(con, stationID) {
  sqlString <- paste("SELECT a.*, t1.SED_DATA_TYPE_EN as SED_DATA_TYPE,",
                     "t2.SAMPLE_REMARK_EN as SAMPLE_REMARK,",
                     "t3.CONCENTRATION_EN AS CONCENTRATION",
                     "FROM SED_SAMPLES a",
                     "LEFT JOIN SED_DATA_TYPES t1 ON a.SED_DATA_TYPE=t1.SED_DATA_TYPE",
                     "LEFT JOIN SAMPLE_REMARK_CODES t2 ON a.SAMPLE_REMARK_CODE=t2.SAMPLE_REMARK_CODE",
                     "LEFT JOIN CONCENTRATION_SYMBOLS t3 ON a.CONCENTRATION_SYMBOL=t3.CONCENTRATION_SYMBOL",
                     ifelse(missing(stationID),"",
                            sprintf("WHERE STATION_NUMBER in (%s)",
                                    paste(sprintf("\'%s\'", stationID), collapse=","))),
                     collapse=" ")
  
  sedSamples <- dbGetQuery(con, sqlString)

  return(sedSamples)
  
}

#' ParticleSizeDistribution
#' @description Gets the particle size distribution (in millimetres) for sediment samples collected at various
#'  station locations
#' @param con an open SQLite database connection
#' @param stationID A vector of Water Survey of Canada station identifiers
#' @return A data frame
#' @importFrom DBI dbGetQuery
#' @export
ParticleSizeDistribution <- function(con, stationID) {
  sqlString <- paste("SELECT a.*, t1.SED_DATA_TYPE_EN as SED_DATA_TYPE_NAME",
                     "FROM SED_SAMPLES_PSD a",
                     "LEFT JOIN SED_DATA_TYPES t1 ON a.SED_DATA_TYPE=t1.SED_DATA_TYPE",
                     ifelse(missing(stationID),"",
                            sprintf("WHERE STATION_NUMBER in (%s)",
                                    paste(sprintf("\'%s\'", stationID), collapse=","))),
                     collapse=" ")
  
  psd <- dbGetQuery(con, sqlString)
  
  return(psd)
}

