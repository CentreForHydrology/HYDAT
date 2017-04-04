
#' @title Gets regulation history for hydrometric station(s)
#' @description Extracts information on flow regulation history affecting a 
#'  hydrometric station.
#' @param con An open SQLite database connection to the HYDAT database
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, year_from, year_to, regulated) which
#'   outlines the period of regulation. If unregulated, then year_from will be 
#'   an NA value.
#' @details A hydrometric station is deemed regulated if the natural flow 
#'  regime is modified more than X% (Need to speak to Lynne!)
#' @importFrom DBI dbGetQuery
#' @export
StationRegulation <- function(con, station_number) {
  
  sqlString <- "SELECT * FROM STN_REGULATION WHERE STATION_NUMBER in (%s)"
  sqlString <- sprintf(sqlString, paste(sprintf("\'%s\'", station_number),
                       collapse=","))
  
  reg <- dbGetQuery(con, sqlString)
  
  # convert all column headers to lower case
  colnames(reg) <- tolower(colnames(reg))
  
  # convert the "Regulated" field to boolean
  reg$regulated <- ifelse(reg$regulated == 0, FALSE, TRUE)
  return(reg)
}

#' @title Gets list of variables available for hydrometric station(s) 
#' @description Gets information on the variables (water level, flow, and 
#'  sediment) collected at a station.
#' @param con An open SQLite database connection to the HYDAT database
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, data_type, sed_data_type, year_from,
#'  year_to, record_length, data_type_name, sed_data_type_name)
#' @importFrom DBI dbGetQuery  
#' @export
StationDataTypes <- function(con, station_number) {
 
  sqlString <- paste(
                "SELECT * FROM STN_DATA_RANGE",
                "LEFT JOIN DATA_TYPES ON STN_DATA_RANGE.DATA_TYPE=DATA_TYPES.DATA_TYPE",
                "LEFT JOIN SED_DATA_TYPES ON STN_DATA_RANGE.SED_DATA_TYPE=SED_DATA_TYPES.SED_DATA_TYPE",
                ifelse(missing(station_number),"",
                       sprintf("WHERE STATION_NUMBER in (%s)",
                               paste(sprintf("\'%s\'", station_number),
                                     collapse=","))),
                collapse=" "
               )
  
  dataTypes <- dbGetQuery(con, sqlString)
  # whack the french columns
  dataTypes <- dataTypes[, -grep("_FR$", colnames(dataTypes))]
  # whack the columns with ".1" (duplicates
  dataTypes <- dataTypes[, -grep("1$", colnames(dataTypes))]
  # change names from '_en' to '_name' for clarity
  colnames(dataTypes) <- gsub("_en$","_name", colnames(dataTypes), ignore.case = TRUE)
  
  # convert column names to lowercase
  colnames(dataTypes) <- tolower(colnames(dataTypes))
  return(dataTypes)
}

#' @title Gets data collection operations for selected hydrometric station(s)
#' @description Gets information on the station data collection schedules for 
#'  one or more station(s). This includes observation (recorder/manual) and 
#'  operation (seasonal/continuous) schedules for each variable recorded at 
#'  each station.
#' @param con An open SQLite database connection to the HYDAT database
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, year_from, year_to, data_type, 
#' measurement, operation)
#' @importFrom DBI dbGetQuery
#' @export
StationDataCollection <- function(con, station_number){
 
  sqlString <- paste(
    "SELECT t1.STATION_NUMBER, t1.YEAR_FROM, t1.YEAR_TO,",
      "t2.DATA_TYPE_EN, t3.MEASUREMENT_EN, t4.OPERATION_EN",
    "FROM STN_DATA_COLLECTION t1",
      "LEFT JOIN DATA_TYPES t2 ON t1.DATA_TYPE=t2.DATA_TYPE",
      "LEFT JOIN MEASUREMENT_CODES t3 ON t1.MEASUREMENT_CODE=t3.MEASUREMENT_CODE",
      "LEFT JOIN OPERATION_CODES t4 ON t1.OPERATION_CODE=t4.OPERATION_CODE",
      ifelse(missing(station_number), "", 
           sprintf("WHERE t1.STATION_NUMBER in (%s)",
                   paste(sprintf("\'%s\'", station_number), collapse=","))),
    collapse=" "
  )
  
  dataCollection <- dbGetQuery(con, sqlString)
  
  # housekeeping on returned column names
  colnames(dataCollection) <- tolower(colnames(dataCollection))
  colnames(dataCollection) <- gsub("_en$", "", colnames(dataCollection))
  
  return(dataCollection)
}

#' @title Gets relevant metadata for hydrometric station(s)
#' @description Gets relevant station metadata such as location, current
#'  operating status, real-time, and whether station is part of the Reference
#'  Hydrometric Basin Network (RHBN).
#' @param con An open SQLite database connection to the HYDAT database
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @return A data frame (station_number, station_name, prov_ter_loc, 
#' hydrometric_status, sediment_status, latitude, longitude, drainage_area_gross,
#' drainage_area_effective, rhbn, real_time, regional_office, contributor, 
#' operator, datum)
#' @importFrom DBI dbGetQuery
#' @export
StationMetadata <- function(con, station_number) {
  
  sqlString <- paste(
      "SELECT i.*, p1.REGIONAL_OFFICE_NAME_EN as REGIONAL_OFFICE,",
        "p2.AGENCY_EN as CONTRIBUTOR, p3.AGENCY_EN as OPERATOR, p4.DATUM_EN as DATUM",
      "FROM STATIONS i",
        "LEFT JOIN REGIONAL_OFFICE_LIST p1 ON i.REGIONAL_OFFICE_ID=p1.REGIONAL_OFFICE_ID",
        "LEFT JOIN AGENCY_LIST p2 ON i.CONTRIBUTOR_ID=p2.AGENCY_ID",
        "LEFT JOIN AGENCY_LIST p3 ON i.OPERATOR_ID=p3.AGENCY_ID",
        "LEFT JOIN DATUM_LIST p4 ON i.DATUM_ID=p4.DATUM_ID",
      ifelse(missing(station_number),"",
             sprintf("WHERE STATION_NUMBER in (%s)",
                     paste(sprintf("\'%s\'", station_number), collapse=","))),
      collapse=" "
    )
  
  metadata <- dbGetQuery(con, sqlString)

  # data frame housekeeping
  metadata$RHBN <- ifelse(metadata$RHBN==1, TRUE, FALSE)
  metadata$REAL_TIME <- ifelse(metadata$REAL_TIME==1, TRUE, FALSE)
  metadata$HYD_STATUS <- ifelse(metadata$HYD_STATUS=="A","ACTIVE", 
                                ifelse(metadata$HYD_STATUS=="D", "DISCONTINUED", NA))
  metadata$SED_STATUS <- ifelse(metadata$SED_STATUS=="A","ACTIVE", 
                                ifelse(metadata$SED_STATUS=="D", "DISCONTINUED", NA))
  metadata <- metadata[,-grep("_ID$", colnames(metadata))]  
  
  colnames(metadata) <- tolower(colnames(metadata))
  
  return(metadata)
}

#' @title Gets ancillary remarks regarding hydrometric station data
#' @description Provides further clarification on data stored in HYDAT database. 
#'  Remarks are somewhat variable, but can provide further clarification on 
#'  hydrometric series.
#' @param con An open SQLite database connection to the HYDAT database
#' @param station_number A vector of Water Survey of Canada station identifiers
#' @importFrom DBI dbGetQuery
#' @export
StationRemarks <- function(con, station_number) {
  sqlString <- paste(
          "SELECT t1.*, t2.REMARK_TYPE_CODE",
          "FROM STN_REMARKS t1",
              "LEFT JOIN STN_REMARK_CODES t2 ON",
              "t1.REMARK_TYPE_CODE=t2.REMARK_TYPE_CODE",
          ifelse(missing(station_number),"",
              sprintf("WHERE STATION_NUMBER in (%s)",
                      paste(sprintf("\'%s\'", station_number), collapse=","))),
          collapse=" ")
  
  remarks <- dbGetQuery(con, sqlString)
  
  remarks <- remarks[,-grep("CODE$", colnames(remarks))]
  remarks <- remarks[,-grep("FR$", colnames(remarks))]
  return(remarks)
}
