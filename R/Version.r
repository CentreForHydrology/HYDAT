


#' @title Gets current version of the HYDAT database 
#' @description Determines the version and publication date 
#'  of the HYDAT database
#' @param con An open SQLite database connection to the HYDAT database
#' @return A data frame (version, date)
#' @importFrom DBI dbGetQuery
#' @export
Version <- function(con){
  sqlString <- "SELECT * FROM VERSION"
  version <- dbGetQuery(con, sqlString)  
  colnames(version) <- tolower(colnames(version))
  version$date <- as.POSIXct(version$date)
  return(version[1,])
}
