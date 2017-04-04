
# first cut function to go to website and determine which
# is the latest version of HYDAT available
LatestHYDATversion <- function(url = "http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/") {
  
  library(xml2)
  
  x <- read_xml(url, as_html=TRUE)
  y <- xml_find_all(x, "//a")
  files <- xml_attr(y, "href")
  
  # now find one with sqlite3 
  # filename: HYDAT_sqlite3_YYYYMMDD.zip
  p <- grep("sqlite3", files, ignore.case = TRUE)
  if(length(p) > 0) {
    db.file <- unlist(strsplit(files[p], "_"))[3]
    db.date <- unlist(strsplit(db.file, "\\."))[1]
    db.date <- as.Date(db.date, "%Y%m%d")
    
    # now what to return????
    
  } else {
    print("SQLite database file not found!")
  }
}

DownloadHYDAT <- function(url = "http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/", file, destfile) {
  
}