# HYDAT
R package for reading from WSC HYDAT databases

## Installation instructions

### Dependencies
**HYDAT** depends on several other packages, which you need to install first, from CRAN, before installing **HYDAT**.

These packages are:
    reshape2,
    lubridate,
    DBI
    
In addition, you need to install the package **RSQLite** in order to actually use HYDAT.

### Installing HYDAT
You can download the complete package, as well as the manual .pdf, by clicking on **releases**. However, you can download and install the most up-to-date version directly from this repository. The procedure is
1. Install the package "devtools" - you only have to do this once. Note that this will also install several dependancies
2. Load the devtools library
3. Install the package.

The commands are:

	install.packages("devtools")
	library(devtools)
	install_github("CentreForHydrology/HYDAT")
	
## Using HYDAT

### Downloading data
**HYDAT** uses SQLite databases of hydrological data from Environment Canada. You can get the most recent database from https://ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1.
Be sure to download the file named "Hydat_sqlite3_YYYYMMDD.zip" where YYYMMDD is the date of the file. Once the file is downloaded, extract the SQLite database file Hydat.sqlite3.

### Using the HYDAT R commands
Getting **HYDAT** data in R takes 3 steps:
1. Connecting to the HYDAT database
2. Retrieving the data
3. Closing the connection.

Steps 1 and 3 require commands from the **RSQLite** package; step 2 requires a **HYDAT** command. This example extracts daily flow values for a given station.

	library(RSQLite)
	library(HYDAT)
	HYDATfile <- 'Hydat.sqlite3'
	WSCstation <- '05HG001'  # South Sask. River at Saskatoon
	HYDAT <- dbConnect(SQLite(), HYDATfile)
	
	dailyvals <- DailyHydrometricData(con = HYDAT, 
	               get_flow = TRUE, 
	               station_number = WSCstation)
	
	dbDisconnect(HYDAT)
  