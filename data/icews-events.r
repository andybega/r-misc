##
##  Create behavioral variables with ICEWS for G&W panel
##  AB
##  February 2015
##


library(lubridate)
library(plyr)


if (db.flag) {
  source("mysql_setup.R")
  conn <- mysql_setup(user="", pw="")
}



# Helper functions --------------------------------------------------------
#
#   Code quad event codes and function to extract relevant event counts from
#   DB table
#


sqlList <- function(char.vector) {
	# Format as SQL char list
	paste0("('", paste(char.vector, collapse="', '"), "')")
}

quadEvents <- function(cameo.codes, source.sector, target.sector) {
	# Gets relevant event counts for quad category from db table for 
	# relevant sector interactions, e.g. gov to gov.
	sql <- paste0("
SELECT c.cowcode,
       Extract(YEAR_MONTH FROM s.event_date) AS date,
       count(*) AS events
FROM   simple_events s
JOIN   eventtypes t USING(eventtype_id)
JOIN   locations l USING(location_id)
JOIN   countries c ON c.id = l.country_id
WHERE  s.event_date >= '1990-12-31' 
  AND  Substring(t.code, 1, 2) IN ", cameo.codes, "
  AND  s.source_country_id = s.target_country_id
  AND  s.source_actor_id IN
       (SELECT dsm.actor_id
        FROM   dict_sector_mappings dsm
        JOIN   dict_sectors ds USING(sector_id)
        WHERE  ds.sector_type_id = ", source.sector, ")
  AND  s.target_actor_id IN
       (SELECT dsm.actor_id
        FROM   dict_sector_mappings dsm
        JOIN   dict_sectors ds USING(sector_id)
        WHERE  ds.sector_type_id = ", target.sector, ")
GROUP  BY c.cowcode, Extract(YEAR_MONTH FROM s.event_date);")
	res <- dbGetQuery(conn, sql)
	res$date <- as.Date(paste0(as.character(res$date), "01"), 
		format="%Y%m%d")
	return(res)
}

icewsToGW <- function(data) {
	# Fix ICEWS country codes for merge to G&W codes.
	# Only works post 2001-03
	cond <- data$cowcode==995 & data$date >= as.Date("2006-06-05")
	data$cowcode[cond] <- 340  # Serbia
	cond <- data$cowcode==995 & data$date < as.Date("2006-06-05")
	data$cowcode[cond] <- 345  # Serbian & Montenegro
	data$cowcode[data$cowcode==255] <- 260  # Germany
	data$cowcode[data$cowcode==679] <- 678  # Yemen
	data$cowcode[data$cowcode==994] <- 626  # South Sudan

	return(data)
}

getIcewsQuad <- function(quad, source.sector, target.sector) {
	# quad: verb.coop, verb.conf, matl.coop, matl.confl
	# sector: government, ethnic, religious, business
	if (!exists("conn")) stop("No MySQL connection ('conn')")

	# Get SQL list of roots codes for quad category
	quad.root.codes <- list(	
		verb_coop="('01', '02', '03', '04', '05')",
		matl_coop="('06', '07', '08')",
		verb_conf="('09', '10', '11', '12', '13')",
		matl_conf="('14', '15', '16', '17', '18', '19', '20')"
	)
	cameo.codes <- quad.root.codes[[quad]]

	# Get source sector type
	sector.types <- list(rel=1, gov=2, dis=3, bus=4, oth=5)
	source.sector.code <- sector.types[[source.sector]]
	target.sector.code <- sector.types[[target.sector]]

	# Get counts from DB
	counts <- quadEvents(cameo.codes, source.sector.code, target.sector.code)

	# Fix ccodes
	counts <- icewsToGW(counts)

	# Lag by 1 month
	require(lubridate)
	month(counts$date) <- month(counts$date) + 1

	# Prepare data for merge
	counts$id <- paste(counts$date, counts$cowcode)
	counts <- counts[, c("id", "events")]
	colnames(counts)[2] <- paste0("i_", quad, "_", toupper(source.sector), "t", 
		toupper(target.sector), "_l1")

	return(counts)
}



# Country and monthly totals ----------------------------------------------
#
#   Event totals for normalizing
#


if (file.exists("raw/icews-totals.rda") & !db.flag) {
  cat("Loading ICEWS totals\n")
	load("raw/icews-totals.rda") 
} else {
	cat("Rebuilding ICEWS country totals\n")
	# Country totals
	dbSendQuery(conn, "DROP TABLE IF EXISTS my_tables.icews_by_country;")
	sql <- "
CREATE TABLE my_tables.icews_by_country AS
SELECT c.cowcode AS cowcode, 
       count(*) AS count
FROM   events e
JOIN   locations l USING(location_id)
JOIN   countries c ON c.id = l.country_id
WHERE  event_date >= '1990-12-31'
GROUP  BY c.cowcode;"
	dbGetQuery(conn, sql)
	country.events <- dbGetQuery(conn, "SELECT * FROM my_tables.icews_by_country;")
	dbSendQuery(conn, "DROP TABLE my_tables.icews_by_country;")

	#country.events$cowcode[!country.events$cowcode %in% unique(ilc_data$gwcode)]

	# Month totals
	cat("Rebuilding ICEWS month totals")
	dbSendQuery(conn, "DROP TABLE IF EXISTS my_tables.icews_by_month;")
	sql <- "
CREATE TABLE my_tables.icews_by_month AS 
SELECT EXTRACT(YEAR_MONTH FROM event_date) AS month, 
        count(*) AS count
FROM   events
GROUP  BY EXTRACT(YEAR_MONTH FROM event_date);"
	dbGetQuery(conn, sql)
	monthly.events <- dbGetQuery(conn, "SELECT * FROM my_tables.icews_by_month;")
	dbSendQuery(conn, "DROP TABLE my_tables.icews_by_month;")

	monthly.events$month <- paste0(as.character(monthly.events$month), "01")
	monthly.events$month <- as.Date(monthly.events$month, format="%Y%m%d")

	# get rid of events in 1970
	monthly.events <- monthly.events[2:nrow(monthly.events), ]
	save(country.events, monthly.events, file="raw/icews-totals.rda")
}

# Take out NA country code
country.events <- country.events[complete.cases(country.events), ]

# Recode for merge
country.events$cowcode[country.events$cowcode==255] <- 260  # Germany
country.events$cowcode[country.events$cowcode==679] <- 678  # Yemen
country.events$cowcode[country.events$cowcode==994] <- 626  # South Sudan
country.events$cowcode[country.events$cowcode==995] <- 345  # Yugoslavia
# Add row for Serbia
country.events <- rbind(country.events,
	c(340, country.events$count[country.events$cowcode==345]))

colnames(country.events) <- c("gwcode", "ctry_total_events")

# Merge with base data
cat("Merging ICEWS country totals\n")
ilc_data <- join(ilc_data, country.events, by="gwcode")

# Add monthly event totals, for normalization
month(monthly.events$month) <- month(monthly.events$month) + 1
colnames(monthly.events) <- c("date", "events_by_mth_l1")
ilc_data <- join(ilc_data, monthly.events, by="date")



# Protest counts ----------------------------------------------------------



# Create table for protest counts
if (file.exists("raw/icews-protest.rda") & !db.flag) {
	cat("Loading ICEWS protest count\n")
	load("raw/icews-protest.rda")
} else {
	cat("Rebuilding ICEWS protest data\n")
	dbSendQuery(conn, "DROP TABLE IF EXISTS my_tables.ab_temp;")
	sql <- "
CREATE TABLE my_tables.ab_temp AS
SELECT c.cowcode,
	   Extract(YEAR_MONTH FROM s.event_date) AS date,
	   count(*) AS events
FROM   simple_events s
JOIN   eventtypes t USING(eventtype_id)
JOIN   locations l USING(location_id)
JOIN   countries c ON c.id = l.country_id
WHERE  s.event_date >= '1990-12-31'
  AND  Substring(t.code, 1, 2) = '14'
  AND  s.target_actor_id IN
       (SELECT dsm.actor_id
		FROM   dict_sector_mappings dsm
		JOIN   dict_sectors ds USING(sector_id)
		WHERE  ds.sector_type_id = 2)
GROUP  BY c.cowcode, Extract(YEAR_MONTH FROM s.event_date);"
	dbGetQuery(conn, sql)
	prot <- dbGetQuery(conn, "SELECT * FROM my_tables.ab_temp")
	dbSendQuery(conn, "DROP TABLE IF EXISTS my_tables.ab_temp;")
	prot$date <- as.Date(paste0(as.character(prot$date), "01"), 
		format="%Y%m%d")
	# Fix GW codes
	prot <- icewsToGW(prot)
	# Lag by 1 month
	month(prot$date) <- month(prot$date) + 1 
  
  # Drop last month if incomplete data; <= because we lagged protest var
  last_month <- dbGetQuery(conn, "SELECT Extract(YEAR_MONTH FROM date) FROM (SELECT max(event_date) AS date FROM simple_events) s1;")
  prot <- prot[prot$date <= as.Date(paste0(last_month, "01"), format="%Y%m%d"), ]

	# Create id and rename vars
	prot$id <- paste(prot$date, prot$cowcode)
	prot <- prot[, c("id", "events")]
	colnames(prot)[2] <- "i_protest_tGOV_l1"
	save(prot, file="raw/icews-protest.rda")
}

#prot$id[!prot$id %in% ilc_data$id]
cat("Joining ICEWS protest data\n")
ilc_data <- join(ilc_data, prot, by="id")
ilc_data[is.na(ilc_data$i_protest_tGOV_l1), "i_protest_tGOV_l1"] <- 0

# check series for turkey protests
# ilc_data[ilc_data$country=="Turkey" & !is.na(ilc_data$country), c("id", "protest.tGOV.l1", "i_protest_tGOV_l1")]



# Pull and merge quad variables -------------------------------------------
#
#   AKA behavioral variables
#


quads <- c("verb_coop", "verb_conf", "matl_coop", "matl_conf")

for (i in seq_along(quads)) {
  cat(paste0(quads[i], "\n"))
  
  # gov to gov
  cat("gov to gov\n")
  secs <- c("gov", "gov")
  file_nm <- paste0("raw/quads/", quads[i], "_", paste(toupper(secs), collapse="t"), ".rda")
  if (file.exists(file_nm) & !db.flag) {
    load(file_nm) 
  } else {
    data <- getIcewsQuad(quads[i], secs[1], secs[2])
    save(data, file=file_nm)
  }
  ilc_data <- join(ilc_data, data, by="id")
  ilc_data[is.na(ilc_data[, colnames(data)[2]]), colnames(data)[2]] <- 0
  
  # dis to dis
  cat("dis to dis\n")
  secs <- c("dis", "dis")
  file_nm <- paste0("raw/quads/", quads[i], "_", paste(toupper(secs), collapse="t"), ".rda")
  if (file.exists(file_nm) & !db.flag) {
    load(file_nm) 
  } else {
    data <- getIcewsQuad(quads[i], secs[1], secs[2])
    save(data, file=file_nm)
  }
  ilc_data <- join(ilc_data, data, by="id")
  ilc_data[is.na(ilc_data[, colnames(data)[2]]), colnames(data)[2]] <- 0
  
  # gov to dis
  cat("gov to dis\n")
  secs <- c("gov", "dis")
  file_nm <- paste0("raw/quads/", quads[i], "_", paste(toupper(secs), collapse="t"), ".rda")
  if (file.exists(file_nm) & !db.flag) {
    load(file_nm) 
  } else {
    data <- getIcewsQuad(quads[i], secs[1], secs[2])
    save(data, file=file_nm)
  }
  ilc_data <- join(ilc_data, data, by="id")
  ilc_data[is.na(ilc_data[, colnames(data)[2]]), colnames(data)[2]] <- 0
  
  # dis to gov
  cat("dis to gov\n")
  secs <- c("dis", "gov")
  file_nm <- paste0("raw/quads/", quads[i], "_", paste(toupper(secs), collapse="t"), ".rda")
  if (file.exists(file_nm) & !db.flag) {
    load(file_nm) 
  } else {
    data <- getIcewsQuad(quads[i], secs[1], secs[2])
    save(data, file=file_nm)
  }
  ilc_data <- join(ilc_data, data, by="id")
  ilc_data[is.na(ilc_data[, colnames(data)[2]]), colnames(data)[2]] <- 0
  
  # rel to gov
  cat("rel to gov\n")
  secs <- c("rel", "gov")
  file_nm <- paste0("raw/quads/", quads[i], "_", paste(toupper(secs), collapse="t"), ".rda")
  if (file.exists(file_nm) & !db.flag) {
    load(file_nm) 
  } else {
    data <- getIcewsQuad(quads[i], secs[1], secs[2])
    save(data, file=file_nm)
  }
  ilc_data <- join(ilc_data, data, by="id")
  ilc_data[is.na(ilc_data[, colnames(data)[2]]), colnames(data)[2]] <- 0
  
  # gov to rel
  cat("gov to rel\n")
  secs <- c("gov", "rel")
  file_nm <- paste0("raw/quads/", quads[i], "_", paste(toupper(secs), collapse="t"), ".rda")
  if (file.exists(file_nm) & !db.flag) {
    load(file_nm) 
  } else {
    data <- getIcewsQuad(quads[i], secs[1], secs[2])
    save(data, file=file_nm)
  }
  ilc_data <- join(ilc_data, data, by="id")
  ilc_data[is.na(ilc_data[, colnames(data)[2]]), colnames(data)[2]] <- 0
}


#
#   Done, disconnect
#
if (db.flag) dbDisconnect(conn)