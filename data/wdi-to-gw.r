#
#   Add WDI data to a G&W state-month panel
#   Andreas Beger
#   
#   Works for 1991 - 2014
#   Country-month data using G&W state list
#
#   This will add WDI variables to a state panel using the G&W rules
#   for state system membership. Since WDI and G&W do not have the same
#   state system membership lists, some changes to the WDI country codes
#   are neccessary. I've checked this back to 1991 for consistency, but it will
#   probably take additional checking for dates prior to that.
#

date1 <- as.Date("1991-01-01")
date2 <- as.Date(as.Date(end_date))

source("state-panel.r")
gwlist <- state_panel(date1, date2, by="month", useGW=TRUE)
colnames(gwlist)[1] <- "gwcode"
# Will give warnings; not a problem. 
# Cshapes goes through 2013-06. Data from that month are used to generate
# state system membership past 2013-06. Not a problem since there have been
# no changes in state system, at least not through 2014-12.


library(countrycode)
library(ggplot2)
library(dplyr)
library(lubridate)
library(magrittr)
library(WDI)

# Get WDI data
ind_need <- c("AG.LND.TOTL.K2", "BX.KLT.DINV.CD.WD", "FP.CPI.TOTL", "FP.CPI.TOTL.ZG", 
              "IT.CEL.SETS.P2", "IT.NET.BBND.P2", "IT.NET.SECR.P6", "IT.NET.USER.P2", 
              "MS.MIL.TOTL.P1", "MS.MIL.XPND.GD.ZS", "NY.GDP.MKTP.KD", "NY.GDP.PCAP.KD", 
              "SH.DYN.MORT", "SP.DYN.LE00.FE.IN", "SP.DYN.LE00.MA.IN", "SP.POP.TOTL", 
              "ST.INT.ARVL", "ST.INT.RCPT.CD")

#foo <- names(ilc_data)[c(29, 32, 34:43, 45:50)]
#dput(gsub(".l1", "", foo))

upd_wdi <- WDIcache()
wdi <- WDI(indicator=ind_need, start = (year(date1)-1), end = year(date2))

wdi$ccode <- countrycode(wdi$iso2c, "iso2c", "cown")

unique(wdi$country[is.na(wdi$ccode)])

#   Fix state anomalies
#   ===================
#
#   Some codes are off. More important problem is the practice of back-dating
#   separate records for countries that split at some point. These need to be
#   recombined and averaged.

# Germany
wdi$ccode[wdi$ccode==255] <- 260

# ex-Yugoslavia
# problem: fill in missing codes for Serbia, Kosovo
# then, WDI does not have data for Yugoslavia/S+M, rather for seperate countries
# these need to be combined to appropriate series. 
# that's too much, so instead just recode Serbia before 2006 as stand in for
# rump Yugo / S+M
wdi$ccode[wdi$country=="Serbia"] <- 340
wdi$ccode[wdi$country=="Kosovo"] <- 347

# Take Serbia before 2006 and fix ccode to 345 as in G&W
sr2006 <- wdi[wdi$ccode==340 & wdi$year <= 2006 & !is.na(wdi$ccode), ]
sr2006$ccode <- 345
wdi <- rbind(wdi, sr2006)
rm(sr2006)

# Yemen
wdi$ccode[wdi$ccode==679] <- 678

# Sudan and South Sudan, need to be combined before 2011-07

# Taiwan?: not in IFS
'Taiwan' %in% wdi$country
unique(wdi$country[is.na(wdi$ccode)])

# Drop missing ccodes
wdi <- wdi[!is.na(wdi$ccode), ]


#   Impute missing values
#   =====================

library(sbgcop)

# We want data for 2015 as well, right now the data end 2014. So add more. 
wdi2015 <- wdi[wdi$year==2014, ]
wdi2015$year <- 2015
# Set substantive vars to NA
wdi2015[, grep("^[A-Z]{2}.", colnames(wdi2015))] <- NA
wdi <- rbind(wdi, wdi2015)
rm(wdi2015)

# Fraction of missing?
sum(is.na(wdi)) / (ncol(wdi) * nrow(wdi))

# Some varialbes are missing values, but this seems to be due to technological
# changes, and we can thus set them to 0 (broadband access before 2001)
wdi$IT.NET.BBND.P2[wdi$year < 2001 & is.na(wdi$IT.NET.BBND.P2)] <- 0
wdi$IT.NET.SECR.P6[wdi$year < 2001 & is.na(wdi$IT.NET.SECR.P6)] <- 0

# Check: USA tourism
with(wdi[wdi$ccode==2, ], plot(year, ST.INT.ARVL, ylim=c(0, 8e+07)))

# Fraction of missing?
sum(is.na(wdi)) / (ncol(wdi) * nrow(wdi))

# For cases in which only the latest year of data is missing, let's do linear
# extrapolation based on last 4 observed data point. SBGCOP tends to give 
# weird imputations for cases like this.

library(forecast)

wdi <- arrange(wdi, ccode, year)
val_cols <- ind_need
vals_upd <- 0
# loop over countries
for (i in unique(wdi$ccode)) {
  # loop over series for a country
  for (c in val_cols) {
    # check if only last value is missing, and at least four points before that
    # all vectors will have same length (square data)
    this_series <- wdi[wdi$ccode==i, c]
    # last 1 or 2 points missing
    ts_rle   <- rle(is.na(this_series))
    tail_n   <- tail(ts_rle$length, n=1)
    tail_na  <- isTRUE(tail(ts_rle$values, n=1))
    basis    <- isTRUE(tail(ts_rle$length, n=2)[1] > 4)
    if (tail_na & basis) {
      x <- forecast(ets(this_series, model="ZZZ"), h=tail_n)$mean
      idx <- (length(this_series) - (tail_n-1)):length(this_series)
      wdi[wdi$ccode==i, c][idx] <- x
      vals_upd = vals_upd + 1
    }
  }
}
message("WDI: Updated ", vals_upd, " values in series with missing last obs.")

# Visualizing different ETS models
#plot(wdi[wdi$ccode==2, "ST.INT.ARVL"])
#plot(forecast(ets(wdi[wdi$ccode==2, "ST.INT.ARVL"], model="ZZZ")))

# Check: USA tourism
with(wdi[wdi$ccode==2, ], plot(year, ST.INT.ARVL, ylim=c(0, 7e+07)))

# Fraction of missing?
sum(is.na(wdi)) / (ncol(wdi) * nrow(wdi))

# Missing by country for each series
foo <- function(x) {sum(is.na(x)) / length(x)}
wdi %>% 
  group_by(ccode) %>% dplyr::select(AG.LND.TOTL.K2:ST.INT.RCPT.CD) %>%
  dplyr::summarise_each(funs(foo)) 

# Impute missing
Y <- wdi[, 4:(ncol(wdi)-1)]
Y_imp <- sbgcop.mcmc(Y, seed=123)

x11()
plot(Y_imp)
dev.off()

Y_new <- as.data.frame(Y_imp$Y.pmean)

tail(Y)
tail(Y_new)

wdi[, 4:(ncol(wdi)-1)] <- Y_new

# Check USA tourism again
with(wdi[wdi$ccode==2, ], plot(year, ST.INT.ARVL, ylim=c(0, 7e+07)))



#   Expand to monthly data
#   ======================


wdi$cy_id <- paste(wdi$year, wdi$ccode)

month <- 1:12
wdi_cm <- expand.grid(wdi$cy_id, month)
colnames(wdi_cm) <- c("cy_id", "month")
wdi_cm <- left_join(wdi_cm, wdi, by="cy_id")

# Lag by 1 year; before creating ID
wdi_cm$date <- as.Date(paste0(wdi_cm$year, "-", wdi_cm$month, "-01"))
year(wdi_cm$date) <- year(wdi_cm$date) + 1

# Add lag identifier to variables
fix <- grep("^[A-Z]{2}.", names(wdi_cm))
names(wdi_cm)[fix] <- names(wdi_cm)[fix] %>% paste0(., ".l1")

# Create ID var; after lagging
wdi_cm$id   <- paste(wdi_cm$date, wdi_cm$ccode)


#   Merge with G&W
#   ==============

# ID mismatches
table(wdi_cm$id %in% gwlist$id)

# Cases in WDI that are not in G&W
wdi_cm %>% 
  filter(!id %in% gwlist$id) %>%
  dplyr::group_by(ccode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date), cntry = unique(country)) %>%
  as.data.frame

# Cases in G&W missing info in WDI
gwlist %>% 
  filter(!id %in% wdi_cm$id) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date)) %>%
  as.data.frame()

# Rename and drop unneeded columns
wdi_cm <- wdi_cm[, c("id", names(wdi_cm)[grepl(".l1", names(wdi_cm))])]

# Check for mismatched IDs
table(wdi_cm$id %in% gwlist$id)
wdi_cm$id[!wdi_cm$id %in% gwlist$id]

# Merge
gwlist <- left_join(gwlist, wdi_cm, by="id")

# Done, `gwlist` now has WDI info from 1991 to 2014
rm(wdi, wdi_cm, ind_need, date1, date2)

