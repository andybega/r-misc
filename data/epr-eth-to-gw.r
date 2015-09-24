#
#   Add EPR (ETH, not UCLA) data to a G&W state-month panel
#   Andreas Beger
#   
#   Works for 1991 - 2014
#   Country-month data using G&W state list
#
#   This will add Zurich EPR version to a G&W panel, aggregating the group-case
#   formatted EPR data to country-year format first along the way.
#

library(plyr)
library(lubridate)
library(dplyr)
library(magrittr)
library(countrycode)

# Create blank G&W panel

# Add extra month since we want lagged variable in the end
date1 <- as.Date("1991-01-01")
date2 <- as.Date(end_date)

source("state-panel.r")
gwlist <- state_panel(date1, date2, by="month", useGW=TRUE)
colnames(gwlist)[1] <- "gwcode"
# Will give warnings; not a problem. 
# Cshapes goes through 2013-06. Data from that month are used to generate
# state system membership past 2013-06. Not a problem since there have been
# no changes in state system, at least not through 2014-12.

# Read EPR data
epr <- read.csv("raw/EPR-2014.csv")

# Convert EPR to country-year ---------------------------------------------
#
#   The data are case-formatted, with one row for each group-period, where the 
#   periods reflect a specific configuration of power relations between a 
#   country's identified groups. 
# 
#   We will convert this to country-year by first expanding the data to 
#   group-years, then summarizing over countries to obtain country-years.
#

years <- range(range(epr$from), range(epr$to))
years <- seq(years[1], years[2], by=1)

epr$id <- 1:nrow(epr)

gy <- expand.grid(id = epr$id, year = years)
gy <- left_join(gy, epr, by="id") %>%
  filter(year >= from & year <= to)

# Summarize

# EPR LA has for the US:
# from  to    exclgrps  exclpop   egipgrps  
# 1946  1965  1           .121        1
# 1966  2008  5           .297        1
# 2009        4           .173        2

epr %>% filter(gwid==2) %>% 
  dplyr::select(from, to, group, umbrella, size, status, reg_aut)
# powerless or discriminated
# monopoly, diminant, junior/senior partner
# see http://www.princeton.edu/~awimmer/AppendixEthnicPolitics.pdf
excl_val <- c("POWERLESS", "DISCRIMINATED", "REGIONAL AUTONOMY")
incl_val <- c("MONOPOLY", "DOMINANT", "JUNIOR PARTNER", "SENIOR PARTNER")

# not clear what to do with `irrelevant` and similar smaller categories
table(epr$status)

# Convert to country-year with known codings
cy <- gy %>%
  group_by(gwid, year) %>%
  dplyr::summarize(
    country  = unique(statename),
    exclgrps = sum(status %in% excl_val),
    exclpop  = sum(subset(size, status %in% excl_val)),
    egipgrps = sum(status %in% incl_val),
    egippop  = sum(subset(size, status %in% incl_val))
    )

# Check USA as example
cy %>% filter(gwid==2) %>% data.frame()


# Expand to monthly data --------------------------------------------------
#
#

# Lag by 1 year; this will introduce leading NA's for some states that gained
# independence after the EPR data start date
cy$year_lead <- cy$year + 1
colnames(cy)[grepl("excl|egip", colnames(cy))] <- paste0(
  colnames(cy)[grepl("excl|egip", colnames(cy))], ".l1")

# Create CY id
cy$cy_id <- paste(cy$year_lead, cy$gwid)

month <- 1:12
epr_cm <- expand.grid(cy_id=cy$cy_id, month=month)
epr_cm <- join(epr_cm, cy, by="cy_id", type="left")

# Create ID var
epr_cm$date <- as.Date(paste0(epr_cm$year_lead, "-", epr_cm$month, "-01"))
epr_cm$id   <- paste(epr_cm$date, epr_cm$gwid)

# Drop data outside desired range
epr_cm %<>% filter(date >= date1 & date <= date2)

# ID mismatches
table(epr_cm$id %in% gwlist$id)

# Cases in EPR that are not in G&W
epr_cm %>% 
  filter(!id %in% gwlist$id) %>%
  dplyr::group_by(gwid) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date), cntry = unique(country)) %>%
  as.data.frame
# Czechoslovakia, Croatia, S+M and Slovenia; are fine and artifacts of state 
# changes.

# Cases in G&W missing info in EPR
gwlist %>% 
  filter(!id %in% epr_cm$id & date <= max(epr_cm$date)) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date),
                   missing = n()) %>%
  arrange(min_date, max_date) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  as.data.frame()
# quite a few smaller countries are missing all, this is fine
# from 1991 a lot of, mainly EE, countries are missing partial data, this is 
# artifact of independence; we will fill these in next
# 


# Merge with G&W ----------------------------------------------------------


# Rename and drop unneeded columns
epr_cm <- epr_cm[, c("id", colnames(epr_cm)[grep(".l1", colnames(epr_cm))])]

# Check for mismatched IDs
table(epr_cm$id %in% gwlist$id)
epr_cm$id[!epr_cm$id %in% gwlist$id]

# What countries are missing EPR info?
gwlist %>% 
  filter(!id %in% epr_cm$id) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date),
                   missing = n()) %>%
  arrange(min_date, max_date) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  as.data.frame()

# Merge
gwlist <- join(gwlist, epr_cm, by="id")


# Impute missing CM values ------------------------------------------------
#
#   A couple of countries gained independence in the early 90's and are missing
#   the first few months months due to EPR yearly colleciton. Back-fill with 
#   the first non-missing values
#
#   For more recent missing values due to lag in EPR updating cycle, we will
#   also carry forward last observed value.

gwlist <- arrange(gwlist, gwcode, desc(date))
val_cols <- colnames(gwlist)[grep(".l1", colnames(gwlist))]
rows_upd <- 0
for (i in unique(gwlist$gwcode)) {
  rows_upd_i <- FALSE
  this_ctry <- gwlist[gwlist$gwcode==i, ]
  any_nonmissing <- any(complete.cases(this_ctry))
  # 2 or more rows, with at least one complete
  if (any_nonmissing & nrow(this_ctry) > 1) {
    # Loop through rows, fill in missing if prev is !missing
    for (j in 2:nrow(this_ctry)) {
      j_missng <- !complete.cases(this_ctry[j  , ])
      j_1_comp <-  complete.cases(this_ctry[j-1, ])                          
      if (j_missng & j_1_comp) {
        rows_upd <- rows_upd + 1
        rows_upd_i <- TRUE
        this_ctry[j, val_cols] <- this_ctry[j-1, val_cols]
      }
    }
  }
  # If any rows have been updated, put in updated country info
  if (rows_upd_i) {
    gwlist[gwlist$gwcode==i, ] <- this_ctry
  }
}
message("EPR: back-filled ", rows_upd, " rows for countries missing first few",
    " months due to independence.")
rm(val_cols, any_nonmissing, j_missng, j_1_comp, rows_upd)
gwlist <- arrange(gwlist, gwcode, date)


# Carry forward imputation for most recent months, using same approach
# as above, just forward in time order.
gwlist <- arrange(gwlist, gwcode, date)
val_cols <- colnames(gwlist)[grep(".l1", colnames(gwlist))]
rows_upd <- 0
for (i in unique(gwlist$gwcode)) {
  rows_upd_i <- FALSE
  this_ctry <- gwlist[gwlist$gwcode==i, ]
  any_nonmissing <- any(complete.cases(this_ctry))
  # 2 or more rows, with at least one complete
  if (any_nonmissing & nrow(this_ctry) > 1) {
    # Loop through rows, fill in missing if prev is !missing
    for (j in 2:nrow(this_ctry)) {
      j_missng <- !complete.cases(this_ctry[j  , ])
      j_1_comp <-  complete.cases(this_ctry[j-1, ])                          
      if (j_missng & j_1_comp) {
        rows_upd <- rows_upd + 1
        rows_upd_i <- TRUE
        this_ctry[j, val_cols] <- this_ctry[j-1, val_cols]
      }
    }
  }
  # If any rows have been updated, put in updated country info
  if (rows_upd_i) {
    gwlist[gwlist$gwcode==i, ] <- this_ctry
  }
}
message("EPR: carry-forward imputed ", rows_upd, " rows for dates after last
observed data in EPR.")
rm(val_cols, any_nonmissing, j_missng, j_1_comp, rows_upd)
gwlist <- arrange(gwlist, gwcode, date)


# Impute missing values ---------------------------------------------------
#
#   Using copula via `sbgcop`
#

# What countries/time periods are missing EPR?
# What countries are missing EPR info?
gwlist %>% 
  filter(is.na(exclpop.l1)) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date),
                   missing = n()) %>%
  arrange(min_date, max_date) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  as.data.frame()

#library(sbgcop)

# Fraction of missing?
sum(is.na(gwlist)) / (ncol(gwlist) * nrow(gwlist))
# 
# # Missing by country for each series
# foo <- function(x) {sum(is.na(x)) / length(x)}
# wdi %>% 
#   group_by(ccode) %>%
#   dplyr::summarise_each(funs(foo)) %>%
#   as.data.frame
# 
# # Impute missing
# Y <- wdi[, 4:(ncol(wdi)-1)]
# Y_imp <- sbgcop.mcmc(Y, seed=123)
# 
# plot(Y_imp)
# 
# Y_new <- as.data.frame(Y_imp$Y.pmean)
# 
# tail(Y)
# tail(Y_new)
# 
# wdi[, 4:(ncol(wdi)-1)] <- Y_new





# Done, `gwlist` now has EPR info from 1991 to 2014
rm(epr, epr_cm, date1, date2, cy, gy)
