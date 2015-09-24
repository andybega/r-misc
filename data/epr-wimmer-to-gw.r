#
#   Add EPR data to a G&W state-month panel
#   Andreas Beger
#   
#   Works for 1991 - 2014
#   Country-month data using G&W state list
#
#   This will add EPR variables to a state panel using the G&W rules
#   for state system membership. Since EPR and G&W do not have the same
#   state system membership lists, some changes to the EPR country codes
#   are neccessary. I've checked this back to 1991 for consistency, but it will
#   probably take additional checking for dates prior to that.
#

date1 <- as.Date("1991-01-01")
date2   <- as.Date("2014-12-31")

source("state-panel.r")
gwlist <- state_panel(date1, date2, by="month", useGW=TRUE)
colnames(gwlist)[1] <- "gwcode"
# Will give warnings; not a problem. 
# Cshapes goes through 2013-06. Data from that month are used to generate
# state system membership past 2013-06. Not a problem since there have been
# no changes in state system, at least not through 2014-12.


library(countrycode)
library(ggplot2)
library(lubridate)
library(foreign)

# Get EPR data
epr <- read.dta("raw/EPR3CountryNewReduced.dta")

epr <- subset(epr, select=c("year", "cowcode", "country", "egipgrps", 
                            "maxegippop", "exclgrps", "exclpop"))

# Fix state anomalies -----------------------------------------------------
#
#   Some codes are off. More important problem is the practice of back-dating
#   separate records for countries that split at some point. These need to be
#   recombined and averaged.
#
#   #/id-mismatches/epr.r 

# Germany
epr$cowcode[epr$cowcode==255] <- 260

# Yemen
epr$cowcode[epr$cowcode==679] <- 678

# Yugoslavia/Serbia
# G&W changes 345 to 340 (Serbia) on 6/5/2006
# recode post 2006 for Serbia, then add record for 2006 Serbia from 2007
# because the exclopop etc. shold be for Serbia, not Yugo
epr$cowcode[epr$cowcode==345 & epr$year > 2006] <- 340
sr2006 <- epr[epr$cowcode==340 & epr$year == 2007, ]
sr2006$year <- 2006
epr <- rbind(epr, sr2006)
epr$country[epr$cowcode==340] <- "Serbia"
rm(sr2007)


# Expand to monthly data --------------------------------------------------
#
#

# Lag by 1 year; this will introduce leading NA's for some states that gained
# independence after the EPR data start date
epr$year_lead <- epr$year + 1

# Create CY id
epr$cy_id <- paste(epr$year_lead, epr$cowcode)

month <- 1:12
epr_cm <- expand.grid(epr$cy_id, month)
colnames(epr_cm) <- c("cy_id", "month")
epr_cm <- join(epr_cm, epr, by="cy_id", type="left")

# Create ID var
epr_cm$date <- as.Date(paste0(epr_cm$year_lead, "-", epr_cm$month, "-01"))
epr_cm$id   <- paste(epr_cm$date, epr_cm$cowcode)

# Drop data outside desired range
epr_cm %<>% filter(date >= date1 & date <= date2)

# Rename key variables
colnames(epr_cm) <- gsub("egipgrps", "egip_groups_count.l1", colnames(epr_cm))
colnames(epr_cm) <- gsub("maxegippop", "egippop.l1", colnames(epr_cm))
colnames(epr_cm) <- gsub("exclgrps", "excl_groups_count.l1", colnames(epr_cm))
colnames(epr_cm) <- gsub("exclpop", "exclpop.l1", colnames(epr_cm))

# ID mismatches
table(epr_cm$id %in% gwlist$id)

# Cases in EPR that are not in G&W
epr_cm %>% 
  filter(!id %in% gwlist$id) %>%
  dplyr::group_by(cowcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date), cntry = unique(country)) %>%
  as.data.frame

# Cases in G&W missing info in EPR
gwlist %>% 
  filter(!id %in% epr_cm$id & date <= max(epr_cm$date)) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date)) %>%
  arrange(min_date, max_date) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  as.data.frame()
# main ones should be Czech Rep. and Slovakia, missing entire year because
# EPR codes Czechoslovaka for 1993


# Merge with G&W ----------------------------------------------------------

# Drop parts of gwlist that are outside EPR end date
epr_end_ym <- max(epr_cm$date)
gwlist <- filter(gwlist, date <= epr_end_ym)

# Rename and drop unneeded columns
epr_cm <- epr_cm[, c("id", colnames(epr_cm)[grep(".l1", colnames(epr_cm))])]

if (!all(names(epr_cm) %in% names(ilc_data))) stop("epr_cm name mismatch")

# Check for mismatched IDs
table(epr_cm$id %in% gwlist$id)
epr_cm$id[!epr_cm$id %in% gwlist$id]

# What countries are missing EPR info?
gwlist %>% 
  filter(!id %in% epr_cm$id & date <= paste0(max(epr$year), "-12-01")) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date)) %>%
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
rm(val_cols, any_nonmissing, j_missng, j_1_comp)
gwlist <- arrange(gwlist, gwcode, date)


# Impute missing values ---------------------------------------------------
#
#   Using copula via `sbgcop`
#

# What countries/time periods are missing EPR?
# What countries are missing EPR info?
gwlist %>% 
  filter(is.na(exclpop.l1) & date <= paste0(max(epr$year), "-01-01")) %>%
  dplyr::group_by(gwcode) %>%
  dplyr::summarize(min_date = min(date), max_date = max(date)) %>%
  arrange(min_date, max_date) %>%
  transform(country = countrycode(gwcode, "cown", "country.name")) %>%
  as.data.frame()

library(sbgcop)

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
rm(epr, epr_cm, date1, date2, epr_end_ym)

# Check missing coverage for ILC
unique(gwlist$gwcode[!gwlist$id %in% ilc_data$id])

# ILC cases missing in EPR; should be 0
ilc_data %>%
  filter(!id %in% gwlist$id & date > "1991-01-01") %>% nrow
