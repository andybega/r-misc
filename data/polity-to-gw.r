#
#   Add Polity IV case data to a G&W state-month panel
#   Andreas Beger
#
#   Works for 1991 - 2014
#   Country-month data using G&W state list
#
#   This will add Polity regime info to a state panel using the G&W rules
#   for state system membership. Since Polity and G&W do not have the same
#   state system membership lists, some changes to the Policy country codes
#   are neccessary. I've checked this back to 1991 for consistency, but it will
#   probably take additional checking for dates prior to that.
#

date1 <- as.Date("1991-01-01")
date2 <- as.Date(end_date)

gwlist <- state_panel(date1, date2, by="month", useGW=TRUE)
colnames(gwlist)[1] <- "gwcode"
# Will give warnings; not a problem. 
# Cshapes goes through 2013-06. Data from that month are used to generate
# state system membership past 2013-06. Not a problem since there have been
# no changes in state system, at least not through 2014-12.

# Polity IV case data
p4c <- read.xls("raw/p4v2014d.xls")

p4c$start <- as.Date(with(p4c, paste(byear, bmonth, bday, sep='-')))
p4c$end   <- as.Date(with(p4c, paste(eyear, emonth, eday, sep='-')))

# Ongoing have NA end date, set to max in data
p4c$end[is.na(p4c$end)] <- date2

# Keep only neccessary vars.
vars <- c('ccode', 'country', 'start', 'end',
          'democ', 'autoc', 
          'xrreg', 'xrcomp', 'xropen', 'xconst', 'parreg', 'parcomp', 'exrec')
p4c <- subset(p4c, select=vars)

p4c$cid <- 1:nrow(p4c)


#   Check for mismatched IDs
#   ========================

p4c %>%
  filter(!ccode %in% gwlist$gwcode & end > "1991-01-01") %>%
  group_by(ccode, country) %>% 
  dplyr::summarize(
    min_date = min(start), max_date = max(end)) %>% as.data.frame

# see the file below for details, and needed recoding follows
#source("R/id-mismatches/polity.R")

# Germany
p4c$ccode[p4c$ccode==255 & p4c$start==as.Date("1990-10-02")] <- 260
# ex-Yugoslavia
# problem: Polity, GW uses the same codes for different countries
# change things around
p4c$ccode[p4c$ccode==342] <- 340
p4c$end[p4c$ccode==345 & p4c$end==as.Date("1991-07-01")] <- as.Date("1991-06-30")
p4c$ccode[p4c$ccode==347] <- 345
p4c$ccode[p4c$ccode==341] <- 347
p4c$ccode[p4c$ccode==348] <- 341
# Ethiopia
p4c$ccode[p4c$ccode==529] <- 530
# Pakistan
p4c$ccode[p4c$ccode==769] <- 770
# South Sudan
p4c$ccode[p4c$ccode==626] <- 625
p4c$ccode[p4c$ccode==525] <- 626
# USSR
p4c$ccode[p4c$ccode==364] <- 365
# Vietname
p4c$ccode[p4c$ccode==818] <- 816
# Yemen
p4c$ccode[p4c$ccode==679] <- 678

# South Sudan has overlapping records, correct end date typo
p4c[p4c$ccode==626, ]
p4c$end[p4c$ccode==626 & p4c$end=="2015-12-14"] <- as.Date("2013-12-14")

# Check again, should not give anything
p4c %>%
  filter(!ccode %in% gwlist$gwcode & end > "1991-01-01") %>%
  group_by(ccode, country) %>% 
  dplyr::summarize(
    min_date = min(start), max_date = max(end)) %>% as.data.frame



#   Expand to monthly
#   =================

# Drop early cases (but keep start_date - 1 month to avoid problems when lagging)
dt <- date1
month(dt) <- month(dt) - 2
p4c %<>% filter(end > dt)

# Date vector
min_date <- min(p4c$start)
day(min_date) <- 1
dates <- seq.Date(from = min_date, to = date2, by = "month")

# Expand grid and merge
p4cm <- expand.grid(p4c$cid, dates)
colnames(p4cm) <- c("cid", "date")
p4cm <- join(p4cm, p4c, by = "cid")
p4cm %<>% filter(date >= start & date <= end)

# Lag indicators by 1 month
month(p4cm$date) <- month(p4cm$date) + 1
p4cm$id <- paste(p4cm$date, p4cm$ccode)  # need to update ID since we changed date

# Drop unneeded dates
p4cm %<>% filter(date >= date1 & date <= date2)

# Rename vars
p4cm %<>% dplyr::select(democ, autoc, xrreg, xrcomp, xropen, xconst, parreg, 
                        parcomp, exrec, id)
fix <- grep("id", names(p4cm), invert=TRUE)
names(p4cm)[fix] <- names(p4cm)[fix] %>% toupper() %>% paste0(., ".l1")
if (!all(names(p4cm) %in% names(ilc_data))) stop("p4cm name mismatch")

# check for duplicated ID's, should not be any
if (sum(duplicated(p4cm$id)) != 0) stop("p4cm duplicated IDs")
#p4cm$id[duplicated(p4cm$id)]

# Clean up
rm(dt, min_date, dates, fix)

#   Merge
#   =====

# Check for mismatched IDs
table(p4cm$id %in% gwlist$id)
p4cm$id[!p4cm$id %in% gwlist$id]
# Should only be former USSR and Yugoslavia, where Polity sometimes starts coding
# early

# What G&W countries do not have Polity information?
gwlist %>% 
  filter(!id %in% p4cm$id) %>% 
  dplyr::group_by(gwcode) %>% 
  dplyr::summarize(min_date = min(date), max_date = max(date),
                   missing=n()) %>% 
  as.data.frame

gwlist <- join(gwlist, p4cm, by = "id")


# Since we lag 1-month, countries that gained independence will miss 1st
# month values. Fix by taking first known value for 1st obs. is missing only
# situations.

vars <- grep(".l1", names(gwlist))
for (g in unique(gwlist$gwcode)) {
  idx <- gwlist$gwcode==g
  g_rows <- nrow(gwlist[idx, ])
  g_1na  <- is.na(gwlist$AUTOC.l1[idx][1])
  g_2val <- !is.na(gwlist$AUTOC.l1[idx][2])
  
  if (g_rows > 1 & g_1na & g_2val) {
    gwlist[idx, vars][1, ] <- gwlist[idx, vars][2, ]
  }
}
rm(idx, g_rows, g_1na, g_2val, vars)

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
message("Polity: carry-forward imputed ", rows_upd, " rows for dates after last
observed data in Polity.")
rm(val_cols, any_nonmissing, j_missng, j_1_comp, rows_upd)
gwlist <- arrange(gwlist, gwcode, date)

# What countries are missing Polity info?
# Should all be missing over entire date range, i.e. missing in Polity completely.
gwlist %>%
  filter(!complete.cases(.)) %>%
  dplyr::group_by(gwcode) %>% 
  dplyr::summarize(min_date = min(date), max_date = max(date),
                   missing=n()) %>% 
  as.data.frame

# Done, `gwlist` now has Polity info from 1991 to 2014
rm(p4c, p4cm, date1, date2)
