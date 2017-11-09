Identify year sequences
================

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library("stringr")
library("magrittr")
```

Date helpers
------------

### Last day of the month

``` r
last_day_month <- function(x) {
  stopifnot(is.Date(x))
  x <- x %m+% months(1)
  day(x) <- 1
  day(x - 1)
}
```

### Date to ISO week and vice versa

Given an ISO week date like "2017-W01" with year and [ISO week](https://en.wikipedia.org/wiki/ISO_week_date), convert to a year-month-day date corresponding to the Thursday of that week.

ISO weeks start on Monday and the first ISO week for a year is the first 4-day week in a year, which corresponds to the first Thursday in a year. As a result, the first and last ISO weeks of a year can include dates that fall in the previous and subsequent, respectively, calendar year.

``` r
isoweek2date <- function(x) {
  # input should be like "2014-W01"
  # output will be a date indexed to Thursday
  valid <- stringr::str_detect(x, "^[0-9]{4}-W[0-9]{2}$")
  stopifnot(all(valid))
  yy      <- substr(x, 1, 4) %>% as.integer()
  isoweek <- substr(x, 7, 8) %>% as.integer()
  
  first <- ymd(sprintf("%s-01-01", yy))
  # how many days to get to first Thursday of year?
  wday_first <- wday(first) - 1
  shift <- ifelse(wday_first > 4, 
                   12 - wday_first, 
                   4 - wday_first)

  out <- first + (isoweek - 1) * 7 + shift
  out
}

test_dates <- c("2015-W01", "2016-W01", "2017-W01", "2017-W02")
data.frame(
  date1 = test_dates,
  date2 = as.character(isoweek2date(test_dates))
)
```

    ##      date1      date2
    ## 1 2015-W01 2015-01-01
    ## 2 2016-W01 2016-01-08
    ## 3 2017-W01 2017-01-05
    ## 4 2017-W02 2017-01-12

``` r
test_dates <- c("2014-W52", "2015-W52", "2015-W53", "2016-W52")
data.frame(
  date1 = test_dates,
  date2 = as.character(isoweek2date(test_dates))
)
```

    ##      date1      date2
    ## 1 2014-W52 2014-12-25
    ## 2 2015-W52 2015-12-24
    ## 3 2015-W53 2015-12-31
    ## 4 2016-W52 2016-12-30

The dates output by this function all fall on Thursday. To use a different weekday for indexing the weeks, add or subtract the difference in days between the desired index week day and Thursday, where Monday = 1 and Thursday = 4. E.g. to get weeks indexed by Monday, add 1 − 4 = −3 to the output dates.

Convert a year-month-day date to corresponding year + ISO week

``` r
date2isoweek <- function(x) {
  x <- as.Date(x)
  sprintf("%s-W%02s", lubridate::year(x), lubridate::isoweek(x))
}

test_dates <- c("2015-01-01", "2015-01-02", "2017-01-05")
data.frame(date2 = test_dates, date1 = date2isoweek(test_dates))
```

    ##        date2    date1
    ## 1 2015-01-01 2015-W01
    ## 2 2015-01-02 2015-W01
    ## 3 2017-01-05 2017-W01

``` r
test_dates <- c("2015-12-24", "2015-12-31", "2016-12-29")
data.frame(date2 = test_dates, date1 = date2isoweek(test_dates))
```

    ##        date2    date1
    ## 1 2015-12-24 2015-W52
    ## 2 2015-12-31 2015-W53
    ## 3 2016-12-29 2016-W52

### Date ceiling and floor

See `?lubridate::floor_date`.
