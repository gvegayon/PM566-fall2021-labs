Lab 5
================

## 1. Read in the data

``` r
library(data.table)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
if (!file.exists("../met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "../met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("../met_all.gz")
```

Merging the data

``` r
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y = "USAF"
)
```

## Question 1

First, generate a representative version of each station. We will use
the averages (median could also be a good way to represent it, but it
will depend on the case).

``` r
station_averages <- met[,.(
  temp      = mean(temp, na.rm = TRUE),
  wind.sp   = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = .(USAFID)]
```

Now, we need to identify the median per variable

``` r
medians <- station_averages[,.(
  temp_50      = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp_50   = quantile(wind.sp, probs = .5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = .5, na.rm = TRUE)
)]

medians
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.461838     1014.691

Now we can find the stations that are the closest to these. (hint:
`which.min()`)

``` r
station_averages[, temp_dist := abs(temp - medians$temp_50)]
median_temp_station <- station_averages[order(temp_dist)][1]
median_temp_station
```

    ##    USAFID     temp  wind.sp atm.press   temp_dist
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

The median temperature station is 720458.

## Question 2

We first need to recover the state variable, by MERGING :)!

``` r
station_averages <- merge(
  x = station_averages, y = stations,
  by.x = "USAFID", by.y = "USAF",
  all.x = TRUE, all.y = FALSE
  )
```

Now we can compute the median per state

``` r
station_averages[, temp_50 := quantile(temp, probs = .5, na.rm = TRUE), by = STATE]
station_averages[, wind.sp_50 := quantile(wind.sp, probs = .5, na.rm = TRUE), by = STATE]
```

Now, the euclidean distance… $\\sqrt{\\sum\_i(x\_i - y\_i)^2}$

``` r
station_averages[, eudist := sqrt(
  (temp - temp_50)^2 + (wind.sp - wind.sp_50)^2
  )]
station_averages
```

    ##       USAFID     temp  wind.sp atm.press temp_dist CTRY STATE  temp_50
    ##    1: 690150 33.18763 3.483560  1010.379 9.5035752   US    CA 22.66268
    ##    2: 720110 31.22003 2.138348       NaN 7.5359677   US    TX 29.75188
    ##    3: 720113 23.29317 2.470298       NaN 0.3908894   US    MI 20.51970
    ##    4: 720120 27.01922 2.504692       NaN 3.3351568   US    SC 25.80545
    ##    5: 720137 21.88823 1.979335       NaN 1.7958292   US    IL 22.43194
    ##   ---                                                                 
    ## 1591: 726777 19.15492 4.673878  1014.299 4.5291393   US    MT 19.15492
    ## 1592: 726797 18.78980 2.858586  1014.902 4.8942607   US    MT 19.15492
    ## 1593: 726798 19.47014 4.445783  1014.072 4.2139153   US    MT 19.15492
    ## 1594: 726810 25.03549 3.039794  1011.730 1.3514356   US    ID 20.56798
    ## 1595: 726813 23.47809 2.435372  1012.315 0.2059716   US    ID 20.56798
    ##       wind.sp_50     eudist
    ##    1:   2.565445 10.5649277
    ##    2:   3.413737  1.9447578
    ##    3:   2.273423  2.7804480
    ##    4:   1.696119  1.4584280
    ##    5:   2.237622  0.6019431
    ##   ---                      
    ## 1591:   4.151737  0.5221409
    ## 1592:   4.151737  1.3437090
    ## 1593:   4.151737  0.4310791
    ## 1594:   2.568944  4.4922623
    ## 1595:   2.568944  2.9131751

## Question 3

## Question 4

Going back to the met dataset.

``` r
met[, state_temp := mean(temp, na.rm = TRUE), by = STATE]
met[, temp_cat := fifelse(
  state_temp < 20, "low-temp", 
  fifelse(state_temp < 25, "mid-temp", "high-temp"))
  ]
```

Let’s make sure that we don’t have NAs

``` r
table(met$temp_cat, useNA = "always")
```

    ## 
    ## high-temp  low-temp  mid-temp      <NA> 
    ##    811126    430794   1135423         0

Now, let’s summarize

``` r
tab <- met[, .(
  N_entries  = .N,
  N_stations = length(unique(USAFID))
), by = temp_cat]

knitr::kable(tab)
```

| temp\_cat | N\_entries | N\_stations |
|:----------|-----------:|------------:|
| mid-temp  |    1135423 |         781 |
| high-temp |     811126 |         555 |
| low-temp  |     430794 |         259 |
