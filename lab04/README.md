Lab 4
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
if (!file.exists("../met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "../met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("../met_all.gz")
```

## 2. Prepare the data

``` r
# Remove temperatures less than -17C
met <- met[temp >= -17]

# Make sure there are no missing data in the key variables coded as 9999, 999, etc
# temp, rh, wind.sp, vis.dist, dew.point, lat, lon, and elev.
met[, range(temp)]
```

    ## [1] -17  56

``` r
met[, range(rh, na.rm = TRUE)]
```

    ## [1]   0.8334298 100.0000000

``` r
met[, range(wind.sp, na.rm = TRUE)]
```

    ## [1]  0 36

``` r
met[, range(vis.dist, na.rm = TRUE)]
```

    ## [1]      0 160000

``` r
met[, range(dew.point, na.rm = TRUE)]
```

    ## [1] -37.2  36.0

``` r
met[, range(lat, na.rm = TRUE)]
```

    ## [1] 24.550 48.941

``` r
met[, range(lon, na.rm = TRUE)]
```

    ## [1] -124.290  -68.313

``` r
met[, range(elev, na.rm = TRUE)]
```

    ## [1]  -13 9999

``` r
met[elev == 9999.0, elev := NA]

# Generate a date variable using the functions as.Date()
# (hint: You will need the following to create a date paste(year, month, day, sep = "-")).
met[, ymd := as.Date(paste(year, month, day, sep = "-"))]

# Using the data.table::week function, keep the observations of
# the first week of the month.
met[, table(week(ymd))]
```

    ## 
    ##     31     32     33     34     35 
    ## 297260 521605 527924 523847 446576

``` r
met <- met[ week(ymd) == 31]

# Compute the mean by station of the variables temp, rh, wind.sp, vis.dist, dew.point, lat, lon, and elev.
met_avg <- met[, .(
  temp      = mean(temp, na.rm = TRUE),
  rh        = mean(rh, na.rm = TRUE),
  wind.sp   = mean(wind.sp, na.rm = TRUE),
  vis.dist  = mean(vis.dist, na.rm = TRUE),
  dew.point = mean(dew.point, na.rm = TRUE),
  lat       = mean(lat, na.rm = TRUE),
  lon       = mean(lon, na.rm = TRUE),
  elev      = mean(elev, na.rm = TRUE), USAFID
), by = "USAFID"]

# Create a region variable for NW, SW, NE, SE based on lon = -98.00 and lat = 39.71 degrees
met_avg[lat >= 39.71 & lon <= -98, region := "Northwest"]
met_avg[lat < 39.71  & lon <= -98, region := "Southwest"]
met_avg[lat >= 39.71 & lon > -98 , region := "Northeast"]
met_avg[lat < 39.71  & lon > -98 , region := "Southeast"]

met_avg[, region2 := fifelse(lat >= 39.71 & lon <= -98, "Northwest",
        fifelse(lat < 39.71  & lon <= -98, "Southwest",
                fifelse(lat >= 39.71 & lon > -98, "Northeast",
                        fifelse(lat < 39.71  & lon > -98, "Southeast", NA_character_))))]

met_avg[, table(region, region2, useNA = "always")]
```

    ##            region2
    ## region      Northeast Northwest Southeast Southwest <NA>
    ##   Northeast       484         0         0         0    0
    ##   Northwest         0       146         0         0    0
    ##   Southeast         0         0       649         0    0
    ##   Southwest         0         0         0       297    0
    ##   <NA>              0         0         0         0    0

``` r
# Create a categorical variable for elevation as in the lecture slides
met_avg[, elev_cat := fifelse(elev > 252, "high", "low")]
```
