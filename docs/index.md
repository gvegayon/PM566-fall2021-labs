Lab 7
================

``` r
if (knitr::is_html_output(excludes = "gfm")) {
  
}
```

## Question 1: How many sars-cov-2 papers?

``` r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/span")

# Turning it into text
# or xml2::xml_text(counts)
counts <- as.character(counts)

# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "114,592"

``` r
stringr::str_extract(counts, "[[:digit:],]+")
```

    ## [1] "114,592"

``` r
stringr::str_replace(counts, "[^[:digit:]]+([[:digit:]]+),([[:digit:]]+)[^[:digit:]]+", "\\1\\2")
```

    ## [1] "114592"
