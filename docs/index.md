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

xml2::xml_find_first(website, "//*[@id=\"search-results\"]/div[2]/div[1]/span")
```

    ## {html_node}
    ## <span class="value">

``` r
xml2::xml_find_first(website, '//*[@id="search-results"]/div[2]/div[1]/span')
```

    ## {html_node}
    ## <span class="value">

``` r
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

## Q2

``` r
library(httr)
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db     = "pubmed",
    term   = "covid19 hawaii",
    retmax = 1000
    )
)

GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/",
  path  = "entrez/eutils/esearch.fcgi",
  query = list(
    db     = "pubmed",
    term   = "covid19 hawaii",
    retmax = 1000
    )
)
```

    ## Response [https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=covid19%20hawaii&retmax=1000]
    ##   Date: 2021-10-08 18:14
    ##   Status: 200
    ##   Content-Type: text/xml; charset=UTF-8
    ##   Size: 4.28 kB
    ## <?xml version="1.0" encoding="UTF-8" ?>
    ## <!DOCTYPE eSearchResult PUBLIC "-//NLM//DTD esearch 20060628//EN" "https://eu...
    ## <eSearchResult><Count>150</Count><RetMax>150</RetMax><RetStart>0</RetStart><I...
    ## <Id>34562997</Id>
    ## <Id>34559481</Id>
    ## <Id>34545941</Id>
    ## <Id>34536350</Id>
    ## <Id>34532685</Id>
    ## <Id>34529634</Id>
    ## <Id>34499878</Id>
    ## ...

``` r
# Extracting the content of the response of GET
ids <- httr::content(query_ids)
ids
```

    ## {xml_document}
    ## <eSearchResult>
    ## [1] <Count>150</Count>
    ## [2] <RetMax>150</RetMax>
    ## [3] <RetStart>0</RetStart>
    ## [4] <IdList>\n  <Id>34562997</Id>\n  <Id>34559481</Id>\n  <Id>34545941</Id>\n ...
    ## [5] <TranslationSet>\n  <Translation>\n    <From>covid19</From>\n    <To>"cov ...
    ## [6] <TranslationStack>\n  <TermSet>\n    <Term>"covid-19"[MeSH Terms]</Term>\ ...
    ## [7] <QueryTranslation>("covid-19"[MeSH Terms] OR "covid-19"[All Fields] OR "c ...
