Lab 9
================

## Problem 2

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  # return(x)
  x
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n * k, lambda), nrow = n, ncol = k, byrow = TRUE)
}

microbenchmark::microbenchmark(
  fun1(n = 1000),
  fun1alt(n = 1000), unit="relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median      uq      max neval
    ##     fun1(n = 1000) 32.15679 33.82955 35.03031 39.53402 44.4096 8.818185   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.0000 1.000000   100

## Problem 3

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  # Position of the max value per row of x.
  idx <- max.col(t(x)) 
  
  # Do something to get the actual max value
  # x[cbind(1, 15)] ~ x[1, 15]
  # Want to access x[1, 16], x[4, 1]
  # x[rbind(c(1, 16), c(4, 1))]
  # Want to access x[4, 16], x[4, 1]
  # x[cbind(4, c(16, 1))]
  x[ cbind(idx, 1:ncol(x)) ]
}

# Do we get the same?
all(fun2(x) == fun2alt(x))

x <- matrix(rnorm(5e4), nrow=10)

# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```
