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
    ##               expr      min       lq     mean   median      uq       max neval
    ##     fun1(n = 1000) 32.76356 35.14271 20.58054 40.53219 41.4344 0.7580101   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.0000 1.0000000   100

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
  # YOUR CODE HERE
}

# Benchmarking
microbenchmark::microbenchmark(
  fun2(),
  fun2alt()
)
```
