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
    ##               expr      min       lq     mean   median       uq    max neval
    ##     fun1(n = 1000) 32.34988 37.95344 34.95931 42.49921 45.66267 6.9312   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000 1.0000   100

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
```

    ## [1] TRUE

``` r
x <- matrix(rnorm(5e4), nrow=10)

# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr      min     lq     mean   median       uq     max neval
    ##     fun2(x) 11.58521 12.525 11.54892 13.05829 11.98592 3.38403   100
    ##  fun2alt(x)  1.00000  1.000  1.00000  1.00000  1.00000 1.00000   100
