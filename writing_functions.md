Writing Functions
================

load packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

setup

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "right"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

Set seed for reproducibility. (get same random numbers each time)

``` r
set.seed(12345)
```

#### Z score function

Z scores substract the mean and divide by the SD.

``` r
x_vec = rnorm(20, mean = 5, sd = 0.3)
```

Compute Z scores for x_vec

``` r
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Function that produces Z scores input collection of numbers; output
collection of z scores

``` r
z_score = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("Need at least 2 numbers to get z scores")
  }
  
  z = (x - mean(x)) /sd(x)
  
  z
}
```

z does not exist outside of the function make sure argument names are
unique from oher variable names in your enviro

Check that this works. Applying this function to the x_vec

``` r
z_score(x = x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Keep checking. See how these breaks. Some produce errors while some kind
of work

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): Need at least 2 numbers to get z scores

get an NA

``` r
z_score(c("hello", "there"))
```

    ## Error in z_score(c("hello", "there")): Argument should be numbers

error message

``` r
z_score(iris)
```

    ## Error in z_score(iris): Argument should be numbers

error message; entire iris dataframe

``` r
z_score(c(TRUE, TRUE, FALSE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE)): Argument should be numbers

error message

Update our function with conditionals

### Multiple outputs

Write a function that returns mean and sd from a sample of numbers

``` r
mean_and_sd = function(x) {
  
    if (!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("Need at least 2 numbers to get z scores")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Check that our function works

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.02 0.250

### Multiple inputs

``` r
x_vec = rnorm(n = 30, mean = 5, sd = 0.5)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.12 0.625

Write a function that uses n, a true mean, and true SD as inputs

``` r
sim_mean_sd = function(n_obs, mu, sigma) {
  
  x_vec = rnorm(n = n_obs, mean = mu, sd = sigma)
  
  tibble(
    mean = mean(x_vec),
    sd = sd(x_vec)
  )
  
}


sim_mean_sd(n_obs = 30, mu = 5, sigma = 0.5)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.12 0.590

``` r
sim_mean_sd(12, 24, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  25.1  2.06

function assigns by positional matching if you did not name anything;
good practice to name your inputs
