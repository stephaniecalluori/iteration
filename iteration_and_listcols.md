Iteration and List Columns
================

I’m an R Markdown document! load packages

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

set seed

``` r
set.seed(12345)
```

### Lists

``` r
vec_numeric = 1:4
vec_char = c("my", "name", "is", "jeff")

tibble(
  num = vec_numeric,
  char = vec_char
)
```

    ## # A tibble: 4 × 2
    ##     num char 
    ##   <int> <chr>
    ## 1     1 my   
    ## 2     2 name 
    ## 3     3 is   
    ## 4     4 jeff

Diff stuff with diff lengths letters is all of the letters

``` r
l = list(
  vec_numeric = 1:5,
  vec_char = LETTERS,
  matrix = matrix(1:10, nrow = 5, ncol = 2),
  summary = summary(rnorm(100))
)
```

Accessing lists \$ general way to access elements in a list; ok to use
\$ here for lists just not for dfs

``` r
l$vec_char
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[[2]]
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.3804 -0.5901  0.4837  0.2452  0.9004  2.4771

### loops

here each element is a collection of numbers

``` r
list_norm_samples =
  list(
    a = rnorm(20, 1, 5),
    b = rnorm(20, 0, 7),
    c = rnorm(20, 20, 1),
    d = rnorm(20, -45, 13)
  )
```

let’s compute the mean and sd for each collection of numbers

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

``` r
mean_and_sd(list_norm_samples$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92

``` r
mean_and_sd(list_norm_samples$b)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30

``` r
mean_and_sd(list_norm_samples$c)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910

``` r
mean_and_sd(list_norm_samples$d)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -44.1  14.0

make sure you use i in the correct places here our output is a list

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm_samples[[i]])
}
```

### use `map`

``` r
output = map(list_norm_samples, mean_and_sd)
```

### create DF

4 samples and 4 names

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm_samples
  )
```

``` r
listcol_df |> pull(samp)
```

    ## $a
    ##  [1]  2.1196270 -4.7811167  3.1120926 -5.6237763  1.7054216 -1.6802400
    ##  [7] -0.5580304  8.7805482 -1.2401665  2.6056177 -5.1508612 -5.6202935
    ## [13]  7.3062114  7.5961586  0.5962312 -1.5254490  0.7392320  4.1443031
    ## [19] 11.9000120  0.6549135
    ## 
    ## $b
    ##  [1]  10.8140452   9.2501641   2.2550610  10.7166858  -2.9486779  -8.1117472
    ##  [7] -12.9175780   8.1012770 -14.8648492  -8.3722206  11.4953439   6.1855838
    ## [13]   3.6741312  -8.2926135  18.5905179  -7.3353960  -7.0778577   4.6824516
    ## [19]   0.9042411  -2.9580381
    ## 
    ## $c
    ##  [1] 18.85974 18.70628 19.40530 18.49919 20.01586 20.54017 18.45271 20.84965
    ##  [9] 20.89601 20.13869 18.38067 20.54840 20.19528 19.19350 19.89138 19.74905
    ## [17] 21.69935 19.65570 20.06777 19.34943
    ## 
    ## $d
    ##  [1] -51.33930 -41.05903 -48.14566 -51.26254 -57.89344 -48.64844 -36.77077
    ##  [8] -61.11764 -22.06392 -45.30784 -42.40103 -27.48649 -44.53104 -34.28045
    ## [15] -67.13473 -38.74765 -12.71385 -39.78226 -42.20270 -68.60426

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30

``` r
mean_and_sd(listcol_df$samp[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910

OR apply function to each element in listcol of the df

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.25  4.92
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  9.30
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8 0.910
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -44.1  14.0

OR add a mean and sd column unnest in order to see values

``` r
listcol_df |> 
  mutate(mean_sd = map(samp, mean_and_sd),
         median = map(samp, median)
         ) |> 
  select(name, mean_sd) |> 
  unnest(mean_sd)
```

    ## # A tibble: 4 × 3
    ##   name     mean     sd
    ##   <chr>   <dbl>  <dbl>
    ## 1 a       1.25   4.92 
    ## 2 b       0.690  9.30 
    ## 3 c      19.8    0.910
    ## 4 d     -44.1   14.0

### NSDUH

goal is to extract html, import 3 tables, and keep track of results

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_import = function(html, table_number, outcome_name) {

  html |> 
  html_table() |> 
  nth(table_number) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent),
    outcome = outcome_name) |> 
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
}

nsduh_import(nsduh_html, 1, "marj")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    9.98 marj   
    ##  2 Alabama 12+   2014-2015    9.6  marj   
    ##  3 Alabama 12-17 2013-2014    9.9  marj   
    ##  4 Alabama 12-17 2014-2015    9.71 marj   
    ##  5 Alabama 18-25 2013-2014   27.0  marj   
    ##  6 Alabama 18-25 2014-2015   26.1  marj   
    ##  7 Alabama 26+   2013-2014    7.1  marj   
    ##  8 Alabama 26+   2014-2015    6.81 marj   
    ##  9 Alabama 18+   2013-2014    9.99 marj   
    ## 10 Alabama 18+   2014-2015    9.59 marj   
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, 4, "cocaine")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    1.23 cocaine
    ##  2 Alabama 12+   2014-2015    1.22 cocaine
    ##  3 Alabama 12-17 2013-2014    0.42 cocaine
    ##  4 Alabama 12-17 2014-2015    0.41 cocaine
    ##  5 Alabama 18-25 2013-2014    3.09 cocaine
    ##  6 Alabama 18-25 2014-2015    3.2  cocaine
    ##  7 Alabama 26+   2013-2014    1.01 cocaine
    ##  8 Alabama 26+   2014-2015    0.99 cocaine
    ##  9 Alabama 18+   2013-2014    1.31 cocaine
    ## 10 Alabama 18+   2014-2015    1.31 cocaine
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, 5, "heroin")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    0.22 heroin 
    ##  2 Alabama 12+   2014-2015    0.27 heroin 
    ##  3 Alabama 12-17 2013-2014    0.1  heroin 
    ##  4 Alabama 12-17 2014-2015    0.08 heroin 
    ##  5 Alabama 18-25 2013-2014    0.45 heroin 
    ##  6 Alabama 18-25 2014-2015    0.64 heroin 
    ##  7 Alabama 26+   2013-2014    0.19 heroin 
    ##  8 Alabama 26+   2014-2015    0.23 heroin 
    ##  9 Alabama 18+   2013-2014    0.23 heroin 
    ## 10 Alabama 18+   2014-2015    0.29 heroin 
    ## # ℹ 500 more rows

import data using a for loop using table number importing 3 tables that
we care about can just do 1:3 bc each input has 3 values

``` r
table_input = list(1, 4, 5)

name_input = list("marj", "cocaine", "heroin")
output = vector("list", length = 3)

for (i in 1:3) {
  
  output[[i]] = nsduh_import(nsduh_html, table_input[[i]], name_input[[i]])
    
}

nsduh_df = bind_rows(output)
```

Try again using maps! want a dataset with outcome name, table number,
and data along with it side by side

``` r
nsduh_import = function(html, table_number) {

  html |> 
  html_table() |> 
  nth(table_number) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |> 
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
}

map(nsduh_df$number, nsduh_import, html = nsduh_html)
```

    ## Warning: Unknown or uninitialised column: `number`.

    ## list()

``` r
nsduh_df = 
  tibble(
    name = c("marj", "cocaine", "heroin"),
    number = c(1, 4, 5)
  ) |> 
  mutate(table = map(number, nsduh_import, html = nsduh_html)) |> 
  unnest(table)
```

our table is name, table number, and the data for each table

### Revisit weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/stephaniecalluori/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-09-28 10:20:07.929139 (8.524)

    ## file min/max dates: 1869-01-01 / 2023-09-30

    ## using cached file: /Users/stephaniecalluori/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-09-28 10:20:18.73663 (3.83)

    ## file min/max dates: 1949-10-01 / 2023-09-30

    ## using cached file: /Users/stephaniecalluori/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-09-28 10:20:22.054464 (0.994)

    ## file min/max dates: 1999-09-01 / 2023-09-30

nest everything from data to tmin into df column

``` r
weather_nest_df = weather_df |> 
  nest(df = date:tmin)
```

Can i regress tmax on tmin for each of these stations

``` r
central_park_df = 
  weather_nest_df |> 
  pull(df) |> 
  nth(1)
```

fit a linear regression for central park

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}

weather_lm(central_park_df)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

to get linear regression for each station let’s make a for loop input is
df for each station

``` r
input_list = weather_nest_df |> pull(df)
output = vector("list", length = 3)

for (i in 1:3) {
  output[[i]] = weather_lm(input_list[[i]])
}
```

Or use map to create a list column with the linear regression for each
station

``` r
weather_nest_df |> 
  mutate(models = map(df, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          df                 models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>  
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
