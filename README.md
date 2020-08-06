
<!-- README.md is generated from README.Rmd. Please edit that file -->

# furrr <a href='https://davisvaughan.github.io/furrr/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/furrr)](https://cran.r-project.org/package=furrr)
[![R build
status](https://github.com/DavisVaughan/furrr/workflows/R-CMD-check/badge.svg)](https://github.com/DavisVaughan/furrr/actions)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/furrr/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/furrr?branch=master)
<!-- badges: end -->

## Overview

The goal of furrr is to combine purrr’s family of mapping functions with
future’s parallel processing capabilities. The result is near drop in
replacements for purrr functions such as `map()` and `map2_dbl()`, which
can be replaced with their furrr equivalents of `future_map()` and
`future_map2_dbl()` to map in parallel.

The code draws heavily from the implementations of purrr and
future.apply and this package would not be possible without either of
them.

## What has been implemented?

Every variant of the following functions have been implemented:

  - `map()`
  - `map2()`
  - `pmap()`
  - `walk()`
  - `imap()`
  - `modify()`
  - `invoke_map()`

This includes atomic variants like `map_dbl()` through
`future_map_dbl()` and predicate variants like `map_at()` through
`future_map_at()`.

## Installation

You can install the released version of furrr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("furrr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("DavisVaughan/furrr")
```

## Example

furrr has been designed to function identically to purrr, so that you
can immediately have familiarity with it.

``` r
library(furrr)
library(purrr)

map(c("hello", "world"), ~.x)
#> [[1]]
#> [1] "hello"
#> 
#> [[2]]
#> [1] "world"

future_map(c("hello", "world"), ~.x)
#> [[1]]
#> [1] "hello"
#> 
#> [[2]]
#> [1] "world"
```

The default backend for future is a sequential one. This means that the
above code will run out of the box, but it will *not* be in parallel.
The design of future makes it incredibly easy to change this so that
your code will run in parallel.

``` r
# Set a "plan" for how the code should run.
# The easiest is `plan(multiprocess)`.
plan(multiprocess)

# This DOES run in parallel!
future_map(c("hello", "world"), ~.x)
#> [[1]]
#> [1] "hello"
#> 
#> [[2]]
#> [1] "world"
```

If you are still skeptical, here is some proof that we are running in
parallel.

``` r
library(tictoc)

# This should take 6 seconds in total running sequentially
plan(sequential)

tic()
nothingness <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc()
#> 6.08 sec elapsed
```

``` r
# This should take ~2 seconds running in parallel, with a little overhead
# in `future_map()` from sending data to the workers. There is generally also
# a one time cost from `plan(multiprocess)` setting up the workers.
plan(multiprocess)

tic()
nothingness <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc()
#> 2.212 sec elapsed
```

## A more compelling use case

This example comes from a vignette from rsample. The vignette performs a
10-fold cross validation with 10 repeats of a GLM on the attrition data
set. If you want all the details with explanation, see [the
vignette](https://rsample.tidymodels.org/articles/Working_with_rsets.html).

The vignette example runs pretty quickly on its own, so to make things
more…interesting we are going to use 20 fold CV with 100 repeats.

``` r
library(rsample)
library(modeldata)
data("attrition")
```

Set up an rsample split tibble of 20 fold CV with 100 repeats.

``` r
set.seed(4622)

folds <- vfold_cv(attrition, v = 20, repeats = 100)

folds
#> #  20-fold cross-validation repeated 100 times 
#> # A tibble: 2,000 x 3
#>    splits            id        id2   
#>    <list>            <chr>     <chr> 
#>  1 <split [1.4K/74]> Repeat001 Fold01
#>  2 <split [1.4K/74]> Repeat001 Fold02
#>  3 <split [1.4K/74]> Repeat001 Fold03
#>  4 <split [1.4K/74]> Repeat001 Fold04
#>  5 <split [1.4K/74]> Repeat001 Fold05
#>  6 <split [1.4K/74]> Repeat001 Fold06
#>  7 <split [1.4K/74]> Repeat001 Fold07
#>  8 <split [1.4K/74]> Repeat001 Fold08
#>  9 <split [1.4K/74]> Repeat001 Fold09
#> 10 <split [1.4K/74]> Repeat001 Fold10
#> # … with 1,990 more rows
```

The model formula below is going to be used in the GLM.

``` r
mod_form <- as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)
```

For each split, we want to analyze predictions on the holdout data, so a
function is created to allow us to apply the model and easily extract
what we need from each split.

``` r
library(yardstick)
#> For binary classification, the first factor level is assumed to be the event.
#> Use the argument `event_level = "second"` to alter this as needed.

# `splits` will be the `rsplit` object with the 95/5 partition

compute_holdout_results <- function(splits, ...) {
  data <- analysis(splits)
  holdout <- assessment(splits)
  
  # Fit the model to the 95%
  model <- glm(..., data = data, family = binomial)
  
  predictions <- predict(model, newdata = holdout)
  
  lvls <- levels(holdout$Attrition)
  
  # Transform probability prediction to class prediction
  hard_prediction <- factor(
    ifelse(predictions > 0, lvls[2], lvls[1]),
    levels = lvls
  )
  
  accuracy_vec(holdout$Attrition, hard_prediction)
}
```

Traditionally we would now map over the splits, applying
`compute_holdout_results()` to each split. Here we’ll show it in
sequential order and in parallel.

``` r
library(purrr)
library(tictoc)

tic()
folds$results <- map_dbl(folds$splits, compute_holdout_results, mod_form)
toc()
#> 30.095 sec elapsed
```

``` r
library(furrr)
plan(multiprocess)

tic()
folds$results <- future_map_dbl(folds$splits, compute_holdout_results, mod_form)
toc()
#> 14.211 sec elapsed
```

We don’t get a 4x improvement on my 4 core Mac, but we do get a nice 2x
speed up without doing any hard work. The reason we don’t get a 4x
improvement is likely because of time spent transferring data to each R
process, so this penalty will be minimized with longer running tasks and
you might see better performance (for example, 100 fold CV with 100
repeats gave 122 seconds sequentially and 48 seconds in parallel).

## A few notes on performance

### Data transfer

It’s important to remember that data has to be passed back and forth
between the cores. This means that whatever performance gain you might
have gotten from your parallelization can be crushed by moving large
amounts of data around. For example, if instead of returning a results
data frame in the above example, we returned the larger glm model object
for each split, our performance drops a bit.

``` r
model_only <- function(splits, ...) {
  # Fit the model to the 95%
  mod <- glm(..., data = analysis(splits), family = binomial)
  
  mod
}

plan(multiprocess)

tic()
folds$results2 <- future_map(folds$splits, model_only, mod_form)
toc()
#> 20.807 sec elapsed
```

Luckily, the glm model is relatively small, so we don’t experience much
loss, but there are model objects out there that can be 10’s of MBs in
size. For models like those, I would advise wrapping up the work you
want each core to do into a function, and only returning the actual
performance metric you are looking for. This might mean a little bit
more work on your side, but it results in smaller objects, and faster
performance.

This performance drop can especially be prominent if using
`future_pmap()` to iterate over rows and return large objects at each
iteration.
