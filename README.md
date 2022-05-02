
<!-- README.md is generated from README.Rmd. Please edit that file -->

# furrr <a href='https://furrr.futureverse.org/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/furrr)](https://cran.r-project.org/package=furrr)
[![R-CMD-check](https://github.com/DavisVaughan/furrr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavisVaughan/furrr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/furrr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DavisVaughan/furrr?branch=main)
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

Every variant of the following functions has been implemented:

-   `map()`
-   `map2()`
-   `pmap()`
-   `walk()`
-   `imap()`
-   `modify()`

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

## Learning

The easiest way to learn about furrr is to browse [the
website](https://furrr.futureverse.org/). In particular, the [function
reference](https://furrr.futureverse.org/reference/index.html) page can
be useful to get a general overview of the functions in the package, and
the following vignettes are deep dives into various parts of furrr:

-   [Common
    gotchas](https://furrr.futureverse.org/articles/articles/gotchas.html)

-   [Learn how furrr “chunks” your
    input](https://furrr.futureverse.org/articles/articles/chunking.html)

-   [carrier - An alternative to automatic globals
    detection](https://furrr.futureverse.org/articles/articles/carrier.html)

-   [Progress notifications with
    progressr](https://furrr.futureverse.org/articles/articles/progress.html)

-   [Using furrr with
    connections](https://furrr.futureverse.org/articles/articles/remote-connections.html)

## Example

furrr has been designed to function as identically to purrr as possible,
so that you can immediately have familiarity with it.

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

The default backend for future (and through it, furrr) is a sequential
one. This means that the above code will run out of the box, but it will
*not* be in parallel. The design of future makes it incredibly easy to
change this so that your code will run in parallel.

``` r
# Set a "plan" for how the code should run.
plan(multisession, workers = 2)

# This does run in parallel!
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
# a one time cost from `plan(multisession)` setting up the workers.
plan(multisession, workers = 3)

tic()
nothingness <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc()
#> 2.212 sec elapsed
```

## Data transfer

It’s important to remember that data has to be passed back and forth
between the workers. This means that whatever performance gain you might
have gotten from your parallelization can be crushed by moving large
amounts of data around. For example, if you are moving large data frames
to the workers, running models in parallel, and returning large model
objects back, the shuffling of data can take a large chunk of that time.
Rather than returning the entire model object, you might consider only
returning a performance metric, or smaller specific pieces of that model
that you are most interested in.

This performance drop can especially be prominent if using
`future_pmap()` to iterate over rows and return large objects at each
iteration.
