
<!-- README.md is generated from README.Rmd. Please edit that file -->

# furrr

The goal of furrr is to simplify the combination of `purrr`’s family of
`map()` functions and `future`’s parallel processing capabilities. A new
set of `future_map*()` functions have been defined, and can be used as
(hopefully) drop in replacements for the corresponding `map*()`
function.

The code draws *heavily* from the implementations of `purrr` and
`future.apply` and this package would not be possible without either of
them. Each `future_map*()` function has additional `future.*` arguments
that are taken from `future_lapply()` and allow fine tuned control over
the parallel execution.

## Installation

You can install the released version of furrr from
[CRAN](https://CRAN.R-project.org) with:

``` r
# NO YOU CANNOT
install.packages("furrr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/furrr")
```

## Example

`furrr` has been designed to function identically to `purrr`, so that
you can immediately have familiarity with it.

``` r
library(furrr)
#> Loading required package: future
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

The default backend for `future` is a sequential one. This means that
the code will run out of the box, but it will *not* be in parallel. The
design of `future` makes this incredibly easy to change so that your
code does run in parallel.

``` r
library(future)

# You set a "plan" for how the code should run. The easiest is `multiprocess`
# On Mac this picks plan(multicore) and on Windows this picks plan(multisession)
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

# This should take 9 seconds in total running sequentially
plan(sequential)
tic()
nothingness <- future_map(c(3, 3, 3), ~Sys.sleep(.x))
toc()
#> 9.058 sec elapsed

# This should take ~3 seconds running in parallel, with a little overhead
plan(multiprocess)
tic()
nothingness <- future_map(c(3, 3, 3), ~Sys.sleep(.x))
toc()
#> 3.076 sec elapsed
```

## A more compelling use case

This example comes from a Vignette from `rsample`. The vignette performs
a 10 fold cross validation with 10 repeats of a GLM on the attrition
data set. If you want all the details with explanation, see [the
vignette](https://topepo.github.io/rsample/articles/Working_with_rsets.html).

The vignette example runs pretty quickly on its own, so to make things
more…interesting we are going to use 20 fold CV with 100 repeats.

``` r
library(rsample)
#> Loading required package: broom
#> Loading required package: tidyr
#> 
#> Attaching package: 'rsample'
#> The following object is masked from 'package:tidyr':
#> 
#>     fill
data("attrition")
names(attrition)
#>  [1] "Age"                      "Attrition"               
#>  [3] "BusinessTravel"           "DailyRate"               
#>  [5] "Department"               "DistanceFromHome"        
#>  [7] "Education"                "EducationField"          
#>  [9] "EnvironmentSatisfaction"  "Gender"                  
#> [11] "HourlyRate"               "JobInvolvement"          
#> [13] "JobLevel"                 "JobRole"                 
#> [15] "JobSatisfaction"          "MaritalStatus"           
#> [17] "MonthlyIncome"            "MonthlyRate"             
#> [19] "NumCompaniesWorked"       "OverTime"                
#> [21] "PercentSalaryHike"        "PerformanceRating"       
#> [23] "RelationshipSatisfaction" "StockOptionLevel"        
#> [25] "TotalWorkingYears"        "TrainingTimesLastYear"   
#> [27] "WorkLifeBalance"          "YearsAtCompany"          
#> [29] "YearsInCurrentRole"       "YearsSinceLastPromotion" 
#> [31] "YearsWithCurrManager"
```

Set up an rsample split tibble of 20 fold CV with 100 repeats.

``` r
set.seed(4622)
rs_obj <- vfold_cv(attrition, v = 20, repeats = 100)
rs_obj
#> #  20-fold cross-validation repeated 100 times 
#> # A tibble: 2,000 x 3
#>    splits       id        id2   
#>    <list>       <chr>     <chr> 
#>  1 <S3: rsplit> Repeat001 Fold01
#>  2 <S3: rsplit> Repeat001 Fold02
#>  3 <S3: rsplit> Repeat001 Fold03
#>  4 <S3: rsplit> Repeat001 Fold04
#>  5 <S3: rsplit> Repeat001 Fold05
#>  6 <S3: rsplit> Repeat001 Fold06
#>  7 <S3: rsplit> Repeat001 Fold07
#>  8 <S3: rsplit> Repeat001 Fold08
#>  9 <S3: rsplit> Repeat001 Fold09
#> 10 <S3: rsplit> Repeat001 Fold10
#> # ... with 1,990 more rows
```

The model formula below is going to be used in the
GLM.

``` r
mod_form <- as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)
```

For each split, we want to calculate assessments on the holdout data, so
a function was created to allow us to apply the model and easily extract
what we need from each split.

``` r
library(broom)
## splits will be the `rsplit` object with the 90/10 partition
holdout_results <- function(splits, ...) {
  # Fit the model to the 90%
  mod <- glm(..., data = analysis(splits), family = binomial)
  # Save the 10%
  holdout <- assessment(splits)
  # `augment` will save the predictions with the holdout data set
  res <- broom::augment(mod, newdata = holdout)
  # Class predictions on the assessment set from class probs
  lvls <- levels(holdout$Attrition)
  predictions <- factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]),
                        levels = lvls)
  # Calculate whether the prediction was correct
  res$correct <- predictions == holdout$Attrition
  # Return the assessment data set with the additional columns
  res
}
```

Finally, `purrr` was used to map over all of the splits, apply the model
to each, and extract the results.

First in sequential order…

``` r
library(purrr)
library(tictoc)

tic()
rs_obj$results <- map(rs_obj$splits, holdout_results, mod_form)
toc()
#> 26.507 sec elapsed
```

Then in parallel…

``` r
library(furrr)
plan(multiprocess)

tic()
rs_obj$results <- future_map(rs_obj$splits, holdout_results, mod_form)
toc()
#> 14.929 sec elapsed
```

If you’re curious, the resulting object looks like this.

``` r
rs_obj
#> #  20-fold cross-validation repeated 100 times 
#> # A tibble: 2,000 x 4
#>    splits       id        id2    results               
#>    <list>       <chr>     <chr>  <list>                
#>  1 <S3: rsplit> Repeat001 Fold01 <data.frame [74 × 35]>
#>  2 <S3: rsplit> Repeat001 Fold02 <data.frame [74 × 35]>
#>  3 <S3: rsplit> Repeat001 Fold03 <data.frame [74 × 35]>
#>  4 <S3: rsplit> Repeat001 Fold04 <data.frame [74 × 35]>
#>  5 <S3: rsplit> Repeat001 Fold05 <data.frame [74 × 35]>
#>  6 <S3: rsplit> Repeat001 Fold06 <data.frame [74 × 35]>
#>  7 <S3: rsplit> Repeat001 Fold07 <data.frame [74 × 35]>
#>  8 <S3: rsplit> Repeat001 Fold08 <data.frame [74 × 35]>
#>  9 <S3: rsplit> Repeat001 Fold09 <data.frame [74 × 35]>
#> 10 <S3: rsplit> Repeat001 Fold10 <data.frame [74 × 35]>
#> # ... with 1,990 more rows
```

We don’t get a 4x improvement on my 4 core Mac, but we do get a nice 2x
speed up without doing any hard work. The reason we don’t get a 4x
improvement is likely because of time spent transfering data to each R
process, so this penalty will be minimized with longer running tasks and
you might see better performance. The implementation of
`future_lapply()` does include a scheduling feature, which carried over
nicely into `furrr` and efficiently breaks up the list of splits into 4
equal subsets. Each is passed to 1 core of my machine.

## What has been implemented?

The full range of `map()` and `map2()` functions have been implemented.
This includes strict versions like `map_dbl()` through
`future_map_dbl()` and predicate versions like `map_at()` and
`map_if()`.

## What has not been implemented (yet)?

  - `pmap()`
  - `walk()`
  - `imap()`
  - `lmap()`
  - `modify()`

## Found a bug?

This package likely has a few bugs, so if you find one, feel free to
open an issue.
