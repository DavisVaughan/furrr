## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on github actions), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

  New maintainer:
    Davis Vaughan <davis@rstudio.com>
  Old maintainer(s):
    Davis Vaughan <dvaughan@business-science.io>
    
## 0.2.0 Submission

This is the second release of furrr. My email has been updated.

## 0.1.0 Resubmission

This resubmission wraps `plan(multisession)` and `plan(multiprocess)` in
`\donttest{}` for examples that use them. This results in the examples still
running correctly, just sequentially and not in parallel. The `future` package
does a similar thing.

## 0.1.0 Submission

This is the first release of `furrr`, a package aimed at running the `purrr`
mapping functions in parallel using futures from the `future` package.
