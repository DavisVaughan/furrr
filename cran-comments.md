## Resubmission

This resubmission wraps `plan(multisession)` and `plan(multiprocess)` in `\donttest{}`
for examples that use them. This results in the examples still running correctly,
just sequentially and not in parallel. The `future` package does a similar thing.

## Comments

This is the first release of `furrr`, a package aimed at running the `purrr` 
mapping functions in parallel using futures from the `future` package.

## Test environments
* local OS X install, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
