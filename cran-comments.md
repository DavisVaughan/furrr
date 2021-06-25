## R CMD check results

0 errors | 0 warnings | 0 notes

## 0.2.3 Submission

Minor release to prepare for a breaking change in testthat.

## 0.2.2 Submission

Minor release to update a test in preparation for a lifecycle 1.0.0 change.
    
## 0.2.1 Submission

Minor release to accommodate a request from the future package.

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
