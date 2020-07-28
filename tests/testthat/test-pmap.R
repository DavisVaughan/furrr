# ------------------------------------------------------------------------------
# Setup

.th <- retrieve_test_helpers()
test_msg <- .th$test_msg
test_dat <- .th$test_dat

test_l <- list(test_dat, test_dat)

# ------------------------------------------------------------------------------
# Testing

for(.e in .th$executors) {

  # Don't test multicore on non-Mac
  if(.e == "multicore" && .th$system.os != "Darwin") {
    next
  }

  plan(.e, substitute = FALSE)

  test_that(test_msg(.e, "equivalence with pmap()"), {
    .f <- identical
    .purrr <- purrr::pmap(test_l, sum)
    .furrr <- future_pmap(test_l, sum)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with vector pmap()s"), {
    .f <- ~as.character(sum(.x, .y))
    .purrr <- purrr::pmap_chr(test_l, .f)
    .furrr <- future_pmap_chr(test_l, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "equivalence with df pmap()s"), {
    .f <- ~data.frame(x = sum(.x, .y))
    .purrr <- purrr::pmap_dfr(test_l, .f)
    .furrr <- future_pmap_dfr(test_l, .f)
    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "named arguments can be passed through"), {

    vec_mean <- function(.x, .y, na.rm = FALSE) {
      mean(c(.x,.y), na.rm = na.rm)
    }

    test_l_na <- test_l
    test_l_na[[1]][1] <- NA

    .purrr <- purrr::pmap(test_l_na, vec_mean, na.rm = TRUE)
    .furrr <- future_pmap(test_l_na, vec_mean, na.rm = TRUE)

    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "arguments can be matched by name"), {

    test_l_named <- test_l
    names(test_l_named) <- c("x", "y")
    test_l_named[["y"]] <- test_l_named[["y"]] * 2

    .f <- function(y, x) {y - x}

    .purrr <- purrr::pmap(test_l_named, .f)
    .furrr <- future_pmap(test_l_named, .f)

    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "unused components can be absorbed"), {

    .f_bad <- function(x)      {x}
    .f     <- function(x, ...) {x}

    .purrr <- purrr::pmap(test_l, .f)
    .furrr <- future_pmap(test_l, .f)

    expect_error(future_pmap(test_l, .f_bad))
    expect_equal(.purrr, .furrr)
  })

  # See issue #16
  test_that(test_msg(.e, "Globals in .l are found (.l could be a fn)"), {

    my_robust_sum <- function(x) sum(x, na.rm = TRUE)  # Can you find me?
    my_robust_sum2 <- function(x) sum(x, na.rm = TRUE) # Can you find me?
    multi_x <- list(c(1, 2, NA), c(2, 3, 4))
    Xs <- purrr::map(multi_x, ~ purrr::partial(my_robust_sum, .x))
    Ys <- purrr::map(multi_x, ~ purrr::partial(my_robust_sum2, .x))

    .purrr <- purrr::pmap(.l = list(Xs, Ys), .f = ~c(.x(), .y()))
    .furrr <- future_pmap(.l = list(Xs, Ys), .f = ~c(.x(), .y()))

    expect_equal(.purrr, .furrr)
  })

  test_that(test_msg(.e, "Globals in .l are only exported to workers that use them"), {

    # This test is difficult to check without manually inspecting what gets exported

    my_mean  <- function(x) { mean(x, na.rm = TRUE) } # Exported to worker 1
    my_mean2 <- function(x) { mean(x, na.rm = TRUE) } # Exported to worker 2

    my_wrapper  <- function(x) { my_mean(x)  }
    my_wrapper2 <- function(x) { my_mean2(x) }

    .l <- list(my_wrapper, my_wrapper2)
    .l2 <- list(1, 2)

    .purrr <- future_pmap(list(.l, .l2), ~.x(.y))
    .furrr <- purrr::pmap(list(.l, .l2), ~.x(.y))

    expect_equal(.purrr, .furrr)
  })

}
