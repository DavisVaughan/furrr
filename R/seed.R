# Adapted from:
# https://github.com/HenrikBengtsson/future.apply/blob/develop/R/rng.R

make_seeds <- function(seed, n) {
  if (is.list(seed)) {
    seed <- validate_supplied_seeds(seed, n)
    return(seed)
  }

  if (is_true(seed)) {
    seed <- as_lecyer_cmrg_seed_from_base_r_seed()
  } else if (is.integer(seed)) {
    seed <- as_lecyer_cmrg_seed_from_integer(seed)
  } else {
    abort("Internal error: Unknown type of `seed` encountered.")
  }

  oseed <- next_random_seed()
  on.exit(set_random_seed(oseed), add = TRUE)

  out <- vector("list", length = n)

  for (i in seq_len(n)) {
    out[[i]] <- nextRNGSubStream(seed)
    seed <- nextRNGStream(seed)
  }

  out
}

# ------------------------------------------------------------------------------

validate_supplied_seeds <- function(seeds, n) {
  # Finally check length of `seed` against `n`
  if (length(seeds) != n) {
    abort(paste0(
      "If `furrr_options(seed = )` is a list, it must have length equal ",
      "to the common length of the inputs, ", n, ", ",
      "not length ", length(seeds), "."
    ))
  }

  # All other checks on a list `seed` were done in `furrr_options()`
  seeds
}

# ------------------------------------------------------------------------------

is_valid_random_seed <- function(seed) {
  oseed <- get_random_seed()
  on.exit(set_random_seed(oseed), add = TRUE)

  env <- globalenv()
  env$.Random.seed <- seed

  res <- tryCatch(
    simpleWarning = function(cnd) cnd,
    sample.int(n = 1L, size = 1L, replace = FALSE)
  )

  ok <- !inherits(res, "simpleWarning")

  ok
}

next_random_seed <- function(seed = get_random_seed()) {
  sample.int(n = 1L, size = 1L, replace = FALSE)
  seed_next <- get_random_seed()

  stopifnot(!any(seed_next != seed))

  invisible(seed_next)
}

get_random_seed <- function() {
  env <- globalenv()
  env$.Random.seed
}

set_random_seed <- function(seed) {
  env <- globalenv()

  if (is.null(seed)) {
    rm(list = ".Random.seed", envir = env, inherits = FALSE)
  } else {
    env$.Random.seed <- seed
  }
}

# ------------------------------------------------------------------------------

as_lecyer_cmrg_seed_from_base_r_seed <- function() {
  oseed <- get_random_seed()

  # Already a L'Ecuyer-CMRG seed?  Then use that as is.
  if (is_lecyer_cmrg_seed(oseed)) {
    return(oseed)
  }

  on.exit(set_random_seed(oseed), add = TRUE)

  RNGkind("L'Ecuyer-CMRG")

  return(get_random_seed())
}

as_lecyer_cmrg_seed_from_integer <- function(seed) {
  # Already a L'Ecuyer-CMRG seed?
  if (is_lecyer_cmrg_seed(seed)) {
    return(seed)
  }

  # Generate a new L'Ecuyer-CMRG seed
  if (length(seed) == 1L) {
    oseed <- get_random_seed()
    on.exit(set_random_seed(oseed), add = TRUE)

    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)

    return(get_random_seed())
  }

  abort(paste0(
    "Integer `seed` must be L'Ecuyer-CMRG RNG seed as returned by ",
    "`parallel::nextRNGStream()` or a single integer."
  ))
}

is_lecyer_cmrg_seed <- function(seed) {
  is.numeric(seed) &&
    length(seed) == 7L &&
    all(is.finite(seed)) &&
    (seed[[1]] %% 10000L == 407L)
}
