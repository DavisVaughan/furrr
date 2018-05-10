#' @importFrom rlang %||%

# ------------------------------------------------------------------------------
# Unexported functions from purrr

probe <- function(.x, .p, ...) {
  if (rlang::is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  }
  else {
    purrr::map_lgl(.x, .p, ...)
  }
}

inv_which <- function(x, sel) {
  if (is.character(sel)) {
    names <- names(x)
    if (is.null(names)) {
      stop("character indexing requires a named object",
           call. = FALSE)
    }
    names %in% sel
  }
  else if (is.numeric(sel)) {
    seq_along(x) %in% sel
  }
  else {
    stop("unrecognised index type", call. = FALSE)
  }
}

vec_index <- function(x){
  names(x) %||% seq_along(x)
}

as_invoke_function <- function(f) {
  if (is.function(f)) {
    list(f)
  }
  else {
    f
  }
}

# ------------------------------------------------------------------------------
# util

# Early abort from mapping function
# To stay consistent with purrr::map_*() return types
# purrr handles this at the C level
get_zero_length_type <- function(.type) {
  switch(
    .type,
    "character" = character(),
    "double"    = double(),
    "list"      = list(),
    "integer"   = integer(),
    "logical"   = logical()
  )
}

# Catch a NULL pointer that can result from serialization of the rlang::`~`
is_bad_rlang_tilde <- function(tilde) {

  # Attempt to call the tilde.
  # If bad, get a "NULL value passed as symbol address" error
  tilde_msg <- tryCatch(
    expr = {
      tilde()
      "no issues"
    },
    error = function(e) {
      e
    }
  )

  if(tilde_msg != "no issues") {
    is_bad <- TRUE
  } else {
    is_bad <- FALSE
  }

  is_bad
}

# ------------------------------------------------------------------------------
# from future.apply

## From R.utils 2.0.2 (2015-05-23)
hpaste <- function(..., sep = "", collapse = ", ", lastCollapse = NULL, maxHead = if (missing(lastCollapse)) 3 else Inf, maxTail = if (is.finite(maxHead)) 1 else Inf, abbreviate = "...") {
  if (is.null(lastCollapse)) lastCollapse <- collapse

  # Build vector 'x'
  x <- paste(..., sep = sep)
  n <- length(x)

  # Nothing todo?
  if (n == 0) return(x)
  if (is.null(collapse)) return(x)

  # Abbreviate?
  if (n > maxHead + maxTail + 1) {
    head <- x[seq_len(maxHead)]
    tail <- rev(rev(x)[seq_len(maxTail)])
    x <- c(head, abbreviate, tail)
    n <- length(x)
  }

  if (!is.null(collapse) && n > 1) {
    if (lastCollapse == collapse) {
      x <- paste(x, collapse = collapse)
    } else {
      xT <- paste(x[1:(n-1)], collapse = collapse)
      x <- paste(xT, x[n], sep = lastCollapse)
    }
  }

  x
} # hpaste()

mdebug <- function(...) {
  if (!getOption("future.debug", FALSE)) return()
  message(sprintf(...))
} ## mdebug()

## When 'default' is specified, this is 30x faster than
## base::getOption().  The difference is that here we use
## use names(.Options) whereas in 'base' names(options())
## is used.
getOption <- local({
  go <- base::getOption
  function(x, default = NULL) {
    if (missing(default) || match(x, table = names(.Options), nomatch = 0L) > 0L) go(x) else default
  }
}) ## getOption()



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

next_random_seed <- function(seed = get_random_seed()) {
  sample.int(n = 1L, size = 1L, replace = FALSE)
  seed_next <- get_random_seed()
  stopifnot(!any(seed_next != seed))
  invisible(seed_next)
}

is_valid_random_seed <- function(seed) {
  oseed <- get_random_seed()
  on.exit(set_random_seed(oseed))
  env <- globalenv()
  env$.Random.seed <- seed
  res <- tryCatch({
    sample.int(n = 1L, size = 1L, replace = FALSE)
  }, simpleWarning = function(w) w)
  !inherits(res, "simpleWarning")
}

is_lecyer_cmrg_seed <- function(seed) {
  is.numeric(seed) && length(seed) == 7L &&
    all(is.finite(seed)) && seed[1] == 407L
}

# @importFrom utils capture.output
as_lecyer_cmrg_seed <- function(seed) {
  ## Generate a L'Ecuyer-CMRG seed (existing or random)?
  if (is.logical(seed)) {
    stopifnot(length(seed) == 1L)
    if (!is.na(seed) && !seed) {
      stop("Argument 'seed' must be TRUE if logical: ", seed)
    }

    oseed <- get_random_seed()

    ## Already a L'Ecuyer-CMRG seed?  Then use that as is.
    if (!is.na(seed) && seed) {
      if (is_lecyer_cmrg_seed(oseed)) return(oseed)
    }

    ## Otherwise, generate a random one.
    on.exit(set_random_seed(oseed), add = TRUE)
    RNGkind("L'Ecuyer-CMRG")
    return(get_random_seed())
  }

  stopifnot(is.numeric(seed), all(is.finite(seed)))
  seed <- as.integer(seed)

  ## Already a L'Ecuyer-CMRG seed?
  if (length(seed) == 7L) {
    if (seed[1] != 407L) {
      stop("Argument 'seed' must be L'Ecuyer-CMRG RNG seed as returned by parallel::nextRNGStream() or an single integer: ", capture.output(str(seed)))
    }
    return(seed)
  }

  ## Generate a new L'Ecuyer-CMRG seed?
  if (length(seed) == 1L) {
    oseed <- get_random_seed()
    on.exit(set_random_seed(oseed), add = TRUE)
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
    return(get_random_seed())
  }

  stop("Argument 'seed' must be of length 1 or 7 (= 1+6):", capture.output(str(seed)))
}


import_from <- function(name, default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    get(name, mode = "function", envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such '%s' function: %s()", package, name))
  }
}

import_future <- function(name, default = NULL) {
  import_from(name, default = default, package = "future")
}
