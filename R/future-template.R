furrr_map_template <- function(.x, .f, ..., .options, .type, .map_fn) {
  dots <- list(...)

  n <- length(.x)
  names <- names(.x)

  # TODO: Can we handle this more elegantly?
  # Size zero handling up front
  if (n == 0L) {
    out <- vector(.type)
    names(out) <- names
    return(out)
  }

  expr_seed <- make_expr_seed(.options$seed)

  expr <- expr({
    ...future_chunk_seeds_idx <- 1L
    ...future_chunk_x <- ...future_chunk_args

    ...future_fn_wrapper <- function(...) {
      !!expr_seed

      ...future_chunk_seeds_idx <<- ...future_chunk_seeds_idx + 1L

      ...future_fn(...)
    }

    ...future_map_fn(
      .x = ...future_chunk_x,
      .f = ...future_fn_wrapper,
      ...
    )
  })

  # TODO: Should this be `parent.env()` of the `future_map()` call?
  # We'd pass through to here as an `.env` argument.
  # See future.apply#62 for details.
  env <- environment()

  furrr_template(
    args = .x,
    fn = .f,
    dots = dots,
    n = n,
    options = .options,
    type = .type,
    map_fn = .map_fn,
    names = names,
    env = env,
    expr = expr,
    extract = furrr_map_extract
  )
}

furrr_map_extract <- function(x, i) {
  x[i]
}

# ------------------------------------------------------------------------------

furrr_map2_template <- function(.x, .y, .f, ..., .options, .type, .map_fn) {
  args <- list(.x, .y)
  dots <- list(...)

  n <- furrr_length_common(args)
  args <- furrr_recycle_common(args, n)

  # Get names after possible recycling
  names <- names(args[[1]])

  # TODO: Can we handle this more elegantly?
  # Size zero handling up front
  if (n == 0L) {
    out <- vector(.type)
    names(out) <- names
    return(out)
  }

  expr_seed <- make_expr_seed(.options$seed)

  expr <- expr({
    ...future_chunk_seeds_idx <- 1L
    ...future_chunk_x <- ...future_chunk_args[[1]]
    ...future_chunk_y <- ...future_chunk_args[[2]]

    ...future_fn_wrapper <- function(...) {
      !!expr_seed

      ...future_chunk_seeds_idx <<- ...future_chunk_seeds_idx + 1L

      ...future_fn(...)
    }

    ...future_map_fn(
      .x = ...future_chunk_x,
      .y = ...future_chunk_y,
      .f = ...future_fn_wrapper,
      ...
    )
  })

  # TODO: Should this be `parent.env()` of the `future_map()` call?
  # We'd pass through to here as an `.env` argument.
  # See future.apply#62 for details.
  env <- environment()

  furrr_template(
    args = args,
    fn = .f,
    dots = dots,
    n = n,
    options = .options,
    type = .type,
    map_fn = .map_fn,
    names = names,
    env = env,
    expr = expr,
    extract = furrr_map2_extract
  )
}

furrr_map2_extract <- function(x, i) {
  map(x, `[`, i)
}

# ------------------------------------------------------------------------------

furrr_pmap_template <- function(.l, .f, ..., .options, .type, .map_fn) {
  dots <- list(...)

  # Special case for `future_pmap(list())`
  if (length(.l) == 0L) {
    out <- vector(.type)
    return(out)
  }

  n <- furrr_length_common(.l)
  args <- furrr_recycle_common(.l, n)

  # Get names after possible recycling
  names <- names(args[[1]])

  # TODO: Can we handle this more elegantly?
  # Size zero handling up front
  if (n == 0L) {
    out <- vector(.type)
    names(out) <- names
    return(out)
  }

  expr_seed <- make_expr_seed(.options$seed)

  expr <- expr({
    ...future_chunk_seeds_idx <- 1L
    ...future_chunk_l <- ...future_chunk_args

    ...future_fn_wrapper <- function(...) {
      !!expr_seed

      ...future_chunk_seeds_idx <<- ...future_chunk_seeds_idx + 1L

      ...future_fn(...)
    }

    ...future_map_fn(
      .l = ...future_chunk_l,
      .f = ...future_fn_wrapper,
      ...
    )
  })

  # TODO: Should this be `parent.env()` of the `future_map()` call?
  # We'd pass through to here as an `.env` argument.
  # See future.apply#62 for details.
  env <- environment()

  furrr_template(
    args = args,
    fn = .f,
    dots = dots,
    n = n,
    options = .options,
    type = .type,
    map_fn = .map_fn,
    names = names,
    env = env,
    expr = expr,
    extract = furrr_pmap_extract
  )
}

furrr_pmap_extract <- function(x, i) {
  map(x, `[`, i)
}

# ------------------------------------------------------------------------------

furrr_template <- function(args,
                           fn,
                           dots,
                           n,
                           options,
                           type,
                           map_fn,
                           names,
                           env,
                           expr,
                           extract) {
  fn <- purrr::as_mapper(fn)

  if (is.null(options$conditions)) {
    options$conditions <- eval(formals(future::Future)[["conditions"]])
  }

  if (is.null(options$seed) || is_false(options$seed)) {
    seeds <- NULL
  } else {
    seeds <- make_seeds(options$seed, n)

    # Ensure we step forward 1 seed on exit to ensure
    # cross-strategy reproducibility
    oseed <- next_random_seed()
    on.exit(set_random_seed(oseed), add = TRUE)

    # Pass `NULL` on to `future()` to signal that we shouldn't check for
    # RNG usage in the expression, as it is handled by furrr
    options$seed <- NULL
  }

  n_workers <- future::nbrOfWorkers()

  chunks <- make_chunks(n, n_workers, options$scheduling, options$chunk_size)
  n_chunks <- length(chunks)

  order <- make_order(n, options$scheduling, options$chunk_size)

  # Reorder `chunks` based on the custom `order`
  if (!is.null(order)) {
    chunks <- map(chunks, FUN = function(chunk) .subset(order, chunk))
  }

  gp <- get_globals_and_packages_fn_and_dots(
    options$globals,
    fn,
    dots,
    env
  )

  globals <- gp$globals
  packages <- gp$packages

  # Add user specified packages
  if (!is.null(options$packages)) {
    packages <- unique(c(packages, options$packages))
  }

  # Require `purrr`
  if (!is.element("purrr", packages)) {
    packages <- c(packages, "purrr")
  }

  # Require `purrr::map*()`
  globals <- c(globals, ...future_map_fn = map_fn)

  # Add per chunk argument placeholders
  globals_extra <- list(
    ...future_chunk_args = NULL,
    ...future_chunk_seeds = NULL,
    ...future_globals_max_size = NULL
  )
  globals_extra <- future::as.FutureGlobals(globals_extra)
  attr(globals_extra, "resolved") <- TRUE
  attr(globals_extra, "total_size") <- 0
  globals <- c(globals, globals_extra)

  future_globals_max_size <- getOption("future.globals.maxSize")
  future_globals_max_size_default <- future_globals_max_size

  if (is.null(future_globals_max_size_default)) {
    future_globals_max_size_default <- 500 * 1024 ^ 2
  }

  # Expression adjustment for per worker max global size
  expr <- expr({
    future_globals_max_size <- getOption("future.globals.maxSize")

    if (!identical(future_globals_max_size, ...future_globals_max_size)) {
      options(future.globals.maxSize = ...future_globals_max_size)
      on.exit(options(future.globals.maxSize = future_globals_max_size), add = TRUE)
    }

    !!expr
  })

  if (is.null(options$prefix)) {
    labels <- NULL
  } else {
    labels <- paste0(options$prefix, "-", seq_len(n_chunks))
  }

  scan_chunk_args_for_globals <- is_true(options$globals)

  futures <- vector("list", length = n_chunks)

  for (i in seq_len(n_chunks)) {
    chunk <- chunks[[i]]
    n_chunk <- length(chunk)

    chunk_args <- extract(args, chunk)

    chunk_globals <- globals
    chunk_globals[["...future_chunk_args"]] <- chunk_args

    chunk_packages <- packages

    if (scan_chunk_args_for_globals) {
      chunk_args_gp <- getGlobalsAndPackages(chunk_args, envir = env, globals = TRUE)

      chunk_globals <- c(chunk_globals, chunk_args_gp$globals)
      chunk_globals <- unique(chunk_globals)

      chunk_packages <- c(chunk_packages, chunk_args_gp$packages)
      chunk_packages <- unique(chunk_packages)
    }

    # Adjust "future.globals.maxSize" option to account for the fact that
    # more than one element is processed per future. Adjustment is done by
    # scaling up the limit by the number of elements in the current chunk.
    # This is an ad-hoc "good enough" approach, see:
    # https://github.com/HenrikBengtsson/future.apply/issues/8
    chunk_globals["...future_globals_max_size"] <- list(future_globals_max_size)
    options(future.globals.maxSize = n_chunk * future_globals_max_size_default)
    on.exit(options(future.globals.maxSize = future_globals_max_size), add = TRUE)

    if (!is.null(seeds)) {
      chunk_seeds <- seeds[chunk]
      chunk_globals[["..future_chunk_seeds"]] <- chunk_seeds
    }

    futures[[i]] <- future(
      expr,
      substitute = FALSE,
      envir = env,
      stdout = options$stdout,
      conditions = options$conditions,
      globals = chunk_globals,
      packages = chunk_packages,
      seed = options$seed,
      lazy = options$lazy,
      label = labels[[i]]
    )
  }

  values <- future::value(futures)

  if (length(values) != length(chunks)) {
    abort("Internal error: Length of `values` not equal to length of `chunks`.")
  }

  out <- vctrs::vec_c(!!!values, .ptype = vector(type))

  if (!is.null(order)) {
    order_inv <- vector("integer", length = n_x)
    idx <- seq2(1L, n_x)
    order_inv[.subset(order, idx)] <- idx
    out <- out[order_inv]
  }

  names(out) <- names

  out
}

# ------------------------------------------------------------------------------

furrr_recycle_common <- function(x, n) {
  n_x <- length(x)
  lengths <- lengths(x)
  idx <- rep_len(1L, n)

  for (i in seq_len(n_x)) {
    elt <- x[[i]]

    # Don't recycle `NULL` elements, they can be indexed fine by `[` and `[[`
    # and are considered like missing arguments
    if (is.null(elt)) {
      next
    }

    length_elt <- lengths[[i]]

    if (length_elt == n) {
      next
    }

    if (length_elt == 1L) {
      x[[i]] <- elt[idx]
      next
    }

    abort(paste0("Internal error: Incompatible lengths at location ", i, "."))
  }

  x
}

# Can't use `vec_size_common()` because we extract elements with `[[` and
# respect length invariants, not size invariants
furrr_length_common <- function(x) {
  # Don't consider `NULL` elements in common size
  x <- compact_null(x)

  # Handle empty pmap input
  if (length(x) == 0L) {
    return(0L)
  }

  lengths <- lengths(x)
  indices <- seq_along(lengths)

  purrr::reduce2(lengths, indices, furrr_length2, .init = 1L)
}

furrr_length2 <- function(x, y, i) {
  if (x == 1L) {
    y
  } else if (y == 1L) {
    x
  } else if (x == y) {
    x
  } else {
    msg <- paste0("Can't recycle length ", x, " and length ", y, " at location ", i, ".")
    abort(msg)
  }
}

compact_null <- function(x) {
  null <- purrr::map_lgl(x, is.null)

  if (any(null)) {
    x[!null]
  } else {
    x
  }
}

# ------------------------------------------------------------------------------

make_expr_seed <- function(seed) {
  if (is_false(seed) || is.na(seed)) {
    return(NULL)
  }

  expr(
    assign(
      x = ".Random.seed",
      value = ...future_chunk_seeds[[...future_chunk_seeds_idx]],
      envir = globalenv(),
      inherits = FALSE
    )
  )
}
