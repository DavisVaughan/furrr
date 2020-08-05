furrr_map_template <- function(x,
                               fn,
                               dots,
                               options,
                               type,
                               map_fn,
                               env_globals) {
  n <- length(x)
  names <- names(x)

  expr_seed <- make_expr_seed(options$seed)

  expr <- expr({
    ...furrr_chunk_seeds_idx <- 1L
    ...furrr_chunk_x <- ...furrr_chunk_args

    ...furrr_fn_wrapper <- function(...) {
      !!expr_seed

      ...furrr_chunk_seeds_idx <<- ...furrr_chunk_seeds_idx + 1L

      ...furrr_fn(...)
    }

    args <- list(
      .x = ...furrr_chunk_x,
      .f = ...furrr_fn_wrapper
    )

    args <- c(args, ...furrr_dots)

    do.call(...furrr_map_fn, args)
  })

  furrr_template(
    args = x,
    fn = fn,
    dots = dots,
    n = n,
    options = options,
    type = type,
    map_fn = map_fn,
    names = names,
    env_globals = env_globals,
    expr = expr,
    extract = furrr_map_extract
  )
}

furrr_map_extract <- function(x, i) {
  x[i]
}

# ------------------------------------------------------------------------------

furrr_map2_template <- function(x,
                                y,
                                fn,
                                dots,
                                options,
                                type,
                                map_fn,
                                env_globals) {
  args <- list(x, y)

  n <- furrr_length_common(args)
  args <- furrr_recycle_common(args, n)

  # Get names after possible recycling
  names <- names(args[[1]])

  expr_seed <- make_expr_seed(options$seed)

  expr <- expr({
    ...furrr_chunk_seeds_idx <- 1L
    ...furrr_chunk_x <- ...furrr_chunk_args[[1]]
    ...furrr_chunk_y <- ...furrr_chunk_args[[2]]

    ...furrr_fn_wrapper <- function(...) {
      !!expr_seed

      ...furrr_chunk_seeds_idx <<- ...furrr_chunk_seeds_idx + 1L

      ...furrr_fn(...)
    }

    args <- list(
      .x = ...furrr_chunk_x,
      .y = ...furrr_chunk_y,
      .f = ...furrr_fn_wrapper
    )

    args <- c(args, ...furrr_dots)

    do.call(...furrr_map_fn, args)
  })

  furrr_template(
    args = args,
    fn = fn,
    dots = dots,
    n = n,
    options = options,
    type = type,
    map_fn = map_fn,
    names = names,
    env_globals = env_globals,
    expr = expr,
    extract = furrr_map2_extract
  )
}

furrr_map2_extract <- function(x, i) {
  map(x, `[`, i)
}

# ------------------------------------------------------------------------------

furrr_pmap_template <- function(l,
                                fn,
                                dots,
                                options,
                                type,
                                map_fn,
                                env_globals) {
  if (is.data.frame(l)) {
    l <- as.list(l)
  }

  n <- furrr_length_common(l)
  args <- furrr_recycle_common(l, n)

  if (length(l) == 0L) {
    # Special case handling of empty `l` like `future_pmap(list())`
    names <- NULL
  } else {
    # Get names after possible recycling
    names <- names(args[[1]])
  }

  expr_seed <- make_expr_seed(options$seed)

  expr <- expr({
    ...furrr_chunk_seeds_idx <- 1L
    ...furrr_chunk_l <- ...furrr_chunk_args

    ...furrr_fn_wrapper <- function(...) {
      !!expr_seed

      ...furrr_chunk_seeds_idx <<- ...furrr_chunk_seeds_idx + 1L

      ...furrr_fn(...)
    }

    args <- list(
      .l = ...furrr_chunk_l,
      .f = ...furrr_fn_wrapper
    )

    args <- c(args, ...furrr_dots)

    do.call(...furrr_map_fn, args)
  })

  furrr_template(
    args = args,
    fn = fn,
    dots = dots,
    n = n,
    options = options,
    type = type,
    map_fn = map_fn,
    names = names,
    env_globals = env_globals,
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
                           env_globals,
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

  gp <- get_globals_and_packages(
    options$globals,
    options$packages,
    map_fn,
    fn,
    dots,
    env_globals
  )

  globals <- gp$globals
  packages <- gp$packages

  future_globals_max_size <- getOption("future.globals.maxSize")
  future_globals_max_size_default <- future_globals_max_size

  if (is.null(future_globals_max_size_default)) {
    future_globals_max_size_default <- 500 * 1024 ^ 2
  }

  # Expression adjustment for per worker max global size
  expr <- expr({
    future_globals_max_size <- getOption("future.globals.maxSize")

    if (!identical(future_globals_max_size, ...furrr_globals_max_size)) {
      options(future.globals.maxSize = ...furrr_globals_max_size)
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
    chunk_globals[["...furrr_chunk_args"]] <- chunk_args

    chunk_packages <- packages

    if (scan_chunk_args_for_globals) {
      chunk_args_gp <- future::getGlobalsAndPackages(
        expr = chunk_args,
        envir = env_globals,
        globals = TRUE
      )

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
    chunk_globals["...furrr_globals_max_size"] <- list(future_globals_max_size)
    options(future.globals.maxSize = n_chunk * future_globals_max_size_default)
    on.exit(options(future.globals.maxSize = future_globals_max_size), add = TRUE)

    if (!is.null(seeds)) {
      chunk_seeds <- seeds[chunk]
      chunk_globals[["...furrr_chunk_seeds"]] <- chunk_seeds
    }

    futures[[i]] <- future(
      expr,
      substitute = FALSE,
      envir = env_globals,
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
    order_inv <- vector("integer", length = n)
    idx <- seq2(1L, n)
    order_inv[.subset(order, idx)] <- idx
    out <- out[order_inv]
  }

  names(out) <- names

  out
}

# ------------------------------------------------------------------------------

make_expr_seed <- function(seed) {
  if (is.null(seed) || is_false(seed)) {
    return(NULL)
  }

  expr(
    assign(
      x = ".Random.seed",
      value = ...furrr_chunk_seeds[[...furrr_chunk_seeds_idx]],
      envir = globalenv(),
      inherits = FALSE
    )
  )
}

# ------------------------------------------------------------------------------

# Required global variable hack for variables used in `expr()`.
# Required to pass R CMD check.
utils::globalVariables(c(
  "...furrr_chunk_args",
  "...furrr_fn",
  "...furrr_map_fn",
  "...furrr_dots",
  "...furrr_globals_max_size",
  "...furrr_chunk_seeds",
  "...furrr_chunk_seeds_idx"
))
