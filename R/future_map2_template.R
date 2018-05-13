future_map2_template <- function(.map, .type, .x, .y, .f, ..., .progress, .options) {

  # Assert future options
  assert_future_options(.options)

  # Create function from .f
  .f <- purrr::as_mapper(.f, ...) # ... required in case you pass .null / .default through for purrr::as_mapper.numeric

  # Debug
  debug <- getOption("future.debug", FALSE)

  ## Nothing to do?
  n.x <- length(.x)
  n.y <- length(.y)
  if (n.x == 0 || n.y == 0)  {
    return(get_zero_length_type(.type))
  }

  ## Improper lengths
  if (n.x != n.y && !(n.x == 1 || n.y == 1)) {
    msg <- sprintf("`.x` (%i) and `.y` (%i) are different lengths", n.x, n.y)
    stop(msg, call. = FALSE)
  }

  ## Recycle .x or .y to correct length if needed
  # At this point, the only allowed extension is if .x or .y is length 1
  if(n.x > n.y) .y <- rep(.y, times = n.x)
  if(n.y > n.x) .x <- rep(.x, times = n.y)

  if (debug) mdebug("future_map_*() ...")

  ## NOTE TO SELF: We'd ideally have a 'future.envir' argument also for
  ## future_lapply(), cf. future().  However, it's not yet clear to me how
  ## to do this, because we need to have globalsOf() to search for globals
  ## from the current environment in order to identify the globals of
  ## arguments 'FUN' and '...'. /HB 2017-03-10
  future.envir <- environment()  ## Used once in getGlobalsAndPackages() below
  envir <- future.envir

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 1. Global variables
  ## 2. Packages
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  .options <- gather_globals_and_packages(.options, .map, .f, .progress, envir, ...)

  globals <- .options$globals
  packages <- .options$packages


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 3. Reproducible RNG (for sequential and parallel processing)
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  seed <- .options$seed
  seeds <- NULL # Placeholder needs to be set to null

  ## Don't use RNGs? (seed = FALSE)
  if (is.logical(seed) && !is.na(seed) && !seed) {
    seed <- NULL
  }

  # Use RNGs?
  if (!is.null(seed)) {
    if (debug) mdebug("Generating random seeds ...")

    ## future_lapply() should return with the same RNG state regardless of
    ## future strategy used. This is be done such that RNG kind is preserved
    ## and the seed is "forwarded" one step from what it was when this
    ## function was called. The forwarding is done by generating one random
    ## number. Note that this approach is also independent on length(.x) and
    ## the diffent FUN() calls.
    oseed <- next_random_seed()
    on.exit(set_random_seed(oseed))

    seeds <- generate_seed_streams(seed, n_seeds = n.x)

    if (debug) mdebug("Generating random seeds ... DONE")
  } ## if (!is.null(seed))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 4. Load balancing ("chunking")
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  chunks <- generate_balanced_chunks(.options$scheduling, n.x)

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 5. Create futures
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  ## Add argument placeholders
  globals_extra <- as.FutureGlobals(list(...future.x_ii = NULL, ...future.y_ii = NULL, ...future.seeds_ii = NULL))
  attr(globals_extra, "resolved") <- TRUE
  attr(globals_extra, "total_size") <- 0
  globals <- c(globals, globals_extra)

  ## At this point a globals should be resolved and we should know their total size
  ##  stopifnot(attr(globals, "resolved"), !is.na(attr(globals, "total_size")))

  ## To please R CMD check
  ...future.map <- ...future.f <- ...future.x_ii <- ...future.y_ii <- ...future.seeds_ii <- temp_file <- NULL

  nchunks <- length(chunks)
  fs <- vector("list", length = nchunks)
  if (debug) mdebug("Number of futures (= number of chunks): %d", nchunks)

  if (debug) mdebug("Launching %d futures (chunks) ...", nchunks)
  for (ii in seq_along(chunks)) {
    chunk <- chunks[[ii]]
    if (debug) mdebug("Chunk #%d of %d ...", ii, length(chunks))

    ## Subsetting outside future is more efficient
    globals_ii <- globals
    globals_ii[["...future.x_ii"]] <- .x[chunk]
    globals_ii[["...future.y_ii"]] <- .y[chunk]
    ##    stopifnot(attr(globals_ii, "resolved"))

    ## Using RNG seeds or not?
    if (is.null(seeds)) {
      if (debug) mdebug(" - seeds: <none>")
      fs[[ii]] <- future({

        # rlang tilde - when serializing with multisession, the pointer becomes 0x0
        # Temp solution is to readd base::`~` into the .f environment if necessary
        ...future.f.env <- environment(...future.f)
        if(!is.null(...future.f.env$`~`)) {
          if(is_bad_rlang_tilde(...future.f.env$`~`)) {
            ...future.f.env$`~` <- base::`~`
          }
        }

        # Make persistent file connection
        if(.progress) {
          temp_file_con <- file(temp_file, "a")
          on.exit(close(temp_file_con))
        }

        ...future.map(seq_along(...future.x_ii), .f = function(jj) {
          ...future.x_jj <- ...future.x_ii[[jj]]
          ...future.y_jj <- ...future.y_ii[[jj]]
          .out <- ...future.f(...future.x_jj, ...future.y_jj, ...)
          if(.progress) update_progress(temp_file_con)
          .out
        })

      }, envir = envir, lazy = .options$lazy, globals = globals_ii, packages = packages)
    } else {
      if (debug) mdebug(" - seeds: [%d] <seeds>", length(chunk))
      globals_ii[["...future.seeds_ii"]] <- seeds[chunk]
      fs[[ii]] <- future({

        # rlang tilde - when serializing with multisession, the pointer becomes 0x0
        # Temp solution is to readd base::`~` into the .f environment if necessary
        ...future.f.env <- environment(...future.f)
        if(!is.null(...future.f.env$`~`)) {
          if(is_bad_rlang_tilde(...future.f.env$`~`)) {
            ...future.f.env$`~` <- base::`~`
          }
        }

        # Make persistent file connection
        if(.progress) {
          temp_file_con <- file(temp_file, "a")
          on.exit(close(temp_file_con))
        }

        ...future.map(seq_along(...future.x_ii), .f = function(jj) {
          ...future.x_jj <- ...future.x_ii[[jj]]
          ...future.y_jj <- ...future.y_ii[[jj]]
          assign(".Random.seed", ...future.seeds_ii[[jj]], envir = globalenv(), inherits = FALSE)
          .out <- ...future.f(...future.x_jj, ...future.y_jj, ...)
          if(.progress) update_progress(temp_file_con)
          .out
        })

      }, envir = envir, lazy = .options$lazy, globals = globals_ii, packages = packages)
    }

    ## Not needed anymore
    rm(list = c("chunk", "globals_ii"))

    if (debug) mdebug("Chunk #%d of %d ... DONE", ii, nchunks)
  } ## for (ii ...)
  if (debug) mdebug("Launching %d futures (chunks) ... DONE", nchunks)

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 6. Print progress
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(.progress) {
    poll_progress(fs, globals$temp_file, n.x)
  }

  ## FINISHED - Not needed anymore
  rm(list = c("chunks", "globals", "envir"))

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 7. Resolve
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  values <- multi_resolve(fs, names(.x))

  if (debug) mdebug("future_map_*() ... DONE")

  values
}
