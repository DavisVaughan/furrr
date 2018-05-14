future_pmap_template <- function(.map, .type, .l, .f, ..., .progress, .options) {

  # Assert future options
  assert_future_options(.options)

  # Create function from .f
  .f <- purrr::as_mapper(.f, ...) # ... required in case you pass .null / .default through for purrr::as_mapper.numeric

  # Debug
  debug <- getOption("future.debug", FALSE)

  # Check .l is list (consistent with purrr)
  if(!is.list(.l)) {
    stop("`.x` is not a list (%s)", typeof(.l), call. = FALSE)
  }

  # ## Nothing to do?
  n.l <- length(.l)
  n_all <- purrr::map_int(.l, length)
  n.l_elems <- max(n_all)

  if(any(n_all == 0L)) {
    return(get_zero_length_type(.type))
  }

  ## Improper lengths
  possible_len_problems <- which(n_all != n.l_elems)
  any_possible_problems <- length(possible_len_problems)

  if(any_possible_problems) {
    for(prob_id in possible_len_problems) {

      # Recycle as necessary
      if(n_all[prob_id] == 1L) {
        .l[[prob_id]] <- rep(.l[[prob_id]], times = n.l_elems)

      # Or exit if improper length (with purrr-like error)
      } else {
        msg <- sprintf("Element %i has length %i, not %i or %i.", prob_id, n_all[prob_id], 1L, n.l_elems)
        stop(msg, call. = FALSE)
      }
    }
  }

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

    seeds <- generate_seed_streams(seed, n_seeds = n.l_elems)

    if (debug) mdebug("Generating random seeds ... DONE")
  } ## if (!is.null(seed))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 4. Load balancing ("chunking")
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  chunks <- generate_balanced_chunks(.options$scheduling, n.l_elems)

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 5. Create futures
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  ## Add argument placeholders
  globals_extra <- as.FutureGlobals(list(...future.lst_ii = rlang::new_list(n.l, names = names(.l)), ...future.seeds_ii = NULL))
  attr(globals_extra, "resolved") <- TRUE
  attr(globals_extra, "total_size") <- 0
  globals <- c(globals, globals_extra)

  ## At this point a globals should be resolved and we should know their total size
  ##  stopifnot(attr(globals, "resolved"), !is.na(attr(globals, "total_size")))

  ## To please R CMD check
  ...future.map <- ...future.f <- ...future.lst_ii <- ...future.seeds_ii <- temp_file <- NULL

  nchunks <- length(chunks)
  fs <- vector("list", length = nchunks)
  if (debug) mdebug("Number of futures (= number of chunks): %d", nchunks)

  if (debug) mdebug("Launching %d futures (chunks) ...", nchunks)
  for (ii in seq_along(chunks)) {
    chunk <- chunks[[ii]]
    if (debug) mdebug("Chunk #%d of %d ...", ii, length(chunks))

    ## Subsetting outside future is more efficient
    globals_ii <- globals

    for(.l_i in seq_along(.l)) {
      globals_ii[["...future.lst_ii"]][[.l_i]] <- .l[[.l_i]][chunk]
    }

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

        # Attach the dots as a named element, these will be recycled
        # double list to keep the names once passed to ...future.f
        ...future.lst_ii$...future.dots <- list(list(...))

        # Make persistent file connection
        if(.progress) {
          temp_file_con <- file(temp_file, "a")
          on.exit(close(temp_file_con))
        }

        # The ... of pmap are passed in ...future.dots
        # The current elements of the list are passed in ...
        ...future.f_wrapper <- function(..., ...future.dots) {
          .out <- do.call(...future.f, c(list(...), ...future.dots))
          if(.progress) update_progress(temp_file_con)
          .out
        }

        ...future.map(...future.lst_ii, ...future.f_wrapper)

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

        # Attach the current seeds to the lst as a named element, these will be iterated over
        ...future.lst_ii$...future.seeds_ii <- ...future.seeds_ii

        # Attach the dots as a named element, these will be recycled
        # double list to keep the names once passed to ...future.f
        ...future.lst_ii$...future.dots <- list(list(...))

        # Make persistent file connection
        if(.progress) {
          temp_file_con <- file(temp_file, "a")
          on.exit(close(temp_file_con))
        }

        # In the wrapper, refer to the random seeds by name to match them in pmap
        ...future.f_wrapper_seed <- function(..., ...future.dots, ...future.seeds_ii) { # ...future.seed_ii will be a single element of that object
          assign(".Random.seed", ...future.seeds_ii, envir = globalenv(), inherits = FALSE)
          .out <- do.call(...future.f, c(list(...), ...future.dots))
          if(.progress) update_progress(temp_file_con)
          .out
        }

        ...future.map(...future.lst_ii, ...future.f_wrapper_seed)

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
    poll_progress(fs, globals$temp_file, n.l_elems)
  }

  ## FINISHED - Not needed anymore
  rm(list = c("chunks", "globals", "envir"))

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 7. Resolve
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  values <- multi_resolve(fs) # no names with pmap

  if (debug) mdebug("future_map_*() ... DONE")

  values
}
