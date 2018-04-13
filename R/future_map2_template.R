future_map2_template <- function(.map, .type, .x, .y, .f, ..., future.globals = TRUE, future.packages = NULL, future.seed = FALSE, future.lazy = FALSE, future.scheduling = 1.0) {

  # Create function from .f
  .f <- purrr::as_mapper(.f, ...) # ... required in case you pass .null / .default through for purrr::as_mapper.numeric

  objectSize <- import_future("objectSize")

  stopifnot(is.function(.f))

  stopifnot(is.logical(future.lazy))

  stopifnot(!is.null(future.seed))

  stopifnot(length(future.scheduling) == 1, !is.na(future.scheduling),
            is.numeric(future.scheduling) || is.logical(future.scheduling))

  ## Nothing to do?
  n.x <- length(.x)
  n.y <- length(.y)
  if (n.x == 0 || n.y == 0)  {
    # To stay consistent with purrr::map_*() return types
    # purrr handles this at the C level
    empty_type <- switch(
      .type,
      "character" = character(),
      "double"    = double(),
      "list"      = list(),
      "integer"   = integer(),
      "logical"   = logical()
    )
    return(empty_type)
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

  debug <- getOption("future.debug", FALSE)

  if (debug) mdebug("future_lapply() ...")

  ## NOTE TO SELF: We'd ideally have a 'future.envir' argument also for
  ## future_lapply(), cf. future().  However, it's not yet clear to me how
  ## to do this, because we need to have globalsOf() to search for globals
  ## from the current environment in order to identify the globals of
  ## arguments 'FUN' and '...'. /HB 2017-03-10
  future.envir <- environment()  ## Not used; just to clarify the above.

  envir <- future.envir

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 1. Global variables
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## The default is to gather globals
  if (is.null(future.globals)) future.globals <- TRUE

  packages <- NULL
  globals <- future.globals
  if (is.logical(globals)) {
    ## Gather all globals?
    if (globals) {
      if (debug) mdebug("Finding globals ...")

      expr <- do.call(call, args = c(list(".f"), list(...)))
      gp <- getGlobalsAndPackages(expr, envir = envir, globals = TRUE)
      globals <- gp$globals
      packages <- gp$packages
      gp <- NULL

      if (debug) {
        mdebug(" - globals found: [%d] %s", length(globals), hpaste(sQuote(names(globals))))
        mdebug(" - needed namespaces: [%d] %s", length(packages), hpaste(sQuote(packages)))
        mdebug("Finding globals ... DONE")
      }
    } else {
      ## globals = FALSE
      globals <- c(".f", names(list(...)), "...")
      globals <- globalsByName(globals, envir = envir, mustExist = FALSE)
    }
  } else if (is.character(globals)) {
    globals <- unique(c(globals, ".f", names(list(...)), "..."))
    globals <- globalsByName(globals, envir = envir, mustExist = FALSE)
  } else if (is.list(globals)) {
    names <- names(globals)
    if (length(globals) > 0 && is.null(names)) {
      stop("Invalid argument 'future.globals'. All globals must be named.")
    }
  } else {
    stop("Invalid argument 'future.globals': ", mode(globals))
  }
  globals <- as.FutureGlobals(globals)
  stopifnot(inherits(globals, "FutureGlobals"))

  names <- names(globals)
  if (!is.element(".f", names)) {
    globals <- c(globals, .f = .f)
  }

  # The purrr function that gets used must be passed as a global
  # This mainly affects multisession
  if (!is.element(".map", names)) {
    globals <- c(globals, .map = .map)
  }

  if (!is.element("...", names)) {
    if (debug) mdebug("Getting '...' globals ...")
    dotdotdot <- globalsByName("...", envir = envir, mustExist = TRUE)
    dotdotdot <- as.FutureGlobals(dotdotdot)
    dotdotdot <- resolve(dotdotdot)
    attr(dotdotdot, "total_size") <- objectSize(dotdotdot)
    if (debug) mdebug("Getting '...' globals ... DONE")
    globals <- c(globals, dotdotdot)
  }

  ## Assert there are no reserved variables names among globals
  reserved <- intersect(c("...future.f", "...future.x_ii", "...future.y_ii",
                          "...future.seeds_ii"), names)
  if (length(reserved) > 0) {
    stop("Detected globals using reserved variables names: ",
         paste(sQuote(reserved), collapse = ", "))
  }

  ## Avoid .f() clash with map(.x, .f, ...) below.
  names <- names(globals)
  names[names == ".f"] <- "...future.f"
  names[names == ".map"] <- "...future.map" # add the map function
  names(globals) <- names

  if (debug) {
    mdebug("Globals to be used in all futures:")
    mdebug(paste(capture.output(str(globals)), collapse = "\n"))
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 2. Packages
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # purrr is always included
  packages <- unique(c(packages, "purrr"))

  if (!is.null(future.packages)) {
    stopifnot(is.character(future.packages))
    future.packages <- unique(future.packages)
    stopifnot(!anyNA(future.packages), all(nzchar(future.packages)))
    packages <- unique(c(packages, future.packages))
  }

  if (debug) {
    mdebug("Packages to be attached in all futures:")
    mdebug(paste(capture.output(str(packages)), collapse = "\n"))
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 3. Reproducible RNG (for sequential and parallel processing)
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  seed <- future.seed

  ## Placeholder for all RNG stream seeds.
  seeds <- NULL

  ## Don't use RNGs? (seed = FALSE)
  if (is.logical(seed) && !is.na(seed) && !seed) seed <- NULL

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

    ## A pregenerated sequence of random seeds?
    if (is.list(seed)) {
      if (debug) mdebug("Using a pre-define stream of random seeds ...", n.x)

      nseed <- length(seed)
      if (nseed != n.x) {
        stop("Argument 'seed' is a list, which specifies the sequence of seeds to be used for each element in '.x', but length(seed) != length(.x): ", nseed, " != ", n.x)
      }

      ## Assert same type of RNG seeds?
      ns <- unique(unlist(lapply(seed, FUN = length), use.names = FALSE))
      if (length(ns) != 1) {
        stop("The elements of the list specified in argument 'seed' are not all of the same lengths (did you really pass RNG seeds?): ", hpaste(ns))
      }

      ## Did use specify scalar integers as meant for set.seed()?
      if (ns == 1L) {
        stop("Argument 'seed' is invalid. Pre-generated random seeds must be valid .Random.seed seeds, which means they should be all integers and consists of two or more elements, not just one.")
      }

      types <- unlist(lapply(seed, FUN = typeof), use.names = FALSE)
      if (!all(types == "integer")) {
        stop("The elements of the list specified in argument 'seed' are not all integers (did you really pass RNG seeds?): ", hpaste(unique(types)))
      }

      ## Check if valid random seeds are specified.
      ## For efficiency, only look at the first one.
      if (!is_valid_random_seed(seed[[1]])) {
        stop("The list in argument 'seed' does not seem to hold elements that are valid .Random.seed values: ", capture.output(str(seeds[[1]])))
      }

      seeds <- seed

      if (debug) mdebug("Using a pre-define stream of random seeds ... DONE", n.x)
    } else {
      if (debug) mdebug("Generating random seed streams for %d elements ...", n.x)

      ## Generate sequence of _all_ RNG seeds starting with an initial seed
      ## '.seed' that is based on argument 'seed'.
      .seed <- as_lecyer_cmrg_seed(seed)

      seeds <- vector("list", length = n.x)
      for (ii in seq_len(n.x)) {
        ## RNG substream seed used in call FUN(.x[[ii]], ...):
        ## This way each future can in turn generate further seeds, also
        ## recursively, with minimal risk of generating the same seeds as
        ## another future. This should make it safe to recursively call
        ## future_lapply(). /HB 2017-01-11
        seeds[[ii]] <- nextRNGSubStream(.seed)

        ## Main random seed for next iteration (= ii + 1)
        .seed <- nextRNGStream(.seed)
      }

      if (debug) mdebug("Generating random seed streams for %d elements ... DONE", n.x)
    }

    if (debug) mdebug("Generating random seeds ... DONE")
  } ## if (!is.null(seed))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 4. Load balancing ("chunking")
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.logical(future.scheduling)) {
    if (future.scheduling) {
      nbr_of_futures <- nbrOfWorkers()
      if (nbr_of_futures > n.x) nbr_of_futures <- n.x
    } else {
      nbr_of_futures <- n.x
    }
  } else {
    ## Treat 'future.scheduling' as the number of futures per worker.
    stopifnot(future.scheduling >= 0)
    nbr_of_workers <- nbrOfWorkers()
    if (nbr_of_workers > n.x) nbr_of_workers <- n.x
    nbr_of_futures <- future.scheduling * nbr_of_workers
    if (nbr_of_futures < 1) {
      nbr_of_futures <- 1L
    } else if (nbr_of_futures > n.x) {
      nbr_of_futures <- n.x
    }
  }

  chunks <- splitIndices(n.x, ncl = nbr_of_futures)
  if (debug) mdebug("Number of chunks: %d", length(chunks))


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
  ...future.map <- ...future.f <- ...future.x_ii <- ...future.y_ii <- ...future.seeds_ii <- NULL

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

        ...future.map(seq_along(...future.x_ii), .f = function(jj) {
          ...future.x_jj <- ...future.x_ii[[jj]]
          ...future.y_jj <- ...future.y_ii[[jj]]
          ...future.f(...future.x_jj, ...future.y_jj, ...)
        })

      }, envir = envir, lazy = future.lazy, globals = globals_ii, packages = packages)
    } else {
      if (debug) mdebug(" - seeds: [%d] <seeds>", length(chunk))
      globals_ii[["...future.seeds_ii"]] <- seeds[chunk]
      fs[[ii]] <- future({

        ...future.map(seq_along(...future.x_ii), .f = function(jj) {
          ...future.x_jj <- ...future.x_ii[[jj]]
          ...future.y_jj <- ...future.y_ii[[jj]]
          assign(".Random.seed", ...future.seeds_ii[[jj]], envir = globalenv(), inherits = FALSE)
          ...future.f(...future.x_jj, ...future.y_jj, ...)
        })

      }, envir = envir, lazy = future.lazy, globals = globals_ii, packages = packages)
    }

    ## Not needed anymore
    rm(list = c("chunk", "globals_ii"))

    if (debug) mdebug("Chunk #%d of %d ... DONE", ii, nchunks)
  } ## for (ii ...)
  if (debug) mdebug("Launching %d futures (chunks) ... DONE", nchunks)

  ## Not needed anymore
  rm(list = c("chunks", "globals", "envir"))

  ## 4. Resolving futures
  if (debug) mdebug("Resolving %d futures (chunks) ...", nchunks)
  values <- values(fs)
  if (debug) mdebug("Resolving %d futures (chunks) ... DONE", nchunks)

  ## Not needed anymore
  rm(list = "fs")

  if (debug) mdebug("Reducing values from %d chunks ...", nchunks)
  values <- fold(values, c)
  names(values) <- names(.x) # purrr does the same thing with map2
  if (debug) mdebug("Reducing values from %d chunks ... DONE", nchunks)

  if (debug) mdebug("future_lapply() ... DONE")

  values
}
