gather_globals_and_packages <- function(.options, .map, .f, .progress, envir, ...) {

  debug <- getOption("future.debug", FALSE)
  objectSize <- import_future("objectSize")

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 1. Global variables
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  ## The default is to gather globals
  if (is.null(.options$globals)) .options$globals <- TRUE

  packages <- NULL
  globals <- .options$globals

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
      stop("Invalid argument '.options$globals'. All globals must be named.")
    }
  } else {
    stop("Invalid argument '.options$globals': ", mode(globals))
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
  reserved <- intersect(c("...future.f", "...future.x_ii", "...future.y_ii", # technically only needed for map2, but this is okay
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

  if (!is.null(.options$packages)) {
    .options$packages <- unique(.options$packages)
    stopifnot(!anyNA(.options$packages), all(nzchar(.options$packages)))
    packages <- unique(c(packages, .options$packages))
  }

  if (debug) {
    mdebug("Packages to be attached in all futures:")
    mdebug(paste(capture.output(str(packages)), collapse = "\n"))
  }

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 3. Progress
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # .progress always needs to be on the workers for the if statement...
  globals <- c(globals, .progress = .progress)

  # ...but we add the tempfile and the function if .progress = TRUE
  if(.progress) {
    temp_file <- tempfile(fileext = ".txt")
    writeLines("falsetick", temp_file)
    globals <- c(globals, update_progress = update_progress, temp_file = temp_file)
  }

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## 3. Extra functions required on the workers
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  globals <- c(globals, is_bad_rlang_tilde = is_bad_rlang_tilde)

  .options$packages <- packages
  .options$globals  <- globals

  .options
}
