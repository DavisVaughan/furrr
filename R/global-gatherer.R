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
  .options$scan_for_x_globals <- FALSE

  if (is.logical(globals)) {

    ## Gather all globals?
    if (globals) {

      if (debug) mdebug("Finding globals ...")

      .options$scan_for_x_globals <- TRUE

      # Find the globals / packages for both .f and .x
      expr <- do.call(call, args = c(list(".f"), list(...)))
      gp_f <- future::getGlobalsAndPackages(expr, envir = envir, globals = TRUE)
      globals <-  gp_f$globals
      packages <- gp_f$packages
      gp_f <- NULL

      if (debug) {
        mdebug(" - globals found: [%d] %s", length(globals), hpaste(sQuote(names(globals))))
        mdebug(" - needed namespaces: [%d] %s", length(packages), hpaste(sQuote(packages)))
        mdebug("Finding globals ... DONE")
      }

    } else {
      ## globals = FALSE
      globals <- c(".f", names(list(...)), "...")
      globals <- globals::globalsByName(globals, envir = envir, mustExist = FALSE)
    }
  } else if (is.character(globals)) {
    globals <- unique(c(globals, ".f", names(list(...)), "..."))
    globals <- globals::globalsByName(globals, envir = envir, mustExist = FALSE)
  } else if (is.list(globals)) {
    names <- names(globals)
    if (length(globals) > 0 && is.null(names)) {
      stop("Invalid argument '.options$globals'. All globals must be named.")
    }
  } else {
    stop("Invalid argument '.options$globals': ", mode(globals))
  }
  globals <- future::as.FutureGlobals(globals)
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
    dotdotdot <- globals::globalsByName("...", envir = envir, mustExist = TRUE)
    dotdotdot <- future::as.FutureGlobals(dotdotdot)
    dotdotdot <- future::resolve(dotdotdot)
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
    mdebug(paste(utils::capture.output(utils::str(globals)), collapse = "\n"))
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
    mdebug(paste(utils::capture.output(utils::str(packages)), collapse = "\n"))
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

  .options$packages <- packages
  .options$globals  <- globals

  .options
}


gather_globals_and_packages_.x_ii <- function(globals, packages, .x_ii, chunk, envir) {

  debug <- getOption("future.debug", FALSE)

  globals_ii <- globals
  packages_ii <- packages

  rm(globals, packages)

  # Search for .x_ii specific globals and packages
  gp <- future::getGlobalsAndPackages(.x_ii, envir = envir, globals = TRUE)
  globals_.x_ii <- gp$globals
  packages_.x_ii <- gp$packages
  gp <- NULL

  if (debug) {
    mdebug(" - globals found in '.x' for chunk #%d: [%d] %s", chunk, length(globals_.x_ii), hpaste(sQuote(names(globals_.x_ii))))
    mdebug(" - needed namespaces for '.x' for chunk #%d: [%d] %s", chunk, length(packages_.x_ii), hpaste(sQuote(packages_.x_ii)))
  }

  # Export them
  if (length(globals_.x_ii) > 0L) {

    reserved <- intersect(
      c("...future.FUN", "...future.x_ii", "...future.seeds_ii"),
      names(globals_.x_ii)
    )

    if (length(reserved) > 0) {
      stop("Detected globals using reserved variables names: ",
           paste(sQuote(reserved), collapse = ", "))
    }

    globals_.x_ii <- future::as.FutureGlobals(globals_.x_ii)
    globals_ii <- unique(c(globals_ii, globals_.x_ii))

  }

  ## Packages needed due to globals in '.x_ii'?
  if (length(packages_.x_ii) > 0L) {
    packages_ii <- unique(c(packages_ii, packages_.x_ii))
  }

  list(globals = globals_ii, packages = packages_ii)
}
