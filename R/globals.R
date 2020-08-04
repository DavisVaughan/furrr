get_globals_and_packages <- function(globals, packages, map_fn, fn, dots, env_globals) {
  objectSize <- import_future("objectSize")

  packages_out <- "purrr"

  # Always get `.f`
  globals_fn <- list(...future_fn = fn)
  globals_fn <- future::as.FutureGlobals(globals_fn)
  globals_fn <- future::resolve(globals_fn)
  attr(globals_fn, "total_size") <- objectSize(globals_fn)

  # Always get `...`.
  globals_dots <- list(...future_dots = dots)
  globals_dots <- future::as.FutureGlobals(globals_dots)
  globals_dots <- future::resolve(globals_dots)
  attr(globals_dots, "total_size") <- objectSize(globals_dots)

  # Always get `map_fn`
  globals_map_fn <- list(...future_map_fn = map_fn)
  globals_map_fn <- future::as.FutureGlobals(globals_map_fn)
  globals_map_fn <- future::resolve(globals_map_fn)
  attr(globals_map_fn, "total_size") <- objectSize(globals_map_fn)

  # Always get chunk specific placeholders
  globals_extra <- list(
    ...future_chunk_args = NULL,
    ...future_chunk_seeds = NULL,
    ...future_globals_max_size = NULL
  )
  globals_extra <- future::as.FutureGlobals(globals_extra)
  globals_extra <- future::resolve(globals_extra)
  attr(globals_extra, "total_size") <- objectSize(globals_extra)

  globals_out <- c(
    globals_fn,
    globals_dots,
    globals_map_fn,
    globals_extra
  )

  # Collect all globals recursively
  # Search in the parent frame of the `future_*()` call for globals
  if (is_true(globals)) {
    dots <- globals_dots[["..."]]

    gp_fn <- future::getGlobalsAndPackages(fn, envir = env_globals, globals = TRUE)
    gp_dots <- future::getGlobalsAndPackages(dots, envir = env_globals, globals = TRUE)

    globals_out <- unique(c(globals_out, gp_fn$globals, gp_dots$globals))
    packages_out <- unique(c(packages_out, gp_fn$packages, gp_dots$packages))
  }

  # Collect only explicitly selected globals,
  # but be lax about it with `mustExist = FALSE`.
  if (is.character(globals)) {
    globals_chr <- globals::globalsByName(globals, envir = env_globals, mustExist = FALSE)
    globals_chr <- future::as.FutureGlobals(globals_chr)

    globals_out <- c(globals_out, globals_chr)
  }

  # Assume supplied named inputs are the globals.
  if (is.list(globals)) {
    globals_lst <- future::as.FutureGlobals(globals)

    globals_out <- c(globals_out, globals_lst)
  }

  # Add user specified packages
  if (!is.null(packages)) {
    packages_out <- unique(c(packages_out, packages))
  }

  out <- list(globals = globals_out, packages = packages_out)

  out
}
