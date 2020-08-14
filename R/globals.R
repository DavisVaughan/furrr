get_globals_and_packages <- function(globals, packages, map_fn, fn, dots, env_globals) {
  objectSize <- import_future("objectSize")

  packages_out <- "purrr"

  # Always get `.f`
  globals_fn <- list(...furrr_fn = fn)
  globals_fn <- future::as.FutureGlobals(globals_fn)
  globals_fn <- future::resolve(globals_fn)
  globals_fn <- set_total_size(globals_fn, objectSize(globals_fn))

  # Always get `...`.
  globals_dots <- list(...furrr_dots = dots)
  globals_dots <- future::as.FutureGlobals(globals_dots)
  globals_dots <- future::resolve(globals_dots)
  globals_dots <- set_total_size(globals_dots, objectSize(globals_dots))

  # Always get `map_fn`
  globals_map_fn <- list(...furrr_map_fn = map_fn)
  globals_map_fn <- future::as.FutureGlobals(globals_map_fn)
  globals_map_fn <- future::resolve(globals_map_fn)
  globals_map_fn <- set_total_size(globals_map_fn, objectSize(globals_map_fn))

  # Always get chunk specific placeholders
  globals_extra <- list(
    ...furrr_chunk_args = NULL,
    ...furrr_chunk_seeds = NULL,
    ...furrr_globals_max_size = NULL
  )
  globals_extra <- future::as.FutureGlobals(globals_extra)
  globals_extra <- future::resolve(globals_extra)
  globals_extra <- set_total_size(globals_extra, objectSize(globals_extra))

  globals_out <- c(
    globals_fn,
    globals_dots,
    globals_map_fn,
    globals_extra
  )

  # Collect all globals recursively
  # Search in the parent frame of the `future_*()` call for globals
  if (is_true(globals)) {
    # Lookup `.f` globals in the function env of `.f` (#153)
    env_fn <- fn_env(fn)

    gp_fn <- future::getGlobalsAndPackages(fn, envir = env_fn, globals = TRUE)
    gp_dots <- future::getGlobalsAndPackages(dots, envir = env_globals, globals = TRUE)

    globals_gp_fn <- enforce_future_globals(gp_fn$globals)
    globals_gp_dots <- enforce_future_globals(gp_dots$globals)

    globals_out <- unique(c(globals_out, globals_gp_fn, globals_gp_dots))
    packages_out <- unique(c(packages_out, gp_fn$packages, gp_dots$packages))
  }

  # Collect only explicitly selected globals,
  # but be lax about it with `mustExist = FALSE`.
  if (is.character(globals)) {
    globals_chr <- globals::globalsByName(globals, envir = env_globals, mustExist = FALSE)
    globals_chr <- enforce_future_globals(globals_chr)
    globals_out <- c(globals_out, globals_chr)
  }

  # Assume supplied named inputs are the globals.
  if (is.list(globals)) {
    globals_lst <- enforce_future_globals(globals)
    globals_out <- c(globals_out, globals_lst)
  }

  # Add user specified packages
  if (!is.null(packages)) {
    packages_out <- unique(c(packages_out, packages))
  }

  out <- list(globals = globals_out, packages = packages_out)

  out
}

# ------------------------------------------------------------------------------

# `getGlobalsAndPackages()` should always return a FutureGlobals object
# with a non-NA `total_size`, but HenrikBengtsson/future#410 proves it
# doesn't, so be careful and ensure correct behavior
enforce_future_globals <- function(x) {
  x <- future::as.FutureGlobals(x)

  if (is.na(get_total_size(x))) {
    objectSize <- import_future("objectSize")
    x <- set_total_size(x, objectSize(x))
  }

  x
}
