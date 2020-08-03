get_globals_and_packages_fn_and_dots <- function(globals,
                                                 fn,
                                                 dots,
                                                 env) {
  if (is.logical(globals)) {
    gp <- get_globals_and_packages_logical(globals, fn, dots, env)
  } else if (is.character(globals)) {
    gp <- get_globals_and_packages_character(globals, env)
  } else if (is.list(globals)) {
    gp <- get_globals_and_packages_list(globals)
  } else {
    abort("Internal error: Unknown type of `globals`.")
  }

  globals <- gp$globals

  # Require `fn` to be exported
  if (!is.element(".f", names(globals))) {
    globals <- c(globals, ...future_fn = fn)
  }

  # Require `...` to be exported
  if (!is.element("...", names(globals))) {
    objectSize <- import_future("objectSize")

    dotdotdot <- globals::globalsByName("...", envir = env, mustExist = TRUE)
    dotdotdot <- future::as.FutureGlobals(dotdotdot)
    dotdotdot <- future::resolve(dotdotdot)

    attr(dotdotdot, "total_size") <- objectSize(dotdotdot)

    globals <- c(globals, dotdotdot)
  }

  gp$globals <- globals

  gp
}

get_globals_and_packages_logical <- function(globals,
                                             fn,
                                             dots,
                                             env) {
  # Don't collect globals
  if (is_false(globals)) {
    return(new_globals_and_packages())
  }

  # Construct `fn()` expression with dots, but no `.x` args
  expr_fn <- rlang::call2(fn, !!!dots)

  gp <- getGlobalsAndPackages(expr_fn, envir = env, globals = TRUE)

  new_globals_and_packages(
    globals = gp$globals,
    packages = gp$packages
  )
}

get_globals_and_packages_character <- function(globals, env) {
  globals <- unique(c(globals, ".f", "..."))
  globals <- globals::globalsByName(globals, envir = env, mustExist = FALSE)
  new_globals_and_packages(globals = globals)
}

get_globals_and_packages_list <- function(globals) {
  new_globals_and_packages(globals = globals)
}

new_globals_and_packages <- function(globals = NULL, packages = NULL) {
  globals <- future::as.FutureGlobals(globals)
  list(globals = globals, packages = packages)
}
