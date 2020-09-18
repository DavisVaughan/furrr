# nocov start

poll_progress <- function(futures, file, n_x) {
  symbol <- get_progress_symbol()

  prefix <- "Progress: "
  suffix <- " 100%"

  width_prefix <- nchar(prefix)
  width_suffix <- nchar(suffix)
  width_carriage <- 1L

  stderr <- stderr()

  while (any_running(futures)) {
    con <- file(file, open = "r")
    n_ticks <- get_n_ticks(con)
    close(con)

    # Console width might change while we poll
    width_max <- console_width()
    width_usable <- width_max - width_prefix - width_suffix - width_carriage

    width_rule <- floor(width_usable * n_ticks / n_x)
    width_space <- width_usable - width_rule

    space <- paste0(rep(" ", times = width_space), collapse = "")
    rule <- paste0(rep(symbol, times = width_rule), collapse = "")

    out <- paste0(prefix, rule, space, suffix)

    cat("\r", out, file = stderr)
    utils::flush.console()
  }
}

get_n_ticks <- function(con) {
  line <- readLines(con, n = 1L, warn = FALSE)

  if (length(line) == 0L) {
    line <- ""
  }

  nchar(line)
}

any_running <- function(futures) {
  !all(future::resolved(futures))
}

console_width <- function() {
  width <- Sys.getenv("RSTUDIO_CONSOLE_WIDTH", getOption("width", 80))
  as.integer(width)
}

# ------------------------------------------------------------------------------

# Adapted from cli's onload properties
# to dynamically switch depending on utf8 availability
get_progress_symbol <- function() {
  if (is_utf8_output()) {
    "\u2500"
  } else {
    "-"
  }
}

is_utf8_output <- function() {
  l10n_info()$`UTF-8` && !is_latex_output()
}

is_latex_output <- function () {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }

  get("is_latex_output", asNamespace("knitr"))()
}

# ------------------------------------------------------------------------------

assert_progress <- function(progress) {
  if (!is_bool(progress)) {
    abort("`.progress` must be a single logical value.")
  }

  invisible(progress)
}

# ------------------------------------------------------------------------------

# - Sequential blocks in the `future()` call, so no progress is ever shown
# - Cluster is generally used for multi-computer setups, and would end up
#   writing into files on the remote workers, which would never be shown.
progress_enabled_plans <- c(
  "multicore",
  "multisession",
  "multiprocess"
)

reconcile_progress_with_strategy <- function(progress) {
  if (is_false(progress)) {
    return(progress)
  }

  plan <- future::plan()
  progress_enabled_plan <- inherits_any(plan, progress_enabled_plans)

  if (!progress_enabled_plan) {
    progress <- FALSE
  }

  progress
}

# nocov end
