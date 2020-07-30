maybe_warn_deprecated_progress <- function(warn, what, env = caller_env(2)) {
  if (!warn) {
    return(invisible())
  }

  what <- paste0("furrr::", what, "(.progress = )")

  details <- paste0(
    "The `.progress` argument has been deprecated in favor of the progressr ",
    "package. It provides a much more robust solution, and is integrated ",
    "with future in such a way that it can relay real-time progress updates ",
    "even on remote connections."
  )

  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = what,
    env = env,
    details = details
  )
}
