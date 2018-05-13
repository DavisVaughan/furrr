poll_progress <- function(fs, temp_file, rule_max_width) {

  debug <- getOption("future.debug", FALSE)
  if (debug) mdebug("Polling for progress ...")

  not_resolved_once <- !all_resolved(fs)

  # Poll the files until all the jobs are complete
  while (!all_resolved(fs)) {

    # -1 because of empty tick needed to init file.
    # Otherwise if we get here too quickly it gives error
    temp_file_con <- file(temp_file, "r")
    n_ticks <- length(readLines(temp_file_con)) - 1
    close(temp_file_con)

    max_width <- console_width()
    progress_width <- 10
    finish_width <- 5
    carriage_width <- 1
    filler_width <- max_width - progress_width - finish_width - carriage_width

    rule_width <- floor(filler_width * n_ticks / rule_max_width)
    space_width <- filler_width - rule_width

    spaces <- paste0(rep(" ", times = space_width), collapse = "")

    # The one line - symbol came from cli::symbols$line
    progress <- paste0(rep("\u2500", times = rule_width), collapse = "")
    all_text <- paste0("Progress: ", progress, spaces, " 100%")

    cat("\r", all_text)
    utils::flush.console()
  }

  if(not_resolved_once) {
    # Separate progress from output
    max_width <- console_width()
    progress_width <- 10
    finish_width <- 5
    carriage_width <- 1
    filler_width <- max_width - progress_width - finish_width - carriage_width
    progress <- paste0(rep("\u2500", times = filler_width), collapse = "")
    all_text <- paste0("Progress: ", progress, " 100%")
    cat("\r", all_text)
    cat("\n\n")
  }

  if (debug) mdebug("Polling for progress ... DONE")

}


# Needed for progress updates
update_progress <- function (file) {
  progress_text <- sprintf("tick\n")
  cat(progress_text, file = file, append = TRUE)
}

# Needed for progress updates
all_resolved <- function (futures) {
  each_resolved <- vapply(futures, future::resolved, FALSE)
  all(each_resolved)
}

console_width <- function() {
  width <- Sys.getenv("RSTUDIO_CONSOLE_WIDTH", getOption("width", 80))
  as.integer(width)
}
