multi_resolve <- function(fs, nms) {

  nchunks <- length(fs)
  debug   <- getOption("future.debug", FALSE)

  ## Resolving futures
  if (debug) mdebug("Resolving %d futures (chunks) ...", nchunks)
  values <- values(fs)
  if (debug) mdebug("Resolving %d futures (chunks) ... DONE", nchunks)

  ## Not needed anymore
  rm(list = "fs")

  ## Fold result together
  if (debug) mdebug("Reducing values from %d chunks ...", nchunks)
  values <- fold(values, c)
  names(values) <- nms
  if (debug) mdebug("Reducing values from %d chunks ... DONE", nchunks)

  values
}
