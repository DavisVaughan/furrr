generate_balanced_chunks <- function(scheduling, n.x) {

  debug <- getOption("future.debug", FALSE)

  if (is.logical(scheduling)) {

    if (scheduling) {
      nbr_of_futures <- nbrOfWorkers()
      if (nbr_of_futures > n.x) nbr_of_futures <- n.x
    } else {
      nbr_of_futures <- n.x
    }

  } else {
    ## Treat 'scheduling' as the number of futures per worker.
    stopifnot(scheduling >= 0)
    nbr_of_workers <- nbrOfWorkers()
    if (nbr_of_workers > n.x) nbr_of_workers <- n.x
    nbr_of_futures <- scheduling * nbr_of_workers
    if (nbr_of_futures < 1) {
      nbr_of_futures <- 1L
    } else if (nbr_of_futures > n.x) {
      nbr_of_futures <- n.x
    }
  }

  chunks <- splitIndices(n.x, ncl = nbr_of_futures)
  if (debug) mdebug("Number of chunks: %d", length(chunks))

  chunks
}
