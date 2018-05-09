generate_seed_streams <- function(seed, n_seeds) {

  debug <- getOption("future.debug", FALSE)

  ## A pregenerated sequence of random seeds?
  if (is.list(seed)) {

    if (debug) mdebug("Using a pre-define stream of random seeds ...", n_seeds)

    nseed <- length(seed)
    if (nseed != n_seeds) {
      stop("Argument 'seed' is a list, which specifies the sequence of seeds to be used for each element in '.x', but length(seed) != length(.x): ", nseed, " != ", n_seeds)
    }

    ## Assert same type of RNG seeds?
    ns <- unique(unlist(lapply(seed, FUN = length), use.names = FALSE))
    if (length(ns) != 1) {
      stop("The elements of the list specified in argument 'seed' are not all of the same lengths (did you really pass RNG seeds?): ", hpaste(ns))
    }

    ## Did use specify scalar integers as meant for set.seed()?
    if (ns == 1L) {
      stop("Argument 'seed' is invalid. Pre-generated random seeds must be valid .Random.seed seeds, which means they should be all integers and consists of two or more elements, not just one.")
    }

    types <- unlist(lapply(seed, FUN = typeof), use.names = FALSE)
    if (!all(types == "integer")) {
      stop("The elements of the list specified in argument 'seed' are not all integers (did you really pass RNG seeds?): ", hpaste(unique(types)))
    }

    ## Check if valid random seeds are specified.
    ## For efficiency, only look at the first one.
    if (!is_valid_random_seed(seed[[1]])) {
      stop("The list in argument 'seed' does not seem to hold elements that are valid .Random.seed values: ", capture.output(str(seeds[[1]])))
    }

    seeds <- seed

    if (debug) mdebug("Using a pre-define stream of random seeds ... DONE", n_seeds)

  } else {

    if (debug) mdebug("Generating random seed streams for %d elements ...", n_seeds)

    ## Generate sequence of _all_ RNG seeds starting with an initial seed
    ## '.seed' that is based on argument 'seed'.
    .seed <- as_lecyer_cmrg_seed(seed)

    seeds <- vector("list", length = n_seeds)
    for (ii in seq_len(n_seeds)) {
      ## RNG substream seed used in call FUN(.x[[ii]], ...):
      ## This way each future can in turn generate further seeds, also
      ## recursively, with minimal risk of generating the same seeds as
      ## another future. This should make it safe to recursively call
      ## future_lapply(). /HB 2017-01-11
      seeds[[ii]] <- nextRNGSubStream(.seed)

      ## Main random seed for next iteration (= ii + 1)
      .seed <- nextRNGStream(.seed)
    }

    if (debug) mdebug("Generating random seed streams for %d elements ... DONE", n_seeds)
  }

  seeds
}
