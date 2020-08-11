make_chunks <- function(n_x, n_workers, scheduling = 1L, chunk_size = NULL) {
  if (is.null(chunk_size)) {
    n_chunks <- compute_n_chunks_from_scheduling(scheduling, n_x, n_workers)
  } else {
    n_chunks <- compute_n_chunks_from_chunk_size(chunk_size, n_x)
  }

  parallel::splitIndices(n_x, n_chunks)
}

compute_n_chunks_from_scheduling <- function(scheduling, n_x, n_workers) {
  if (is.logical(scheduling)) {
    compute_n_chunks_from_scheduling_logical(scheduling, n_x, n_workers)
  } else if (is.integer(scheduling)) {
    compute_n_chunks_from_scheduling_integer(scheduling, n_x, n_workers)
  } else if (identical(scheduling, Inf)) {
    n_x
  }
}

compute_n_chunks_from_scheduling_logical <- function(scheduling, n_x, n_workers) {
  # One future per element of `x`
  if (is_false(scheduling)) {
    return(n_x)
  }

  # One future per worker (if there are enough elements in `x`)
  if (n_workers > n_x) {
    n_x
  } else {
    n_workers
  }
}

# Treat:
# N chunks / 1 workers = scheduling
# So:
# N chunks = (N chunks / 1 workers) * N workers
compute_n_chunks_from_scheduling_integer <- function(scheduling, n_x, n_workers) {
  if (n_workers > n_x) {
    n_workers <- n_x
  }

  n_chunks <- scheduling * n_workers

  # Catch `scheduling == 0L`
  if (n_chunks < 1L) {
    n_chunks <- 1L
  }

  if (n_chunks > n_x) {
    n_chunks <- n_x
  }

  n_chunks
}

# From `future.apply:::makeChunks()`, with description of:
# "Same definition as parallel:::staticNChunks() in R (>= 3.5.0)"
compute_n_chunks_from_chunk_size <- function(chunk_size, n_x) {
  max(1L, ceiling(n_x / chunk_size))
}

# ------------------------------------------------------------------------------

# Custom ordering can be used to process the elements of `x` in. It is
# extracted from `scheduling` or `chunk_size` as the `"ordering"` attribute.
make_order <- function(n_x, scheduling, chunk_size) {
  if (is.null(chunk_size)) {
    ordering <- attr(scheduling, "ordering", exact = TRUE)
    compute_order(ordering, n_x, "scheduling")
  } else {
    ordering <- attr(chunk_size, "ordering", exact = TRUE)
    compute_order(ordering, n_x, "chunk_size")
  }
}

compute_order <- function(ordering, n_x, arg) {
  if (is.null(ordering)) {
    return(NULL)
  }

  if (is.character(ordering) && identical(ordering, "random")) {
    order <- sample_n_with_reset(n_x)
  } else if (is.integer(ordering)) {
    order <- ordering
  } else if (is.double(ordering)) {
    order <- vctrs::vec_cast(ordering, integer(), x_arg = "ordering")
  } else if (is.function(ordering)) {
    order <- ordering(n_x)
  } else {
    abort(paste0(
      "Unknown type of `", arg, "` attribute, `ordering`. The attribute must ",
      "be a character string, an integer, or a function."
    ))
  }

  if (!is.integer(order)) {
    abort("Order computed from `ordering` attribute must be an integer.")
  }

  if (length(order) != n_x) {
    abort("Order computed from `ordering` must have length equal to `.x`.")
  }

  order
}

# Reset on exit to not modify randomness state
sample_n_with_reset <- function(n) {
  oseed <- .GlobalEnv$.Random.seed

  on.exit({
    if (is.null(oseed)) {
      rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      .GlobalEnv$.Random.seed <- oseed
    }
  })

  sample.int(n = n, size = n, replace = FALSE)
}
