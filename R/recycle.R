furrr_recycle_common <- function(x, n) {
  n_x <- length(x)
  lengths <- lengths(x)
  idx <- rep_len(1L, n)

  for (i in seq_len(n_x)) {
    elt <- x[[i]]

    # Don't recycle `NULL` elements, they can be indexed fine by `[` and `[[`
    # and are considered like missing arguments
    if (is.null(elt)) {
      next
    }

    length_elt <- lengths[[i]]

    if (length_elt == n) {
      next
    }

    if (length_elt == 1L) {
      x[[i]] <- elt[idx]
      next
    }

    abort(paste0("Internal error: Incompatible lengths at location ", i, "."))
  }

  x
}

# Can't use `vec_size_common()` because we extract elements with `[[` and
# respect length invariants, not size invariants
furrr_length_common <- function(x) {
  # Don't consider `NULL` elements in common size
  x <- compact_null(x)

  # Handle empty pmap input
  if (length(x) == 0L) {
    return(0L)
  }

  lengths <- lengths(x)
  indices <- seq_along(lengths)

  purrr::reduce2(lengths, indices, furrr_length2, .init = 1L)
}

furrr_length2 <- function(x, y, i) {
  if (x == 1L) {
    y
  } else if (y == 1L) {
    x
  } else if (x == y) {
    x
  } else {
    msg <- paste0("Can't recycle length ", x, " and length ", y, " at location ", i, ".")
    abort(msg)
  }
}

compact_null <- function(x) {
  null <- purrr::map_lgl(x, is.null)

  if (any(null)) {
    x[!null]
  } else {
    x
  }
}
