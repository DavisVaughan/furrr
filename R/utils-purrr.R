# nocov start

probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  }
  else {
    purrr::map_lgl(.x, .p, ...)
  }
}

inv_which <- function(x, sel) {
  if (is.character(sel)) {
    names <- names(x)
    if (is.null(names)) {
      stop("character indexing requires a named object", call. = FALSE)
    }
    names %in% sel
  } else if (is.numeric(sel)) {
    if (any(sel < 0)) {
      !seq_along(x) %in% abs(sel)
    } else {
      seq_along(x) %in% sel
    }

  } else {
    stop("unrecognised index type", call. = FALSE)
  }
}

vec_index <- function(x){
  names(x) %||% seq_along(x)
}

check_tidyselect <- function(){
  if (!is_installed("tidyselect")) {
    abort("Using tidyselect in `future_map_at()` requires tidyselect")
  }
}

at_selection <- function(nm, .at) {
  if (is_quosures(.at)){
    check_tidyselect()
    .at <- tidyselect::vars_select(.vars = nm, !!!.at)
  }
  .at
}

# nocov end
