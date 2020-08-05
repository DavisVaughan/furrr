probe <- function(.x, .p, ...) {
  if (rlang::is_logical(.p)) {
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
      stop("character indexing requires a named object",
           call. = FALSE)
    }
    names %in% sel
  }
  else if (is.numeric(sel)) {
    seq_along(x) %in% sel
  }
  else {
    stop("unrecognised index type", call. = FALSE)
  }
}

vec_index <- function(x){
  names(x) %||% seq_along(x)
}

as_invoke_function <- function(f) {
  if (is.function(f)) {
    list(f)
  }
  else {
    f
  }
}
