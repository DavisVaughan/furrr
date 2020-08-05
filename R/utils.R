cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}


import_future <- function(name, default = NULL) {
  import_from(name, default = default, package = "future")
}

import_from <- function(name, default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    get(name, mode = "function", envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such '%s' function: %s()", package, name))
  }
}

compat_id <- function(id) {
  if (is_null(id)) {
    rlang::zap()
  } else {
    id
  }
}

# The point of this helper is to prevent `vec_cbind()` from creating
# packed data frames. It tries to be as performant as possible,
# only checking for data frame existence if there are names on the input.
furrr_list_cbind <- function(x) {
  x <- furrr_list_cbind_names_fixup(x)
  vctrs::vec_cbind(!!!x)
}

furrr_list_cbind_names_fixup <- function(x) {
  names <- names(x)

  if (is.null(names)) {
    return(x)
  }

  is_df <- purrr::map_lgl(x, is.data.frame)

  if (!any(is_df)) {
    return(x)
  }

  # Prevent packing in `vec_cbind()` by assigning `""` names
  names[is_df] <- ""

  names(x) <- names

  x
}
