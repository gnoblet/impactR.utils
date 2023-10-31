#' Get a subset of a vector
#'
#' `subvec_in()` gets a subset of a vector if vector elements are in a given `set`. `subvec_not_in()` get a subset of a vector if vector elements are NOT in a fiven `set`.
#'
#' @param vector A vector to subset.
#' @param set A set-vector.
#'
#' @return A subset of a list or a vector.
#'
#' @export
subvec_in <- function(vector, set) {
  vector[vector %in% set]
}

#' @rdname subvec_in
#' @export
subvec_not_in <- function(vector, set) {
  vector[!(vector %in% set)]
}
