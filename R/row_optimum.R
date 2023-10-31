#'  Row-wise optimum (default to pmax) of several columns
#'
#' @param df A data frame.
#' @param ... Numeric column names.
#' @param optimum Should we calculate "max", "min" or "both"? Default to TRUE.
#' @param max_name Column name for the mutated pmax.
#' @param min_name Column name for the mutated pmin.
#' @param na_rm Remove NAs. Default to TRUE.
#' @param keep To be used within mutate. Default to "all".
#'
#' @importFrom rlang `:=`
#'
#' @return A data frame with pmax, pmin or both (and all columns or none, depending on "keep")
#'
#' @export
row_optimum <- function(df, ...,  optimum = "max", max_name = "pmax", min_name = "pmin", na_rm = TRUE, keep = "all") {

  cols <- rlang::enquos(...)
  quoted_cols <- purrr::map_chr(cols, rlang::as_name)
  purrr::map(quoted_cols, \(x) if_not_in_stop(df, x, "df", "..."))
  purrr::map(quoted_cols, \(x) {if(!is.numeric(dplyr::pull(df, dplyr::all_of(x)))) {abort_bad_argument(x, "be numeric", df[[x]])}})


  if (optimum == "both") {
    to_return <- dplyr::mutate(df,
                               !!max_name := pmax(!!!cols  , na.rm = na_rm),
                               !!min_name := pmin(!!!cols, na.rm = na_rm),
                                       .keep = keep)
  } else if (optimum == "max") {
    to_return <- dplyr::mutate(df, !!max_name := pmax(!!!cols, na.rm = na_rm), .keep = keep)
  } else if (optimum == "min") {
    to_return <- dplyr::mutate(df, !!min_name := pmin(!!!cols, na.rm = na_rm), .keep = keep)
  } else {
    stop("Arg 'optimum' should be either 'max', 'min' or 'both'.")
  }


  return(to_return)
}
