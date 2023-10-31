#' Remove columns from a data frame
#'
#' @param df A data frame.
#' @param ... Column names.
#'
#' @return A tibble with columns removed
#'
#' @export
deselect <- function(df, ...) {
  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(df, quoted_cols, "df", arg = "...")

  dplyr::select(df, -dplyr::all_of(quoted_cols))
}
