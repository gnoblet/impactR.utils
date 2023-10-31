#' Rename columns
#'
#' @param df A data frame.
#' @param old_cols A vector of old quoted variable names.
#' @param new_cols A vector of new quoted variable names.
#'
#' @return Updated tibble with new names
#'
#' @export
rename_cols <- function(df, old_cols, new_cols) {
  if_not_in_stop(df, old_cols, "df", "old_cols")

  if (length(old_cols) != length(new_cols)) {
    abort_bad_argument("`new_cols`", "be the same length as `old_cols`")
  }

  df <- dplyr::rename_with(df, ~new_cols, .cols = dplyr::all_of(old_cols))

  return(df)
}
