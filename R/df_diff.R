#' Remove columns from a data frame that exists in another tibble
#'
#' @param df_a A data frame to remove columns from.
#' @param df_b A data frame to extract columns names from.
#' @param ... Columns from df_b and df_a to keep in data frame.
#'
#' @return A tibble with some columns removed.
#'
#' @export
df_diff <- function(df_a, df_b, ...) {
  #-------- Checks

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(df_b, quoted_cols, "df_b", arg = "cols")

  #-------- Make the diff

  df_b <- dplyr::select(df_b, -dplyr::all_of(quoted_cols))

  cols_df_b <- colnames(df_b)

  df_a <- dplyr::select(df_a, -dplyr::any_of(cols_df_b))

  return(df_a)
}
