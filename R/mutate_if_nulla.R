#' Mutate with replacement if NULL or NA values
#'
#' @param df A data frame.
#' @param col A column of `df`.
#' @param replacement A replacement of the same type as `col`.
#'
#' @return A mutated data frame.
#' @export
mutate_if_nulla <- function(df, col, replacement) {
  #---- Checks

  # is .tbl a data.frame or coercible to one?
  if (!is.data.frame(df)) rlang::abort(".tbl must have 'data.frame' among its classes.")

  # col in .tbl
  col_name <- rlang::as_name(rlang::enquo(col))
  if_not_in_stop(df, col_name, "df", "col")

  # replacement type string
  if (typeof(df[[col_name]]) != typeof(replacement)) {
    abort_bad_argument("replacement", "be the same type as `col`", not = replacement, arg2 = "col", df[[col_name]])
  }

  df <- dplyr::mutate(df, "{{ col }}" := ifelse(is.na({{ col }}) | is.null({{ col }}), replacement, {{ col }}))

  return(df)
}
