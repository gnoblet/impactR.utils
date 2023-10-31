#' Replace values of a data frame's columns to one value
#'
#' `recode_values()` recodes a singular value. `recode_na()` recodes usual values of NAs, for instance 999, NULL, NaN, and NA.
#'
#' @param df A data frame.
#' @param values A vector of values.
#' @param to_value A value.
#' @param ... Column names.
#'
#' @return A data frame with recoded values to one value.
#'
#' @details If the column type is a character and the replacement a numeric, then the numeric is coerced to a character. If the column type is a numeric and the replacement is a character, then the column is coerced to character. NAs will remains NAs of the right type.
#'
#' @export
recode_values <- function(df, values, to_value, ...) {
  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(df, quoted_cols, "df", arg = "...")

  dplyr::mutate(df, dplyr::across(.cols = dplyr::all_of(quoted_cols), \(x) replace(x, x %in% values, to_value)))
}


#' @rdname recode_values
#' @export
recode_na <- function(df, ...) {
  nas <- c(
    NULL, "NULL", "N/A", "n/a", 999, 998, 888, " ", Inf, -Inf,
    9999, "(vide)", "(empty)", "d/m", "", "NA", "na", "", " ",
    NaN, "NaN", "Na", -999, -9999, -998, -888
  )

  recode_values(df, nas, NA, ...)
}
