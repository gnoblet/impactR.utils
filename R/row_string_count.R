#' Count the number of row-wise occurrences of a pattern
#'
#' `row_pattern_count()` count row-wise occurences of a pattern over the data frame; `row_na_count()` count row-wise number of NAs over the data frame.
#'
#' @param df A data frame.
#' @param pattern A pattern to pass to `stringr::str_count()`. Default to "dnk".
#' @param new_colname The newly-mutated column name. Default to "count".
#'
#' @return A mutated data frame.
#'
#' @export
row_string_count <- function(df, pattern = "", new_colname = "count") {
  df <- dplyr::mutate(df, "{new_colname}" := purrr::pmap_int(
    purrr::keep(df, \(x) is.character(x)),
    ~ sum(stringr::str_count(c(...), pattern), na.rm = T)
  ))

  return(df)
}


#' @rdname  row_string_count
#' @export
row_na_count <- function(df, new_colname = "count") {
  df <- dplyr::mutate(df, "{new_colname}" := purrr::pmap_int(
    df,
    ~ sum(is.na(c(...)), na.rm = T)
  ))

  return(df)
}
