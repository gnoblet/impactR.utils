#' Get colnames that start, end, or both with a string
#'
#' @param df  A data frame.
#' @param pattern_start A start string
#'
#' @return A vector of column names
#'
#' @export
df_col_start <- function(df, pattern_start){
  cols <- colnames(df)[startsWith(colnames(df), pattern_start)]

  if (length(cols) == 0) rlang::warn("The resulting vector is empty.")

  return(cols)
}


#' @rdname df_col_start
#'
#' @param pattern_end An end string.
#'
#' @export
df_col_end <- function(df, pattern_end){
  cols <- colnames(df)[endsWith(colnames(df), pattern_end)]

  if (length(cols) == 0) rlang::warn("The resulting vector is empty.")

  return(cols)
}




#' @rdname df_col_start
#'
#' @export
df_col_start_end <- function(df, pattern_start, pattern_end){
  cols <- colnames(df)[startsWith(colnames(df), pattern_start) & endsWith(colnames(df), pattern_end)]

  if (length(cols) == 0) rlang::warn("The resulting vector is empty.")

  return(cols)
}
