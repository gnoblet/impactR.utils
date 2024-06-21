#' @title Stop statement "If not in colnames" with colnames
#'
#' @param df A dataframe
#' @param cols A vector of column names (quoted)
#' @param df_name Provide the tibble name as a character string
#' @param arg Default to NULL.
#'
#' @return A stop statement
#'
#' @export
if_not_in_stop <- function(df, cols, df_name, arg = NULL) {
  missing_cols <- subvec_not_in(cols, colnames(df))

  if (is.null(arg)) {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns are missing in `{df_name}`:")
    } else {
      msg <- glue::glue("The following column is missing in `{df_name}`:")
    }
  } else {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns from `{arg}` are missing in `{df_name}`:")
    } else {
      msg <- glue::glue("The following column from `{arg}` is missing in `{df_name}`:")
    }
  }
  if (length(missing_cols) >= 1) {
    rlang::abort(
      c("Missing columns",
        "*" =
          paste(
            msg,
            paste(
              missing_cols,
              collapse = ", "
            )
          )
      )
    )
  }
}


#' @title Abord bad argument
#'
#' @param arg1 An argument
#' @param must What arg must be
#' @param not Optional. What arg must not be.
#' @param arg2 Another argument
#' @param same The same type as arg1
#'
#' @return A stop statement
#'
#' @export
abort_bad_argument <- function(arg1, must, not = NULL, arg2 = NULL, same = NULL) {
  msg <- glue::glue("`{arg1}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  if (!is.null(same) & !is.null(arg2)) {
    same <- typeof(same)
    msg_i <- glue::glue("`{arg2}` is {same}.")
    msg <- c(msg, "i" = msg_i)
  }

  rlang::abort("error_bad_argument",
    message = msg,
    arg1 = arg1,
    must = must,
    not = not,
    arg2 = arg2,
    same = same
  )
}
