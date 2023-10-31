#' Left join a list of dataframes
#'
#' [left_joints()] is a simple reducing functions based on [dplyr::left_join()]. [left_joints_dup()] is a reduce wrapper around [left_join_dup()].
#'
#' @param list A list of data frames.
#' @param ... Columns to join by.
#'
#' @return A left-joined data frame.
#'
#' @export
left_joints <- function(list, ...) {
  # add checks on the list
  # - check if empty list
  # - check if less than two elements
  # - check if elements are not data frames

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  purrr::imap(list, ~ if_not_in_stop(.x, quoted_cols, .y, arg = "..."))

  joined <- purrr::reduce(
    list,
    dplyr::left_join,
    by = quoted_cols
    )

  return(joined)
}


#' @rdname left_joints
#'
#' @inheritParams left_join_dup
#'
#' @export
left_joints_dup <- function(list, ..., duplicated_columns = 3){

  # Check if ... columns are in datasets
  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  purrr::imap(list, ~ if_not_in_stop(.x, quoted_cols, .y, arg = "..."))

  joined <- purrr::reduce(
    list,
    \(x, y) left_join_dup(x, y, quoted_cols, duplicated_columns = duplicated_columns)
  )

  return(joined)
}
