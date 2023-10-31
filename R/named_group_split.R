#' Named group split
#'
#' @param df A data frame.
#' @param group Quoted column name to group by.
#'
#' @return  A split and named list of data frames.
#'
#' @export
named_group_split <- function(df, group) {

  if_not_in_stop(df, group, "df", "group")
  if (length(group) > 1) rlang::abort("Please provide on one grouping column.")

  names <- dplyr::group_keys(dplyr::group_by(df, !!rlang::sym(group)))

  names <- dplyr::pull(names, !!rlang::sym(group))

  l <- dplyr::group_split(df, !!rlang::sym(group))

  l <- purrr::set_names(l, names)

  return(l)
}
