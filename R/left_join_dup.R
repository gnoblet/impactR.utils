#' Left join a list of dataframes
#'
#' @param df_a A dataframe.
#' @param df_b Another dataframe.
#' @param ... Columns to join by.
#' @param duplicated_columns Strategy between 1 and 4. Default to 3. See section "Treating duplicated columns".
#'
#' @section Treating duplicated columns:
#'
#' There are 4 ways implemented on treating columns with the same names in both datasets:
#'
#' * 1: an usual left-join with suffix being empty for df_a and ".dup_col" for df_b;
#' * 2: removing all duplicated columns from df_b, keeping only those of df_a;
#' * 3: remove the column from df_b if all values from df_b that are not NAs equal the values from df_a;
#' * 4: remove the column from df_b if all values from df_b are identical to values from df_a.
#'
#'
#' @return A left-joined data frame.
#'
#' @export
left_join_dup <- function(df_a, df_b, ..., duplicated_columns = 3) {
  # add checks on the list
  # - check if empty list
  # - check if less than two elements
  # - check if elements are not data frames

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  if_not_in_stop(df_a, quoted_cols, "df_a", arg = "...")
  if_not_in_stop(df_b, quoted_cols, "df_b", arg = "...")

  # Do not remove duplicates
  if (duplicated_columns == 1) {

    joined <- dplyr::left_join(
    df_a,
    df_b,
    by = quoted_cols
  )
  } else if (duplicated_columns == 2) {

    df_b <- df_diff(df_b, df_a, ...)

    joined <- dplyr::left_join(
      df_a,
      df_b,
      by = quoted_cols
    )

  } else if (duplicated_columns %in% c(3,4)) {

    joined <- dplyr::left_join(
      df_a,
      df_b,
      by = quoted_cols,
      suffix = c("", ".dup_col")
    )

    nn <- intersect(names(df_a), names(df_b))
    nn <- nn[!(nn %in% quoted_cols)]
    nn.dup_col <- paste0(nn, ".dup_col")

    if (duplicated_columns == 3){

      # check if all values are identical or if the dup column is NA
      nn_id_all <- purrr::map2_lgl(nn, nn.dup_col, \(x,y) all(joined[[x]] == joined[[y]] | is.na(joined[y])))

    } else if (duplicated_columns == 4) {

      # check if it's an exact match including NAs
      nn_id_all <- purrr::map2_lgl(nn, nn.dup_col, \(x,y) identical(joined[[x]], joined[[y]]))

    }

    # Get names of TRUE values
    nn_id_all <- names(purrr::set_names(nn_id_all, nn)[nn_id_all])
    nn_id_all.dup_col <-  paste0(nn_id_all, ".dup_col")

    joined <- dplyr::select(joined, -dplyr::all_of(nn_id_all.dup_col))
  }

  return(joined)
}
