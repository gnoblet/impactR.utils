#' Count missing values for vars (by group)
#'
#' @param df A data frame.
#' @param vars Variable names.
#' @param group Grouping column names. Default to NULL.
#' @param pivot Logical. If TRUE, return a pivoted data frame.
count_missing_values <- function(df, vars, group = NULL, pivot = TRUE) {

  #------ Checks

  # Check that df is a data.frame, if not stop
  if (!is.data.frame(df)) {
    rlang::abort("df must be a 'data.frame'.")
  }

  # Check that vars are in df, if not stop
  if_not_in_stop(df, vars, "df")

  # Check if group exists and if not in stop
  if (!is.null(group)) {
    if_not_in_stop(df, group, "df", "group")
  }

  # Check if grouping columns are in vars, if yes, warn which ones and remove them
  # Paste collapse with glue '\n'
  if (!is.null(group)) {
    if (any(group %in% vars)) {
      rlang::warn(
        glue::glue(
          "The following grouping columns are in vars and will be removed:\n",
          glue::glue_collapse(group[group %in% vars], sep = ", ", last = ", and ")
        )
      )
      vars <- vars[!(vars %in% group)]
    }
  }

  #------ Count missing values

  # Get total number of observations
  n_tot <- nrow(df)

  if (!is.null(group)) {

    # Group
    r <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group)))

    # Summarize
    r <- dplyr::summarize(
      r,
      n_tot = n_tot,
      n_group_tot = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vars),
        \(x) sum(is.na(x)),
        .names = "missing_{.col}"),
      .groups = "keep"
    )

    # Mutate
    r <- dplyr::mutate(
      r,
      dplyr::across(
        dplyr::starts_with("missing_"),
        \(x) x / n_tot,
        .names = "prop_{.col}")
      )

    # Ungroup
    r <- dplyr::ungroup(r)

  } else {

    # Summarize
    r <- dplyr::summarize(
      df,
      n_tot = n_tot,
      n_group_tot = dplyr::n(),
      dplyr::across(
        dplyr::all_of(vars),
        \(x) sum(is.na(x)),
        .names = "missing_{.col}")
      )

    # Mutate
    r <- dplyr::mutate(
      r,
      dplyr::across(
        dplyr::starts_with("missing_"),
        \(x) x / n_tot,
        .names = "prop_{.col}")
    )

  }

  if (pivot) {

    r_prop <- tidyr::pivot_longer(
      dplyr::select(r, -dplyr::starts_with("missing_")),
      cols = dplyr::starts_with("prop_"),
      names_to = "var",
      values_to = "prop_na_count_tot"
      )

    r_prop <- dplyr::mutate(r_prop, "var" := stringr::str_remove(!!rlang::sym("var"), "prop_missing_"))

    r_n <- tidyr::pivot_longer(
      dplyr::select(r, -c(dplyr::starts_with("prop"), dplyr::all_of(c("n_tot", "n_group_tot")))),
      cols = dplyr::starts_with("missing_"),
      names_to = "var",
      values_to = "na_count_tot"
      )

    r_n <- dplyr::mutate(r_n, "var" := stringr::str_remove(!!rlang::sym("var"), "missing_"))

    r <- dplyr::left_join(r_prop, r_n, by = c("var", group))
    r <- dplyr::mutate(r, "var" := stringr::str_remove(!!rlang::sym("var"), "prop_missing_"))
    r <- dplyr::relocate(r, !!rlang::sym("var"), !!rlang::sym("prop_na_count_tot"), !!rlang::sym("na_count_tot"), .before = dplyr::everything())
    r <- dplyr::relocate(r, n_tot, .after = dplyr::everything())
  }

  return(r)
}
