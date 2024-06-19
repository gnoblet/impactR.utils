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
      dplyr::select(r, -starts_with("missing_")),
      cols = dplyr::starts_with("prop_"),
      names_to = "var",
      values_to = "prop_na_count_tot"
      )

    r_prop <- dplyr::mutate(r_prop, var = stringr::str_remove(var, "prop_missing_"))

    r_n <- tidyr::pivot_longer(
      dplyr::select(r, -c(dplyr::starts_with("prop"), dplyr::all_of(c("n_tot", "n_group_tot")))),
      cols = dplyr::starts_with("missing_"),
      names_to = "var",
      values_to = "na_count_tot"
      )

    r_n <- dplyr::mutate(r_n, var = stringr::str_remove(var, "missing_"))

    r <- dplyr::left_join(r_prop, r_n, by = c("var", group))
    r <- dplyr::mutate(r, var = stringr::str_remove(var, "prop_missing_"))
    r <- dplyr::relocate(r, var, prop_na_count_tot, na_count_tot, .before = dplyr::everything())
    r <- dplyr::relocate(r, n_tot, .after = dplyr::everything())
  }

  return(r)
}


# # Example usage
df <- tibble::tibble(
  group1 = sample(rep(c("A", "A", "B", "B", "C", "C"), 100)),
  group2 = sample(rep(c("idp", "displaced", "idp", "displaced", "idp", "displaced"), 100)),
  col1 = sample(rep(c(1, 2, 10, 4, 5, NA), 100)),
  col2 = rep(c(NA, 2, 3, 4, 8, 6), 100),
  col3 = rep(c(1, NA, 3, NA, NA, 6), 100)
)



count_missing_values(df, vars = c("col1", "col2", "group1"), group = c("group1"))
t <- count_missing_values(df, vars = c("col1", "col2", "col3"), group = NULL, pivot = T)
library(impactR.viz)
t <- t |> dplyr::mutate(prop_na_count_tot = round(prop_na_count_tot * 100, 2))
bar(
  t,
  x = "var",
  y = "prop_na_count_tot"
)
library(gt)
t |>
  gt() |>
  tab_header(
    title = "Number and proportion of missing values",
  ) |>
  fmt_number(
    columns = c("prop_na_count_tot", "na_count_tot")
  )
