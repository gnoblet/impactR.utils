# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.r-project.org"))

# Install necessary packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tibble", quietly = TRUE)) {
  install.packages("tibble")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("glue", quietly = TRUE)) {
  install.packages("glue")
}
if (!requireNamespace("rlang", quietly = TRUE)) {
  install.packages("rlang")
}

# Install the package from GitHub
devtools::install_github("gnoblet/impactR.utils@dev")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(glue)
library(testthat)
library(impactR.utils)

# Ensure the function is loaded
if (!exists("count_missing_values", where = "package:impactR.utils")) {
  stop("The function 'count_missing_values' is not found in the 'impactR.utils' package.")
}

# Define helper function to check if columns exist
check_columns <- function(df, cols) {
  all(cols %in% names(df))
}

# Unit tests for count_missing_values function
test_that("count_missing_values handles different scenarios", {
  
  # Sample data
  df <- tibble::tibble(
    group1 = sample(rep(c("A", "A", "B", "B", "C", "C"), 100)),
    group2 = sample(rep(c("idp", "displaced", "idp", "displaced", "idp", "displaced"), 100)),
    col1 = sample(rep(c(1, 2, 10, 4, 5, NA), 100)),
    col2 = rep(c(NA, 2, 3, 4, 8, 6), 100),
    col3 = rep(c(1, NA, 3, NA, NA, 6), 100)
  )
  
  # Test case 1: No grouping, pivot = TRUE
  result <- count_missing_values(df, vars = c("col1", "col2", "col3"), group = NULL)
  expect_true(check_columns(result, c("var", "prop_na_count_tot", "na_count_tot", "n_tot")))
  expect_equal(nrow(result), 3)  # Should have 3 rows for col1, col2, col3
  
  # Test case 2: Grouping by one column, pivot = TRUE
  result <- count_missing_values(df, vars = c("col1", "col2"), group = c("group1"))
  expect_true(check_columns(result, c("group1", "var", "prop_na_count_tot", "na_count_tot", "n_tot", "n_group_tot")))
  expect_equal(nrow(result), 6)  # Should have 6 rows for col1 and col2, grouped by group1
  
  # Test case 3: Grouping by multiple columns, pivot = TRUE
  result <- count_missing_values(df, vars = c("col1", "col2"), group = c("group1", "group2"))
  expect_true(check_columns(result, c("group1", "group2", "var", "prop_na_count_tot", "na_count_tot", "n_tot", "n_group_tot")))
  expect_equal(nrow(result), 12)  # Should have 12 rows for col1 and col2, grouped by group1 and group2
  
  # Test case 4: No grouping, pivot = FALSE
  result <- count_missing_values(df, vars = c("col1", "col2", "col3"), group = NULL, pivot = FALSE)
  expect_true(check_columns(result, c("missing_col1", "prop_missing_col1", "missing_col2", "prop_missing_col2", "missing_col3", "prop_missing_col3", "n_tot", "n_group_tot")))
  expect_equal(nrow(result), 1)  # Should have 1 row for no grouping
  
  # Test case 5: Grouping by one column, pivot = FALSE
  result <- count_missing_values(df, vars = c("col1", "col2"), group = c("group1"), pivot = FALSE)
  expect_true(check_columns(result, c("group1", "missing_col1", "prop_missing_col1", "missing_col2", "prop_missing_col2", "n_tot", "n_group_tot")))
  expect_equal(nrow(result), 3)  # Should have 3 rows for col1 and col2, grouped by group1
  
  # Test case 6: Grouping by multiple columns, pivot = FALSE
  result <- count_missing_values(df, vars = c("col1", "col2"), group = c("group1", "group2"), pivot = FALSE)
  expect_true(check_columns(result, c("group1", "group2", "missing_col1", "prop_missing_col1", "missing_col2", "prop_missing_col2", "n_tot", "n_group_tot")))
  expect_equal(nrow(result), 6)  # Should have 6 rows for col1 and col2, grouped by group1 and group2
  
  # Additional checks for warnings
  expect_warning(count_missing_values(df, vars = c("col1", "col2", "group1"), group = c("group1")), 
                 regexp = "The following grouping columns are in vars and will be removed")
})

# Save the test results to a text file
#test_file <- "test_results.txt"
#sink(test_file)
#testthat::test_dir(".", reporter = "summary")
#sink()

#cat("Test results saved to", test_file)
