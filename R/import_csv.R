# Hello import!

# All functions to import csv and xlsx
# To-do:
# - add other imports maybe, .rds, .shp, etc.


#' Initial import with clean names (not an option!)
#'
#' @param path The path to the file.
#' @param delim Field separator, default is ",".
#' @param clean_names Boolean. If TRUE, uses [janitor::clean_names()] to column clean names.
#' @param guess_max Maximum guess of types. Default to 21474836.
#'
#' @return A dataframe.
#'
#' @description Import a dataset from a CSV or a XLSX file. It ensures that the right type of column is guessed using the max value of guess_max by default.
#'
#' @describeIn import_csv Import a .csv file.
#'
#' @export
import_csv <- function(path, delim = ",", clean_names = TRUE,  guess_max = 21474836){

  if (rlang::is_missing(path)) { rlang::abort("Please provide the path to the file.") }
  if (!file.exists(path)) { rlang::abort("This file does not exist.") }

  df <- readr::read_delim(path, delim = delim, guess_max = 21474836)

  df <- readr::type_convert(df)

  if (clean_names) df <- janitor::clean_names(df)

  return(df)
}



#' XLSX sheet initial import with clean names (not an option!)
#'
#' @param sheet A character string of the sheet name or the sheet position
#'
#' @describeIn import_csv Import one sheet from a .xlsx.
#'
#' @export
import_xlsx <- function(path, sheet = 1, clean_names = TRUE, guess_max = 21474836){

  df <- rio::import(path, which = sheet, guess_max = 21474836)
  df <- tibble::as_tibble(df)
  df <- readr::type_convert(df)

  if (clean_names) df <- janitor::clean_names(df)

  return(df)
}



#' Entire XLSX initial import with clean names (not an option!)
#'
#' @describeIn import_csv Import all sheets from a .xlsx.
#'
#' @export
import_full_xlsx <- function(path = NULL, clean_names = TRUE,  guess_max = 21474836){

  dfs <- rio::import_list(path, guess_max = 21474836)
  dfs <- purrr::map(
    dfs,
    \(x) {
      df <- tibble::as_tibble(x)
      df <- readr::type_convert(df)

      if (clean_names) df <- janitor::clean_names(df)

      return(df)
    }
  )

  return(dfs)
}
