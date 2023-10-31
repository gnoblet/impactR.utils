#' Date to day and month
#'
#' @param vec A vector of type POSIXct.
#'
#' @return The same vector with only the day and month in the env locale.
#'
#' @export
date_to_dm <- function(vec){
  format(vec,format = "%d %b")
}
