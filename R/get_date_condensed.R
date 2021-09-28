#' get_date_condensed
#' 
#' Get the date as a codensed string in the format yyyymmdd, 
#' without any white space. 
#' 
#' @return A string of today's date in the format yyyymmdd
#' @export

get_date_condensed <- function() {
  format(Sys.Date(), "%Y%m%d")
}