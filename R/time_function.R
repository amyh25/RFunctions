#' Time function
#'
#' `time_function` function uses `Sys.time()` to calculate how long a
#' function takes to run
#'
#' @param my_func A function
#' @return A time difference
#'
#' @export

time_function <- function(my_func, ...) {
  start_time <- Sys.time()
  my_func(...)
  end_time <- Sys.time()
  end_time - start_time
}
