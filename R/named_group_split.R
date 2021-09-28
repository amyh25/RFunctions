#' Reads function given output directory
#'
#' A wrapper for dplyr group_split which adds names to the resultant list.
#'
#' @param .tbl Input tibble
#' @return A list of tibbles
#' @export
#' 
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = "_")))
  
  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}
