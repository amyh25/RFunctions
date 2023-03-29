#' @name named_group_split
#' credit goes to romainfrancois on https://github.com/tidyverse/dplyr/issues/4223
#' @param .tbl a tibble
#' @return a tibble

named_group_split <- function(.tbl, sep_str = " / ", ...) {
    grouped <- group_by(.tbl, ...)
    names <- rlang::inject(paste(!!!group_keys(grouped), sep = sep_str))

    grouped %>%
        group_split() %>%
        rlang::set_names(names)
}