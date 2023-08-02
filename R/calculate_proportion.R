#' calculate_proportion
#' @param .tbl a tibble
#' @param var the variable to calculate propotion of
#' @param group_var a grouping variable to calculate proportion within
#' @return a tibble

calculate_proportion <- function(.tbl, var, group_var) {
  var <- enquo(var)
  group_var <- enquo(group_var)
  out <- .tbl %>%
    group_by(!!var,!!group_var) %>%
    summarise(count = n()) %>%
    group_by(!!group_var) %>%
    mutate(total = sum(count), prop = count / total)
  out <- out %>%
    pivot_wider(
      id_cols = !!group_var,
      names_from = !!var,
      values_from = prop,
      values_fill = 0
    ) %>%
    pivot_longer(
      cols = 2:ncol(.),
      names_to = rlang::as_name(var),
      values_to = "prop"
    ) %>%
    left_join(select(out,-prop),
              by = c(rlang::as_name(group_var),
                     rlang::as_name(var)))
  return(out)
}
