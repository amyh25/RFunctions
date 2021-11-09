#' make_dummy_cols
#' 
#' Makes dummy columns in a data frame, given an ID variable
#' and a dummy variable
#' 
#' @param data_df A data frame
#' @param id_var An identity variable to pivot against
#' @param dummy_var A dummy variable 
#' @return A data frame
#'
#' @export

make_dummy_cols <- function(data_df, id_var, dummy_var) {
  id_var <- enquo(id_var)
  dummy_var <- enquo(dummy_var)
  dummy_df <- data_df %>% 
    select(!!id_var, !!dummy_var) %>% 
    mutate(var = 1) %>% 
    pivot_wider(id_cols = !!id_var, 
                names_from = !!dummy_var, 
                values_from = var, 
                values_fill = 0) %>% 
    mutate_if(is.numeric, as.factor)
  left_join(data_df, dummy_df, by = rlang::as_name(id_var))
}