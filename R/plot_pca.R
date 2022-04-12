#' Time function
#'
#' plot_pca Plots a pca with a specific coloring scheme. 
#' Also specifies the variance of each principal component. 
#'
#' @param pca_out output of prcomp
#' @param comp1 integer value for PC on x-axis
#' @param comp2 integer value for PC on y-axis
#' @param sep A string used for separating the sample name. Used with col_index
#' @param col_index An index specifying a particular piece of the separated sample name string. Used with sep
#' @param str_extract_regex A string specifying the regular expression to be extracted from the sample name for the color variable
#' @param col_label The label of the color variable
#' @return A ggplot of a pca
#'
#' @export

plot_pca <- function(pca_out, comp1, comp2, 
                     sep = NULL, col_index = NULL, 
                     str_extract_regex = NULL, col_label = "") {
  
  if (!is.numeric(comp1))
    stop("comp1 must be numeric")
  else if(!is.integer(comp1)) {
    warning("forcing comp1 to be an int")
    comp1 <- as.integer(comp1)
  }
  
  if (!is.numeric(comp2)) {
    stop("comp2 must be numeric")
  } else if (!is.integer(comp2)) {
    warning("forcing comp2 to be an int")
    comp2 <- as.integer(comp2)
  }
  
  pca_df <- pca_out$x %>% 
    as_tibble(rownames = "sample")
  importance_df <- summary(pca_out)$importance %>% 
    t() %>% as_tibble(rownames = "PC") 
  prop_var_vec <- importance_df %>% 
    named_group_split(PC) %>% 
    map(~.$`Proportion of Variance`)
  
  pc_x <- paste0("PC", comp1)
  pc_y <- paste0("PC", comp2)
  p <- pca_df %>% 
    ggplot() + 
    aes_string(pc_x, pc_y) +
    geom_point(size = 3, alpha = 0.8) + 
    xlab(paste0(pc_x, " (", format(prop_var_vec[[pc_x]]*100, digits = 4), "%)")) + 
    ylab(paste0(pc_y, " (", format(prop_var_vec[[pc_y]]*100, digits = 4), "%)")) + 
    coord_fixed()
  
  if (!is.null(sep)) {
    if (is.numeric(col_index) && !is.integer(col_index)){
      warning(paste0("col_index is not an integer, forcing to int"))
      col_index <- as.integer(col_index)
    }
    if (!is.numeric(col_index))
      stop("col_index must be a number if sep is set")
    col_vec <- pca_df$sample %>% str_split(sep) %>% map_chr(~.[col_index]) %>% factor()
    p <- p + aes(color = col_vec) + 
      labs(color = col_label)
  }
  
  if (!is.null(str_extract_regex)) {
    col_vec <- pca_df$sample %>% str_extract(str_extract_regex) 
    p <- p + aes(color = col_vec) + 
      labs(color = col_label)
  }
  
  return(p)
}