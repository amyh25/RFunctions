#' @name my_theme
#' @return my_theme with modifications of my preferences
#' @export

my_theme <- function(...) {
  theme_classic(
    strip.background = element_blank(), 
    panel.background = element_blank(), 
    plot.background = element_blank(), 
    ...
  )
}

#' @name theme_presentation
#' 
#' @return my_theme with size modifications for presentations
#' 
#' @export

theme_presentation <- function() {
  my_theme(
    
  )
}

#' @name theme_poster
#' 
#' @return my_theme with size modifications for poster
#' 
#' @export

theme_poster <- function() {
  my_theme(
    
  )
}

#' @name theme_figure
#' 
#' @return my_theme with size modifications for figures
#' 
#' @export

theme_figure <- function() {
  my_theme(
    
  )
}