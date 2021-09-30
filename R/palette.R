#' plot_palette
#'
#' `plot_palette` function generates a barplot to visualize the palette colors
#'
#' @param palette A vector of colors
#' @return A default R barplot
#'
#' @export
#' 
plot_palette <- function(palette) {
  barplot(1:length(palette), col=palette)
}

#' make_coolors_url
#'
#' `make_coolors_url` function generates string
#' to use with the website coolors.co, which generates
#' aesthetically pleasing color palettes
#'
#' @param palette A vector of colors
#' @return A string that is a valid URL to paste into a browser
#'
#' @export
#' 
make_coolors_url <- function(palette) {
  palette_clean <- map_chr(palette, str_sub, start = 2)
  paste0("https://coolors.co/", paste0(palette_clean, collapse = "-"))
}