% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_umap}
\alias{plot_umap}
\title{plot_umap}
\usage{
plot_umap(
  .tbl,
  x = UMAP_1,
  y = UMAP_2,
  color_str = "value",
  pixels = c(512, 512),
  pointsize = 0,
  fix_coords = TRUE,
  label = FALSE,
  textsize = 3,
  label_groups = NULL,
  repel_min_segment_length = 0.1,
  arrange_points = TRUE
)
}
\arguments{
\item{.tbl}{a data table with umap cols}

\item{x}{UMAP 1}

\item{y}{UMAP 2}

\item{pixels}{the pixels input to \code{geom_scattermore}}

\item{pointsize}{the pointsize input to \code{geom_scattermore}}

\item{fix_coords}{whether or not to use \code{coord_fixed}, TRUE by default}

\item{label}{bool specifying whether to label the color category}

\item{textsize}{int specifying textsize; only applies if label=TRUE}

\item{label_groups}{character vector specifying additional groups to label}

\item{repel_min_segment_length}{min segment length to pass to \code{geom_text_repel}}

\item{arrange_points}{whether or not to arrange the points so the highest value ones are on top}

\item{color}{the color to show}
}
\value{
ggplot2 object
}
\description{
Plots a 2D UMAP with the aesthetics that I like
}
