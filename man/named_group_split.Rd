% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/named_group_split.R
\name{named_group_split}
\alias{named_group_split}
\title{Reads function given output directory}
\usage{
named_group_split(.tbl, ...)
}
\arguments{
\item{.tbl}{Input tibble}
}
\value{
A list of tibbles
}
\description{
A wrapper for dplyr group_split which adds names to the resultant list.
}
