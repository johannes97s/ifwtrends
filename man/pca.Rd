% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca}
\alias{pca}
\title{Principal component analysis on Google Trends time series}
\usage{
pca(
  keywords = NA,
  categories = 0,
  geo = "DE",
  time = str_c("2006-01-01 ", Sys.Date())
)
}
\arguments{
\item{keywords}{A vector (chr) with search queries (or a single search query).}

\item{categories}{A vector (num) with Google Trends category numbers.}

\item{geo}{A geographical region to restrict the search queries to.}

\item{time}{A string consisting of a start date
and an end date (separated with a space).
Check the example for an example.}
}
\value{
Tibble with monthly principal components
next to the actual time series.
}
\description{
\code{pca} computes for several search queries
or several categories the principal
components of the monthly time series.
}
\examples{
pca(keywords = c("ikea", "saturn"), time = "2018-01-01 2020-01-01")

}
