% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gttrend_adj.R
\name{gttrend_adj}
\alias{gttrend_adj}
\title{Trend adjustment of a Google Trends time series}
\usage{
gttrend_adj(
  timeseries = NULL,
  keyword = NA,
  category = NA,
  geo = "DE",
  timeframe = paste("2006-01-01", Sys.Date()),
  method = "moving_avg",
  log.trafo = FALSE
)
}
\arguments{
\item{timeseries}{A already created time
series to trend adjust. Preferably already as tsibble or tibble.
If another data format is given, it will be coerced to a tsibble.}

\item{keyword}{A vector (chr) of keywords to search for.}

\item{category}{A vector (numeric) of category numbers to search for.}

\item{geo}{The region to search in.}

\item{timeframe}{A time frame to search the queries in consisting of
a start date and an end date in YYYY-MM-DD form.}

\item{method}{As trend adjustment method, one can choose
between "moving_avg", "firstdiff", and "comtrend." See Details for
more information.}

\item{log.trafo}{Logical,
indicates if value should be transformed to log(value).}
}
\value{
Returns a tibble with trend adjusted values and a
date column.

For a trend method, there can be choosen between
\code{"firstdiff"}, \code{"moving_avg"} and \code{"comtrend"}.
If you choose \code{"firstdiff"},
first differences with \code{lag = 1} are computed.
If you choose the moving average,
the time series will be decomposed into its components
and the trend will be subtracted
from the whole time series (using loess).
With \code{"comtrend"},
there is a polynom of degree 5
with id-fixed effects estimated,
which captures the common trend of a sample of categories
in Google Trends.
The residuals are then used as the adjusted series.
For further detail, see Woloszko et al. (2020) and
the function \code{\link[=est_trend]{est_trend()}}.
}
\description{
This function can either trend adjust a given time series
or create a new time series based on a Google Trends search query
and then directly trend adjust it.
}
\examples{
# Trend adjusting a already established series.
series <- trendecon::ts_gtrends("ikea", time = "all")
gttrend_adj(series, log.trafo = TRUE, method = "moving_avg")

# Search for a new series and trend adjust it
gttrend_adj(
  category = 179, timeframe = "2015-01-01 2021-01-01",
  method = "moving_avg", log.trafo = FALSE
)
}
