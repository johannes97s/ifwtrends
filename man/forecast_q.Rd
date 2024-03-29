% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_q.R
\name{forecast_q}
\alias{forecast_q}
\title{Quarterly forecast evaluation of vintages}
\usage{
forecast_q(r_list, data, fd = T)
}
\arguments{
\item{r_list}{A Google Trends time series in form of a tibble (or tsibble)
which is used to predict the next quarter in data.}

\item{data}{A tibble with some data you want to predict for the next quarter
that is somewhat correlated with the data in r_list.}

\item{fd}{A logical value to indicate if you want to use the first
differences in r_list (true) or normal values (false).}
}
\value{
Returns a list with the the predicted data and the used model
results.
}
\description{
\code{\link[=forecast_q]{forecast_q()}} uses quarterly vintages generated by \code{\link[=roll]{roll()}} to predict the
value of another time series for the next quarter.
}
\section{Methods}{

Google Data should be log-transformed currently it uses a
RIDGE-Regression but other models such as OLS,
LASSO or PCA are available.
}

\section{Usage example}{

Check the \code{service_imports.R} file under \code{/vignettes} for an
real example and to understand how to use this function and its
twin, \code{\link[=forecast_m]{forecast_m()}}.
}

