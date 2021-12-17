#' ifwtrends: A package for evaluating data from Google Trends
#'
#' The package ifwtrends is based on trendecon and gtrendsR and takes
#' data from Google Trends to support economic forecasting.
#'
#' @section Functions:
#'
#' * [daily_series()]: Creates a daily series for time frames where Google
#' usually doesn't provide daily data. Uses the Chow-Lin-method.
#'
#' * [est_trend()]: Estimates a common trend between all Google Trends categories
#' and uses that to trend adjust in some other functions. This is updated on
#' a monthly basis in the package. However, the user may use this function
#' for himself on his own.
#'
#' * [factorR2()]: Estimates the R squared between keywords.
#'
#' * [forecast_m()]: Makes a monthly forecast on a time series based on the data
#' of some Google Trends data. **Does currently not work correctly.**
#'
#' * [forecast_q()]: Makes a quarterly orecast on a time series based on the data
#' of some Google Trends data. **Does currently not work correctly.**
#'
#' * [gtpreparation()]: Makes a search query and applies a
#' trend adjustment (with the common
#' trend), a seasonal adjustment and, if wished, adds lag variables
#' to the resulting data frame.
#'
#' * [gtsearch()]: Makes a simple Google search for either
#' a keyword or a category.
#'
#' * [gtseas_adj()]: Seasonal adjusts a given time series or uses
#' [gtsearch()] internally to seasonal
#' adjust a search query. The seasonal adjustment
#' is done via X-13-ARIMA-SEATS from [seasonal::seas()].
#'
#' * [gttrend_adj()]: Trend adjusts a given time series or uses
#' [gtsearch()] internally to trend
#' adjust a search query. It either can use
#' first differences, a moving average or the common trend computed
#' by [est_trend()] to adjust the time series.
#'
#' * [pca()]: Does a Principal Component Analysis on a search query.
#'
#' * [roll()]: Creates some rolling Google
#' search queries for different time frames.
#'
#' * [simple_daily_series()]: A more simplistic alternativ for
#' [daily_series()] that doesn't rely on inner functions from other packages.
#'
#' For in-depth information and showcases, see the German vignette
#' at \code{vignette(topic = "ifwtrends-demo", package = "ifwtrends")}
#'
#' @docType package
#' @name ifwtrends
NULL
