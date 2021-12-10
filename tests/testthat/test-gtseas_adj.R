check_connection <- function() {
  connection_available <- tryCatch(
    {
      temp <- gtrendsR::gtrends(keyword = "Ikea", geo = "NL", time = "2021-09-01 2021-10-01")
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# -------------------------------------------------------------------------

test_that("function returns a tsibble", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2015-01-01 2021-06-01")
  expectationfirstdiff <- gtseas_adj(series, freq = "month", log.traf = TRUE, method = "firstdiff")

  expect_s3_class(expectationfirstdiff, "tbl_ts")
  expect_s3_class(expectationfirstdiff, "tbl")

  series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2015-01-01 2020-12-01")
  expectationarima <- gtseas_adj(series, log.traf = TRUE, method = "arima")

  expect_s3_class(expectationarima, "tbl_ts")
  expect_s3_class(expectationarima, "tbl")
})

# -------------------------------------------------------------------------


test_that("ARIMA works in general", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  expectation <- gtseas_adj(AirPassengers, freq = "month", log.traf = TRUE, method = "arima")

  expect_equal(dim(expectation), c(144, 2))

})

# -------------------------------------------------------------------------


test_that("ARIMA won't work on weekly/daily data, but firstdiff does!", {

  series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2020-01-01 2021-06-01")
  expectationfirstdiff <- gtseas_adj(series, freq = "month", log.traf = TRUE, method = "firstdiff")
  expect_s3_class(expectationfirstdiff, "tbl_ts")

  series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2020-01-01 2020-12-01")
  expect_error(gtseas_adj(series, log.traf = TRUE, method = "arima"))
})
