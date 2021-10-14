test_that("function returns a list", {

  dat <- pca(keywords = c("ikea", "saturn"),
             categories = 0,
             geo = "DE",
             time = paste("2020-01-01", "2020-06-01"))

  # dat braucht eine spalte "time" statt "date"
  roller <- roll(keyword = c("ikea", "saturn"), start_series = "2019-01-01", start_period = "2020-01-01", end = "2020-06-01")

  expect_s3_class(forecast_m(roller, dat), "list")
})
