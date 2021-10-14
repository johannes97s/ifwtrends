test_that("function returns a list", {

  dat <- pca(keywords = c("Pluto", "Saturn"),
             categories = 0,
             geo = "DE",
             time = paste("2020-01-01", "2020-06-01"))

  roller <- roll(keyword = c("ikea", "saturn"), start_period = "2020-01-01", end = "2020-06-01")

  expect_s3_class(forecast_q(roller, dat), "list")
})
