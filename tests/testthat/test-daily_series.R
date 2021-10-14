test_that("function returns a tibble", {
  expect_s3_class(daily_series(keyword = "Ikea", geo = "NL", from = "2021-09-01"), "tbl")
})
