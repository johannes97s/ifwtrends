test_that("roll returns a list of tibbles", {
  expectation <- roll(
    keyword = NA,
    category = c(67, 1003),
    start_series = "2006-01-01",
    start_period = "2018-01-01",
    end = Sys.Date(),
    fun = gtpreparation,
    lags = 4
  )

  expect_type(expectation, "list")
  expect_s3_class(expectation[[1]], "tbl")
  expect_equal(expectation[[1]]$time[1], as.Date("2006-05-01"))
})
