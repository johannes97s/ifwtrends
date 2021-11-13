test_that("returns a tbl in due length", {
  keywords <- c("jupiter", "mars")
  time_frame <- "2020-01-01 2020-06-01"

  expectation <- gtsearch(
    keywords = keywords,
    time_frame = timeframe
  )
  expect_s3_class(
    expectation,
    "tbl"
  )

  # tbl has the right dimensions
  expect_equal(dim(expectation), c(153, 3))

  # tibble has the correct colnames
  expect_equal(colnames(expectation)[2:3], keywords)

  # tibble has the correct first date
  expect_equal(
    as.character(expectation$date[1]),
    substring(time_frame, 1, 10)
  )

  # tibble has the correct last date
  expect_equal(
    as.character(expectation$date[153]),
    substring(time_frame, 12, 21)
  )
})
