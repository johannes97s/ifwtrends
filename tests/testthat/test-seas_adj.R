test_that("function returns a tibble
          with the same length as the
          actual seas() function from functional", {
  expect_s3_class(seas_adj(AirPassengers), "tbl")

  expectation <- seasonal::final(seasonal::seas(AirPassengers))
  expect_length(seas_adj(AirPassengers), length(ts_tbl(expectation)))
})

test_that("function returns approximately the same values as the seas() function", {

  expectation <- seasonal::final(seasonal::seas(AirPassengers))
  expect_equal(seas_adj(AirPassengers), ts_tbl(expectation), tolerance = 5)
})
