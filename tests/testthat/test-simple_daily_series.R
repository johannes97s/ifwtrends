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

test_that("returns a tbl in due length", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  keyword <-  "mars"
  start <- "2020-01-01"

  expectation <- simple_daily_series(
    keyword = keyword, geo = "DE",
    from = start, verbose = F
  )
  expect_s3_class(
    expectation,
    "tbl"
  )

  # tbl has the right dimension
  expect_equal(dim(expectation)[2], 2)

  # tibble has the correct colnames
  expect_equal(colnames(expectation)[2], keyword)

  # tibble has the correct first date
  expect_equal(
    as.character(expectation$date[1]),
    substring(start, 1, 10)
  )
})
