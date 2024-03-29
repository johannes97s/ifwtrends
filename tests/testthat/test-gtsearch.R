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


test_that("returns a tibble in due length", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  keywords <- c("jupiter", "mars")
  time_frame <- "2020-01-01 2020-06-01"

  expectation <- gtsearch(
    keyword = keywords,
    timeframe = time_frame
  )
  expect_s3_class(
    expectation,
    "tbl"
  )

  # tbl has the right dimensions
  expect_equal(dim(expectation), c(306, 3))

  # tibble has the correct colnames
  expect_equal(colnames(expectation)[2:3], c("hits", "keyword"))

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

# -------------------------------------------------------------------------

test_that("Function returns a tbl_ts if wished", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  keywords <- c("jupiter", "mars")
  time_frame <- "2020-01-01 2020-06-01"

  expectation1 <- gtsearch(
    keyword = keywords,
    timeframe = time_frame,
    as_tbl_ts = TRUE
  )

  expect_s3_class(
    expectation1,
    "tbl_ts"
  )

  expectation2 <- gtsearch(
    keyword = keywords,
    timeframe = time_frame,
    as_tbl_ts = FALSE
  )

  expect(!("tbl_ts" %in% class(expectation2)),
         failure_message = "Tibble is a tsibble when it should not be!")

})

# -------------------------------------------------------------------------


