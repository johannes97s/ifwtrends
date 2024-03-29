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

test_that("function returns a tibble with the correct dimensions", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  keywords <- c("nolan", "tarantino")
  time <- "2018-01-01 2020-01-01"

  expectation <- pca(keywords = keywords, time = time)

  # correct dimensions
  expect_equal(dim(expectation), c(25, 5))

  # correct s3 class tibble
  expect_s3_class(expectation, "tbl")

  expect_equal(colnames(expectation)[4:5], keywords)

  expect_equal(as.character(expectation$date[1]), substring(time, 1, 10))

  expect_equal(as.character(expectation$date[25]), substring(time, 12, 21))

})
