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

test_that("function returns a tibble", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  expect_s3_class(gtpreparation(keyword = "ikea", time = "2014-01-01 2021-06-01"), "tbl")
})

test_that("time frame < 5 years returns an error", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  expect_error(gtpreparation(keyword = "ikea", time = "2017-01-01 2021-06-01"))
})
