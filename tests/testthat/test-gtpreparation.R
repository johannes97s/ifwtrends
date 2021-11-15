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

  expect_s3_class(gtpreparation(keyword = "ikea", time = "2021-01-01 2021-06-01"), "tbl")
})
