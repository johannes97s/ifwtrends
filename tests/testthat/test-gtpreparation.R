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


test_that("function returns a tibble", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  expect_s3_class(gtpreparation(keyword = "ikea", time = "2014-01-01 2021-06-01"), "tbl")
})

# -------------------------------------------------------------------------


test_that("time frame < 5 years returns an error", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  expect_error(gtpreparation(keyword = "ikea", time = "2017-01-01 2021-06-01"))
})

# -------------------------------------------------------------------------


test_that("Returned tibble has the right number of columns", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  # check all possible lag arguments
  for (lag in 0:4) {
    returnvalue <- gtpreparation(keyword = "ikea", time = "2014-01-01 2021-06-01", lags = lag)

    expect_equal(dim(returnvalue)[2], (lag + 3))
  }

})

# -------------------------------------------------------------------------

test_that("Returned tibble starts with the correct month", {

  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  start <- "2014-01-01"
  end <- "2021-06-01"
  correctmonth <- month(start)

  for (lag in 0:4) {
    returnvalue <- gtpreparation(keyword = "ikea", time = paste(start, end), lags = lag)
    firstdate <- as.Date(returnvalue[1, 2]$time)

    expect_equal( (month(firstdate) - lag) , correctmonth)
  }
})

# -------------------------------------------------------------------------


