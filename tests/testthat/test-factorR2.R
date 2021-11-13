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

test_that("function returns a tibble
          respective a list (when a plot is wished)", {
  skip_if_not(check_connection(), message = "No connection couldn't be established!")

  dat <- pca(
    keywords = c("Pluto", "Saturn"),
    categories = 0,
    geo = "DE",
    time = paste("2020-01-01", "2020-06-01")
  )


  keyword_length <- (dim(dat)[2] - 1) / 2
  series <- dat %>% select(date, (2 + keyword_length):(1 + keyword_length * 2))
  factors <- dat %>% select(date, 2:(1 + keyword_length))

  # Without / with plot
  wo_plot_result <- factorR2(series, factors, plot = F)
  w_plot_result <- factorR2(series, factors, plot = T)

  # Expect the correct class respective type of the result
  expect_s3_class(wo_plot_result, "tbl")
  expect_type(w_plot_result, "list")
})
