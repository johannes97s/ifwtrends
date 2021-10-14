test_that("function returns a tibble
          respective a list (when a plot is wished)", {
  dat <- pca(keywords = c("Pluto", "Saturn"),
             categories = 0,
             geo = "DE",
             time = paste("2020-01-01", "2020-06-01"))

  series <- dat %>% select(date, 6:9)
  factors <- dat %>% select(date, 2:5)

  expect_s3_class(factorR2(series, factors, plot = T), "list")
  expect_s3_class(factorR2(series, factors, plot = F), "tbl")
})
