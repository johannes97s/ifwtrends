test_that("function returns a tibble with the correct dimensions", {

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
