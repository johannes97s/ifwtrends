test_that("function returns a tibble", {

  expect_s3_class(gtpreparation(keyword = c("ikea", "saturn"), time = "2021-01-01 2021-06-01"), "tbl")
})
