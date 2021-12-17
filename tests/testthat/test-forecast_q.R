test_that("forecasting on a quarterly data returns a functioning tibble", {

  datastream <- readRDS(system.file("testdata","datastream_service_imports",package="ifwtrends"))
  r_list <- readRDS(system.file("testdata","googledata_service_imports",package="ifwtrends"))

  expectation <- forecast_q(r_list, datastream, fd = T)$forec
  expect_s3_class(expectation, "tbl")
})
