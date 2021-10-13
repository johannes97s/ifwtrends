## code to prepare `cat_trend_job` dataset goes here
category_trends <- est_trend()
usethis::use_data(category_trends[[1]], category_trends[[2]],
                  overwrite = TRUE, internal = TRUE)
