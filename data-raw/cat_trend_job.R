## code to prepare `cat_trend_job` dataset goes here
category_trends <- est_trend()

# Unpacking the list into two dataframes because
# use_data() won't take a list as argument
catsample <- category_trends[[1]]
comtrend <- category_trends[[2]]

usethis::use_data(catsample, comtrend,
                  overwrite = TRUE, internal = TRUE)
