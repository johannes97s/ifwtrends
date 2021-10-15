est_trend <- function() {
  end <- Sys.Date()
  dates <- seq.Date(
    from = as.Date("2006-01-01"),
    to = end,
    by = "month"
  )

  result <- vector("list", length = 2)

  series <- tibble(date = dates)
  missing <- NULL

  # Creates a sample of 250 Google Trends categories and
  # a fixed category (67 is arbitrary chosen).

  cat_samp <- unique(c(
    sample(gtrendsR::categories$id, 250, replace = FALSE),
    "67"
  ))

  k <- 0

  for (i in cat_samp) {
    Sys.sleep(0.1)

    g <- gtrends(
      geo = "DE",
      time = str_c("2006-01-01 ", end),
      category = i
    )$interest_over_time

    if (is.null(g)) {
      missing <- c(missing, i)
    } else {
      series <- bind_cols(series, !!as.character(eval(i)) := g$hits)
    }

    k <- k + 1

  }

  series <- series %>%
    pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%
    mutate(value = log(value)) %>%
    arrange(id)

  result[[1]] <- series

  fit <- unname(
    lm(
      value ~ id - 1 + poly(as.numeric(date), 5, raw = T),
      data = series
    )$fitted.values
  )

  comtrend <- series %>%
    mutate(trend = fit) %>%
    filter(id == 67) %>%
    select(date, trend)

  result[[2]] <- comtrend

  return(result)
}


## code to prepare `cat_trend_job` dataset goes here
category_trends <- est_trend()

# Unpacking the list into two dataframes because
# use_data() won't take a list as argument
catsample <- category_trends[[1]]
comtrend <- category_trends[[2]]

usethis::use_data(catsample, comtrend,
                  overwrite = TRUE, internal = TRUE)
