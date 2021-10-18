#' Generate a daily time series from a broader time frame.
#'
#' (experimental) This is a more simplistic approach to generate daily
#' data in a much broader time frame, where Google usually
#' doesn't provide daily data at all. See more in section
#' Method.
#'
#' @param keyword A string with a single search query. At the moment,
#' multiple search queries are not allowed.
#' @param geo A geographical region to restrict the search query to.
#' @param from Start date of the search query.
#' @param verbose Boolean value. True, if some status messages
#' should be displayed in the console during runtime.
#'
#' @return A tibble with a date column and a value column of a
#' daily time series.
#'
#' @section Method:
#' This function searches for multiple time periods
#' with a duration of 9 months with sometimes significantly
#' overlapping ranges of Google Trends data. The overlapping
#' period is then used to ensure that the time series has
#' a consistent scale. As of now, \code{simple_daily_series}
#' and \code{daily_series} have a somewhat comparable
#' trend to a certain degree. Important to note,
#' \code{simple_daily_series} won't have a value in the time
#' series that is above 100. But this function will also have outliers.
#' If you search for a smoothed (and adjusted) time series, try
#' the [tempdisagg::td()] function from the \link[tempdisagg] package.
#'
#' @examples
#' simple_daily_series <- function(keyword = "covid-19",
#' geo = "DE",
#' from = "2020-01-01",
#' verbose = TRUE)
#'
#' @import rlang tibble
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr last_col
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom lubridate as_date
#' @importFrom lubridate today
#' @importFrom lubridate days
#' @importFrom magrittr %>%
#' @export
simple_daily_series <- function(keyword = "arbeitslos",
                                geo = "DE",
                                from = "2006-01-01",
                                verbose = TRUE) {

  stopifnot("simple_daily_series(): At the moment, you can only use one keyword" = length(keyword) == 1)
  stopifnot("simple_daily_series(): Google Trends data only goes back to 2006-01-01. So start at least there." = as.Date(from) - as.Date("2006-01-01") >= 0)
  stopifnot("simple_daily_series(): The time frame must atleast include a week!" = as.Date(Sys.Date()) - as.Date(from) >= 7)

  # construct a time frame
  start <- as_date(from)
  end <- as_date(today())
  timeframe <- paste(start, end)
  check_length_timeframe <- seq.Date(start, end, by = "month")

  # For timeframes up to 9 months, Google provides daily data
  if (length(check_length_timeframe) < 9) {
    query <- gtsearch(keywords = keyword, geo = geo, time_frame = timeframe)

    return(query)

    # weekly data is provided for query between 9 month and 5 years,
    # and any query longer than 5 years will only return monthly data.
  } else {
    start_d <- from
    end_d <- end
    init_end_d <- end_d

    # The length(days) of each timeframe fragment for fetching google trends data,
    # need to be <269 in order to obtain daily data.
    delta <- days(269)
    # The length(days) of the overlap period used for scaling/normalization
    overlap <- days(100)

    itr_d <- end_d - delta
    overlap_start <- NA

    # create empty tibbles with correct column types
    df <- tibble(date = today(), value = numeric())
    ol <- tibble(date = today(), value = numeric())


    #--------------
    while (end_d > start_d) {
      # compute time frame
      tf <- paste(itr_d, end_d)
      if (verbose) {
        print(paste("Fetching \\", keyword, "\\ for period:", tf))
      }

      # search for the keyword in a given time frame
      temp <- gtsearch(keywords = keyword, geo = geo, time_frame = tf)

      # creates a copy of temp with empty data
      ol_temp <- tibble(temp)
      ol_temp[, 2:dim(ol_temp)[2]] <- NA


      if (!is.na(overlap_start)) {
        if (verbose) {
          print(paste("Normalize by overlapping period:", overlap_start, end_d))
        }

        # normalize using the maximum value of the overlapped period
        y1 <- max(temp[, 2], na.rm = TRUE)
          # temp %>%
          # filter(date >= overlap_start & date < end_d) %>%
          # select({{ keyword }}) %>%
          # slice_max(n = 1, order_by = {{ keyword }}) %>%
          # pull()

        y2 <- max(select(df, last_col()), na.rm = TRUE)
          # df %>%
          # filter(date >= overlap_start & date < end_d) %>%
          # select(last_col()) %>%
          # slice_max(n = 1, order_by = {{ keyword }}) %>%
          # pull()

        coef <- y2 / y1

        temp <- temp %>%
          mutate(across(where(is.numeric), ~ .x * coef))

        # replace ol_temp with ones
        ol_temp[
          ol_temp$date >= overlap_start & ol_temp$date < end_d,
          2:dim(ol_temp)[2]
        ] <- 1
      }

      # merge tibbles
      df <- full_join(df, temp, by = "date")
      ol <- full_join(ol, ol_temp, by = "date")

      # shift the timeframe for next iteration
      overlap_start <- itr_d
      end_d <- end_d - (delta - overlap)
      itr_d <- itr_d - (delta - overlap)

      # sort tibbles by date
      df <- df %>%
        arrange(date)
      temp <- temp %>%
        arrange(date)

      # if clause
      # ....
      #
    }

    # taking averages for overlapped period.
    # before: save a date column
    dates <- df$date
    df <- rowMeans(df[, -1], na.rm = TRUE)
    # ol <- apply(ol[, -1], 1, function(x) max(x, na.rm = TRUE))

    # combine adjusted values with a date column
    df <- tibble(dates, df)
    colnames(df) <- c("date", {{keyword}})

    # cut to only relevant time frame
    df <- df[df$date >= start_d & df$date < init_end_d, ]

    # get maximum
    maxi <- max(df[, 2], na.rm = TRUE)

    # get a vector with values only
    relevant_cols <- df %>%
      select({{ keyword }}) %>%
      pull()

    # re-normalized to the overall maximum value to have max = 100
    result <- df %>%
      mutate(
        {{keyword }} := round((100 * relevant_cols / maxi), 4)
      )

    # check if last value is zero
    if (result[dim(result)[1], 2] == 0) {
        result <- result[1:dim(result)[1] - 1, ]
    }

    return(result)

    # df <- full_join(df, ol, by = "date")
    #
    # # cut to only relevant time frame
    # df <- df[df$date >= start_d & df$date < init_end_d, ]
    #
    #
    # # combine all columns to one column with the relevant time series
    # df <- df %>%
    #   mutate(!!keyword := as.numeric(coalesce(!!!rlang::syms(names(.)[-1])))) %>%
    #   select(date, {{ keyword }})
    #
    # # get maximum
    # maxi <- max(df[, {{keyword}}])
    #
    #
    # # get a vector with values only
    # relevant_cols <- df |> select({{ keyword }}) |> pull()
    #
    # # re-normalized to the overall maximum value to have max =100
    # result <- df |>
    #   mutate(
    #     {{keyword }} := round((100 * relevant_cols / maxi), 4)
    #   )
    #
    # return(result)
  }
}
