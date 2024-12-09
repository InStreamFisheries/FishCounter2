#' A function that plots daily peak signal size (pss) for Logie counter data
#'
#' This function plots a time series of average daily peak signal size (pss) and hourly pss for Logie counter data.
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param description The type of counter data to be plotted. Must be "U" (ups), "D" (downs), or "E" (events).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the first day in the dataset.
#' @param last_day The last day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold peak signal size (pss) value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold peak signal size (pss) value to be plotted. Defaults to 130.
#' @param ch The channel to be plotted. Defaults to all channels. Needs to be specified as an object or vector (e.g. 1, or c(1, 2)).
#' @param print_to_file If TRUE, the plot is saved to the working directory. Defaults to FALSE.
#' @return Returns a plot of the average daily peak signal size (for the specified description) and hourly pss.
#' @export

plot_pss_date <- function(dataset,
                          description,
                          first_day = NULL,
                          last_day = NULL,
                          min_pss = NULL,
                          max_pss = NULL,
                          ch = NULL,
                          print_to_file = FALSE) {

  # Subset the description of data to be plotted
  if (description == "U") {
    dataset <- subset(dataset, description == "U")
  } else if (description == "D") {
    dataset <- subset(dataset, description == "D")
  } else if (description == "E") {
    dataset <- subset(dataset, description == "E")
  }

  if(is.null(min_pss)) {
    min_pss <- 0
  }

  if(is.null(max_pss)) {
    max_pss <- 130
  }

  dataset$jday <- lubridate::yday(lubridate::ymd(dataset$date))

  if(is.null(first_day)) {
    first_day <- min(dataset$jday, na.rm = TRUE)
  }

  if(is.null(last_day)) {
    last_day <- max(dataset$jday, na.rm = TRUE)
  }

  if(is.null(ch)) {
    ch <- seq(min(dataset$channel, na.rm = TRUE), max(dataset$channel, na.rm = TRUE), 1)
  }


  # Prepare data for plotting -----------------------------------------------


  dataset1 <- dataset |>
    dplyr::filter(channel %in% ch) |>
    dplyr::filter(jday >= first_day, jday <= last_day) |>
    dplyr::mutate(date.time = lubridate::ymd_hms(date.time),
                  date.time = as.POSIXct(round(date.time, "hours"))) |>
    dplyr::filter(!is.na(pss))

  # Determine the daily mean pss
  mean_pss <- dataset1 |>
    dplyr::group_by(date) |>
    dplyr::summarize(mean_pss = mean(pss)) |>
    # dplyr::mutate(date_alt = as.POSIXct(strptime(date, "%Y-%m-%d"))) |> # Reformat date
    dplyr::mutate(date_alt = as.POSIXct(lubridate::ymd(date))) |>
    dplyr::filter(!is.na(mean_pss))

  r <- as.POSIXct(range(mean_pss$date_alt))

  # Create a ch label for the file name
  ch_name <- paste(ch, collapse = "")


  # Generate plot -----------------------------------------------------------

  pss_date <- ggplot2::ggplot(dataset1, ggplot2::aes(x = date.time, y = pss)) +
    ggplot2::geom_point(color = "#00000010", size = 1.5) +
    ggplot2::geom_point(data = mean_pss, ggplot2::aes(x = date_alt, y = mean_pss), color = "red", size = 1.5) +
    ggplot2::ylab(sprintf("PSS (%s)", description)) +
    ggplot2::xlab("") +
    ggplot2::scale_y_continuous(limits = c(min_pss, max_pss),
                                breaks = seq(0, 200, 20),
                                labels = seq(0, 200, 20)) +
    ggplot2::scale_x_datetime(breaks = scales::date_breaks("1 week"),
                              labels = scales::date_format("%b %d")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )



  # Outputs -----------------------------------------------------------------

  ggplot2::ggsave(sprintf("plot_pss_date_%s_channels%s.png", description, ch_name), pss_date, height = 4, width = 5, units = "in")

  return(pss_date)

}
