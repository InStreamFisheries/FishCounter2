#' A function that plots average hourly peak signal size (pss) for Logie counter data
#'
#' This function plots the average hourly peak signal size (pss) for Logie counter data.
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param description The type of counter data to be plotted. Must be "U" (ups), "D" (downs), or "E" (events).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the first day in the dataset.
#' @param last_day The last day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold peak signal size (pss) value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold peak signal size (pss) value to be plotted. Defaults to 130.
#' @param ch The channel to be plotted. Defaults to all channels. Needs to be secified as an object or vector (e.g. 1, or c(1, 2)).
#' @param print_to_file If TRUE, plot is saved to the working directory. Defaults to FALSE.
#' @return Returns a list of two plots of average hourly pss data. One plot displays a time series of average hourly pss for all channels combined (as specified by ch). A second plot displays a time series of average hourly pss separated into each channel.
#' @export

plot_pss_hour <- function(dataset,
                          description,
                          first_day = NULL,
                          last_day  = NULL,
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

  dataset1 <- na.omit(dataset) # Remove NA

  dataset1$jday <- lubridate::yday(lubridate::ymd(dataset1$date)) # Create a year day column

  if(is.null(first_day)) {
    first_day <- min(dataset1$jday, na.rm = TRUE)
  }

  if(is.null(last_day)) {
    last_day <- max(dataset1$jday, na.rm = TRUE)
  }

  if(is.null(ch)) { # Default to all channels
    ch <- seq(min(dataset1$channel, na.rm = TRUE), max(dataset1$channel, na.rm = TRUE), 1)
  }


  # Create datasets for plotting --------------------------------------------


  dataset2 <- dplyr::filter(dataset1, channel %in% ch) # subset the appropriate channels

  dataset3 <- dataset2 |>
    dplyr::filter(jday >= first_day, jday <= last_day) |>
    dplyr::mutate(hour = lubridate::hms(time),
                  hour_24 = lubridate::hour(hour),
                  count = 1,
                  time_of_day = strptime(time, format = "%H:%M:%S"),
                  time_of_day = as.POSIXct(round(time_of_day, "mins")))

  hour_counts <- dataset3 |>
    dplyr::group_by(hour_24) |>
    dplyr::summarize(hour_count = sum(count))

  hour_counts_ch <- dataset3 |>
    dplyr::group_by(channel, hour_24) |>
    dplyr::summarize(hour_count = sum(count), .groups = 'drop') |>
    dplyr::mutate(channel_lab = dplyr::case_when(channel == 1 ~ "Channel 1",
                                                 channel == 2 ~ "Channel 2",
                                                 channel == 3 ~ "Channel 3",
                                                 channel == 4 ~ "Channel 4"))


  # Plot channels combined --------------------------------------------------

  # Create a ch label for the file name
  ch_name <- paste(ch, collapse = "")

  plot_counts <- ggplot2::ggplot(hour_counts, ggplot2::aes(x = hour_24, y = hour_count)) +
    ggplot2::geom_point(fill = "#00000070", size = 1.5) +
    ggplot2::geom_line(color = "#00000070") +
    ggplot2::ylab(sprintf("%s Counts", description)) +
    ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(limits = c(0, 23)) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  PSS_Size <- ggplot2::ggplot(dataset3, ggplot2::aes(x = time_of_day, y = pss)) +
    ggplot2::geom_point(color = "#00000010", size = 1.5) +
    ggplot2::ylab("PSS Size") +
    ggplot2::xlab("Time of Day") +
    ggplot2::scale_x_datetime(date_labels = "%H:%M",
                              date_breaks = "4 hours") +
    ggplot2::scale_y_continuous(limits = c(min_pss, max_pss),
                                breaks = seq(0, 200, 20),
                                labels = seq(0, 200, 20)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )

  combined <- cowplot::plot_grid(plotlist = list(plot_counts, NULL, PSS_Size),
                                 nrow = 3,
                                 rel_heights = c(1, -0.05, 3),
                                 align = "v")


  # Plot channels separated -------------------------------------------------

  separated <- ggplot2::ggplot(hour_counts_ch, ggplot2::aes(x = as.numeric(hour_24), y = hour_count, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~channel_lab) +
    ggplot2::ylab(sprintf("%s Counts", description)) + ggplot2::xlab("Hour of Day") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())


  # Outputs -----------------------------------------------------------------


  if (print_to_file == TRUE) {

    cowplot::save_plot(sprintf("plot_pss_hour_%s_channels%s.png", description, ch_name), combined, base_height = 4, base_width = 5)
    ggplot2::ggsave(sprintf("plot_pss_hour_%s_channels%s_separated.png", description, ch_name), separated, width = 8, height = 6, units = "in")
  }

  return(list(separate = separated,
              combined = combined))

}
