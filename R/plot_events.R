#' A function that plots events per hour of Logie counter data
#'
#' This function plots a time series of hourly events and up counts separated by channel for Logie counter data.
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the first day in the dataset.
#' @param last_day The last day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold peak signal size (pss) value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold peak signal size (pss) value to be plotted. Defaults to 130.
#' @param print_to_file If TRUE, the plot is saved to the working directory. Defaults to FALSE.
#' @return Returns a list with time series plot and data frame of hourly events and up counts separated by channel for Logie counter data.
#' @export

plot_events <- function(dataset,
                        first_day = NULL,
                        last_day = NULL,
                        min_pss = NULL,
                        max_pss = NULL,
                        print_to_file = FALSE) {

  dataset$jday <- lubridate::yday(lubridate::ymd(dataset$date))

  if(is.null(first_day)) {
    first_day <- min(dataset$jday, na.rm = TRUE)
  }

  if(is.null(last_day)) {
    last_day <- max(dataset$jday, na.rm = TRUE)
  }

  if(is.null(min_pss)) {
    min_pss <- 0
  }

  if(is.null(max_pss)) {
    max_pss <- 130
  }


  # Prepare data ------------------------------------------------------------

  d1 <- dataset |>
    dplyr::filter(jday >= first_day, jday <= last_day) |>
    dplyr::filter(pss >= min_pss & pss <= max_pss)

  # Events
  events_hour1 <- d1 |>
    dplyr::filter(description == "E") |>
    dplyr::mutate(no = 1,
                  date.time = lubridate::ymd_hms(date.time),
                  date_time_alt = lubridate::round_date(date.time, "hour"))


  events_hour_channel <- events_hour1 |>
    dplyr::group_by(date_time_alt, channel) |>
    dplyr::summarise(no_events = sum(no), .groups = 'drop')


  # Ups
  up_hour1 <- d1 |>
    dplyr::filter(description == "U") |>
    dplyr::mutate(no = 1,
                  date.time = lubridate::ymd_hms(date.time),
                  date_time_alt = lubridate::round_date(date.time, "hour"))

  up_hour_channel <- up_hour1 |>
    dplyr::group_by(date_time_alt, channel) |>
    dplyr::summarise(no_ups = sum(no), .groups = 'drop')

  # Merge events and ups
  hour_channel <- dplyr::full_join(events_hour_channel, up_hour_channel, by = c("date_time_alt", "channel")) |>
    tidyr::replace_na(list(no_ups = 0)) |>
    dplyr::mutate(channel_lab = dplyr::case_when(channel == 1 ~ "Channel 1",
                                                 channel == 2 ~ "Channel 2",
                                                 channel == 3 ~ "Channel 3",
                                                 channel == 4 ~ "Channel 4"))


  hour_channel_long <- tidyr::gather(hour_channel, type, count, no_events:no_ups)

  mean_events_per_hour <- hour_channel_long |>
    dplyr::group_by(channel_lab, type) |>
    dplyr::summarise(mean = mean(count), .groups = 'drop')


  # Plot --------------------------------------------------------------------

  r <- range(events_hour_channel$date_time_alt)

  event_plot <- ggplot2::ggplot(hour_channel_long, ggplot2::aes(x = date_time_alt, y = count, group = type, color = type)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~channel_lab, nrow = 4) +
    ggplot2::ylab("Count per Hour") +
    ggplot2::xlab("") +
    ggplot2::scale_x_datetime(limits = c(r[1], r[2]),
                              date_breaks = "4 days",
                              labels = scales::date_format("%b %d")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = c(1,1),
                   legend.justification = c(1, 1),
                   legend.background = ggplot2::element_rect(colour = NA, fill = "transparent")) +
    ggplot2::scale_color_manual(values = c("black", "blue"),
                                breaks = c("no_events", "no_ups"),
                                labels = c("Events", "Up Counts"))



# Outputs -----------------------------------------------------------------

  # Alternate .csv output format
  event_plot_dat <- hour_channel |>
    dplyr::select(channel, date_time_alt, no_events) |>
    dplyr::arrange(channel, date_time_alt)


  if (print_to_file == TRUE) {
    ggplot2::ggsave("plot_events.png", event_plot, width = 7, height = 6, units = "in")
    write.csv(event_plot_dat, file = "plot_events_data.csv", row.names = FALSE)
  }

  return(list(event_plot = event_plot,
              event_plot_dat = hour_channel))

}
