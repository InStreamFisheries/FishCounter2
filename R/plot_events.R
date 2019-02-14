#' A function that plots events per hour of Logie counter data
#'
#' This function plots the number of events per hour, channel, and day.
#' @param dataset The dataset used to create the plots.
#' @param first_day The first day of the dataset you want to use. This parameter needs to be specified in year day format. Defaults to the first day in the dataset
#' @param last_day The last day of the dataset you want to use. This parameter needs to be specified in year day format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold PSS value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold PSS value to be plotted. Defaults to 130.
#' @param print_to_file If TRUE, plot is saved to the working directory (defaults to FALSE).
#' @return Generates a plot displaying the hourly count of ups and events for Logie counter data.

plot_events <- function(dataset, first_day = NULL, last_day = NULL, min_pss = NULL, max_pss = NULL, print_to_file = FALSE) {

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(dplyr))
  
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

  d1 <- dplyr::filter_(dataset, ~jday >= first_day, ~jday <= last_day)
  d1 <- subset(d1, pss >= min_pss & pss <= max_pss)

  events_hour1 <- data.frame(dplyr::filter_(d1, ~description == "E"), no = 1)
  events_hour1$date_time_alt <- as.character(as.POSIXct(strptime(paste(events_hour1$date,
                                                                       events_hour1$time,sep = " "), '%Y-%m-%d %H')))
  events_hour_channel <- plyr::ddply(events_hour1, c("date_time_alt", "channel"),
                                     summarize, no_events = sum(no))

  events_hour_channel$date_time_alt <- as.POSIXct(strptime(events_hour_channel$date_time_alt,
                                                           format = "%Y-%m-%d %H"))

  r <- range(events_hour_channel$date_time_alt)

  up_hour1 <- data.frame(dplyr::filter_(d1, ~description == "U"), no = 1)
  up_hour1$date_time_alt <- as.character(as.POSIXct(strptime(paste(up_hour1$date,
                                                                   up_hour1$time,sep = " "), '%Y-%m-%d %H')))
  up_hour_channel <- plyr::ddply(up_hour1, c("date_time_alt", "channel"),
                                 summarize, no_ups = sum(no))

  up_hour_channel$date_time_alt <- as.POSIXct(strptime(up_hour_channel$date_time_alt,
                                                       format = "%Y-%m-%d %H"))
  hour_channel <- merge(events_hour_channel, up_hour_channel, all  =TRUE)
  hour_channel[is.na(hour_channel)] <- 0
  hour_channel$channel_lab <- plyr::revalue(as.factor(hour_channel$channel), c("1" = "Channel 1", "2" ="Channel 2", "3" = "Channel 3", "4" = "Channel 4"))

  hour_channel_long <- tidyr::gather(hour_channel, type, count, no_events:no_ups)

  mean_events_per_hour <- plyr::ddply(hour_channel_long, c("channel_lab", "type"), summarize, mean = mean(count))

  # Option with only one axis
  one_axis <- ggplot(hour_channel_long, aes(x = date_time_alt, y = count, group = type, color = type)) +
    geom_line() +
    facet_wrap(~channel_lab, nrow = 4) +
    ylab("Count per Hour") + xlab("") +
    scale_x_datetime(limits = c(r[1], r[2]), date_breaks = "4 days", labels = scales::date_format("%b %d")) +
    # geom_hline(data = subset(mean_events_per_hour, type == "no_events"), aes(yintercept = mean), colour = "red",
    #            linetype = "dashed") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(1,1),
          legend.justification = c(1, 1),
          legend.background = element_rect(colour = NA, fill = "transparent")) +
    scale_color_manual(values = c("black", "blue"),
                       breaks = c("no_events", "no_ups"),
                       labels = c("Events", "Up Counts"))

  print(one_axis)

  if (print_to_file == TRUE) {
    ggsave("plot_events.png", one_axis, width = 7, height = 6, units = "in")
  }

  events_hour_ch <- plyr::ddply(hour_channel, c("channel"), function(x) {
    data.frame(date_time = x$date_time_alt, no_events = x$no_events)
  })

  # I don't know why not all hours are included here, but I won't change until someone comments.
  #print(events_hour_ch)
  write.csv(events_hour_ch, file = "plot_events_data.csv", row.names = FALSE)
}
