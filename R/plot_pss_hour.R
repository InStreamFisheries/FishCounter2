#' A function that plots the peak signal size (PSS) by hour of Logie counter data
#'
#' This function plots the PSS by hour for Logie counter data
#' @param dataset The dataset used to create the plots.
#' @param description The type of counter event to be plotted. Must be "U", "D", or "E".
#' @param first_day The first day of the dataset you want to use. This parameter needs to be specified in year day format. Defaults to the first day in the dataset
#' @param last_day The last day of the dataset you want to use. This parameter needs to be specified in year day format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold PSS value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold PSS value to be plotted. Defaults to 130.
#' @param ch The channel to be plotted. Defaults to all channels. Needs to be inputed as an object or vector (e.g. 1, or c(1, 2))
#' @param print_to_file If TRUE, plot is saved to the working directory (defaults to FALSE).
#' @return Generates a plot of total up counts per hour and peak pss size by hour.
#' @export

plot_pss_hour <- function(dataset, description, first_day = NULL, last_day  = NULL, min_pss = NULL, max_pss = NULL, ch = NULL, print_to_file = FALSE) {

  suppressWarnings(library(ggplot2))
  
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

  # Create a ch label for the file name
  ch_name <- paste(ch, collapse = "")

  dataset2 <- subset(dataset1, channel %in% ch) # subset the appropriate channels
  dataset3 <- dplyr::filter_(dataset2, ~jday >= first_day, ~jday <= last_day)

  dataset3$hour <- strptime(dataset3$time, format = "%H:%M:%S")
  dataset3$hour <- as.POSIXct(round(dataset3$hour, "mins"))

  dataset3$count <- 1
  dataset3$hour_24 <- substring(dataset3$hour, first = 12, last = 13)

  hour_counts <- plyr::ddply(dataset3, c("channel", "hour_24"), summarize, hour_count = sum(count))

  m <- matrix(c(0,0,0,0,
                0,1,2,0,
                0,1,2,0,
                0,0,0,0), 4, 4)

  if (print_to_file == TRUE) {
    png(sprintf("plot_pss_hour_%s_channels%s.png", description, ch_name), height = 8, width = 10, units = "in", res = 1000)
  }

  layout(m, widths = c(0.75,2,2,0.25), heights = c(0.05,0.75,2,0.4))
  par(mar = c(0,1,0,0), oma = c(0,0,0,0), cex = 1.25)

  plot(hour_count ~ hour_24, data = hour_counts,
       col = "#00000070", pch = 19, cex = 1.5, axes = FALSE, las = 1,
       xlab = "", ylab = "", type = "b")

  axis(2, las = 1, col = "grey60")
  box(col = "grey60")

  mtext(sprintf("%s Counts", description),
        side = 2,
        line = 4,
        outer = FALSE,
        cex = 1.5,
        padj = 0)


  plot(pss ~ hour, data = dataset3,
       col = "#00000010", pch = 19, cex = 1.5, axes = FALSE, las = 1,
       xlab = "", ylab = "", ylim = c(min_pss, max_pss))

  axis.POSIXct(1, dataset3$hour, format = "%H:%M", cex.axis = 1, col = "grey60")

  axis(2, las = 1, col = "grey60")

  box(col = "grey60")

  mtext("PSS Size",
        side = 2,
        line = 4,
        outer = FALSE,
        cex = 1.5,
        padj = 1)

  mtext("Time of Day",
        side = 1,
        line = 3,
        outer = FALSE,
        cex = 1.5)

  if(print_to_file == TRUE) {
    dev.off()
  }

  # Create a panel plot with just the top part of the above graph
  hour_counts$channel_lab <- suppressMessages(plyr::revalue(as.factor(hour_counts$channel), c("1" = "Channel 1", "2" ="Channel 2", "3" = "Channel 3", "4" = "Channel 4")))

  all_channel_plot <- ggplot(hour_counts, aes(x = as.numeric(hour_24), y = hour_count, group = 1)) +
    geom_point() +
    geom_line() +
    facet_wrap(~channel_lab) +
    ylab(sprintf("%s Counts", description)) + xlab("Hour of Day") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank())

  print(all_channel_plot)

  if (print_to_file == TRUE) {
    ggsave(sprintf("plot_pss_hour_%s_channels%s_separated.png", description, ch_name), all_channel_plot, width = 8, height = 6, units = "in")
  }

}
