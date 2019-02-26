#' A function that plots daily peak signal size (pss) for Logie counter data
#'
#' This function plots a time series of average daily peak signal size (pss) and hourly pss for Logie counter data.
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param description The type of counter data to be plotted. Must be "U" (ups), "D" (downs), or "E" (events).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day format. Defaults to the first day in the dataset.
#' @param last_day The last day of the dataset to be plotted, which must be specified in year-day format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold peak signal size (pss) value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold peak signal size (pss) value to be plotted. Defaults to 130.
#' @param ch The channel to be plotted. Defaults to all channels. Needs to be inputed as an object or vector (e.g. 1, or c(1, 2)).
#' @param print_to_file If TRUE, the plot is saved to the working directory. Defaults to FALSE.
#' @return Generates a plot of the average daily peak signal size (for the specified description) and hourly pss. 

plot_pss_date <- function(dataset, description, first_day = NULL, last_day = NULL, min_pss = NULL, max_pss = NULL,
                          ch = NULL, print_to_file = FALSE) {

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

  # Create a ch label for the file name
  ch_name <- paste(ch, collapse = "")

  dataset <- subset(dataset, channel %in% ch)
  dataset <- dplyr::filter_(dataset, ~jday >= first_day, ~jday <= last_day)
  dataset$date.time <- lubridate::ymd_hms(dataset$date.time)
  dataset$date.time <- as.POSIXct(round(dataset$date.time, "hours")) # Round to the nearest hour.

  # Determine the daily mean pss
  mean_pss <- plyr::ddply(dataset, c("date"),
                             summarize, mean_pss = mean(pss))
  mean_pss$date_alt <- as.POSIXct(strptime(mean_pss$date, "%Y-%m-%d"))
  r <- as.POSIXct(range(mean_pss$date_alt))

  # Plot
  if (print_to_file == TRUE) {
    png(sprintf("plot_pss_date_%s_channels%s.png", description, ch_name), height = 6, width = 7, units = "in", res = 1000)
  }

  par(mfrow = c(1, 1),
      mar = c(2, 2, 2, 2),
      oma = c(2, 2, 2, 2))

  plot(pss ~ date.time, data = dataset,
       col = "#00000010",
       pch = 19,
       cex = 1.5,
       axes = FALSE,
       las = 1,
       xlab = "",
       ylab = "",
       ylim = c(min_pss, max_pss))

  par(new=TRUE)

  plot(mean_pss ~ date_alt, data = mean_pss, typ = "p", col = "red", pch = 19,
       cex = 1.5, axes = FALSE, las = 1, xlab = "", ylab = "",
       ylim = c(min_pss, max_pss))

  axis.POSIXct(1, at = seq(r[1], r[2], by = "2 days"), format = "%b %d", cex.axis = 0.85,
               col = "grey60")

  axis(2, las = 1, col = "grey60")

  box(col = "grey60")

  mtext(sprintf("PSS (%s)", description), side = 2, line = 2.5, outer = FALSE, cex = 1.5)

  if(print_to_file == TRUE) {
    dev.off()
  }
}
