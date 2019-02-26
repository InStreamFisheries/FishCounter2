#' A function that generates histograms of Logie counter data
#'
#' This function generates historgrams of ups, downs, or events separated by channel for Logie counter data. 
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param description The type of counter data to be plotted. Must be "U" (ups), "D" (downs), or "E" (events).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day format. Defaults to the first day in the dataset.
#' @param last_day The last day of the dataset to be plotted, which must be specified in year-day format. Defaults to the last day in the dataset.
#' @param min_pss The lower threshold peak signal size (pss) value to be plotted. Defaults to 0.
#' @param max_pss The upper threshold peak signal size (pss) value to be plotted. Defaults to 130.
#' @param print_to_file If TRUE, the histogram is saved to the working directory. Defaults to FALSE.
#' @return Generates a histogram of peak signal size for either up counts, down counts, or events for each counter channel. 
#' Also prints to the console a summary of the total number of events per channel for the specified description.

hist_records <- function(dataset, description, first_day = NULL, last_day = NULL, min_pss = NULL, max_pss = NULL, print_to_file = FALSE) {

  suppressWarnings(library(ggplot2))
  
  if(missing(description)) {
    stop(paste("Need to specify the description as character:", "U", "D", "E"))
  }

  record_type <- description


  if(description == "E") {
    description_lab <- "EVENTS"
  }

  if(description == "U") {
    description_lab <- "UP"
  }

  if(description == "D") {
    description_lab <- "DOWN"
  }

  dataset$jday <- lubridate::yday(lubridate::ymd(dataset$date))

  if(is.null(first_day)) {
    first_day <- min(dataset$jday, na.rm=TRUE)
  }

  if(is.null(last_day)) {
    last_day <- max(dataset$jday, na.rm=TRUE)
  }

  if(is.null(min_pss)) {
    min_pss <- 0
  }

  if(is.null(max_pss)) {
    max_pss <- 130
  }

  d1 <- dplyr::filter_(dataset, ~jday >= first_day, ~jday <= last_day)
  d1 <- subset(d1, pss >= min_pss & pss <= max_pss)
  d <- dplyr::select(d1, channel, description, pss)

  # Remove na pss
  d <- subset(d, !is.na(pss))

  # Create a summary of the number of events per channel
  d_summary <- plyr::ddply(subset(d, description == record_type), c("channel"), function(x) {
    count <- length(x$pss)
    data.frame(count)
  })

  colnames(d_summary) <- c("channel", sprintf("no_%s", description))

  # Print d_summary to the console
  print(d_summary)

  d$channel_lab <- plyr::revalue(as.factor(d$channel), c("1" = "Channel 1", "2" ="Channel 2", "3" = "Channel 3", "4" = "Channel 4"))

  hist_plot <- ggplot(data = subset(d, description == record_type), aes(x = pss)) +
    geom_histogram(breaks = seq(0, 130, 5), fill = "grey", col = "black", na.rm = TRUE) +
    facet_wrap(facets = ~channel_lab, nrow = 4, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 150, 20), labels = seq(0, 150, 20), limits = c(0, 130)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.background = element_blank()) +
    ylab(sprintf("Frequency of %s counts", description_lab)) + xlab("peak signal size (pss)")

  print(hist_plot)

  if (print_to_file == TRUE) {
    ggsave(filename = sprintf("hist_records_%s.png", description_lab), height = 6, width = 5, units = "in")
  }

}
