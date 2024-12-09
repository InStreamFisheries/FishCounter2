#' A function that generates histograms of Logie counter data
#'
#' This function generates histograms of ups, downs, or events separated by channel for Logie counter data.
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param description The type of counter data to be plotted. Must be "U" (ups), "D" (downs), or "E" (events).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the first day in the dataset.
#' @param last_day The last day of the dataset to be plotted, which must be specified in year-day (yday). Defaults to the last day in the dataset.
#' @param min_pss The lower threshold peak signal size (pss) to be plotted. Defaults to 0.
#' @param max_pss The upper threshold peak signal size (pss) to be plotted. Defaults to 130.
#' @param print_to_file If TRUE, the histogram is saved to the working directory. Defaults to FALSE.
#' @return Returns a histogram of peak signal size for either up counts, down counts, or events for each counter channel. The histograms and a summary of the total number of events per channel are printed to the console.
#' @export

hist_records <- function(dataset,
                         description,
                         first_day = NULL,
                         last_day = NULL,
                         min_pss = NULL,
                         max_pss = NULL,
                         print_to_file = FALSE) {

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

  d <- dataset |>
    dplyr::filter(jday >= first_day, jday <= last_day) |>
    dplyr::filter(pss >= min_pss & pss <= max_pss) |>
    dplyr::select(channel, description, pss) |>
    dplyr::filter(!is.na(pss)) |>
    dplyr::mutate(channel = as.factor(channel)) |>
    dplyr::mutate(channel_lab = dplyr::case_when(channel == 1 ~ "Channel 1",
                                                 channel == 2 ~ "Channel 2",
                                                 channel == 3 ~ "Channel 3",
                                                 channel == 4 ~ "Channel 4"))


  # Summarize events per channel --------------------------------------------

  d_summary <- d |>
    dplyr::filter(description == record_type) |>
    dplyr::group_by(channel) |>
    dplyr::summarise(count = dplyr::n())

  colnames(d_summary) <- c("channel", sprintf("no_%s", description))


  # Plot histograms of count frequency --------------------------------------


  hist_plot <- d |>
    dplyr::filter(description == record_type) |>
    ggplot2::ggplot(ggplot2::aes(x = pss)) +
    ggplot2::geom_histogram(breaks = seq(0, 130, 5), fill = "grey", col = "black", na.rm = TRUE) +
    ggplot2::facet_wrap(facets = ~channel_lab, nrow = 4, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq(0, 150, 20), labels = seq(0, 150, 20), limits = c(0, 130)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +
    ggplot2::ylab(sprintf("Frequency of %s Counts", description_lab)) +
    ggplot2::xlab("Peak Signal Size (PSS)")



  # Outputs -----------------------------------------------------------------

  # Save to file
  if (print_to_file == TRUE) {
    ggplot2::ggsave(filename = sprintf("hist_records_%s.png", description_lab), height = 6, width = 5, units = "in")
  }

  # Return hist_plot
  return(hist_plot)

}
