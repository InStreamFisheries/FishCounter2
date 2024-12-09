#' A function that plots daily up and down counts as well as cumulative counts of Logie counter data
#'
#' This funtion plots a time series of up and down counts and cumulative counts for Logie counter data.
#' @param dataset The cleaned counter dataset used to populate histograms (i.e., counter_data as created by bind_counter_data()).
#' @param first_day The first day of the dataset to be plotted, which must be specified in year-day (yday) format. Defaults to the first day in the dataset.
#' @param print_to_file If TRUE, the plot is saved to the working directory. Defaults to FALSE.
#' @return Two plots are returned as a list: a time series of daily up, down, and cumulative total counts, and a time series of up counts and cumulative up counts. Both plots are also printed to the console.
#' @export

plot_abundance <- function(dataset,
                           first_day = NULL,
                           print_to_file = FALSE) {

  dataset$jday <- lubridate::yday(lubridate::ymd(dataset$date))

  if(is.null(first_day)) {
    first_day <- min(dataset$jday)
  }

  dataset <- dataset |>
    dplyr::filter(jday >= first_day) |>
    dplyr::mutate(date = as.POSIXct(lubridate::ymd(date)))


  # Prepare up and down data, then merge ------------------------------------

  updata <- dataset |>
    dplyr::filter(description == "U") |>
    dplyr::mutate(count = 1,
                  cumulative_count = cumsum(count))

  ups <- updata |>
    dplyr::group_by(date) |>
    dplyr::summarise(ups = sum(count))

  downdata <- dataset |>
    dplyr::filter(description == "D") |>
    dplyr::mutate(count = 1,
                  cumulative_count = cumsum(count))

  downs <- downdata |>
    dplyr::group_by(date) |>
    dplyr::summarise(downs = sum(count))

  counts <- dplyr::full_join(ups, downs, by = "date") |>
    tidyr::replace_na(list(ups = 0,
                           downs = 0)) |>
    dplyr::mutate(up_sub_down = ups - downs,
                  daily_total = cumsum(up_sub_down))



  # Plot up data only -------------------------------------------------------

  up <- updata |>
    dplyr::group_by(date) |>
    dplyr::summarise(daily_count = sum(count),
                     count = max(cumulative_count))

  r <- as.POSIXct(range(up$date))

  ups1 <- ggplot2::ggplot(up, ggplot2::aes(x = date, y = daily_count)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylab("Daily Up Count") + ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = function(x) range(pretty(x)),
                                breaks = function(x) pretty(x)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())

  ups2 <- ggplot2::ggplot(up, ggplot2::aes(x = date, y = count)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylab("Cumulative Up Count") + ggplot2::xlab("") +
    # ggplot2::scale_y_continuous(limits = function(x) range(pretty(x)),
    #                             breaks = function(x) pretty(x)) +
    ggplot2::scale_x_datetime(limits = c(r[1], r[2]),
                              date_breaks = "4 days",
                              labels = scales::date_format("%b %d")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  up_plot <- cowplot::plot_grid(plotlist = list(ups1, ups2),
                                nrow = 2)




  # Plot both ups and downs -------------------------------------------------

  r.count <- as.POSIXct(range(counts$date))

  counts.long <- tidyr::pivot_longer(counts,
                                     cols = 2:5,
                                     names_to = "category",
                                     values_to = "count")

  counts.long1 <- dplyr::filter(counts.long, category %in% c("ups", "downs"))
  counts.long2 <- dplyr::filter(counts.long, category == "daily_total")

  total1 <- ggplot2::ggplot(counts.long1, ggplot2::aes(x = date, y = count, color = category)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylab("Daily Count") + ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = function(x) range(pretty(x)),
                                breaks = function(x) pretty(x)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = c(1, 1),
                   legend.justification = c("right", "top"),
                   legend.background = ggplot2::element_rect(colour = NA, fill = "transparent")) +
    ggplot2::scale_color_manual(values = c("black", "grey"),
                                breaks = c("ups", "downs"),
                                labels = c("Up Counts", "Down Counts"))

  total2 <- ggplot2::ggplot(counts.long2, ggplot2::aes(x = date, y = count)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylab("Cumulative Daily Count") + ggplot2::xlab("") +
    ggplot2::scale_y_continuous(limits = function(x) range(pretty(x)),
                                breaks = function(x) pretty(x)) +
    ggplot2::scale_x_datetime(limits = c(r.count[1], r.count[2]),
                              date_breaks = "4 days",
                              labels = scales::date_format("%b %d")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  total_plot <- cowplot::plot_grid(plotlist = list(total1, total2),
                                   nrow = 2)


  # Outputs -----------------------------------------------------------------

  if (print_to_file == TRUE) {

    cowplot::save_plot("plot_abundance_up.png", up_plot, base_height = 6, base_width = 7)
    cowplot::save_plot("plot_abundance_total.png", total_plot, base_height = 6, base_width = 7)

  }


  return(list(ups = up_plot,
              totals = total_plot))


}
