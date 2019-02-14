#' A function that plots daily up and down counts as well as cumulative counts of Logie counter data
#'
#' Plots daily up and down counts.
#' @param dataset The dataset used to create the plots.
#' @param first_day The first day of the dataset you want to use. This parameter needs to be specified in year day format. Defaults to the first day in the dataset
#' @param print_to_file If TRUE, plot is saved to the working directory (defaults to FALSE).
#' @return Two plots are created, one with daily up, down, and cumulative total counts, as well as one with ups and cumulative up counts only.

plot_abundance <- function(dataset, first_day, print_to_file = FALSE) {

  suppressWarnings(library(ggplot2))
  suppressMessages(library(dplyr))
  
  dataset$jday <- lubridate::yday(lubridate::ymd(dataset$date))

  if(is.null(first_day)) {
    first_day <- min(dataset$jday)
  }

  dataset <- filter_(dataset, ~jday >= first_day)
  dataset$jday <- NULL # I'm not sure why this is here

  dataset$round.date <- as.POSIXct(round(lubridate::ymd_hms(dataset$date.time), "day")) # Round up to the nearest day. Why is this here?

  updata <- data.frame(filter_(dataset, ~description == "U"), count = 1)
  updata$cummulative_count <- cumsum(updata$count)

  up <- plyr::ddply(updata, ~round.date, summarize,
              daily_count = sum(count),
              count = max(cummulative_count))

  r <- as.POSIXct(range(up$round.date))

  ups1 <- ggplot(up, aes(x = round.date, y = daily_count)) +
    geom_point() +
    geom_line() +
    ylab("Daily Up Count") + xlab("") +
    theme_bw() +
    #scale_x_datetime(limits = c(r[1], r[2]), date_breaks = "4 days", labels = scales::date_format("%b %d")) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())

  ups2 <- ggplot(up, aes(x = round.date, y = count)) +
    geom_point() +
    geom_line() +
    ylab("Cumulative Up Count") + xlab("") +
    scale_x_datetime(limits = c(r[1], r[2]), date_breaks = "4 days", labels = scales::date_format("%b %d")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  grid::grid.newpage()
  grid::grid.draw(rbind(ggplotGrob(ups1), ggplotGrob(ups2), size = "last"))

  if (print_to_file == TRUE) {
    png("plot_abundance_up.png", width = 7, height = 6, units = "in", res = 1000)
    grid::grid.newpage()
    grid::grid.draw(rbind(ggplotGrob(ups1), ggplotGrob(ups2), size = "last"))
    dev.off()
  }

  ###################################
  # Plot with Both UP and DOWN data #
  ###################################

  downdata <- data.frame(filter_(dataset, ~description == "D"), count = 1)

  ups <- plyr::ddply(updata, ~round.date, summarize,
                     daily_count = sum(count))
  colnames(ups) <- c("round.date", "ups")

  downs <- plyr::ddply(downdata, ~round.date, summarize,
                       daily_count = sum(count))
  colnames(downs) <- c("round.date", "downs")

  counts <- merge(ups, downs, by = "round.date", all = TRUE)

  counts[is.na(counts)] <- 0 # Turn na counts into 0

  counts <- counts %>%
    mutate(up_sub_down = ups - downs) %>%
    mutate(daily_total = cumsum(up_sub_down))

  # Find max up or down count for upper ylim
  upperY <- max(rbind(counts$ups, counts$downs))

  r.count <- as.POSIXct(range(counts$round.date))

  counts.long <- tidyr::gather(counts, category, count, ups:daily_total)
  counts.long1 <- subset(counts.long, category %in% c("ups","downs"))
  counts.long2 <- subset(counts.long, category == "daily_total")

  total1 <- ggplot(counts.long1, aes(x = round.date, y = count, color = category)) +
    geom_point() +
    geom_line() +
    ylab("Daily Count") + xlab("") +
    theme_bw() +
    scale_y_continuous(limits = c(0, upperY)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(1,1),
          legend.justification = c(1, 1),
          legend.background = element_rect(colour = NA, fill = "transparent")) +
    scale_color_manual(values = c("grey", "black"),
                      breaks = c("ups", "downs"),
                      labels = c("Up Counts", "Down Counts"))

  total2 <- ggplot(counts.long2, aes(x = round.date, y = count)) +
    geom_point() +
    geom_line() +
    ylab("Cumulative Daily Count") + xlab("") +
    scale_x_datetime(limits = c(r.count[1], r.count[2]), date_breaks = "4 days", labels = scales::date_format("%b %d")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  grid::grid.newpage()
  grid::grid.draw(rbind(ggplotGrob(total1), ggplotGrob(total2), size = "last"))

  if (print_to_file == TRUE) {
    png("plot_abundance_total.png", width = 7, height = 6, units = "in", res = 1000)
    grid::grid.newpage()
    grid::grid.draw(rbind(ggplotGrob(total1), ggplotGrob(total2), size = "last"))
    dev.off()
  }
}
