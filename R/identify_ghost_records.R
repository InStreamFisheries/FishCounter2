#' A function used to identify possible ghost traces in resistivity counter data
#' It identifies traces with the same direction within 2 seconds of the previous trace if it has an equal or smaller pss on a different channel
#' The description column is used for comparison
#' The data must contain a "description", "channel", "pss", "date", and "time" column
#' @return the data with a "ghost" column identifying possible ghost traces produced by the counter
#' @export

ghosts <- function(data){

  out <- data %>%
    mutate(date.time = as.POSIXct(paste0(date, time), format = "%Y-%m-%d %H:%M:%S")) %>%
    group_by(description) %>%
    arrange(date.time) %>%
    mutate(time_diff = date.time - lag(date.time),
           ghost = case_when(is.na(time_diff) ~ NA_character_,
                             abs(time_diff) <= 2 & channel != lag(channel) & pss <= lag(pss) ~ "possible ghost",
                             TRUE ~ "")) %>%
    select(-time_diff, -date.time) %>%
    ungroup()

  return(out)

}

