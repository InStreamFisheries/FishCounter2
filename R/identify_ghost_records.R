# identify ghosts in counter data

#' This function detects possible ghost traces in graphics validated excel sheets
#' it identifies traces with the same direction within 2 seconds of the previous trace if it has an equal or smaller pss on a different channel
#' A date.time column must be present in the excel file
#' The graphics validation column must be named graphics_validation
#' The excel file must have a "channel" and "pss" column

ghosts <- function(data){

  out <- data %>%
    mutate(date.time = as.POSIXct(date.time, format = "%Y-%m-%d %H:%M:%S")) %>%
    group_by(graphics_validation) %>%
    arrange(date.time) %>%
    mutate(time_diff = date.time - lag(date.time),
           ghost = case_when(is.na(time_diff) ~ NA_character_,
                             abs(time_diff) <= 2 & channel != lag(channel) & pss <= lag(pss) ~ "possible ghost",
                             TRUE ~ "")) %>%
    ungroup()

  return(out)

}
