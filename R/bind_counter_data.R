#' A function that combines and processes raw Logie counter data files.
#'
#' A function to combine and process raw Logie counter data. Raw Logie counter files are combined and errors and duplicates are 
#' removed.
#' @param path_to_folder The file path to the folder that contains raw Logie counter data files (.txt) to be processed.
#' @param no_channels The number of counter channels that were operated. Data with channel numbers greater than no_channels 
#' are assumed to be false data and are removed from the final data frame.
#' @param site Name of the study river. The site name appears as a column in the resulting data frames and in the file name 
#' of output .csv files (i.e., siteyear.csv).  
#' @param year Year of counter operation. The year is used to name output .csv files (i.e., siteyear.csv).
#' @param max_pss The maximum peak signal size (pss).
#' @param print_removed Defaults to FALSE. If TRUE, error data that were removed from the final data frame are printed to the path_to_folder as .csv files.
#' @return A list containing four elements: counter_data (cleaned master data file), 
#' wrong_pss (data containing errors in pss that were removed from counter_data), wrong_channel 
#' (data containing errors in channel that were removed from counter_data), and wrong_description 
#' (data containing errors in description that were removed from counter data). Counter_data is written 
#' to the path_to_folder location as a .csv file. If print_removed is TRUE, wrong_pss, wrong_channel, and wrong_description 
#' .csv files are also created.


bind_counter_data <- function(path_to_folder, no_channels, site, year, max_pss, print_removed = FALSE) {

  counter_paths <- dir(path_to_folder, full.names = TRUE)
  names(counter_paths) <- basename(counter_paths)

  counter_data1 <- plyr::ldply(counter_paths,
                               read.table,
                               header = FALSE,
                               sep = "",
                               fill = TRUE,
                               stringsAsFactors = FALSE)[, c(1:7)]

  # stringsAsFactors=FALSE is important because conversion
  # of numeric factors to numeric can be problematic.

  colnames(counter_data1) <- c("file.name",
                               "date",
                               "time",
                               "conductivity",
                               "channel",
                               "description",
                               "pss")

  counter_data2 <- subset(counter_data1, description == "U" | description == "D" | description == "E")

  row_rm1 <- subset(counter_data1, description != "U" & description != "D" & description != "E")

  date.alt <- lubridate::dmy(counter_data2$date)
  counter_data2$jday <- lubridate::yday(date.alt)
  counter_data3 <- subset(counter_data2, jday != "NA") # check to see if I need to convert to jday

  counter_data4 <- data.frame("file.name" = counter_data3$file,
                              "date.time" = as.character(lubridate::dmy_hms(paste(counter_data3$date, counter_data3$time))),
                              "date" = as.character(lubridate::dmy(counter_data3$date)),
                              "time" = as.character(counter_data3$time),
                              "conductivity" = as.numeric(counter_data3$conductivity),
                              "channel" = as.numeric(counter_data3$channel),
                              "description" = counter_data3$description,
                              "pss" = suppressWarnings(as.numeric(counter_data3$pss))) # Warnings are suppressed from turning * into NA

  # Create a new column for dummy fish true or false
  counter_data4$dummy <- !grepl(pattern = "*", x = counter_data4$pss)

  counter_data5 <- subset(counter_data4, channel <= no_channels)
  row_rm5 <- subset(counter_data4, channel > no_channels | is.na(channel))
  # removes any errors in channel number

  counter_data6 <- counter_data5[!duplicated(counter_data5[, c(2, 6)]), ]
  # removes any duplicate data

  counter_data7 <- subset(counter_data6, pss <= max_pss | is.na(pss))
  row_rm7 <- subset(counter_data6, pss > max_pss)

  # gets rid of levels that have been subseted out.

  counter_data8 <- droplevels(counter_data7)
  # gets rid of levels that have been subseted out.

  counter_data9 <- counter_data8[order(counter_data8$date.time), ]
  counter_data <- data.frame("site" = site, counter_data9)
  # Now write a new text file with only the graphics data.
  # The row names, column names and quotes must be removed.

  write.csv(x = counter_data[, -3],
            file = paste(path_to_folder,
                         site,
                         year,
                         ".csv",
                         sep=""),
            row.names = FALSE)

  if(print_removed == "TRUE") {

    write.csv(x = row_rm1,
              file = paste(path_to_folder,
                           site,
                           year,
                           "wrongDescription",
                           ".csv",
                           sep = ""),
              row.names = FALSE)

    write.csv(x = row_rm5,
              file = paste(path_to_folder,
                           site,
                           year,
                           "wrongChannel",
                           ".csv",
                           sep = ""),
              row.names = FALSE)

    write.csv(x = row_rm7,
              file = paste(path_to_folder,
                           site,
                           year,
                           "wrongPSS",
                           ".csv",
                           sep = ""),
              row.names = FALSE)
  }

  final_list <- list(counter_data = counter_data,
                     wrong_pss = row_rm7,
                     wrong_channel = row_rm5,
                     wrong_description = row_rm1)

  return(final_list)

}
