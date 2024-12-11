#' A function that combines and processes raw Logie graphics data files.
#'
#' A function to combine and process raw Logie graphics data. Raw Logie data files are combined and errors and duplicates are removed.
#' @param path_to_folder The file path to the folder that contains raw Logie graphics data files (.txt) to be processed.
#' @param output_folder The file path to the folder for output files. Defaults to path_to_folder if unassigned.
#' @param site Name of the study river. The site is used to name output .csv files (i.e., siteyear.csv).
#' @param year Year of counter operation. The year is used to name output .csv files (i.e., siteyear.csv).
#' @param max_pss The maximum peak signal size (pss).
#' @param print_removed Defaults to FALSE. If TRUE, pss error data that were removed from the final data frame are printed to the path_to_folder as a .csv file.
#' @return A list containing two elements: signal_data (cleaned master data file), and wrong_pss (data containing errors in pss that were removed from signal_data). Signal_data is written to the path_to_folder location as a .csv file. If print_removed is TRUE, wrong_pss is also returned as a .csv file. An additional file, all_signal_data.csv, is written to path_to_folder, which combines all raw graphics data contained in the data files into one master graphics file.
#' @export

bind_signal_data <- function(path_to_folder,
                             output_folder = NULL,
                             site,
                             year,
                             max_pss,
                             print_removed = FALSE) {

  output_folder <- if(is.null(output_folder)){
    path_to_folder
  }

  #"\\.txt$" tells r that the files are text files.
  signal_paths <- dir(path_to_folder, pattern = "\\.txt$", full.names = TRUE)

  names(signal_paths) <- basename(signal_paths)

  signal_data1 <- plyr::ldply(signal_paths,
                              read.table,
                              header = FALSE,
                              sep = "",
                              fill = TRUE,
                              stringsAsFactors = FALSE)

  signal_data2 <- subset(signal_data1[, c(1:8)], V1 == "S")[, -2]

  signal_data3 <- droplevels(signal_data2)

  colnames(signal_data3) <- c("file.name",
                              "date",
                              "time",
                              "conductivity",
                              "channel",
                              "description",
                              "pss")

  signal_data4 <- data.frame("file.name" = signal_data3$file,
                             "date.time" = lubridate::dmy_hms(paste(signal_data3$date, signal_data3$time)),
                             "date" = as.character(lubridate::dmy(signal_data3$date)),
                             "time" = as.character(signal_data3$time),
                             "conductivity" = as.numeric(signal_data3$conductivity),
                             "channel" = as.numeric(signal_data3$channel),
                             "description" = signal_data3$description,
                             "pss" = suppressWarnings(as.numeric(signal_data3$pss))) # Some of the numbers have *, which results in an NA

  # Create a new column for dummy fish true or false
  signal_data4$dummy <- !grepl(pattern = "*", x = signal_data4$pss)

  signal_data5 <- signal_data4[!duplicated(signal_data4[, c(2, 6)]), ] # Remove duplicates

  signal_data <- subset(signal_data5, pss <= max_pss | is.na(pss))
  row_rm5 <- subset(signal_data5, pss > max_pss)

  signal_data <- signal_data[order(signal_data$date.time), ]

  write.csv(x = signal_data[, -2],
            file = paste0(output_folder,"/",
                         site,
                         year,
                         ".csv"),
            row.names = FALSE)

  if(print_removed == "TRUE") {

    write.csv(x = row_rm5[, -2],
              file = paste0(output_folder,"/",
                           site,
                           year,
                           "wrongPSS",
                           ".csv"),
              row.names = FALSE)
  }

  final_list <- list(signal_data = signal_data,
                     wrong_pss = row_rm5)

  return(final_list$signal_data)

}
