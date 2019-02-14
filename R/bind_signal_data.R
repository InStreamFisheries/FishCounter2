#' A function that combines individual Logie graphics data files.
#'
#' A function to combine and process Logie graphics data.
#' @param path_to_folder The file path for the folder that contains all data files for processing.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param max_pss The maximum pss size.
#' @param rows_rm Determines whether the rows removed from the data are returned as a .csv file in the working directory. Defaults to FALSE.
#' @return A master data file of processed Logie signal data. The data are written to file as well as stored in the environment. All individual signal data files are combined into one master graphics file and written to the folder path as "all_signal_data.csv".


bind_signal_data <- function(path_to_folder, site, year, max_pss, rows_rm = FALSE){

  #"\\.txt$" tells r that the files are text files.
  signal_paths <- dir(path_to_folder, pattern = "\\.txt$", full.names = TRUE)

  names(signal_paths) <- basename(signal_paths)

  if(missing(site)) {
    site <- ""
  }

  if(missing(year)) {
    year <- ""
  }

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
            file = paste(path_to_folder,
                         site,
                         year,
                         ".csv",
                         sep = ""),
            row.names = FALSE)

  if(rows_rm == "TRUE") {

    write.csv(x = row_rm5[, -2],
              file = paste(path_to_folder,
                           site,
                           year,
                           "wrongPSS",
                           ".csv",
                           sep = ""),
              row.names = FALSE)
  }

  final_list <- list(signal_data = signal_data,
                     wrong_pss = row_rm5)
  return(final_list)

  # Add a function that binds all of the signal data together and creates a master text file

  data.list <- list()

  for(i in 1:length(signal_paths)) {
    temp <- read.table(file = signal_paths[i], header = FALSE, sep = "", fill = TRUE)
    data <- subset(temp[,c(1:7)], V1=="S" | V1=="D"| V1=="F")
    colnames(data) <- c("code", "date", "time", "X", "channel", "description", "signal")
    data.list[[i]] <- data
  }

  data.all <- do.call("rbind", data.list)

  data.all.2 <- ifelse(data.all$code=="S", as.character(paste(data.all$code, data.all$date, data.all$time, data.all$X, data.all$channel, data.all$description, data.all$signal, sep=" ")), as.character(paste(data.all$code, data.all$date, sep=" ")))

  write.table(x = data.all.2, file = sprintf("%s/all_signal_data.txt", path_to_folder), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")

}
