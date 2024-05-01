#' @title Format and export tsv files
#' @description this package is designed to format tsv files in a folder, adjust their content and then export the modified data frames to another folder
#' @param input_folder Path to the folder containing the input text file
#' @param output_folder Path to the folder where processed files will be exported
#' @return A list of processed data frames.
#' @import readr
#' @import dplyr
#' @import magrittr
#' @examples
#' dir.create(file.path("input"))# create input folder
#' dir.create(file.path("output"))# create output folder
#' download.file("https://raw.githubusercontent.com/noromalala/data_tsv/master/tsv_data.zip", "input/tsv_data.zip", mode = "wb")
#' unzip("input/tsv_data.zip", exdir = "input")
#' input_folder <- "input/sarah"
#' output_folder <- "output"
#' processed_data <- format_tsv(input_folder, output_folder)
#' @export
# Function to process text files in a folder and export them
format_tsv <- function(input_folder, output_folder) {
  # Define input and output folder paths

  # Check if input folder exists
  if (!dir.exists(input_folder)) {
    stop("Input folder does not exist. Please provide a valid input folder path.")
  }

  # Get the list of files in the input folder
  file_list <- list.files(path = input_folder, full.names = TRUE)

  # Initialize an empty list to store the processed data frames
  processed_data <- list()

  # Iterate over each file in the input folder
  for (file_path in file_list) {
    # Read the file with specific column types to prevent rounding of milliseconds
    file_data <- readr::read_tsv(
      file_path,
      col_names = FALSE,
      col_types = readr::cols(
        .default = readr::col_character(),  # Set default column type as character
        `X2` = readr::col_time(format = "%H:%M:%OS"),  # Column 3 with milliseconds
        `X4` = readr::col_time(format = "%H:%M:%OS"),  # Column 5 with milliseconds
        `X6` = readr::col_time(format = "%H:%M:%OS")   # Column 7 with milliseconds
      )
    )

    # Add a new column with 'default' values
    file_data$X1_new <- 'default'

    # Reorder and rename columns
    file_data <- file_data |>
      dplyr::select(X1_new, everything()) |>
      dplyr::rename(X1 = X1_new, X2 = X1, X3 = X2, X4 = X3, X5 = X4, X6 = X5, X7 = X6, X8 = X7, X9 = X8)

    # Append the processed data frame to the list
    processed_data[[basename(file_path)]] <- file_data

    # Generate output file path for the processed file
    output_file <- file.path(output_folder, basename(file_path))

    # Export the processed data frame as a text file without column names
    readr::write_tsv(file_data, output_file, col_names = FALSE)
  }

  # Return the list of processed data frames
  return(processed_data)
}
#' @title Metric calculator
#' @description this function calculate breathing metrics
#' @param data A data frame containing respiratory data.
#' @param file_path The file path of the data source.
#' @param observer The name of the observer.
#' @return A data frame with calculated respiratory metrics.
#' @examples
#' # example code
# Define the file path and observer
#' file_path <- "https://raw.githubusercontent.com/noromalala/data_tsv/master/Sarah.txt"
#' observer <- "Sarah"
#' respiratory_data <- read.table(file_path, header = TRUE)
#' metrics <- CalculateRespiratoryMetrics(respiratory_data, file_path, observer)
#' print(metrics)
#' @export
CalculateRespiratoryMetrics <- function(data, file_path, observer) {

  # Extract relevant columns for calculation
  relevant_data <- data[, c(4, 6, 8)]  # Columns containing time in seconds

  # Calculate metrics
  total_duration_sec <- max(relevant_data$X2) - min(relevant_data$X1)
  breaths_per_minute <- nrow(relevant_data) / (total_duration_sec / 60)
  mean_breath_duration <- mean(relevant_data$X3)
  RR_intervals <- diff(relevant_data$X1)
  RRI_variability <- sd(RR_intervals)

  # Extract the file name
  file_name <- gsub("\\.txt$", "", basename(file_path))

  # Return the results as a data frame
  return(data.frame(
    Observer = observer,
    File_Name = file_name,
    Total_Duration_seconds = total_duration_sec,
    Breaths_Per_Minute = breaths_per_minute,
    Mean_Breath_Duration = mean_breath_duration,
    RRI_Variability = RRI_variability
  ))
}

