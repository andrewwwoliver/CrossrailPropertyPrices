library(httr)
library(jsonlite)

# Replace with your own credentials
username <- "#"
password <- "#"

auth_url <- "https://opendata.nationalrail.co.uk/authenticate"
response <- POST(auth_url, body = list(username = username, password = password), encode = "json")

# Check if the authentication was successful
if (response$status_code == 200) {
  token <- content(response)$token
  print(paste("Authentication successful. Token:", token))
} else {
  stop("Authentication failed. Check your credentials.")
}

# URL for the timetable data
timetable_url <- "https://opendata.nationalrail.co.uk/api/staticfeeds/3.0/timetable"

# Perform the GET request with the token
timetable_response <- GET(timetable_url, add_headers(`X-Auth-Token` = token))

# Check if the request was successful
if (timetable_response$status_code == 200) {
  # Save the zip file
  zip_file <- "timetable_data.zip"
  writeBin(content(timetable_response, "raw"), zip_file)
  print(paste("Data downloaded successfully and saved as", zip_file))
} else {
  stop("Failed to download the data. Check your token and URL.")
}



# Unzip the downloaded file
unzip(zip_file, exdir = "timetable_data")

library(readr)

library(readr)

# Define the path to the MCA file
mca_file_path <- "timetable_data/RJTTF190.MCA"

library(readr)
library(dplyr)

# Define column widths for each record type based on the specification
col_widths <- list(
  HD = c(2, 20, 6, 4, 7, 7, 1, 1, 6, 6, 20),
  BS = c(2, 1, 6, 6, 6, 7, 1, 1, 2, 4, 4, 1, 8, 1, 3, 4, 3, 6, 1, 1, 1, 1, 4, 4, 1, 1),
  BX = c(2, 4, 5, 2, 1, 8, 1, 57),
  LO = c(2, 8, 5, 4, 3, 3, 2, 2, 12, 2, 37),
  LI = c(2, 8, 5, 5, 5, 4, 4, 3, 3, 3, 12, 2, 2, 2, 20),
  CR = c(2, 8, 2, 4, 4, 1, 8, 1, 3, 4, 3, 6, 1, 1, 1, 1, 4, 4, 4, 5, 8, 5),
  LT = c(2, 8, 5, 4, 3, 3, 12, 43),
  AA = c(2, 1, 6, 6, 6, 7, 1, 2, 1, 7, 1, 1, 1, 1, 31, 1),
  TI = c(2, 7, 2, 6, 1, 26, 5, 4, 3, 16, 8),
  TA = c(2, 7, 2, 6, 1, 26, 5, 4, 3, 16, 7, 1),
  TD = c(2, 7, 71),
  ZZ = c(2, 78)
)

# Function to read and process a specific record type
process_line <- function(line) {
  record_type <- substr(line, 1, 2)
  widths <- col_widths[[record_type]]
  
  if (is.null(widths)) {
    return(NULL)
  }
  
  data <- read_fwf(I(line), fwf_widths(widths), col_types = cols(.default = "c"))
  data$record_type <- record_type
  return(data)
}

# Function to read the entire file and process each line
read_and_process_file <- function(file_path) {
  con <- file(file_path, "r")
  on.exit(close(con))
  
  all_data <- list()
  
  while(TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) {
      break
    }
    
    processed_line <- process_line(line)
    if (!is.null(processed_line)) {
      all_data <- append(all_data, list(processed_line))
    }
  }
  
  return(bind_rows(all_data))
}

# Example usage of the function
file_path <- "timetable_data/RJTTF190.MCA"
all_data <- read_and_process_file(file_path)

# Print a summary of the processed data
print(all_data)

save(all_data, file = "all_data.RData")


