install.packages("httr")
install.packages("jsonlite")
install.packages("magrittr")

# Load necessary libraries
library(httr)
library(jsonlite)
library(magrittr)
library(readr)

# Set up the API URL
url <- "https://api.stlouisfed.org/fred/series/observations"

# Function to get data for a single series and handle pagination
get_series_data <- function(series_id, api_key) {
  # Initialize an empty data frame to store all the data for the current series
  all_data <- data.frame()
  
  page <- 1  # Start at page 1
  has_more_data <- TRUE
  
  while (has_more_data) {
    # Define the query parameters
    params <- list(
      series_id = series_id,
      api_key = api_key,
      file_type = "json",
      limit = 10000,  # Reasonable limit per request
      page = page
    )
    
    # Make the API request
    response <- GET(url, query = params)
    
    # Print the HTTP status code for debugging
    print(paste("Status code for series", series_id, ":", http_status(response)$status_code))
    
    # Check if the request was successful
    if (http_status(response)$category == "Success") {
      # Parse the JSON response
      data <- content(response, "text") %>% fromJSON()
      
      # Extract the observations
      observations <- data$observations
      
      # If observations are returned, append to the all_data data frame
      if (length(observations) > 0) {
        all_data <- rbind(all_data, as.data.frame(observations))
      }
      
      # Check if there are more pages of data
      has_more_data <- !is.null(data$places) && data$places$next_page > page
      page <- page + 1
    } else {
      # Print the response content if the request failed for debugging
      print(content(response, "text"))
      stop("Failed to retrieve data from FRED API. Status code: ", http_status(response)$status_code)
    }
  }
  
  return(all_data)
}

# Define the list of series IDs you want to load
series_ids <- c("APU0000708111", "UNRATE", "GDP")  # Add more series IDs as needed

# Your FRED API key
api_key <- "22fa574e26bd85d092267789c733aa70"

# Directory to save the CSV file
setwd("~/Optiver-Challenge")
output_directory <- "."

# Loop through each series ID and save data to a separate CSV file
for (series_id in series_ids) {
  cat("Retrieving data for series:", series_id, "\n")
  
  # Retrieve the data
  series_data <- tryCatch({
    get_series_data(series_id, api_key)
  }, error = function(e) {
    # In case of an error, print the error message and return NULL
    print(paste("Error retrieving data for series:", series_id, ":", e$message))
    NULL
  })
  
  # If data was retrieved successfully, save it to a CSV file
  if (!is.null(series_data)) {
    # Define the file name with the path (saving to /optiver/optiver-challenge directory)
    file_name <- file.path(output_directory, paste0(series_id, "_data.csv"))
    
    # Save the data to a CSV file
    write.csv(series_data, file_name, row.names = FALSE)
    
    cat("Data for", series_id, "saved to", file_name, "\n")
  } else {
    cat("No data retrieved for series:", series_id, "\n")
  }
}

cat("All data retrieval and saving complete.\n")




