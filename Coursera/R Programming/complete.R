complete <- function(directory, id = 1:332) {
  # Create a data frame to store the results
  results <- data.frame(id = integer(), nobs = integer(), stringsAsFactors = FALSE)
  
  # Loop over each ID
  for (monitor_id in id) {
    # Create the file name with leading zeros
    file_name <- sprintf("%03d.csv", monitor_id)
    
    # Create the full path to the file
    file_path <- file.path(directory, file_name)
    
    # Read the data
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Count the number of complete cases
    nobs <- sum(complete.cases(data))
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(id = monitor_id, nobs = nobs))
  }
  
  # Return the results data frame
  return(results)
}

# Save the function to complete.R
#writeLines(inspect(complete), con = "complete.R")

# Example usage:
# directory <- "/mnt/data"
# complete_cases <- complete(directory, 1:10)
# print(complete_cases)
