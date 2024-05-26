corr <- function(directory, threshold = 0) {
  # List all files in the directory
  file_list <- list.files(directory, full.names = TRUE)
  
  # Initialize a vector to store the correlations
  correlations <- numeric()
  
  # Loop over each file
  for (file_path in file_list) {
    # Read the data
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Count the number of complete cases
    complete_cases <- data[complete.cases(data), ]
    
    # Check if the number of complete cases exceeds the threshold
    if (nrow(complete_cases) > threshold) {
      # Calculate the correlation between sulfate and nitrate
      corr_value <- cor(complete_cases$sulfate, complete_cases$nitrate, use = "complete.obs")
      # Append the correlation to the vector
      correlations <- c(correlations, corr_value)
    }
  }
  
  return(correlations)
}

# Example usage:
# directory <- "/mnt/data"
# correlations <- corr(directory, threshold = 50)
# print(correlations)
