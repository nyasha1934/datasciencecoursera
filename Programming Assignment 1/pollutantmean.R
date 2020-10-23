# Assign the directory
pollutantmean <- function(directory, pollutant, id = 1:332) {
    files_list <- list.files("specdata", full.names = TRUE, pattern = "csv")
    
    # Create an empty vector
    dat <- data.frame()
    
    # For loop to run through files and combine them into a single data frame
    for (i in 1:332) {
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    
    # Subset to get sulfate/nitrate columns and calculate mean
    poll_subset <- subset(dat, ID %in% id, select = pollutant)
    colMeans(poll_subset, na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

