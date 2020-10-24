polutantmean <- function(directory, pollutant, id = 332) {
    
    # Assign a directory
    files_list <- list.files("specdata", full.names = TRUE, pattern = "csv")
    
    # Create an empty vector
    dat <- data.frame()
    
    # For loop to read all the data files at once and store in vector
    for(i in 1:length(files_list)) {
        
        # Where to read the files to obtain pollutant values
        read <- read.csv(files_list[i])
        dat <- c(dat, read[[pollutant]])
    }
    
    # Return value and exclude missing values
    mean(dat, na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23) 

