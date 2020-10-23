pollutantmean <- function(directory, pollutant, id = 1:332) {
    files_list <- list.files("specdata", full.names = TRUE, pattern = "csv")
    dat <- data.frame()
    for (i in 1:332){
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    poll_subset <- subset(dat, ID %in% id, select = pollutant)
    colMeans(poll_subset, na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
