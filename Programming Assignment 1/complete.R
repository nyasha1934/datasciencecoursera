# Assign the directory
complete <- function(directory, id = 1:332) {
  files_list <- list.files("specdata", full.names = TRUE)
  
  # Create an empty vector to populate with observations
  obs <- data.frame()
  
  # For loop to check all the files and all complete cases from all cases
  for(i in id) {
    comDf <- sum(complete.cases(read.csv(files_list[i])))
    obs <- rbind(obs, comDf)
  }
  
  # Create a data frame with two columns, add column names for each column
  report <- cbind(id, obs)
  colnames(report) <- c("id", "nobs")
  report
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3) 
