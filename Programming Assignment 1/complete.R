complete <- function(directory, id = 1:332) {
  files_list <- list.files("specdata", full.names = TRUE)
  obs <- data.frame()
  for(i in id) {
    comDf <- sum(complete.cases(read.csv(files_list[i])))
    obs <- rbind(obs, comDf)
  }
  report <- cbind(id, obs)
  colnames(report) <- c("id", "nobs")
  report
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3) 
