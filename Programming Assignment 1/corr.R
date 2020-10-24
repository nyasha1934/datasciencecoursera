corr <- function(directory, threshold = 0) {
  
  # Assign a directory
  files_list <- list.files("specdata", full.names = TRUE)
  
  # Create an empty numeric vector to return the result
  result <- numeric()
  
  # For loop to read the files list and select for complete cases
  for(i in 1:332) {
    read <- (read.csv(files_list[i]))
    
    # Select for rows of complete cases
    comDf <- read[complete.cases(read), ]
    
    # Set conditions for threshold to calculate correlation between sulfate and nitrate
    if(nrow(comDf) > threshold) {
      result <- c(result, cor(comDf$sulfate, comDf$nitrate))
    } else 0
  }
  # Set return value
  return(result)
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)


# Selecting a rounding sample from the data (quiz questions testing function)
cr <- corr("specdata")
cr <- sort(cr)
RNGversion("3.5.1")
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)
cr <- sort(cr)
n <- length(cr)
RNGversion("3.5.1")
set.seed(197)
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)
n <- length(cr)
cr <- corr("specdata", 1000)
cr <- sort(cr)
print(c(n,round(cr, 4)))
