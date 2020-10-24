complete <- function(directory, id = 1:332) {
  
  # Assign a directory
  files_list <- list.files("specdata", full.names = TRUE)
  
  # Create an empty vector to store the number of observations
  nobs <- numeric()
  
  # For loop to read all the files and store sum of complete cases for each
  # id in the empty vector
  for(i in id) {
    read <- read.csv(files_list[i])
    nobs <- c(nobs, sum(complete.cases(read)))
  }
  
  # Return value of a data frame with two columns labeled "id" and "nobs"
  data.frame(id, nobs)
} 

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3) 


# Using a "Rounding" sample to observe complete cases
RNGversion("3.5.1")
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])




