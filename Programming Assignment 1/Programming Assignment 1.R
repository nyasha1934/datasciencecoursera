# PROGRAMMING ASSIGNMENT PART 1

# Setting your working directory:
getwd()
setwd("/Users/MB/Documents/Coursera/Data Science Specialization/datasciencecoursera/Programming Assignment 1")


# Getting the data

# download and unzip data file as instucted
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip", exdir = "specdata")

# list the files in "specdata"
list.files("specdata")

# the files are located in a subfolder in the main 'programming assignment' folder but we want R to read it directly
files_full <- list.files("specdata", full.names = TRUE)
files_full                  ## remove the addition empty "specdata" folder; there are 332 monitor files

# now we can try to look at a sample file i.e. "020.csv" again
head(read.csv(files_full[20]))

# First, let's play around with our data for a bit, to get a feel for it!
    # take a look at monitor 1 "specdata/001.csv"
    # set a variable for the first file called 'mon1"

mon1 <- read.csv(files_full[1])         ## use "files_full" to by pass subfolder in directory
mon1
# [1] NA

mon1 <- read.csv(("specdata/001.csv"))  ## or call file name directly
mon1
# [1] NA

dim(mon1)         ## shows the dimension of 'mon1': 1461 rows and 4 columns
names(mon1)       ## shows the column names of the file
# [1] "Date"    "sulfate" "nitrate" "ID"


# Next, let's practice subsetting the data
  # show the first row of the "sulfate" column
mon1[1, "sulfate"]      ## all the pollutant values that we see are NA
# [1] NA
mon1[1400, "sulfate"]
# [1] NA

  # check the mean "sulfate" and "nitrate" values, remove missing values (NAs)
mean(mon1$sulfate, na.rm = TRUE)
# [1] 3.880701          ## so there are some values in the data frame

mean(mon1$nitrate, na.rm = TRUE)
# [1] 0.5499098

# Let's make a larger dataset with monitors "001" and "002"
mon1_mon2 <- rbind(mon1, read.csv("specdata/002.csv"))
  #check the rbind by calling heads and tails for "mon1_mon2"
head(mon1_mon2)             ## shows the top of the data frame "001.csv"
tail(mon1_mon2)             ## shows the bottom of the data frame "002.csv"

names(mon1_mon2)
mon1_mon2

   
  # let's check out 'mon2' first
mon2 <- read.csv(("specdata/002.csv"))  ## or call file name directly
mon2
dim(mon2)
# [1] 3652    4
dim(mon1)                               ## each file has a different number of rows i.e.dates monitored
# [1] 1461    4

# Create Combined Data Set of All Files

  # start by creating an empty data frame 'dat' 
dat <- data.frame()
  # create a loop to rbind all the list files in 'dat'
for (i in 1:332) {
      dat <- rbind(dat, read.csv(files_full[i]))
}
str(dat)
summary(dat)

  # calculate the median 'sulfate' and 'nitrate' pollutant values, exclude missing values
median(dat$sulfate, na.rm = TRUE)
# [1] 2.37
median(dat$nitrate, na.rm = TRUE)
# [1] 0.821

  #calculate the median 'sulfate' values at the beginning 2003-01-01
dat_start <- dat[which(dat[, "Date"] == "2003-01-01"),]   ## do not forget the "" around dates
dat_start
median(dat_start$sulfate, na.rm = TRUE)
# [1] 1.83
median(dat_start$nitrate, na.rm = TRUE)
# [1] 1.805
median(dat_start$sulfate, dat_start$nitrate, na.rm = TRUE)




# PART 1 

# Write a function names "pollutantmean" that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors (csv.files)
  # The function takes three arguments: "directory", "pollutant", and "id"
  # Goal: given a vector monitor ID numbers, "pollutantmean" reads the monitors' particulate matter data from the specified directory and returns the mean of the pollutant accross all monitors
  # must ignore any missing "NA" values

# A generic basic constuct of our function
pollutantmean <- function(directory, pollutant, id) {#content of the funtion}
  
  # What goes in the "content of the function?"
  # 1. we need a data frame that combines all the CSVs
  # 2. we'll subset the data using argument 'pollutant' and 'id'
  # 3. take the pollutant mean of that subset. 
  
  # to get all the data in single data frame, use list.files() and rbind() function


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


# Another Way To Do It (no need to "rbind", just read all the files in the list)

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



# PART 2

# Write a function that reads a directory full of files
  # and reports the number of completely observed cases in each data file
  # The function should return a data frame where the first column is the name of the file
    # and the second column is the number of complete cases

# Let's looks at a single data file
mon2 <- read.csv(files_list[2])
mon2

  # show only complete cases in the "002.csv" file
mon2com <- mon2[complete.cases(mon2), ]
mon2com
  # count the number of complete.cases
nrow(mon2com)


## ROUGH DRADT, PLAYING AROUND WITH SOME FUNCTIONS
# Create a single data frame with all the files listed
dat <- data.frame()
  # rbind all the files
  for(i in 1:332) {
      dat <- rbind(dat, read.csv(files_list[i]))
  }
  # select only the complete.cases and store in "comDf"
comDf <- dat[complete.cases(dat), ]


## AN ADVANCED SOLUTION

complete <- function(directory, id = 1:332) {
    x = list.files("specdata")
    y = x[match(id, as.numeric(sub(".csv", "", x)))]
    z = file.path("specdata", y)
    a = function(z) sum(complete.cases(read.csv(z)))
    data.frame(id =id, nobs = unlist(lapply(z,a)))
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

## BEGINNERS (more understandable)

# Assign the directory
complete <- function(directory, id = 1:332) {
  files_list <- list.files("specdata", full.names = TRUE)
  
  # Create an empty vector to populate with number of observations
  nobs <- data.frame()
  
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



# ANOTHER SIMPLIFIED WAY TO DO IT

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



# PART 3

# Write a function that takes a directory of data files and a threshold for
  # complete cases and calculates the correlation between sulfate and nitrate
  # for monitor locations where the number of completely observed cases
  # (on all variables) is greater than the threshold.

# Return a vector of correlations for the monitors that meet the threshold
# If no monitors meet the threshold, the function should return a numeric vector length of 0

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


  