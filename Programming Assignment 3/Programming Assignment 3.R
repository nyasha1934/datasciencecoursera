## Programming Assignment 3

# Set your working directory to the desired folder
setwd("/Users/MB/Documents/Coursera/Data Science Specialization/datasciencecoursera/Programming Assignment 3")

  # confirm set working directory
getwd()

# Getting the data

# download and unzip data file as instucted
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
download.file(dataset_url, "data.zip")
unzip("data.zip", exdir = "qualitydata")


# list the files 
list.files("qualitydata")

# the files are located in a subfolder in the main 'programming assignment' folder but we want R to read it directly
files_full <- list.files("qualitydata", full.names = TRUE)
files_full                  



## 1. Plot the 30-day mortality rates for heart attack

  # explore the outcome file
outcome <- read.csv("qualitydata/outcome-of-care-measures.csv", colClasses = "character")
outcome

  # select the first 6 rows of data
head(outcome)

  # names of the columns
names(outcome)

  # dimensions of data frame 
dim(outcome)
# [1] 4706   46

  # structure of the dataset
str(outcome)


  # make a histogram of the 30-day death rates from heart attack (column)
outcome[, 11] <- as.numeric(outcome[, 11])  # we read the data as character but we need to coerce the column to be numeric
outcome[, 11]
hist(outcome[, 11], xlab = "Days", ylab = "Mortality Rates", main = "30-Day Death Rates from Heart Attack")


## 2. Finding the best hospital in a state

# Write a function called "best" that takes two arguments: the 2-character abbreviated
  # name of a state and an outcome name
  # The function reads "outcome-of-care-measures.csv" file and returns a character
    # vector with the name of the hospital with the best (lowest) 30-day mortality
    # for the specified outcome in that state
  # Hospital name is provided in the "Hospital.Name" variable
  # The outcomes can be "heart attack", "heart failure", or "pneumonia"
  # Hospitals that do not have data on a particular outcome should be excluded 
    # from the set of hospitals when deciding rankings
  # If there is a tie for "best" for a given outcome, the hospital names should 
    # be sorted in alphabetical order and their first hospital in that set should be chosen


best <- function(state, outcome){
    # set file to be read
    read <- read.csv("qualitydata/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
    
    # create the data frame for the outcomes observed
    dat2 <- as.data.frame(cbind(read[, 2],    # hospital
                                read[, 7],    # state
                                read[, 11],   # heart attack
                                read[, 17],   # heart failure
                                read[, 23]),  # pneumonia
                        stringsAsFactors = FALSE)
    
    # set column names for the new data set
    colnames(dat2) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
      # check the state and outcome are valid
    if(!state %in% dat2[, "state"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        si <- which(dat2[, "state"] == state)
        ts <- dat2[si, ]                      # get data from called state
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)      # set min of all values present in the argument
        result <- ts[, "hospital"][which(oi == min_val)]
        output <- result[order(result)]       # if tie, place in alphabetical order
    }
    return(output)
}
  ## Testing the function: 
best("TX", "heart attack")

best("TX", "heart failure")

best("AZ", "heart attack")



## 3. Ranking hospitals by outcome in a state

# Write a function called "rankhospital" that takes 3 arguments: 
  # the 2-character abbreviated name of a state (state)
  # an outcome (outcome)
  # ranking of a hospital in that state for that oucome (num)

# The function reads "outcome-of-care-measures.csv" file and returns a character
  # vector with the name of the hospital that has the ranking specified by the 
  # 'num' argument

# If the 'num' given in larger than the number of hospitals in the called state, 
  # the function returns NA

# Hospitals that do not have data on a particular outcome should be excluded from
  # the set of hospitals when deciding rankings

# If hospitals tie in ranking, break the tie using order() to return results alphabetically

rankhospital <- function(state, outcome, rank = "best") {
    # set file to be read
    read <- read.csv("qualitydata/outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
    # create the data frame for the outcomes observed
    dat2 <- as.data.frame(cbind(read[, 2],    # hospital
                              read[, 7],    # state
                              read[, 11],   # heart attack
                              read[, 17],   # heart failure
                              read[, 23]),  # pneumonia
                        stringsAsFactors = FALSE)
  
    # set column names for the new data set
    colnames(dat2) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    # check that state and outcome are valid
    if (!state %in% dat2[, "state"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('invalid outcome')
    } else if (is.numeric(rank)) {
        si <- which(dat2[, "state"] == state)
        ts <- dat2[si, ]                          # get data from called state
        ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
        ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
        output <- ts[, "hospital"][rank]
    } else if (!is.numeric(rank)) {
      if (rank == "best") {
          output <- best(state, outcome)
      } else if (rank == "worst") {
        si <- which(dat2[, "state"] == state)
        ts <- dat2[si, ]
        ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
        ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
        output <- ts[, "hospital"][1]
      } else {
        stop('invalid rank')
      }
    }
    return(output)
}

## Testing the function
rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")


## 4. Ranking hospitals in all states

# Write a function that takes two arguments: outcome name (outcome) and hospital rank (num)

# The function reads the "outcome-of-care-measures.csv" file and returns a 2-column
  # data frame containing the hospital of each state that has the ranking specified in 'num'

# Hospitals missing data on a particular outcome must be excluded the data set when 
  # deciding rankings
# Handle ties the same way as "rankhospital"

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    read <- read.csv("qualitydata/outcome-of-care-measures.csv", colClasses = "character")
    dat2   <- as.data.frame(cbind(read[, 2],   # hospital
                                  read[, 7],   # state
                                  read[, 11],  # heart attack
                                  read[, 17],  # heart failure
                                  read[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(dat2) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    dat2[, eval(outcome)] <- as.numeric(dat2[, eval(outcome)])
    
    ## Check that state and outcome are valid
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
      } else if (is.numeric(num)) {
        by_state <- with(dat2, split(dat2, state))
        ordered  <- list()
        for (i in seq_along(by_state)){
          by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
          ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
      } else if (!is.numeric(num)) {
        if (num == "best") {
          by_state <- with(dat2, split(dat2, state))
          ordered  <- list()
          for (i in seq_along(by_state)){
            by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                               by_state[[i]][, "hospital"]), ]
            ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
          }
          result <- do.call(rbind, ordered)
          output <- as.data.frame(result, stringsAsFactors = FALSE)
          rownames(output) <- output[, 2]
        } else if (num == "worst") {
          by_state <- with(dat2, split(dat2, state))
          ordered  <- list()
          for (i in seq_along(by_state)){
            by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                               by_state[[i]][, "hospital"], 
                                               decreasing = TRUE), ]
            ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
          }
          result <- do.call(rbind, ordered)
          output <- as.data.frame(result, stringsAsFactors = FALSE)
          rownames(output) <- output[, 2]
      } else {
        stop('invalid num')
      }
    }
    return(output)
  }

## Testing the function:

head(rankall("heart attack", 20), 10)












