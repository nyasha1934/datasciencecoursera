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
                        stringAsFactors = FALSE)
  
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
