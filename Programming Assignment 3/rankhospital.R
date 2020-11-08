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
                        stringAsFactors = FALSE)
  
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