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