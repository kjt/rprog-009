rankhospital <- function(state, outcome, num = "best") {
  # read outcome data
  data_all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # check valid state
  if (!(state %in% data$State)) {
    stop ("invalid state")
  }
  
  # check valid outcome
  valid_outcomes <- list("heart attack" = "Heart.Attack", 
                         "heart failure" = "Heart.Failure",
                         "pneumonia" = "Pneumonia")
  if (!(outcome %in% names(valid_outcomes))) {
    stop ("invalid outcome")    
  }

  # check valid number
  if (!is.numeric(num) & !(num == "best") & !(num == "worst")) {
    stop ("invalid number")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",
               valid_outcomes[outcome], 
               sep="")
  data_state <- data_all[which(data_all$"State"==state),]
  hospitals <- data_state[,"Hospital.Name"]
  mortality <- suppressWarnings(as.numeric(data_state[,col]))
  data_state_sorted <- data_state[order(mortality,hospitals,na.last=NA),]
  index = 0

  if (class(num) == "numeric")
  {
    if (num > nrow(data_state_sorted)) {
      return (NA)
    }
    else {
      index = num
    }
  }
  else
  if (num == "best") {
    index = 1
  }
  else
  if (num == "worst") {
    index = nrow(data_state_sorted)
  }

  data_state_sorted$"Hospital.Name"[index]
}
