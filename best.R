# Returns a character vector with the name of the hospital
# that has the best (i.e. lowest) 30-day mortality for the
# specified outcome in that state.

best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",
               valid_outcomes[outcome], 
               sep="")
  data_state <- data_all[which(data_all$"State"==state),]
  observations <- suppressWarnings(as.numeric(data_state[,col]))
  smallest <- min(observations,na.rm=TRUE)
  hospitals <- data_state$"Hospital.Name"[which(observations==smallest)]
  sort(hospitals,decreasing=FALSE)[1]
}
