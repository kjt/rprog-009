rankall <- function(outcome, num = "best") {
  # read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  rank_hospital <- function (data, outcome) {
    col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",
                 valid_outcomes[outcome], 
                 sep="")
    hospitals <- data[,"Hospital.Name"]
    mortality <- suppressWarnings(as.numeric(data[,col]))
    data_sorted <- data[order(mortality,hospitals,na.last=NA),]
    index = 0
    
    if (class(num) == "numeric")
    {
      if (num < 1 | num > nrow(data_sorted)) {
        return (c("hospital"=NA,"state"=data$State[1]))
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
        index = nrow(data_sorted)
      }
  
    c("hospital"=data_sorted$"Hospital.Name"[index],"state"=data$State[1])
  }

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data_by_state <- split(data, factor(data$"State"))
  results <- sapply(data_by_state, rank_hospital, outcome)
  data.frame(t(results))
}
