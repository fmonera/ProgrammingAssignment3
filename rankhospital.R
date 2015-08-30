rankhospital <- function( state, outcome, num = "best") {
  ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(dt[,7])
  if( !(state %in% states) ) {
    stop("invalid state")
  }
  
  if(outcome == "heart attack") cl = 11
  else if (outcome == "heart failure") cl = 17
  else if (outcome == "pneumonia") cl = 23
  else stop("invalid outcome")
  
  
  # Get only the needed columns
  selected <- dt[,c(2,7,cl)]
  cnm <- c("Hospital.Name", "State", "Outcome")
  colnames(selected) <- cnm
  # Filter by state
  aux <- split(selected, selected$State)
  filtered <- aux[[state]]
  # make numeric field
  filtered[,3] <- suppressWarnings(as.numeric(filtered[,3]))
  
  # remove na
  nona <- na.omit(filtered)
  
  sorted <- nona[order(nona$Hospital.Name), , drop=FALSE]
  sorted <- sorted[order(sorted$Outcome), , drop=FALSE]
  #sorted
  
  # return required data
  if(num == "best") {
    sorted[1,1]
  } else if(num == "worst") {
    sorted[nrow(sorted), 1]
  } else {
    sorted[num, 1]
  }
}
