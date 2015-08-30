best <- function(state, outcome) {
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
  # Filter by state
  aux <- split(selected, selected$State)
  filtered <- aux[[state]]
  # make numeric field
  filtered[,3] <- suppressWarnings(as.numeric(filtered[,3]))
  # select the row(s) with miniimum rate
  selected <- filtered[which(filtered[,3]==min(filtered[,3], na.rm=TRUE)),]
  
  result <- selected$Hospital.Name
  # Return only the first hospital in alphabetical order in case the best
  # ones tied
  sort(result)[1]
}




