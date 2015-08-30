rankall <- function( outcome, num = "best") {
  ## Read outcome data
  dt <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(dt[,7])
  
  if(outcome == "heart attack") cl = 11
  else if (outcome == "heart failure") cl = 17
  else if (outcome == "pneumonia") cl = 23
  else stop("invalid outcome")
  
  # Get only the needed columns
  selc <- dt[,c(2,7,cl)]
  cnm <- c("hospital", "state", "outcome")
  colnames(selc) <- cnm
  
  # make numeric field
  selc[,3] <- suppressWarnings(as.numeric(selc[,3]))
  
  # sort
  sorted <- selc[order(selc$hospital), , drop=FALSE]
  sorted <- sorted[order(sorted$outcome), , drop=FALSE]

  # remove na
  nona <- na.omit(sorted)
  
  # Filter by state
  listbystates <- split(nona, nona$state)
  result = c()
  
  for(currentstate in states) {
    stdf <- listbystates[[currentstate]]
    numhospitals <- nrow(stdf)
    if(num == "best") {
      rsel <- 1
    } else if(num == "worst") {
      rsel <- numhospitals
    } else {
      rsel <- num
    }
    
    if (rsel > numhospitals) {
      res <- NA
    } else {
      res <- stdf[rsel, 1]
    }
    result <- rbind(result, c(res, currentstate))
  }
  result <- result[order(result[, 2]), ]
  rownames(result) <- result[,2]
  colnames(result) <- c("hospital", "state")
  data.frame(result)
}
