rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        ## Check that state and outcome are valid
        validStates <- unique(data[,"State"])
        if (!is.element(state, validStates)) stop("invalid state")
       
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomeIndex <- match(outcome, validOutcomes)
        if (is.na(outcomeIndex)) stop("invalid outcome")
        
        ## Get the column name (from documentation)
        conditions <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        condition <- conditions[outcomeIndex]
        
        ## Reduce data set to one state
        data <- data[data$State == state,]
        
        ## Convert column to numeric and drop NAs
        suppressWarnings(data[, condition] <- as.numeric(data[, condition]))
        data <- data[complete.cases(data[condition]),]
        data <- data[order(data[condition]),]
        
        ## Find the desired rank      
        if (num == "best") {
                rank <- 1
        } else if (num == "worst") {
                rank <- nrow(data)
        } else {
                rank <- as.numeric(num)
                if (rank < 1 || rank > nrow(data)) {
                        return(NA)
                }
        }
  
        ## Check for multiple hospitals with the same rating. If found then 
        ## order by rank and name to pick the right one.
        value <- data[rank, condition]
        hospitalNames <- data$Hospital.Name[data[condition] == value]
        if (length(hospitalNames) > 1) {
                data <- data[order(data[condition], data$Hospital.Name),]
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        data$Hospital.Name[rank]
}