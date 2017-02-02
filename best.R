best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Check that state and outcome are valid
        validStates <- unique(data[,"State"])
        if (!any(state == validStates)) stop("invalid state")
        
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
        
        ## Find the minimum value. If tie values then sort by hospital 
        ## name
        data <- data$Hospital.Name[data[condition] == min(data[condition])]
        if (length(data) > 1) {
                data <- data[order(data)]
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        data
}