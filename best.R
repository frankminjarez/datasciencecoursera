best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Check that state and outcome are valid
        validStates <- unique(data["State"])
        if (!any(state == validStates)) {
                stop("invalid state")
        }
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!any(outcome == validOutcomes)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
       
        ## Reduce data set to one state
        data <- data[data$State == state,]
        
        ## Get the column name (from documentation)
        if (outcome == "heart attack") {
                condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else {
                condition <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        ## Convert column to numeric and drop NAs
        suppressWarnings(data[, condition] <- as.numeric(data[, condition]))
        data <- data[complete.cases(data[condition]),]
        
        ## Find the minimum value. If tie values then sort by hospital 
        ## name
        data <- data$Hospital.Name[data[condition] == min(data[condition])]
        if (length(data) > 1) {
                data <- data[order(data)]
        }
        data
}