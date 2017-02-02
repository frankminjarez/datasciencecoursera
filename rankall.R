rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        outcomeIndex <- match(outcome, validOutcomes)
        if (is.na(outcomeIndex)) stop("invalid outcome")
        
        ## Get the column name (from documentation)
        conditions <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        condition <- conditions[outcomeIndex]
        
        ## Find all the states in the data set
        validStates <- unique(data["State"])
        validStates <- validStates[order(validStates$State),]
        
        ## Convert column to numeric and drop NAs
        suppressWarnings(data[, condition] <- as.numeric(data[, condition]))
        data <- data[complete.cases(data[condition]),]
        data <- data[order(data[condition]),]
        
        ## Find the desired rank      
        if (num == "best") {
                rank <- 1
        } else if (num != "worst") {
                rank <- as.numeric(num)
        }
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospitalName <- character()
        for (state in validStates) {
                ## Break out the state data
                stateData <- data[data$State == state,]
                
                ## Find the desired rank for worst case    
                if (num == "worst") {
                        rank <- nrow(stateData)
                }
                
                ## Rank too big or small
                if (rank < 1 || rank > nrow(stateData)) {
                        hospitalName <- c(hospitalName, NA)
                        next
                }
                
                ## Check for multiple hospitals with the same rating. If found then 
                ## order by rank and name to pick the right one.
                value <- stateData[rank, condition]
                hospitalNames <- stateData$Hospital.Name[stateData[condition] == value]
                if (length(hospitalNames) > 1) {
                        stateData <- stateData[order(stateData[condition], stateData$Hospital.Name),]
                }
                
                ## Return hospital name in that state with the given rank
                ## 30-day death rate
                hospitalName <- c(hospitalName, stateData$Hospital.Name[rank])
        }
        df <- data.frame(hospital=hospitalName, state=validStates, row.names=validStates)
        df
}