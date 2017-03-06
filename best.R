## best.R
## Jon Green - 03/05/2017
##
## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state.

best <- function(state, outcome) { 
        ## Read outcome data
        outcomesDF <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        
        ## Check that state and outcome are valid
        if (!is.element(state, outcomesDF[,7])) stop("invalid state")
        if (outcome == "heart attack") column <- 11
        else if (outcome == "heart failure") column <- 17
        else if (outcome == "pneumonia") column <- 23
        else stop("invalid outcome")

        ## Extract hospital name, state, mortality rate into new DF        
        tableDF <- outcomesDF[outcomesDF$State == state,c(2,column)]
        
        ## Sort by name so that which.min() returns the first one in case of
        ## a tie
        tableDF <- tableDF[order(tableDF$Hospital.Name),]
        
        ## Return hospital name in that state with lowest 30-day death rate
        tableDF[which.min(tableDF[,2]),1]
        
}