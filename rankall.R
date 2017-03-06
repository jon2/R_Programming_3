## rankall.R
## Jon Green - 03/05/2017
##
## The function reads the outcome-of-care-measures.csv file and returns a
## 2-column data frame containing the hospital in each state that has the
## ranking specified in 'num'

rankall <- function(outcome, num = "best") { 
        ## Read outcome data
        outcomesDF <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        
        ## Check that state and outcome are valid
        if (!is.element(state, outcomesDF[,7])) stop("invalid state")
        if (outcome == "heart attack") column <- 11
        else if (outcome == "heart failure") column <- 17
        else if (outcome == "pneumonia") column <- 23
        else stop("invalid outcome")
        
        ## Extract hospital name, state, mortality rate into new DF        
        tableDF <- na.omit(outcomesDF[c(7, 2,column)])
        names(tableDF) <- c("state", "hospital", "rate")

        ## Sort and split
        sortedDF <- arrange(tableDF, state, rate, hospital)
        states <- sortedDF$state
        sortedList <- split(sortedDF, states)
        
        outList <- lapply(sortedList, function(x) {
                if(num == "best") x[1,2]
                else if (num == "worst") x[nrow(x),2]
                else x[as.numeric(num),2]
        })
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the ## (abbreviated) state name
        data.frame(hospital=unlist(outList), state=names(outList))
}
