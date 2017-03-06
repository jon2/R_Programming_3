## rankhospital.R
## Jon Green - 03/05/2017
##
## The function reads the outcome-of-care-measures.csv file and returns a
## character vector with the name of the hospital that has the ranking
## specified by the 'num' argument

library(plyr)

rankhospital <- function(state, outcome, num = "best") { 
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
        names(tableDF) <- c("hospital", "rate")
        
        ## Sort
        sortedDF <- na.omit(arrange(tableDF, rate, hospital))
        
        ## Return hospital name in that state with the given rank 
        ## 30-day death rate
        if(num == "best") sortedDF[1,1]
        else if (num == "worst") sortedDF[nrow(sortedDF),1]
        else sortedDF[as.numeric(num),1]
}
