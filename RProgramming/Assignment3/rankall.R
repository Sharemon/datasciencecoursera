# rankall
## This function ranks all hospitals by outcome and num and show the result.
## The outcome can be one of "heart attack", "heart failure" and "pneumonia". 
## The num variable can take values "best", "worst", or an integer.

rankall <- function(outcome, num = "best")
{
    # Read dataset
    hospital_data_all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check if outcome are valid
    if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    
    # Get all state
    states <- unique(hospital_data_all$State)

    # Return hospital name in that state with lowest 30-day death rate for each state
    hospital_name <- NULL
    for (state in states)
    {
        ## Get data by given state
        hospital_data <- split(hospital_data_all, hospital_data_all$State)[state][[1]]
        
        ## Get column of mortality outcome of different type
        numCol <- if(outcome == "heart attack"){11}else if(outcome == "heart failure"){17}else{23}
        
        ## Re-order the dataset with mortality outcome
        hospital_data <- hospital_data[order(as.numeric(hospital_data[,numCol]), hospital_data[,2]),]
        
        ## Get rid of all NA with specific outcome
        hospital_data <- hospital_data[!is.na(as.numeric(hospital_data[,numCol])),]
        
        ## Get wanted row number
        numRow <- if(num == "best"){1}else if(num == "worst"){nrow(hospital_data)}else{num}
        
        ## If num is too large, return NA
        if (numRow>nrow(hospital_data)) 
        {
            hospital_name <- c(hospital_name, NA)
        }
        else
        {
            ## List the wanted hospital name
            hospital_name <- c(hospital_name, hospital_data[numRow,2])
        }
    }
    
    ## Show Result
    result <- data.frame(hospital = hospital_name, state=states)
    result[order(result[,2]),]
}

