# rankhospital
## This function is uesd to rate the best hospital with given state
## and outcome type and output the name of wanted ranked hospital.The outcome
## can be one of "heart attack", "heart failure" and "pneumonia". The num
## variable can take values “best”, “worst”, or an integer indicating the ranking
## The state and outcome will be checked if invalid.

rankhospital <- function(state, outcome, num = "best")
{
    # Read dataset
    hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check if state and outcome are valid
    if (!(state %in% unlist(hospital_data["State"]))) stop("invalid state")
    if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")

    # Return hospital name in that state with lowest 30-day death rate
    ## Get data by given state
    hospital_data <- split(hospital_data, hospital_data$State)[state][[1]]
    
    ## Get column of mortality outcome of different type
    numCol <- if(outcome == "heart attack"){11}else if(outcome == "heart failure"){17}else{23}
    
    ## Re-order the dataset with mortality outcome
    hospital_data <- hospital_data[order(as.numeric(hospital_data[,numCol]), hospital_data[,2]),]
    
    ## Get rid of all NA with specific outcome
    hospital_data <- hospital_data[!is.na(as.numeric(hospital_data[,numCol])),]
    
    ## Get wanted row number
    numRow <- if(num == "best"){1}else if(num == "worst"){nrow(hospital_data)}else{num}
    
    ## If num is too large, return NA
    if (numRow>nrow(hospital_data)) return(NA)
    
    ## Show the wanted hospital name
    hospital_data[numRow,2]
}