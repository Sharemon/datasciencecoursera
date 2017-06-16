# best 
## This function is uesd to rate the best hospital with given state
## and outcome type and output the name of best hospital.The outcome can
## be one of "heart attack", "heart failure" and "pneumonia". The state
## and outcome will be checked if invalid.

best <- function(state, outcome)
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
    
    ## Show the best hospital name
    hospital_data[1,2]
}