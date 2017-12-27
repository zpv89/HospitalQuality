best <- function(state,outcome){
  
## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Rename some columns for ease of coding later
  colnames(data)[11] <- "heart attack"
  colnames(data)[17] <- "heart failure"
  colnames(data)[23] <- "pneumonia"
  
## Check that state and outcome are valid
  validstates <- as.list(unique(data[,7]))
  if (!state %in% validstates){
    stop ("invalid state")
  }
  
  validoutcomes <- list("heart attack","heart failure","pneumonia")
  if (!outcome %in% validoutcomes){
    stop ("invalid outcome")
  }
  
## Return hospital name in that state with the lowest 30-day death rate
  
  ## extract smaller subset of data for calculations
  df <- data.frame(data$Hospital.Name,data$State,data$"heart attack",data$"heart failure",data$pneumonia)
  ## rename columns again for ease later
  colnames(df) <- c("Hospital.Name","State","heart attack","heart failure","pneumonia")
  
  ## Extract state relevant data
  get_state_data <- subset(df,State==state)
  
  ## Rank hospitals by the right outcome death rate
  ranking <- rank(as.numeric(as.character(get_state_data[[outcome]])),na.last = TRUE)
  lowest <- which.min(ranking)
  hname <- as.character(get_state_data[lowest,1])
  
  return(hname)
  
}