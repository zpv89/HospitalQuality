rankhospital <- function(state,outcome,num="best"){

## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Rename & reclass some columns for ease of coding later
  colnames(data)[11] <- "heart attack"
  colnames(data)[17] <- "heart failure"
  colnames(data)[23] <- "pneumonia"
  data[, 11] <- as.numeric(data[, 11])
  data[, 17]<-as.numeric(data[, 17])
  data[, 23]<-as.numeric(data[, 23])
  
## Check that state and outcome are valid
  validstates <- as.list(unique(data[,7]))
  if (!state %in% validstates){
    stop ("invalid state")
  }
  
  validoutcomes <- list("heart attack","heart failure","pneumonia")
  if (!outcome %in% validoutcomes){
    stop ("invalid outcome")
  }
  
## Return hospital name in that state with the given rank 30-day death rate
  ## Manipulate data
  df=data.frame(data$Hospital.Name,  data$State, data[[outcome]]) 
  colnames(df)[1]<-"Hospital.Name"
  colnames(df)[2]<- "State"
  colnames(df)[3]<- "Rate"
  df[, 3]<-as.numeric(df[, 3])
  ## Clean the data of NaNs
  df_clean <- df[ ! is.na( df[, 3] ) , ]
  
  ## Get the required state data
  get_state_data=subset(df_clean, State==state)
  
  ##ranking <- rank(get_state_data[,3],na.last = TRUE,ties.method = "min")
  ranking <- c(1:nrow(get_state_data))
  ranked <- order(get_state_data[,3],get_state_data$Hospital.Name)
  
  final <- data.frame(get_state_data[ranked,1],get_state_data[ranked,3],ranking)
  colnames(final)[1]<-"Hospital.Name"
  colnames(final)[2]<- "Rate"
  colnames(final)[3]<- "Ranking"
  final[, 2]<-as.numeric(final[, 2])
  final[,3]<-as.integer(final[,3])
  
  if (class(num)=="character"){
    if (num == "best"){
      hospital <- which.min(ranking)
      best <- final[hospital,1]
      print(as.character(best))
    } else {
      hospital <- which.max(ranking)
      worst <- final[hospital,1]
      print(as.character(worst))
  }} else {
      hospital <- match(num,ranking)
      output <- final[hospital,1]
      print(as.character(output))
    }
  
  
}