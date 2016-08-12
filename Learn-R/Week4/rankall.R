rankall <- function(outcome, num = "best") {
  library(dplyr)
  data1 <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  data <- tbl_df(data1)
  finalset <- select(data, 2,7,11,17,23)
  colnames(finalset)[1] <- "Name"
  colnames(finalset)[3] <- "heart.attack"
  colnames(finalset)[4] <- "heart.failure"
  colnames(finalset)[5] <- "pneumonia"
  if(num == "best"){num = 1}
  
  states <- unique(data1[,7])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if ((outcome %in% outcomes) == FALSE) {
    stop(print("invalid outcome"))
  }
  finalset[,3] <- lapply(finalset[,3], as.numeric)
  finalset[,4] <- lapply(finalset[,4], as.numeric)
  finalset[,5] <- lapply(finalset[,5], as.numeric)
  
  if(outcome =="heart attack"){
    
    finalset <- finalset[complete.cases(finalset[,3]),]
    finalset <- arrange(finalset,heart.attack,State,Name)
  }
  if(outcome =="heart failure"){
    
    finalset <- finalset[complete.cases(finalset[,4]),]
    finalset <- arrange(finalset,heart.failure,State,Name)
  }
  if(outcome =="pneumonia"){
   
    finalset <- finalset[complete.cases(finalset[,5]),]
    finalset <- arrange(finalset,pneumonia,State,Name)
  }
  output = NULL
  for(i in 1:length(states)){
    tmp <- filter(finalset, State == states[i])
    if(num == "worst"){
      temp = dim(tmp)[1]
      output <- rbind(output,c(tmp$Name[temp],states[i]))
      next
    }
    output <- rbind(output,c(tmp$Name[num],states[i]))
  }
  colnames(output) = c("Name","State")
  output <- tbl_df(output)
  output <- arrange(output,State)
  #output <- output[complete.cases(output[,2]),]
  
  #View(output)
  output
  
}