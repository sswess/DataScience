rankhospital <- function(state, outcome, num = "best"){
  library(dplyr)
  data1 <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  data <- tbl_df(data1)
  finalset <- select(data, 2,7,11,17,23)
  colnames(finalset)[1] <- "Name"
  colnames(finalset)[3] <- "heart.attack"
  colnames(finalset)[4] <- "heart.failure"
  colnames(finalset)[5] <- "pneumonia"
  
  states <- unique(data1[,7])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if ((state %in% states) == FALSE) {
    stop(print("invalid state"))
  }
  else if ((outcome %in% outcomes) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  
  finalset <- filter(finalset, State == state)
  if(outcome =="heart attack"){
    finalset[,3] <- lapply(finalset[,3], as.numeric)
    finalset <- finalset[complete.cases(finalset[,3]),]
    finalset <- arrange(finalset,heart.attack,Name)
  }
  if(outcome =="heart failure"){
    finalset[,4] <- lapply(finalset[,4], as.numeric)
    finalset <- finalset[complete.cases(finalset[,4]),]
    finalset <- arrange(finalset,heart.failure,Name)
  }
  if(outcome =="pneumonia"){
    finalset[,5] <- lapply(finalset[,5], as.numeric)
    finalset <- finalset[complete.cases(finalset[,5]),]
    finalset <- arrange(finalset,pneumonia,Name)
  }
  if(num == "worst"){num = dim(finalset)[1]}
  if(num == "best"){num = 1}
  finalset$Name[num]
}