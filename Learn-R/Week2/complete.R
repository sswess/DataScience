complete <- function(directory, id = 1:332) {
  #open files
  files <- list.files(directory, full.names = TRUE) 
  #setup data
  dat <- data.frame()
  
  for (i in id) {
    file1 = read.csv(files[i])
    nobs <- sum(complete.cases(file1))
    x <- c(i,nobs)
    dat <- rbind(dat, x)
  }
  names(dat) <- c("id","nobs")
  dat
}