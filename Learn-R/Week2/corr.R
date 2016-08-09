corr <- function(directory, threshold = 0) {
  all_full <- list.files(directory, full.names = TRUE)
  dat = NULL
  for (i in 1:322) {
    data <- read.csv(all_full[i])
    data <- na.omit(data)
    if (nrow(data) > threshold) {
      dat = c(dat, cor(data[,2], data[,3]))
    }
  }
  dat
}