fortify.stl <- function(stl.data) {
  d <- data.frame(stl.data$time.series)
  d$time <- as.Date(time(stl.data$time.series))
  d
}