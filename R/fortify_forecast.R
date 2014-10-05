fortify.forecast <- function(forecast.data) {
  require(dplyr)
  forecasted <- as.data.frame(forecast.data)
  forecasted$Time <- as.Date(time(forecast.data$mean))
  fitted <- data.frame(Time = as.Date(time(forecast.data$fitted)),
                       Original = forecast.data$x,
                       Fitted = forecast.data$fitted)
  
  rownames(fitted) <- NULL
  rownames(forecasted) <- NULL
  
  dplyr::rbind_list(fitted, forecasted)
}