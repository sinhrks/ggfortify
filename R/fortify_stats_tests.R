#' Convert \code{stats::htest} to data.frame.
#' 
#' @param data \code{stats::htest} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(t.test(rnorm(10)))
#' ggplot2::fortify(t.test(mpg ~ am, data = mtcars))
#' ggplot2::fortify(wilcox.test(mpg ~ am, data = mtcars))
#' ggplot2::fortify(cor.test(mtcars$wt, mtcars$mpg))
#' @export
fortify.htest <- function(data) {
  dplyr::tbl_df(broom::tidy(data))
}


#' Autoplot \code{stats::htest}.
#' 
#' @param data \code{stats::htest} instance
#' @return ggplot
#' @examples
#' ggplot2::autoplot(t.test(rnorm(10)))
#' ggplot2::autoplot(t.test(mpg ~ am, data = mtcars))
#' ggplot2::autoplot(wilcox.test(mpg ~ am, data = mtcars, conf.int = TRUE))
#' ggplot2::autoplot(cor.test(mtcars$wt, mtcars$mpg))
#' @export
autoplot.htest <- function(data) {
  plot.data <- ggplot2::fortify(data)
  plot.data$x <- 1:nrow(plot.data)
  
  if ('estimate' %in% colnames(plot.data)) {
    # with confint
    mapping <- ggplot2::aes(x = x, y = estimate, ymin = conf.low, ymax = conf.high)
    ggplot2::ggplot(data = plot.data, mapping = mapping) + ggplot2::geom_pointrange()  
  } else {
    stop("Unable to plot data without estimated value. Specify 'conf.int=TRUE'")
  }
} 
