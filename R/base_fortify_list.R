#' Convert list to data.frame.
#' 
#' @param data list instance
#' @return data.frame
#' @export
fortify.list <- function(data) {
  klass <- infer(data)
  if (klass == 'mds-like') {
    return(fortify(data$points))
  }
  stop('Unable to infer class from input list')
}

#' Autoplot list.
#' 
#' @param data list instance
#' @return ggplot
#' @export
autoplot.list <- function(data, ...) {
  klass <- infer(data)
  if (klass == 'mds-like') {
    return(ggplot2::autoplot(data$points[, 1:2], geom = 'point', ...))
  }
  stop('Unable to infer class from input list')
}

#' Infer class name
#' 
#' @param data list instance
#' @return character
#' @export
infer <- function(data, ...) {
  if (check.attrs(data, c('points', 'eig', 'x', 'ac', 'GOF'))) {
    # cmdscale
    return('mds-like')
  } else if (check.attrs(data, c('points', 'stress'))) {
    # isoMDS
    return('mds-like')
  } else if (check.attrs((data), c('points', 'stress', 'call'))) {
    # sammon
    return('mds-like')
  }
  stop('Unable to infer class from input list')
}

#' Infer class name
#' 
#' @param data list instance
#' @param expected expected character vector
#' @return logical
check.attrs <- function(data, expected) {
  n <- names(data)
  if (length(n) == length(expected) && all(n == expected)) {
    return(TRUE)
  }
  return (FALSE)
}

