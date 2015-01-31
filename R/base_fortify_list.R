#' Convert list to data.frame.
#' 
#' @param data \code{list} instance
#' @return data.frame
#' @export
fortify.list <- function(data) {
  klass <- infer(data)
  if (klass == 'mds-like') {
    return(ggplot2::fortify(data$points))
  } else if (klass == 'dlmSmooth') {
    return(ggplot2::fortify(dlm::dropFirst(data$s)))
  } else if (klass == 'KFASSignal') {
    return(ggplot2::fortify(data$signal))
  }
  stop('Unable to infer class from input list')
}

#' Autoplot list.
#' 
#' @param data \code{list} instance
#' @param ... Keywords passed to autoplot
#' @return ggplot
#' @export
autoplot.list <- function(data, ...) {
  klass <- infer(data)
  if (klass == 'mds-like') {
    return(ggplot2::autoplot(data$points[, 1:2], geom = 'point', ...))
  } else if (klass == 'dlmSmooth') {
    return(ggplot2::autoplot(dlm::dropFirst(data$s), ...))
  } else if (klass == 'KFASSignal') {
    return(ggplot2::autoplot(data$signal, ...))
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
  } else if (check.attrs(data, c('points', 'stress', 'call'))) {
    # sammon
    return('mds-like')
  } else if (check.attrs(data, c('s', 'U.S', 'D.S'))) {
    # dlm::dlmSmooth
    return('dlmSmooth')
  } else if (check.attrs(data, c('signal', 'variance'))) {
      # KFAS::signal
      return('KFASSignal')
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

