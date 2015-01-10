#' Backtransform \code{scale}-ed object
#' 
#' @param data Scaled data
#' @param center Centered vector
#' @param scale Scale vector
#' @return data.frame
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggfortify::unscale(base::scale(df))
#' @export
unscale <- function(data, center = NULL, scale = NULL) {
  if (is.null(scale)) {
    scale <- attr(data, 'scaled:scale')
  }
  if (is.null(center)) {
    center <- attr(data, 'scaled:center')
  }
  if (!is.null(scale) && !is.logical(scale)) {
    data <- base::scale(data, center = FALSE, scale = 1 / scale)
  }
  if (!is.null(center) && !is.logical(center)) {
    data <- base::scale(data, center = -center, scale = FALSE)
  }
  as.data.frame(data)
}


#' Expand \code{stats::formula} expression 
#' 
#' @param formula \code{stats::formula} instance
#' @return list
#' @examples
#' ggfortify:::parse.formula(y ~ x)
parse.formula <- function(formula) {
  
  # not to replace forecast::getResponse
  # library(nlme)
  
  vars <- terms(as.formula(formula))
  endog <- if(attr(vars, 'response'))
    nlme::getResponseFormula(formula)
  exog <- nlme::getCovariateFormula(formula)
  group <- nlme::getGroupsFormula(formula)
  
  result <- list(response = all.vars(endog),
                 covariates = all.vars(exog),
                 groups = all.vars(group))
  result
}


#' Wrapper for cbind 
#' 
#' @param data
#' @param original
#' @return list
#' @examples
#' ggfortify:::cbind.original(iris[1:2], iris[3:5])
cbind.original <- function(data, original = NULL) {
  
  if (!is.null(original)) {
    if (!is.data.frame(original)) {
      original <- ggplot2::fortify(original)
    }
    dots <- names(original)[! colnames(original) %in% colnames(data)]
    if (length(dots) != length(colnames(original))) {
      original <- dplyr::select_(original, .dots = dots)
    }
    data <- cbind(data, original)
  }
  data
}


#' Show deprecate warning
#' 
#' @param old.kw Keyword being deprecated
#' @param new.kw Keyword being replaced
#' @return NULL
#' @examples
#' ggfortify:::deprecate.warning('old', 'new')
deprecate.warning <- function(old.kw, new.kw) {
  message <- paste0("Argument `", old.kw, "` is being deprecated. Use `", new.kw, "` instead.")
  warning(message, call. = FALSE)
}

#' Raise error for unsupported type
#' 
#' @return NULL
#' @examples
#' ggfortify:::stop.unsupported.type()
stop.unsupported.type <- function() {
  stop(paste0('Unsupported class for autoplot: ', class(data)), call. = FALSE)
}

