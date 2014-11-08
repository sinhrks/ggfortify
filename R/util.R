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