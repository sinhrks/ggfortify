#' Backtransform \code{scale}-ed object
#'
#' @param data Scaled data
#' @param center Centered vector
#' @param scale Scale vector
#' @return data.frame
#' @examples
#' df <- iris[-5]
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

#' Wrapper for cbind
#'
#' @param df1 1st data
#' @param df2 2nd data
#' @return list
#' @examples
#' ggfortify:::cbind_wraps(iris[1:2], iris[3:5])
cbind_wraps <- function(df1, df2) {
  if (is.null(df1)) {
    return(df2)
  } else if (!is.data.frame(df1)) {
    df1 <- ggplot2::fortify(df1)
  }
  if (is.null(df2)) {
    return(df1)
  } else if (!is.data.frame(df2)) {
    df2 <- ggplot2::fortify(df2)
  }
  # prioritize df1 columns
  dots <- names(df2)[! colnames(df2) %in% colnames(df1)]
  if (length(dots) != length(colnames(df2))) {
    df2 <- dplyr::select(df2, all_of(dots))
  }
  return(cbind(df1, df2))
}


#' Check object is target class, or object is \code{data.frame} fortified from target.
#'
#' @param object instance to be checked. For data.frame, check whether it is fortified from target class
#' @param target class name
#' @return logical
#' @examples
#' ggfortify:::is_derived_from(prcomp(iris[-5]), 'prcomp')
is_derived_from <- function(object, target) {
  if (is(object, target)) {
    return(TRUE)
  } else {
    # 'base_class' attr is only added by autoplot.aareg for now
    atr <- attr(object, 'base_class')
    # this null check should be separeted to avoid logical(0)
    if (is.null(atr)) {
      return(FALSE)
    }
    return(atr == target)
  }
  return(FALSE)
}


#' Show deprecate warning
#'
#' @param old.kw Keyword being deprecated
#' @param new.kw Keyword being replaced
#' @return NULL
#' @examples
#' ggfortify:::deprecate.warning('old', 'new')
deprecate.warning <- function(old.kw, new.kw) {
  message <- paste0("Argument '", old.kw, "' is being deprecated. Use '", new.kw, "' instead.")
  warning(message, call. = FALSE)
}

#' Flatten dataframe contains matrix
#'
#' tains list or matrix as column
#'
#' @param df \code{data.frame} to be flatten
flatten <- function(df) {
  ismatrix <- vapply(df, is.matrix, logical(1))
  if (any(ismatrix)) {
    return(data.frame(c(df[!ismatrix], do.call(data.frame, df[ismatrix])),
                      stringsAsFactors = FALSE))
  } else {
    return(df)
  }
}

#' Post process for fortify.
#'
#' @param data data.frame
#' @param klass instance to be added as base_class attr, should be original model before fortified
#' @return data.frame
post_fortify <- function(data, klass = NULL) {
  if (is(data, 'tbl_df')) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    stop('data must be a data.frame')
  }
  if (!is.null(klass)) {
    attr(data, 'base_class') <- class(klass)
  }
  as.data.frame(data)
}
