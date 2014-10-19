#' Convert \code{base::table} to data.frame.
#' 
#' @param data \code{base::table} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(Titanic)
#' @export
fortify.table <- function(data) {
  dplyr::tbl_df(as.data.frame(data))
}

#' Convert \code{base::matrix} to data.frame.
#' 
#' @param data \code{base::matrix} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(matrix(1:6, nrow=2, ncol=3))
#' @export
fortify.matrix <- function(data) {
  dplyr::tbl_df(as.data.frame(data))
}