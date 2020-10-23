#' Convert a spline basis to a tibble
#'
#' @param x object of class "basis"
#' @param ... Ignored.
#' @return A tibble constructed from the underlying matrix of the
#'     basis object. Each column will possess all the attributes from
#'     the source object, except that the "class" attribute will be
#'     renamed to "basis.class" to avoid interfering with dplyr
#'     operations.
#' @details This function is needed because the default method for
#'     converting a matrix object with an additional class attribute
#'     to a tibble causes issues because each column of the resulting
#'     tibble has the attributes, including the matrix class, copied
#'     from the source. Having matrices as columns in a tibble causes
#'     dplyr to throw errors, so a special method is needed to avoid
#'     copying the class attribute.
#' @examples
#' library(splines)
#' library(tibble)
#' x <- seq(0, 1, by=0.001)
#' spl <- bs(x, df=6)
#' as_tibble(spl)
#' @importFrom tibble as_tibble
as_tibble.basis <- function(x, ...) {
    attr(x, "basis.class") <- attr(x, "class")
    as_tibble(unclass(x))
}

#' Convert spline basis instances to \code{data.frame}
#'
#' @param model spline basis object
#' @param data x-values at which to evaluate the splines. Optional. By
#'     default, an evenly spaced sequence of 256 values covering the
#'     range of the splines will be used.
#' @param n If data is not provided, instead use an evenly-spaced
#'     sequence of x-values of this length (plus one, since both
#'     endpoints are included). If data is provided, this argument is
#'     ignored.
#' @inheritParams fortify_base
#' @return data.frame with 3 columns: Spline (character), x (numeric),
#'     and y (numeric); giving the interpolated x and y values for
#'     each of the splines in the basis.
#' @examples
#' library(splines)
#' x <- seq(0, 1, by=0.001)
#' spl <- bs(x, df=6)
#' fortify(spl)
#' @importFrom tibble as_tibble
#' @importFrom stats predict
#' @export
fortify.basis <- function(model, data, n=256, ...) {
    attrs <- attributes(model)
    bounds <- attrs$Boundary.knots
    if (missing(data)) {
        ## Add 1 to length.out because both endpoints are included
        data <- seq(from=bounds[1], to=bounds[2], length.out=n + 1)
    }
    predict(model, data) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(x=data) %>%
        tidyr::pivot_longer(colnames(model), names_to="Spline", values_to="y") %>%
        dplyr::arrange(Spline, x) %>%
        dplyr::select(Spline, x, y)
}

#' Autoplot spline basis instances
#'
#' @param object spline basis object
#' @param data x-values at which to evaluate the splines. Optional. By
#'     default, an evenly spaced sequence of 256 values covering the
#'     range of the splines will be used.
#' @param n If data is not provided, instead use an evenly-spaced
#'     sequence of x-values of this length (plus one, since both
#'     endpoints are included). If data is provided, this argument is
#'     ignored.
#' @param ... Ignored.
#' @return ggplot
#' @examples
#' library(splines)
#' x <- seq(0, 1, by=0.001)
#' spl <- bs(x, df=6)
#' autoplot(spl)
#' autoplot(spl, n=5)
#' @export
autoplot.basis <- function(object, data, n=256, ...) {
    fortified <- ggplot2::fortify(object, data, n)
    all.knots <- c(attr(object, "Boundary.knots"),
                   attr(object, "knots")) %>%
        unname %>% sort
    knot.df <- ggplot2::fortify(object,
                                data=all.knots)
    ggplot(fortified) +
        aes_string(x="x", y="y", group="Spline", color="Spline") +
        geom_line() +
        geom_point(data=knot.df) +
        scale_color_discrete(guide=FALSE)
}
