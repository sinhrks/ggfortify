library(splines)

context('test basis')

test_that('fortify.basis and autoplot.basis work for ns and bs', {
    skip_on_cran()
    skip_on_travis()
    n <- 256
    df <- 6
    expected_num_rows <- (n+1) * df
    bspl <- bs(diamonds$price, df=df)
    nspl <- ns(diamonds$price, df=df)
    fortified.bs <- ggplot2::fortify(bspl, n=n)
    fortified.ns <- ggplot2::fortify(nspl, n=n)
    expect_true(is.data.frame(fortified.bs))
    expect_true(is.data.frame(fortified.ns))
    expected_names <- c('Spline', 'x', 'y')
    expect_equal(names(fortified.bs), expected_names)
    expect_equal(names(fortified.ns), expected_names)
    expect_equal(dim(fortified.bs), c(expected_num_rows, 3))
    expect_equal(dim(fortified.ns), c(expected_num_rows, 3))
    p.bs <- autoplot(bspl)
    p.ns <- autoplot(nspl)
    expect_true(is(p.bs, "ggplot"))
    expect_true(is(p.ns, "ggplot"))
})
