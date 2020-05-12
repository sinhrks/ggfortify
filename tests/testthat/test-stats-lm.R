context('test stats lm')

# Note the sort() here is to make sure the results match in CRAN MKL build
expect_sorted_equal <- function(a, b) expect_equal(sort(a), sort(b))

test_that('fortify.lm works for USArrests', {
  skip_on_cran()
  skip_on_travis()
  fortified <- ggplot2::fortify(lm(Murder ~ Assault + UrbanPop, data = USArrests))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Murder', 'Assault', 'UrbanPop', '.hat', '.sigma',
                      '.cooksd', '.fitted', '.resid', '.stdresid')
  expect_equal(colnames(fortified), expected_names)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(glm(Murder ~ Assault + UrbanPop, data = USArrests))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Murder', 'Assault', 'UrbanPop', '.hat', '.sigma',
                      '.cooksd', '.fitted', '.resid', '.stdresid')
  expect_equal(colnames(fortified), expected_names)
  expect_equal(rownames(fortified), rownames(USArrests))
})

test_that('autoplot.lm can accept + operator', {
  skip_on_cran()
  skip_on_travis()
  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), size = 5) + theme_bw()
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p@plots), 4)

  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), which = 1:6) + scale_colour_brewer()
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p@plots), 6)
})


test_that('autoplot.lm works for USArrests', {
  skip_on_cran()
  skip_on_travis()
  lm.out <- lm(Murder ~ Assault + UrbanPop, data = USArrests)
  p <- autoplot(lm.out)
  expect_true(is(p, 'ggmultiplot'))

  # Residuals vs Fitted
  assert_lm1 <- function(p) {
    expect_equal(length(p$layers), 4)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomHline'))
    expect_true(is(p$layers[[4]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(10.98829412396, 12.61896752124, 12.55584079695, 9.32452021199, 11.27584647663, 8.69296623258))
    expect_sorted_equal(ld1$y, c(2.211705876045, -2.618967521238, -4.455840796945, -0.524520211986, -2.275846476629, -0.792966232577))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))

    expect_sorted_equal(ld4$x, c(9.80152442166, 10.45296738353, 12.55584079695))
    expect_sorted_equal(ld4$y, c(7.59847557834, -4.55296738353, -4.45584079695))
    expect_equal(ld4$label, c("Georgia", "Delaware", "Arizona"))
  }
  p <- autoplot(lm.out, which = 1)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm1(p[[1]])

  # Q-Q plot
  assert_lm2 <- function(p) {
    expect_equal(length(p$layers), 3)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[3]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(1.0364333894938, -1.2265281200366, -1.8807936081513, -0.0250689082587, -0.9541652531462, -0.1763741647809))
    expect_sorted_equal(ld1$y, c(0.876705970824, -1.066488546121, -1.793656590602, -0.208487054020, -0.928454258706, -0.312994001894))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))

    expect_sorted_equal(ld3$x, c(2.32634787404, -2.32634787404, -1.88079360815))
    expect_sorted_equal(ld3$y, c(2.99084842914, -1.79592192731, -1.79365659060))
    expect_equal(ld3$label, c("Georgia", "Delaware", "Arizona"))
  }
  p <- autoplot(lm.out, which = 2)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm2(p[[1]])

  # Scale Location
  assert_lm3 <- function(p) {
    expect_equal(length(p$layers), 3)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(10.98829412396, 12.61896752124, 12.55584079695, 9.32452021199, 11.27584647663, 8.69296623258))
    expect_sorted_equal(ld1$y, c(0.936325782420, 1.032709323150, 1.339274650922, 0.456603826112, 0.963563313284, 0.559458668620))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))

    expect_sorted_equal(ld3$x, c(9.80152442166, 10.45296738353, 12.55584079695))
    expect_sorted_equal(ld3$y, c(1.72940695880, 1.34012011675, 1.33927465092))
    expect_equal(ld3$label, c("Georgia", "Delaware", "Arizona"))
  }
  p <- autoplot(lm.out, which = 3)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm3(p[[1]])

  # Cook's distance
  assert_lm4 <- function(p) {
    expect_equal(length(p$layers), 2)
    expect_true(is(p$layers[[1]]$geom, 'GeomLinerange'))
    expect_true(is(p$layers[[2]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(1, 2, 3, 4, 5, 6))
    expect_sorted_equal(ld1$y, c(0.011780302261585, 0.039384807123982, 0.084368193847733, 0.000749502795764, 0.031007744531649, 0.001212457891192))

    ld2 <- head(ggplot2:::layer_data(p, 2))
    expect_sorted_equal(ld2$x, c(33, 11, 10))
    expect_sorted_equal(ld2$y, c(0.1238456342775, 0.1116513094557, 0.0934772277625))
    expect_equal(ld2$label, c("North Carolina", "Hawaii", "Georgia"))
  }
  p <- autoplot(lm.out, which = 4)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm4(p[[1]])

  # Residuals vs Leverage
  assert_lm5 <- function(p) {
    expect_equal(length(p$layers), 5)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomHline'))
    expect_true(is(p$layers[[4]]$geom, 'GeomBlank'))
    expect_true(is(p$layers[[5]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(0.0439588522340, 0.0941055398350, 0.0729343164009, 0.0491849650378, 0.0974013722825, 0.0357999991845))
    expect_sorted_equal(ld1$y, c(0.876705970824, -1.066488546121, -1.793656590602, -0.208487054020, -0.928454258706, -0.312994001894))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))
    ld5 <- head(ggplot2:::layer_data(p, 5))

    expect_sorted_equal(ld5$x, c(0.1831430655646, 0.1213004188902, 0.0303971016107))
    expect_sorted_equal(ld5$y, c(-1.28729694510, 1.55769202765, 2.99084842914))
    expect_equal(ld5$label, c("North Carolina", "Hawaii", "Georgia"))
  }
  p <- autoplot(lm.out, which = 5)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm5(p[[1]])

  # Cook's dist vs Leverage
  assert_lm6 <- function(p) {
    expect_equal(length(p$layers), 11)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomBlank'))
    expect_true(is(p$layers[[4]]$geom, 'GeomText'))
    expect_true(is(p$layers[[5]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[6]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[7]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[8]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[9]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[10]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[11]]$geom, 'GeomAbline'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(0.0439588522340, 0.0941055398350, 0.0729343164009, 0.0491849650378, 0.0974013722825, 0.0357999991845))
    expect_sorted_equal(ld1$y, c(0.011780302261585, 0.039384807123982, 0.084368193847733, 0.000749502795764, 0.031007744531649, 0.001212457891192))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))

    expect_sorted_equal(ld4$x, c(0.1831430655646, 0.1213004188902, 0.0303971016107))
    expect_sorted_equal(ld4$y, c(0.1238456342775, 0.1116513094557, 0.0934772277625))
    expect_equal(ld4$label, c("North Carolina", "Hawaii", "Georgia"))

    ld5 <- head(ggplot2:::layer_data(p, 5))
    ld6 <- head(ggplot2:::layer_data(p, 6))
    ld7 <- head(ggplot2:::layer_data(p, 7))
    ld8 <- head(ggplot2:::layer_data(p, 8))
  }
  p <- autoplot(lm.out, which = 6)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm6(p[[1]])

  # All
  p <- autoplot(lm.out, which = c(1, 2, 3, 4, 5, 6))
  expect_true(is(p, 'ggmultiplot'))
  assert_lm1(p[[1]])
  assert_lm2(p[[2]])
  assert_lm3(p[[3]])
  assert_lm4(p[[4]])
  assert_lm5(p[[5]])
  assert_lm6(p[[6]])
})

test_that('autoplot.lm works for binomial', {
  skip_on_cran()
  skip_on_travis()
  library(MASS)
  data(menarche)
  glm.out <- glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)
  p <- autoplot(glm.out)
  expect_true(is(p, 'ggmultiplot'))

  # Residuals vs Fitted
  assert_glm1 <- function(p) {
    expect_equal(length(p$layers), 4)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomHline'))
    expect_true(is(p$layers[[4]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(-6.19596641798, -4.56399806976, -3.96016978091, -3.55217769386, -3.14418560680, -2.73619351974))
    expect_sorted_equal(ld1$y, c(-1.237231196221, -2.036310110290, -1.873973216148, -0.804382661466, -0.995331982416, -0.160716281922))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))

    expect_sorted_equal(ld4$x, c(-4.56399806976, -3.96016978091, -1.10422517152))
    expect_sorted_equal(ld4$y, c(-2.03631011029, -1.87397321615, 1.36753877427))
    expect_equal(ld4$label, c("2", "3", "10"))
  }
  p <- autoplot(glm.out, which = 1)
  expect_true(is(p, 'ggmultiplot'))
  assert_glm1(p[[1]])

  # Q-Q plot
  assert_glm2 <- function(p) {
    expect_equal(length(p$layers), 3)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[3]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(-1.554773594597, -2.053748910632, -1.281551565545, -0.305480788099, -0.524400512708, 0.201893479142))
    expect_sorted_equal(ld1$y, c(-24.50740077119, -29.77401740678, -18.46742851899, -9.12522512251, -9.74576038627, -1.56377387491))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))

    expect_sorted_equal(ld3$x, c(2.05374891063, -2.05374891063, -1.55477359460))
    expect_sorted_equal(ld3$y, c(36.3641401028, -29.7740174068, -24.5074007712))
    expect_equal(ld3$label, c("25", "2", "1"))
  }
  p <- autoplot(glm.out, which = 2)
  expect_true(is(p, 'ggmultiplot'))
  assert_glm2(p[[1]])

  # Scale Location
  assert_glm3 <- function(p) {
    expect_equal(length(p$layers), 3)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(-6.19596641798, -4.56399806976, -3.96016978091, -3.55217769386, -3.14418560680, -2.73619351974))
    expect_sorted_equal(ld1$y, c(4.95049500264, 5.45655728521, 4.29737460771, 3.02079875571, 3.12182004386, 1.25050944615))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))

    expect_sorted_equal(ld3$x, c(7.46360865666, -4.56399806976, -6.19596641798))
    expect_sorted_equal(ld3$y, c(6.03026865926, 5.45655728521, 4.95049500264))
    expect_equal(ld3$label, c("25", "2", "1"))
  }
  p <- autoplot(glm.out, which = 3)
  expect_true(is(p, 'ggmultiplot'))
  assert_glm3(p[[1]])

  # Cook's distance
  assert_glm4 <- function(p) {
    expect_equal(length(p$layers), 2)
    expect_true(is(p$layers[[1]]$geom, 'GeomLinerange'))
    expect_true(is(p$layers[[2]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(1, 2, 3, 4, 5, 6))
    expect_sorted_equal(ld1$y, c(0.01740112703384, 0.07680098087888, 0.04095037805283, 0.02157996281830, 0.02880296835881, 0.00103150878141))

    ld2 <- head(ggplot2:::layer_data(p, 2))
    expect_sorted_equal(ld2$x, c(10, 15, 17))
    expect_sorted_equal(ld2$y, c(0.1195191318650, 0.0984796718074, 0.0975367745871))
    expect_equal(ld2$label, c("10", "15", "17"))
  }
  p <- autoplot(glm.out, which = 4)
  expect_true(is(p, 'ggmultiplot'))
  assert_glm4(p[[1]])

  # Residuals vs Leverage
  assert_glm5 <- function(p) {
    expect_equal(length(p$layers), 5)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomHline'))
    expect_true(is(p$layers[[4]]$geom, 'GeomBlank'))
    expect_true(is(p$layers[[5]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(0.0417141770381, 0.0645018028661, 0.0423719600257, 0.0675630552621, 0.0612564434679, 0.0704890316373))
    expect_sorted_equal(ld1$y, c(-1.263872692299, -2.105340961150, -1.914983130008, -0.833015273652, -1.027293345029, -0.166698855175))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))
    ld5 <- head(ggplot2:::layer_data(p, 5))

    expect_sorted_equal(ld5$x, c(0.0987328446734, 0.1069957513737, 0.0965342062351))
    expect_sorted_equal(ld5$y, c(1.44049871978, 1.30959756960, -1.30015928340))
    expect_equal(ld5$label, c("10", "15", "17"))
  }
  p <- autoplot(glm.out, which = 5)
  expect_true(is(p, 'ggmultiplot'))
  assert_glm5(p[[1]])

  # Cook's dist vs Leverage
  assert_glm6 <- function(p) {
    expect_equal(length(p$layers), 8)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomBlank'))
    expect_true(is(p$layers[[4]]$geom, 'GeomText'))
    expect_true(is(p$layers[[5]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[6]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[7]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[8]]$geom, 'GeomAbline'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(0.0417141770381, 0.0645018028661, 0.0423719600257, 0.0675630552621, 0.0612564434679, 0.0704890316373))
    expect_sorted_equal(ld1$y, c(0.01740112703384, 0.07680098087888, 0.04095037805283, 0.02157996281830, 0.02880296835881, 0.00103150878141))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))

    expect_sorted_equal(ld4$x, c(0.0987328446734, 0.1069957513737, 0.0965342062351))
    expect_sorted_equal(ld4$y, c(0.1195191318650, 0.0984796718074, 0.0975367745871))
    expect_equal(ld4$label, c("10", "15", "17"))

    ld5 <- head(ggplot2:::layer_data(p, 5))
    ld6 <- head(ggplot2:::layer_data(p, 6))
    ld7 <- head(ggplot2:::layer_data(p, 7))
    ld8 <- head(ggplot2:::layer_data(p, 8))
  }
  p <- autoplot(glm.out, which = 6)
  expect_true(is(p, 'ggmultiplot'))
  assert_glm6(p[[1]])

  # All
  p <- autoplot(glm.out, which = c(1, 2, 3, 4, 5, 6))
  expect_true(is(p, 'ggmultiplot'))
  assert_glm1(p[[1]])
  assert_glm2(p[[2]])
  assert_glm3(p[[3]])
  assert_glm4(p[[4]])
  assert_glm5(p[[5]])
  assert_glm6(p[[6]])
})

test_that('autoplot.lm works for polynomial fit', {
  skip_on_cran()
  skip_on_travis()
  lm.out <- lm(mpg ~ poly(hp, 2, raw = TRUE), data = mtcars)
  p <- autoplot(lm.out)
  expect_true(is(p, 'ggmultiplot'))

  # Residuals vs Fitted
  assert_lm1 <- function(p) {
    expect_equal(length(p$layers), 4)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomHline'))
    expect_true(is(p$layers[[4]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(22.0370777250, 22.0370777250, 24.2110834069, 22.0370777250, 15.9676503638, 22.6512422231))
    expect_sorted_equal(ld1$y, c(-1.037077725009, -1.037077725009, -1.411083406907, -0.637077725009, 2.732349636196, -4.551242223095))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))

    expect_sorted_equal(ld4$x, c(21.6786786013, 28.3220263408, 22.6512422231))
    expect_sorted_equal(ld4$y, c(8.72132139874, 5.57797365923, -4.55124222310))
    expect_equal(ld4$label, c("Lotus Europa", "Toyota Corolla", "Valiant"))
  }
  p <- autoplot(lm.out, which = 1)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm1(p[[1]])

  # Q-Q plot
  assert_lm2 <- function(p) {
    expect_equal(length(p$layers), 3)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[3]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))

    expect_sorted_equal(ld1$x, c(-0.2776904398216, -0.1970990842943, -0.5334097062413, 0.0391760855031, 0.9467817563010, -1.6759397227734))
    expect_sorted_equal(ld1$y, c(-0.344361871272, -0.344361871272, -0.471006426443, -0.211541789241, 0.917989725233, -1.512317646071))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))

    expect_sorted_equal(ld3$x, c(2.15387469406, 1.67593972277, -1.67593972277))
    expect_sorted_equal(ld3$y, c(2.89542798400, 1.92989370759, -1.51231764607))
    expect_equal(ld3$label, c("Lotus Europa", "Toyota Corolla", "Valiant"))
  }
  p <- autoplot(lm.out, which = 2)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm2(p[[1]])

  # Scale Location
  assert_lm3 <- function(p) {
    expect_equal(length(p$layers), 3)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(22.0370777250, 22.0370777250, 24.2110834069, 22.0370777250, 15.9676503638, 22.6512422231))
    expect_sorted_equal(ld1$y, c(0.586823543557, 0.586823543557, 0.686299079442, 0.459936723083, 0.958117803422, 1.229763247975))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))

    expect_sorted_equal(ld3$x, c(21.6786786013, 28.3220263408, 22.6512422231))
    expect_sorted_equal(ld3$y, c(1.70159571697, 1.38920614294, 1.22976324798))
    expect_equal(ld3$label, c("Lotus Europa", "Toyota Corolla", "Valiant"))
  }
  p <- autoplot(lm.out, which = 3)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm3(p[[1]])

  # Cook's distance
  assert_lm4 <- function(p) {
    expect_equal(length(p$layers), 2)
    expect_true(is(p$layers[[1]]$geom, 'GeomLinerange'))
    expect_true(is(p$layers[[2]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(1, 2, 3, 4, 5, 6))
    expect_sorted_equal(ld1$y, c(0.001744913090237, 0.001744913090237, 0.004075898589070, 0.000658470054463, 0.019367006055189, 0.034788166928193))

    ld2 <- head(ggplot2:::layer_data(p, 2))
    expect_sorted_equal(ld2$x, c(31, 20, 28))
    expect_sorted_equal(ld2$y, c(0.514354642953, 0.165885431468, 0.122374068009))
    expect_equal(ld2$label, c("Maserati Bora", "Toyota Corolla", "Lotus Europa"))
  }
  p <- autoplot(lm.out, which = 4)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm4(p[[1]])

  # Residuals vs Leverage
  assert_lm5 <- function(p) {
    expect_equal(length(p$layers), 5)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomHline'))
    expect_true(is(p$layers[[4]]$geom, 'GeomBlank'))
    expect_true(is(p$layers[[5]]$geom, 'GeomText'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(0.0422770645623, 0.0422770645623, 0.0522384229500, 0.0422770645623, 0.0644989134682, 0.0436403288747))
    expect_sorted_equal(ld1$y, c(-0.344361871272, -0.344361871272, -0.471006426443, -0.211541789241, 0.917989725233, -1.512317646071))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))
    ld5 <- head(ggplot2:::layer_data(p, 5))

    expect_sorted_equal(ld5$x, c(0.735896676687, 0.117868091819, 0.041953824628))
    expect_sorted_equal(ld5$y, c(-0.744167110444, 1.929893707586, 2.895427983998))
    expect_equal(ld5$label, c("Maserati Bora", "Toyota Corolla", "Lotus Europa"))
  }
  p <- autoplot(lm.out, which = 5)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm5(p[[1]])

  # Cook's dist vs Leverage
  assert_lm6 <- function(p) {
    expect_equal(length(p$layers), 11)
    expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
    expect_true(is(p$layers[[2]]$geom, 'GeomLine'))
    expect_true(is(p$layers[[3]]$geom, 'GeomBlank'))
    expect_true(is(p$layers[[4]]$geom, 'GeomText'))
    expect_true(is(p$layers[[5]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[6]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[7]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[8]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[9]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[10]]$geom, 'GeomAbline'))
    expect_true(is(p$layers[[11]]$geom, 'GeomAbline'))

    ld1 <- head(ggplot2:::layer_data(p, 1))
    expect_sorted_equal(ld1$x, c(0.0422770645623, 0.0422770645623, 0.0522384229500, 0.0422770645623, 0.0644989134682, 0.0436403288747))
    expect_sorted_equal(ld1$y, c(0.001744913090237, 0.001744913090237, 0.004075898589070, 0.000658470054463, 0.019367006055189, 0.034788166928193))
    ld2 <- head(ggplot2:::layer_data(p, 2))
    ld3 <- head(ggplot2:::layer_data(p, 3))
    ld4 <- head(ggplot2:::layer_data(p, 4))

    expect_sorted_equal(ld4$x, c(0.735896676687, 0.117868091819, 0.041953824628))
    expect_sorted_equal(ld4$y, c(0.514354642953, 0.165885431468, 0.122374068009))
    expect_equal(ld4$label, c("Maserati Bora", "Toyota Corolla", "Lotus Europa"))

    ld5 <- head(ggplot2:::layer_data(p, 5))
    ld6 <- head(ggplot2:::layer_data(p, 6))
    ld7 <- head(ggplot2:::layer_data(p, 7))
    ld8 <- head(ggplot2:::layer_data(p, 8))
  }
  p <- autoplot(lm.out, which = 6)
  expect_true(is(p, 'ggmultiplot'))
  assert_lm6(p[[1]])

  # All
  p <- autoplot(lm.out, which = c(1, 2, 3, 4, 5, 6))
  expect_true(is(p, 'ggmultiplot'))
  assert_lm1(p[[1]])
  assert_lm2(p[[2]])
  assert_lm3(p[[3]])
  assert_lm4(p[[4]])
  assert_lm5(p[[5]])
  assert_lm6(p[[6]])
})

test_that('autoplot.lm works with factors', {
  skip_on_cran()
  skip_on_travis()
  lm.out <- aov(Petal.Length ~ Species, data = iris)
  p <- autoplot(lm.out, which = 5)
  expect_true(is(p, 'ggmultiplot'))
  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(p[[1]]$mapping$x, quo(.nf))

  lm.out <- aov(Petal.Length ~ Species, data = iris)
  p <- autoplot(lm.out, which = c(1, 2, 3, 4, 5, 6))
  expect_true(is(p, 'ggmultiplot'))
  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(p[[5]]$mapping$x, quo(.nf))

  lm.out <- lm(Volume ~ Girth, data = trees)
  p <- autoplot(lm.out, which = c(1, 2, 3, 4, 5, 6))
  expect_true(is(p, 'ggmultiplot'))
  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(p[[5]]$mapping$x, quo(.hat))
})


test_that('autoplot.lm works with characters', {
  skip_on_cran()
  skip_on_travis()
  iris <- transform(iris, Species_chr = as.character(Species), stringsAsFactors = FALSE)
  lm.out <- aov(Petal.Length ~ Species_chr, data = iris)
  p <- autoplot(lm.out, which = 5)
  expect_true(is(p, 'ggmultiplot'))
  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(p[[1]]$mapping$x, quo(.nf))

  lm.out <- aov(Petal.Length ~ Species_chr, data = iris)
  p <- autoplot(lm.out, which = c(1, 2, 3, 4, 5, 6))
  expect_true(is(p, 'ggmultiplot'))
  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(p[[5]]$mapping$x, quo(.nf))
})

test_that('autoplot.lm can be used in ggsave()', {
  skip_on_cran()
  skip_on_travis()
  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), size = 5)
  ggsave(p, file='temp.png', h = 6, w = 6, units = "in", dpi = 300)
  expect_true(file.exists('temp.png'))
  unlink('temp.png')
})

test_that('fortify.lm works with AsIs response', {
  skip_on_cran()
  skip_on_travis()
  mdl_cars <- lm(I(dist ^ 2) ~ speed, data = cars)
  p <- autoplot(mdl_cars, which = 1:6)
  expect_true(is(p, 'ggmultiplot'))
  # Previous problems occurred when printing, so write to file
  ggsave(p, file='temp-test-AsIs.png', h = 6, w = 6, units = "in", dpi = 50)
  expect_true(file.exists('temp-test-AsIs.png'))
  unlink('temp-test-AsIs.png')
})
