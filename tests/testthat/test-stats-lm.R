context('test stats lm')

test_that('fortify.lm works for USArrests', {
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
  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), size = 5) + theme_bw()
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p@plots), 4)

  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), which = 1:6) + scale_colour_brewer()
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p@plots), 6)
})

test_that('fortify.lm works for binomial', {
  library(MASS)
  data(menarche)
  glm.out <- glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)
  p <- autoplot(glm.out)
  expect_true(is(p, 'ggmultiplot'))
  # Todo: add geom and label coordinates checks

  # Residuals vs Fitted
  p <- autoplot(glm.out, which = 1)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p[[1]]$layers), 4)
  expect_true(is(p[[1]]$layers[[1]]$geom, 'GeomPoint'))
  expect_true(is(p[[1]]$layers[[2]]$geom, 'GeomLine'))
  expect_true(is(p[[1]]$layers[[3]]$geom, 'GeomHline'))
  expect_true(is(p[[1]]$layers[[4]]$geom, 'GeomText'))

  ld1 <- head(ggplot2:::layer_data(p[[1]], 1))
  expect_equal(ld1$x, c(-6.19596641798, -4.56399806976, -3.96016978091, -3.55217769386, -3.14418560680, -2.73619351974))
  expect_equal(ld1$y, c(-1.237231196221, -2.036310110290, -1.873973216148, -0.804382661466, -0.995331982416, -0.160716281922))
  ld2 <- head(ggplot2:::layer_data(p[[1]], 2))
  ld3 <- head(ggplot2:::layer_data(p[[1]], 3))
  ld4 <- head(ggplot2:::layer_data(p[[1]], 4))

  expect_equal(ld4$x, c(-4.56399806976, -3.96016978091, -1.10422517152))
  expect_equal(ld4$y, c(-2.03631011029, -1.87397321615, 1.36753877427))
  expect_equal(ld4$label, c("2", "3", "10"))

  # Q-Q
  p <- autoplot(glm.out, which = 2)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p[[1]]$layers), 3)
  expect_true(is(p[[1]]$layers[[1]]$geom, 'GeomPoint'))
  expect_true(is(p[[1]]$layers[[2]]$geom, 'GeomAbline'))
  expect_true(is(p[[1]]$layers[[3]]$geom, 'GeomText'))

  ld1 <- head(ggplot2:::layer_data(p[[1]], 1))
  expect_equal(ld1$x, c(-1.554773594597, -2.053748910632, -1.281551565545, -0.305480788099, -0.524400512708, 0.201893479142))
  expect_equal(ld1$y, c(-24.50740077119, -29.77401740678, -18.46742851899, -9.12522512251, -9.74576038627, -1.56377387491))
  ld2 <- head(ggplot2:::layer_data(p[[1]], 2))
  ld3 <- head(ggplot2:::layer_data(p[[1]], 3))

  expect_equal(ld3$x, c(2.05374891063, -2.05374891063, -1.55477359460))
  expect_equal(ld3$y, c(36.3641401028, -29.7740174068, -24.5074007712))
  expect_equal(ld3$label, c("25", "2", "1"))

  # Scale Location
  p <- autoplot(glm.out, which = 3)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p[[1]]$layers), 3)
  expect_true(is(p[[1]]$layers[[1]]$geom, 'GeomPoint'))
  expect_true(is(p[[1]]$layers[[2]]$geom, 'GeomLine'))
  expect_true(is(p[[1]]$layers[[3]]$geom, 'GeomText'))

  ld1 <- head(ggplot2:::layer_data(p[[1]], 1))
  expect_equal(ld1$x, c(-6.19596641798, -4.56399806976, -3.96016978091, -3.55217769386, -3.14418560680, -2.73619351974))
  expect_equal(ld1$y, c(4.95049500264, 5.45655728521, 4.29737460771, 3.02079875571, 3.12182004386, 1.25050944615))
  ld2 <- head(ggplot2:::layer_data(p[[1]], 2))
  ld3 <- head(ggplot2:::layer_data(p[[1]], 3))

  expect_equal(ld3$x, c(7.46360865666, -4.56399806976, -6.19596641798))
  expect_equal(ld3$y, c(6.03026865926, 5.45655728521, 4.95049500264))
  expect_equal(ld3$label, c("25", "2", "1"))

  # Cook's distance
  p <- autoplot(glm.out, which = 4)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p[[1]]$layers), 2)
  expect_true(is(p[[1]]$layers[[1]]$geom, 'GeomLinerange'))
  expect_true(is(p[[1]]$layers[[2]]$geom, 'GeomText'))

  ld1 <- head(ggplot2:::layer_data(p[[1]], 1))
  expect_equal(ld1$x, c(1, 2, 3, 4, 5, 6))
  expect_equal(ld1$y, c(0.01740112703384, 0.07680098087888, 0.04095037805283, 0.02157996281830, 0.02880296835881, 0.00103150878141))
  ld2 <- head(ggplot2:::layer_data(p[[1]], 2))

  expect_equal(ld2$x, c(10, 15, 17))
  expect_equal(ld2$y, c(0.1195191318650, 0.0984796718074, 0.0975367745871))
  expect_equal(ld2$label, c("10", "15", "17"))

  # Residuals vs Leverage
  p <- autoplot(glm.out, which = 5)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p[[1]]$layers), 5)
  expect_true(is(p[[1]]$layers[[1]]$geom, 'GeomPoint'))
  expect_true(is(p[[1]]$layers[[2]]$geom, 'GeomLine'))
  expect_true(is(p[[1]]$layers[[3]]$geom, 'GeomHline'))
  expect_true(is(p[[1]]$layers[[4]]$geom, 'GeomBlank'))
  expect_true(is(p[[1]]$layers[[5]]$geom, 'GeomText'))

  ld1 <- head(ggplot2:::layer_data(p[[1]], 1))
  expect_equal(ld1$x, c(0.0417141770381, 0.0645018028661, 0.0423719600257, 0.0675630552621, 0.0612564434679, 0.0704890316373))
  expect_equal(ld1$y, c(-1.263872692299, -2.105340961150, -1.914983130008, -0.833015273652, -1.027293345029, -0.166698855175))
  ld2 <- head(ggplot2:::layer_data(p[[1]], 2))
  ld3 <- head(ggplot2:::layer_data(p[[1]], 3))
  ld4 <- head(ggplot2:::layer_data(p[[1]], 4))
  ld5 <- head(ggplot2:::layer_data(p[[1]], 5))

  expect_equal(ld5$x, c(0.0987328446734, 0.1069957513737, 0.0965342062351))
  expect_equal(ld5$y, c(1.44049871978, 1.30959756960, -1.30015928340))
  expect_equal(ld5$label, c("10", "15", "17"))

  # Cook's dist vs Leverage
  p <- autoplot(glm.out, which = 6)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p[[1]]$layers), 8)
  expect_true(is(p[[1]]$layers[[1]]$geom, 'GeomPoint'))
  expect_true(is(p[[1]]$layers[[2]]$geom, 'GeomLine'))
  expect_true(is(p[[1]]$layers[[3]]$geom, 'GeomBlank'))
  expect_true(is(p[[1]]$layers[[4]]$geom, 'GeomText'))
  expect_true(is(p[[1]]$layers[[5]]$geom, 'GeomAbline'))
  expect_true(is(p[[1]]$layers[[6]]$geom, 'GeomAbline'))
  expect_true(is(p[[1]]$layers[[7]]$geom, 'GeomAbline'))
  expect_true(is(p[[1]]$layers[[8]]$geom, 'GeomAbline'))

  ld1 <- head(ggplot2:::layer_data(p[[1]], 1))
  expect_equal(ld1$x, c(0.0417141770381, 0.0645018028661, 0.0423719600257, 0.0675630552621, 0.0612564434679, 0.0704890316373))
  expect_equal(ld1$y, c(0.01740112703384, 0.07680098087888, 0.04095037805283, 0.02157996281830, 0.02880296835881, 0.00103150878141))
  ld2 <- head(ggplot2:::layer_data(p[[1]], 2))
  ld3 <- head(ggplot2:::layer_data(p[[1]], 3))
  ld4 <- head(ggplot2:::layer_data(p[[1]], 4))

  expect_equal(ld4$x, c(0.0987328446734, 0.1069957513737, 0.0965342062351))
  expect_equal(ld4$y, c(0.1195191318650, 0.0984796718074, 0.0975367745871))
  expect_equal(ld4$label, c("10", "15", "17"))

  ld5 <- head(ggplot2:::layer_data(p[[1]], 5))
  ld6 <- head(ggplot2:::layer_data(p[[1]], 6))
  ld7 <- head(ggplot2:::layer_data(p[[1]], 7))
  ld8 <- head(ggplot2:::layer_data(p[[1]], 8))
})

test_that('autoplot.lm can be used in ggsave()', {
  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), size = 5)
  ggsave(p, file='temp.png', h = 6, w = 6, units = "in", dpi = 300)
  expect_true(file.exists('temp.png'))
  unlink('temp.png')
})


