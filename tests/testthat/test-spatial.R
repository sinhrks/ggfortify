# test cases are based on:
# http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html

test_that('test fortify.SpatialPoints', {
  skip_if_not_installed("sp")
  library(sp)

  x <- c(1, 2, 3, 4)
  y <- c(5, 6, 7, 8)

  # SpatialPoints
  coords <- cbind(x, y)
  sp <- SpatialPoints(coords)
  exp <- data.frame(long = c(1, 2, 3, 4), lat = c(5, 6, 7, 8))

  res <- fortify(sp)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('long', 'lat'))
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPoints')

  res <- fortify(sp, rename = FALSE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('x', 'y'))
  expect_equal(res$x, exp$long)
  expect_equal(res$y, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPoints')

  # SpatialPointsDataFrame
  data <- data.frame(city = c('a', 'b', 'c', 'd'))
  spdf <- SpatialPointsDataFrame(coords, data)

  res <- fortify(spdf)
  exp <- data.frame(city = c('a', 'b', 'c', 'd'),
                    long = c(1, 2, 3, 4), lat = c(5, 6, 7, 8))
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('city', 'long', 'lat'))
  expect_equal(res$city, exp$city)
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPointsDataFrame')

  coordinates(data) <- cbind(x, y)
  expect_true(is(data, 'SpatialPointsDataFrame'))
  res <- fortify(data, rename = FALSE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('city', 'x', 'y'))
  expect_equal(res$city, exp$city)
  expect_equal(res$x, exp$long)
  expect_equal(res$y, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPointsDataFrame')

  res <- fortify(data)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('city', 'long', 'lat'))
  expect_equal(res$city, exp$city)
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPointsDataFrame')

  # coordinates with custom names
  data <- data.frame(city = c('a', 'b', 'c', 'd'),
                     mylon = c(1, 2, 3, 4), mylat = c(5, 6, 7, 8))
  coordinates(data) <- ~ mylon + mylat
  res <- fortify(data, rename = FALSE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('city', 'mylon', 'mylat'))
  expect_equal(res$city, exp$city)
  expect_equal(res$mylon, exp$long)
  expect_equal(res$mylat, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPointsDataFrame')

  res <- fortify(data)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('city', 'long', 'lat'))
  expect_equal(res$city, exp$city)
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialPointsDataFrame')
})

test_that('test fortify.Line, Lines', {
  skip_if_not_installed("sp")
  library(sp)

  x1 <- c(1, 2, 3, 4)
  y1 <- c(5, 6, 7, 8)

  x2 <- c(11, 12, 13)
  y2 <- c(15, 16, 17)

  x3 <- c(9, 8, 7)
  y3 <- c(6, 5, 4)

  l1 <- Line(cbind(x1, y1))
  l2 <- Line(cbind(x2, y2))
  l3 <- Line(cbind(x3, y3))

  # defined in ggplot2::fortify.Line
  exp <- data.frame(long = c(1, 2, 3, 4), lat = c(5, 6, 7, 8),
                    order = c(1, 2, 3, 4))
  res <- fortify(l1)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)

  # Lines
  ls1 <- Lines(list(l1), ID = "ls1")
  ls23 <- Lines(list(l2, l3), ID = "ls23")

  # defined in ggplot2::fortify.Lines
  exp <- data.frame(long = c(1, 2, 3, 4), lat = c(5, 6, 7, 8),
                    order = c(1, 2, 3, 4),
                    piece = as.factor(c(1, 1, 1, 1)),
                    id = rep('ls1', 4),
                    group = as.factor(rep('ls1.1', 4)),
                    stringsAsFactors = FALSE)
  res <- fortify(ls1)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)

  exp <- data.frame(long = c(11, 12, 13, 9, 8, 7), lat = c(15, 16, 17, 6, 5, 4),
                    order = c(1, 2, 3, 4, 5, 6),
                    piece = as.factor(c(1, 1, 1, 2, 2, 2)),
                    id = rep('ls23', 6),
                    group = as.factor(rep(c('ls23.1', 'ls23.2'), c(3, 3))),
                    stringsAsFactors = FALSE)
  res <- fortify(ls23)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)

  # SpatialLines, result must have "base_class"
  sl1 <- SpatialLines(list(ls1))
  sl12 <- SpatialLines(list(ls1, ls23))

  res <- fortify(sl1)
  expect_true(is.data.frame(res))
  expect_equal(res$long, c(1, 2, 3, 4))
  expect_equal(res$lat, c(5, 6, 7, 8))
  expect_equal(res$order, c(1, 2, 3, 4))
  expect_equal(res$piece, as.factor(c(1, 1, 1, 1)))
  expect_equal(res$id, rep('ls1', 4))
  expect_equal(res$group, as.factor(rep('ls1.1', 4)))
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialLines')

  res <- fortify(sl12)
  expect_true(is.data.frame(res))
  expect_equal(res$long, c(1, 2, 3, 4, 11, 12, 13, 9, 8, 7))
  expect_equal(res$lat, c(5, 6, 7, 8, 15, 16, 17, 6, 5, 4))
  expect_equal(res$order, c(1, 2, 3, 4, 1, 2, 3, 4, 5, 6))
  expect_equal(res$piece, as.factor(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2)))
  expect_equal(res$id, rep(c('ls1', 'ls23'), c(4, 6)))
  expect_equal(res$group, as.factor(rep(c('ls1.1', 'ls23.1', 'ls23.2'), c(4, 3, 3))))
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialLines')

  # defined in ggplot2::fortify.SpatialLinesDataFrame
  sldf <- SpatialLinesDataFrame(sl12,
                                data.frame(Z = c("Road", "River"),
                                           row.names = c("ls1", "ls23")))
  res <- fortify(sl12)
  expect_true(is.data.frame(res))
  expect_equal(res$long, c(1, 2, 3, 4, 11, 12, 13, 9, 8, 7))
  expect_equal(res$lat, c(5, 6, 7, 8, 15, 16, 17, 6, 5, 4))
  expect_equal(res$order, c(1, 2, 3, 4, 1, 2, 3, 4, 5, 6))
  expect_equal(res$piece, as.factor(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2)))
  expect_equal(res$id, rep(c('ls1', 'ls23'), c(4, 6)))
  expect_equal(res$group, as.factor(rep(c('ls1.1', 'ls23.1', 'ls23.2'), c(4, 3, 3))))
  expect_equal(attr(res, 'base_class')[[1]], 'SpatialLines')
})

test_that('test fortify.Polygon, Polygons', {
  skip_if_not_installed("sp")
  library(sp)

  x1 <- c(1, 1, 2, 2, 1)
  y1 <- c(1, 2, 2, 1, 1)
  r1 <- cbind(x1, y1)
  p1 <- Polygon(r1)
  ps1 <- Polygons(list(p1), ID = "a")

  x2 <- c(3, 3, 4, 4, 3)
  y2 <- c(3, 4, 4, 3, 3)
  r2 <- cbind(x2, y2)
  p2 <- Polygon(r2)
  ps2 <- Polygons(list(p2), ID = "b")

  # defined in ggplot2
  exp <- data.frame(long = c(1, 1, 2, 2, 1),
                    lat = c(1, 2, 2, 1, 1),
                    order = c(1, 2, 3, 4, 5),
                    hole = rep(FALSE, 5))
  res <- fortify(p1)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)

  exp <- data.frame(long = c(1, 1, 2, 2, 1),
                    lat = c(1, 2, 2, 1, 1),
                    order = c(1, 2, 3, 4, 5),
                    hole = rep(FALSE, 5),
                    piece = as.factor(rep(1, 5)),
                    id = rep('a', 5),
                    group = as.factor(rep('a.1', 5)),
                    stringsAsFactors = FALSE)
  res <- fortify(ps1)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)

  # SpatialPolygons
  sps <- SpatialPolygons(list(ps1, ps2))
  # defined in ggplot2
  exp <- data.frame(long = c(1, 1, 2, 2, 1, 3, 3, 4, 4, 3),
                    lat = c(1, 2, 2, 1, 1, 3, 4, 4, 3, 3),
                    order = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
                    hole = rep(FALSE, 10),
                    piece = as.factor(rep(1, 10)),
                    id = rep(c('a', 'b'), c(5, 5)),
                    group = as.factor(rep(c('a.1', 'b.1'), c(5, 5))),
                    stringsAsFactors = FALSE)
  res <- fortify(sps)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)

  # SpatialPolugonsDataFrame
  spdf <- SpatialPolygonsDataFrame(sps, data.frame(N = c("one", "two"),
                                                   row.names = c("a", "b")))
  # defined in ggplot2
  exp <- data.frame(long = c(1, 1, 2, 2, 1, 3, 3, 4, 4, 3),
                    lat = c(1, 2, 2, 1, 1, 3, 4, 4, 3, 3),
                    order = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
                    hole = rep(FALSE, 10),
                    piece = as.factor(rep(1, 10)),
                    id = rep(c('a', 'b'), c(5, 5)),
                    group = as.factor(rep(c('a.1', 'b.1'), c(5, 5))),
                    stringsAsFactors = FALSE)
  res <- fortify(sps)
  expect_true(is.data.frame(res))
  expect_equal(res, exp)
})

test_that('test autoplot.SpatialPoints', {
  skip_if_not_installed("sp")
  library(sp)

  x <- c(1, 2, 3, 4)
  y <- c(5, 6, 7, 8)

  # SpatialPoints
  coords <- cbind(x, y)
  sp <- SpatialPoints(coords)

  p <- autoplot(sp)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$y, c(5, 6, 7, 8))
  expect_equal(ld$colour, rep('black', 4))
  expect_equal(ld$alpha, rep(NA, 4))

  data <- data.frame(col1 = c('a', 'b', 'a', 'a'))
  spdf <- SpatialPointsDataFrame(coords, data)

  p <- autoplot(spdf, colour = 'col1')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPoint'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$y, c(5, 6, 7, 8))
  expect_equal(ld$colour, c("#F8766D", "#00BFC4", "#F8766D", "#F8766D"))
  expect_equal(ld$alpha, rep(NA, 4))
})

test_that('test autoplot.Line, Lines', {
  skip_if_not_installed("sp")
  library(sp)

  x1 <- c(1, 2, 3, 4)
  y1 <- c(5, 6, 7, 8)

  x2 <- c(11, 12, 13)
  y2 <- c(15, 16, 17)

  x3 <- c(9, 8, 7)
  y3 <- c(6, 5, 4)

  l1 <- Line(cbind(x1, y1))
  l2 <- Line(cbind(x2, y2))
  l3 <- Line(cbind(x3, y3))

  ls1 <- Lines(list(l1), ID = "ls1")
  ls23 <- Lines(list(l2, l3), ID = "ls23")

  p <- autoplot(l1)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$y, c(5, 6, 7, 8))
  expect_equal(ld$colour, rep('black', 4))
  expect_equal(ld$alpha, rep(NA, 4))

  # Lines
  p <- autoplot(ls1)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$y, c(5, 6, 7, 8))
  expect_equal(ld$colour, rep('black', 4))
  expect_equal(ld$alpha, rep(NA, 4))

  # Lines with group
  p <- autoplot(ls23)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(11, 12, 13, 7, 8, 9))
  expect_equal(ld$y, c(15, 16, 17, 4, 5, 6))
  expect_equal(ld$group, c(1, 1, 1, 2, 2, 2))
  expect_equal(ld$colour, rep('black', 6))
  expect_equal(ld$alpha, rep(NA, 6))

  # Lines with group
  p <- autoplot(ls23, colour = 'group')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(11, 12, 13, 7, 8, 9))
  expect_equal(ld$y, c(15, 16, 17, 4, 5, 6))
  expect_equal(ld$group, c(1, 1, 1, 2, 2, 2))
  expect_equal(ld$colour, rep(c("#F8766D", "#00BFC4"), c(3, 3)))
  expect_equal(ld$alpha, rep(NA, 6))
  # add layer
  p <- autoplot(ls1, p = p)
  expect_equal(length(p$layers), 2)
  expect_true(is(p$layers[[2]]$geom, 'GeomLine'))

  # SpatialLines, result must be the same as Lines,
  sl1 <- SpatialLines(list(ls1))
  sl12 <- SpatialLines(list(ls1, ls23))

  p <- autoplot(sl1)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$y, c(5, 6, 7, 8))
  expect_equal(ld$colour, rep('black', 4))
  expect_equal(ld$alpha, rep(NA, 4))

  p <- autoplot(sl12, colour = 'group')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x,  c(1, 2, 3, 4, 11, 12, 13, 7, 8, 9))
  expect_equal(ld$y, c(5, 6, 7, 8, 15, 16, 17, 4, 5, 6))
  expect_equal(ld$group, c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3))
  expect_equal(ld$colour, rep(c("#F8766D", "#00BA38", "#619CFF"), c(4, 3, 3)))
  expect_equal(ld$alpha, rep(NA, 10))

  # defined in ggplot2::fortify.SpatialLinesDataFrame
  sldf <- SpatialLinesDataFrame(sl12,
                                data.frame(Z = c("Road", "River"),
                                           row.names = c("ls1", "ls23")))
  p <- autoplot(sldf, colour = 'group')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x,  c(1, 2, 3, 4, 11, 12, 13, 7, 8, 9))
  expect_equal(ld$y, c(5, 6, 7, 8, 15, 16, 17, 4, 5, 6))
  expect_equal(ld$group, c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3))
  expect_equal(ld$colour, rep(c("#F8766D", "#00BA38", "#619CFF"), c(4, 3, 3)))
  expect_equal(ld$alpha, rep(NA, 10))

  p <- autoplot(sldf, colour = 'Z')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x,  c(1, 2, 3, 4, 11, 12, 13, 7, 8, 9))
  expect_equal(ld$y, c(5, 6, 7, 8, 15, 16, 17, 4, 5, 6))
  expect_equal(ld$group, c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3))
  expect_equal(ld$colour, rep(c("#00BFC4", "#F8766D"), c(4, 6)))
  expect_equal(ld$alpha, rep(NA, 10))
})

test_that('test autoplot.Polygon, Polygons', {
  skip_if_not_installed("sp")
  library(sp)

  x1 <- c(1, 1, 2, 2, 1)
  y1 <- c(1, 2, 2, 1, 1)
  r1 <- cbind(x1, y1)
  p1 <- Polygon(r1)
  ps1 <- Polygons(list(p1), ID = "a")

  x2 <- c(3, 3, 4, 4, 3)
  y2 <- c(3, 4, 4, 3, 3)
  r2 <- cbind(x2, y2)
  p2 <- Polygon(r2)
  ps2 <- Polygons(list(p2), ID = "b")

  p <- autoplot(p1)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPolygon'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 1, 2, 2, 1))
  expect_equal(ld$y, c(1, 2, 2, 1, 1))
  expect_equal(ld$colour, rep('black', 5))
  expect_equal(ld$fill, rep('grey20', 5))
  expect_equal(ld$alpha, rep(NA, 5))

  p <- autoplot(ps1)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPolygon'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(1, 1, 2, 2, 1))
  expect_equal(ld$y, c(1, 2, 2, 1, 1))
  expect_equal(ld$colour, rep('black', 5))
  expect_equal(ld$fill, rep('grey20', 5))
  expect_equal(ld$alpha, rep(NA, 5))

  # SpatialPolygons
  sps <- SpatialPolygons(list(ps1, ps2))

  p <- autoplot(sps)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPolygon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x, c(1, 1, 2, 2, 1, 3, 3, 4, 4, 3))
  expect_equal(ld$y, c(1, 2, 2, 1, 1, 3, 4, 4, 3, 3))
  expect_equal(ld$colour, rep('black', 10))
  expect_equal(ld$fill, rep('grey20', 10))
  expect_equal(ld$alpha, rep(NA, 10))

  p <- autoplot(sps, fill = 'group')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPolygon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x, c(1, 1, 2, 2, 1, 3, 3, 4, 4, 3))
  expect_equal(ld$y, c(1, 2, 2, 1, 1, 3, 4, 4, 3, 3))
  expect_equal(ld$colour, rep('black', 10))
  expect_equal(ld$fill, rep(c('#F8766D', '#00BFC4'), c(5, 5)))
  expect_equal(ld$alpha, rep(NA, 10))

  # SpatialPolugonsDataFrame
  spdf <- SpatialPolygonsDataFrame(sps, data.frame(N = c("one", "two"),
                                                   row.names = c("a", "b")))

  p <- autoplot(spdf)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPolygon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x, c(1, 1, 2, 2, 1, 3, 3, 4, 4, 3))
  expect_equal(ld$y, c(1, 2, 2, 1, 1, 3, 4, 4, 3, 3))
  expect_equal(ld$colour, rep('black', 10))
  expect_equal(ld$fill, rep('grey20', 10))
  expect_equal(ld$alpha, rep(NA, 10))

  p <- autoplot(spdf, fill = 'N')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPolygon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$x, c(1, 1, 2, 2, 1, 3, 3, 4, 4, 3))
  expect_equal(ld$y, c(1, 2, 2, 1, 1, 3, 4, 4, 3, 3))
  expect_equal(ld$colour, rep('black', 10))
  expect_equal(ld$fill, rep(c('#F8766D', '#00BFC4'), c(5, 5)))
  expect_equal(ld$alpha, rep(NA, 10))
})
