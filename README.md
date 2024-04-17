**Note**: This package has been maintained by [@terrytangyuan](https://github.com/terrytangyuan) since 2015. Please [consider sponsoring](https://github.com/sponsors/terrytangyuan)!

[![cran](http://www.r-pkg.org/badges/version/ggfortify)](https://CRAN.R-project.org/package=ggfortify)
[![download](https://cranlogs.r-pkg.org/badges/grand-total/ggfortify)](https://CRAN.R-project.org/package=ggfortify)

# ggfortify

This package offers ``fortify`` and ``autoplot`` functions to allow automatic ``ggplot2`` to visualize statistical result of popular R packages. Check out our [R Journal paper] (https://journal.r-project.org/archive/2016-2/tang-horikoshi-li.pdf) for more details on the overall architecture design and a gallery of visualizations created with this package. Also check out [autoplotly package](https://github.com/terrytangyuan/autoplotly) that could automatically generate interactive visualizations with [plotly.js](https://plot.ly/) style based on ``ggfortify``. The generated visualizations can also be easily extended using ``ggplot2`` syntax while staying interactive.


## Installation

- Install the latest stable release from CRAN:
```
install.packages('ggfortify')
```

- Install the development version from Github:
```
if (!require("remotes")) install.packages("remotes")
  remotes::install_github('sinhrks/ggfortify')
```

## Examples

* [Concepts and Basics of ggfortify](https://cran.r-project.org/web/packages/ggfortify/vignettes/basics.html)
* [Plotting Diagnostics for LM and GLM with ggplot2 and ggfortify](https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html)
* [Plotting Time Series with ggplot2 and ggfortify](https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html)
* [Plotting PCA, clustering, LFDA and MDS](https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html)
* [Plotting Survival Curves using ggplot2 and ggfortify](https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_surv.html)
* [Plotting Probability Distributions with ggplot2 and ggfortify](https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_dist.html)
* [Automatic Generation of Interactive Visualizations in ggplot2 and plotly Styles](https://terrytangyuan.github.io/2018/02/12/autoplotly-intro/)
* [一行R代码实现繁琐的可视化](http://terrytangyuan.github.io/2015/11/24/ggfortify-intro/)

## Reference/Citation

To cite ggfortify in publications, please use (available via ``citation("ggfortify")``):
```
  Yuan Tang, Masaaki Horikoshi, and Wenxuan Li (2016). ggfortify: Unified Interface to Visualize
  Statistical Result of Popular R Packages. The R Journal, 8.2, 478-489.

  Masaaki Horikoshi and Yuan Tang (2016). ggfortify: Data Visualization Tools for Statistical
  Analysis Results. https://CRAN.R-project.org/package=ggfortify
```

## Coverage

This covers following classes:

- ``base::matrix``
- ``base::table`` (supports ``fortify`` only)
- ``cluster::clara``
- ``cluster::fanny``
- ``cluster::pam``
- ``cluster::silhouette``
- ``changepoint::cpt``
- ``fGarch::fGARCH``
- ``forecast::bats``
- ``forecast::forecast``
- ``forecast::ets``
- ``forecast::nnetar``
- ``fracdiff::fracdiff``
- ``glmnet::cv.glmnet``
- ``glmnet::glmnet``
- ``KFAS::KFS``
- ``KFAS::signal`` (inference)
- ``lfda::lfda``
- ``lfda::klfda``
- ``lfda::self``
- ``maps::map``
- ``MASS::isoMDS`` (inference)
- ``MASS::sammon`` (inference)
- ``raster::RasterBrick``
- ``raster::RasterCommon``
- ``raster::RasterLayer``
- ``raster::RasterStack``
- ``ROCR::performance``
- ``sp::Line``
- ``sp::Lines``
- ``sp::Polygon``
- ``sp::Polygons``
- ``sp::SpatialLines``
- ``sp::SpatialLinesDataFrame``
- ``sp::SpatialPoints``
- ``sp::SpatialPointsDataFrame``
- ``sp::SpatialPolygons``
- ``sp::SpatialPolygonsDataFrame``
- ``splines::basis``
- ``stats::acf``
- ``stats::ar``
- ``stats::Arima``
- ``stats::cmdscale`` (inference)
- ``stats::decomposed.ts``
- ``stats::density``
- ``stats::factanal``
- ``stats::glm``
- ``stats::HoltWinters``
- ``stats::kmeans``
- ``stats::lm``
- ``stats::prcomp``
- ``stats::princomp``
- ``stats::spec``
- ``stats::stepfun``
- ``stats::stl``
- ``stats::ts``
- ``survival::survfit``
- ``survival::survfit.cox``
- ``survival::survfitms``
- ``strucchange::breakpoints``
- ``strucchange::breakpointsfull``
- ``timeSeries::timeSeries``
- ``tseries::irts``
- ``vars::varprd``
- ``xts::xts``
- ``zoo::zooreg``

## Helper Functions

- ``ggdistribution`` to plot PDF/CDF
- ``ggcpgram`` to plot ``cpgram``
- ``ggtsdiag`` to plot ``tsdiag``
- ``ggfreqplot`` to generalize ``monthplot``
