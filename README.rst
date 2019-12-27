
.. image:: https://travis-ci.org/sinhrks/ggfortify.svg?branch=master
    :target: https://travis-ci.org/sinhrks/ggfortify
.. image:: https://coveralls.io/repos/sinhrks/ggfortify/badge.svg?branch=master&service=github
    :target: https://coveralls.io/github/sinhrks/ggfortify?branch=master
.. image:: http://www.r-pkg.org/badges/version/ggfortify
    :target: https://cran.r-project.org/web/packages/ggfortify/index.html

ggfortify
=========

This package offers ``fortify`` and ``autoplot`` functions to allow automatic ``ggplot2`` to visualize statistical result of popular R packages. Check out our `R Journal paper <https://journal.r-project.org/archive/2016-2/tang-horikoshi-li.pdf>`_ for more details on the architecture. There's also `autoplotly package <https://github.com/terrytangyuan/autoplotly>`_ that could automatically generate interactive visualizations with `plotly.js <https://plot.ly/>`_ style.

**Note**: For functions which returns ``list``, ``ggfortify`` tries to infer a background class using its attribute names. Such functions are marked as "(inference)".

Installation
------------

- Install the latest stable release from CRAN: ::

    install.packages('ggfortify')

- Install the development version from Github: ::

    if (!require("devtools")) install.packages("devtools")
    install_github('sinhrks/ggfortify')

Examples
--------

* `Concepts and Basics of ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/basics.html>`_
* `Plotting Diagnostics for LM and GLM with ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html>`_
* `Plotting Time Series with ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html>`_
* `Plotting PCA, clustering, LFDA and MDS <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html>`_
* `Plotting Survival Curves using ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_surv.html>`_
* `Plotting Probability Distributions with ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_dist.html>`_
* `Automatic Generation of Interactive Visualizations in ggplot2 and plotly Styles <https://terrytangyuan.github.io/2018/02/12/autoplotly-intro/>`_
* `一行R代码实现繁琐的可视化 <http://terrytangyuan.github.io/2015/11/24/ggfortify-intro/>`_

Reference/Citation
------------

To cite ggfortify in publications, please use (available via ``citation("ggfortify")``):

  Yuan Tang, Masaaki Horikoshi, and Wenxuan Li (2016). ggfortify: Unified Interface to Visualize
  Statistical Result of Popular R Packages. The R Journal, 8.2, 478-489.

  Masaaki Horikoshi and Yuan Tang (2016). ggfortify: Data Visualization Tools for Statistical
  Analysis Results. https://CRAN.R-project.org/package=ggfortify

Coverage
-----------

This covers following classes:

- ``base::matrix``
- ``base::table`` (supports ``fortify`` only)
- ``cluster::clara``
- ``cluster::fanny``
- ``cluster::pam``
- ``changepoint::cpt``
- ``dlm::dlmFilter``
- ``dlm::dlmSmooth`` (inference)
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

Helper Functions
----------------

- ``ggdistribution`` to plot PDF/CDF
- ``ggcpgram`` to plot ``cpgram``
- ``ggtsdiag`` to plot ``tsdiag``
- ``ggfreqplot`` to generalize ``monthplot``

ggplot2 Families
----------------

There are some useful plotting packages using ggplot2. ``ggfortify`` will not focus on area already covered by these packages.

* `GGally <http://cran.r-project.org/web/packages/GGally/index.html>`_
* `dendextend:ggdend <http://cran.r-project.org/web/packages/dendextend/index.html>`_
* `ggRandomForests <http://cran.r-project.org/web/packages/ggRandomForests/>`_
* `ggmcmc <http://cran.r-project.org/web/packages/ggmcmc/index.html>`_
