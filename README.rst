
.. image:: https://travis-ci.org/sinhrks/ggfortify.svg?branch=master
    :target: https://travis-ci.org/sinhrks/ggfortify
.. image:: https://coveralls.io/repos/sinhrks/ggfortify/badge.svg?branch=master&service=github
    :target: https://coveralls.io/github/sinhrks/ggfortify?branch=master
.. image:: http://www.r-pkg.org/badges/version/ggfortify
    :target: https://cran.r-project.org/web/packages/ggfortify/index.html
.. image:: http://cranlogs.r-pkg.org/badges/ggfortify
    :target: http://cran.rstudio.com/package=ggfortify

ggfortify
=========

Define ``fortify`` and ``autoplot`` functions to allow ``ggplot2`` to handle some popular R packages.

**NOTE** For functions which returns ``list``, ``ggfortify`` tries to infer a background class using its attribute names. Such functions are marked as "(inference)".


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
- ``MASS::isoMDS`` (inference)
- ``MASS::sammon`` (inference)
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
- ``strucchange::breakpoints``
- ``strucchange::breakpointsfull``
- ``timeSeries::timeSeries``
- ``tseries::irts``
- ``vars::varprd``
- ``xts::xts``
- ``zoo::zooreg``

Installation
------------

- Install the latest stable release from CRAN: ::

    install.packages('ggfortify')

- Install the development version from Github: ::

    library(devtools)
    install_github('sinhrks/ggfortify')

Helper Functions
----------------

- ``ggdistribution`` to plot PDF/CDF
- ``ggcpgram`` to plot ``cpgram``
- ``gglagplot`` to plot ``lag.plot``
- ``ggtsdiag`` to plot ``tsdiag``
- ``ggfreqplot`` to generalize ``monthplot``

Examples
--------

* `Concepts and Basics of ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/basics.html>`_
* `Plotting Diagnostics for LM and GLM with ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html>`_
* `Plotting Time Series with ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html>`_
* `Plotting PCA, clustering, LFDA and MDS <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html>`_
* `Plotting Survival Curves using ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_surv.html>`_
* `Plotting Probability Distributions with ggplot2 and ggfortify <https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_dist.html>`_
* `Introduction (Chinese) <https://cran.r-project.org/web/packages/ggfortify/vignettes/intro_Chinese.html>`_

ggplot2 Families
----------------

There are some useful plotting packages using ggplot2. ``ggfortify`` will not focus on area already covered by these packages.

* `GGally <http://cran.r-project.org/web/packages/GGally/index.html>`_
* `ggdendro <http://cran.r-project.org/web/packages/ggdendro/index.html>`_
* `ggRandomForests <http://cran.r-project.org/web/packages/ggRandomForests/>`_
* `ggmcmc <http://cran.r-project.org/web/packages/ggmcmc/index.html>`_
