
.. image:: https://travis-ci.org/sinhrks/ggfortify.svg?branch=master
    :target: https://travis-ci.org/sinhrks/ggfortify

ggfortify
=========

Define ``fortify`` and ``autoplot`` functions to allow ``ggplot2`` to handle some popular R packages.

**NOTE** For functions which returns ``list``, ``ggfortify`` tries to infer a background class using its attributes. Such functions are marked as "(inference)".


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
- ``KFAS::KFS``
- ``KFAS::signal`` (inference)
- ``MASS::isoMDS`` (inference)
- ``MASS::sammon`` (inference)
- ``stats::acf``
- ``stats::ar``
- ``stats::Arima``
- ``stats::cmdscale`` (inference, see the `doc <http://rpubs.com/sinhrks/plot_mds>`_)
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

Helper Functions
----------------

- ``ggdistribution`` to plot PDF/CDF
- ``ggcpgram`` to plot ``cpgram``
- ``gglagplot`` to plot ``lag.plot``
- ``ggtsdiag`` to plot ``tsdiag``
- ``ggfreqplot`` to generalize ``monthplot``

Examples
--------

* `Concepts and Basics of ggfortify <http://rpubs.com/sinhrks/basics>`_
* `Plotting Diagnostics for LM and GLM with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_lm>`_
* `Plotting Time Series with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_ts>`_
* `Plotting Time Series Statistics with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_tsstats>`_
* `Plotting State Space Time Series with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_ts_dlm>`_
* `Plotting PCA/clustering results using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_pca>`_
* `Plotting Multidimensional Scaling using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_mds>`_
* `Plotting Survival Curves using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_surv>`_
* `Plotting Probability Distributions with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_dist>`_
