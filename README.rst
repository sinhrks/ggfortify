
.. image:: https://travis-ci.org/sinhrks/ggfortify.svg?branch=master
    :target: https://travis-ci.org/sinhrks/ggfortify

ggfortify
=========

Define ``fortify`` and ``autoplot`` functions to allow ``ggplot2`` to handle some popular R packages.

This covers following classes:

- ``base::matrix``
- ``base::table``
- ``changepoint::cpt``
- ``fGarch::fGARCH``
- ``forecast::bats``
- ``forecast::forecast``
- ``forecast::ets``
- ``fracdiff::fracdiff``
- ``MSwM::MSM.lm``
- ``stats::acf``
- ``stats::ar``
- ``stats::Arima``
- ``stats::decomposed.ts``
- ``stats::density``
- ``stats::factanal``
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

* `Plotting Time Series with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_ts>`_
* `Plotting Time Series Statistics with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_tsstats>`_
* `Plotting PCA/clustering results using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_pca>`_
* `Plotting Survival Curves using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_surv>`_
* `Plotting Probability Distributions with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_dist>`_