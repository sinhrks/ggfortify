
.. image:: https://travis-ci.org/sinhrks/ggfortify.svg?branch=master
    :target: https://travis-ci.org/sinhrks/ggfortify

ggfortify
=========

Define ``fortify`` and ``autoplot`` functions to allow ``ggplot2`` to handle some popular R packages.

This covers following classes:

- ``base::matrix``
- ``base::table``
- ``changepoint::cpt``
- ``forecast::bats``
- ``forecast::forecast``
- ``forecast::ets``
- ``fracdiff::fracdiff``
- ``stats::acf``
- ``stats::ar``
- ``stats::Arima``
- ``stats::decomposed.ts``
- ``stats::factanal``
- ``stats::HoltWinters``
- ``stats::kmeans``
- ``stats::prcomp``
- ``stats::princomp``
- ``stats::spec``
- ``stats::stl``
- ``stats::ts``
- ``survival::survfit``
- ``survival::survfit.cox``
- ``timeSeries::timeSeries``
- ``tseries::irts``
- ``vars::varprd``
- ``xts::xts``

Helper Functions
----------------

- `ggdistribution` to plot CDF/PDF


Examples
--------

* `Plotting Time Series with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_ts>`_
* `Plotting PCA/clustering results using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_pca>`_
* `Plotting Survival Curves using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_surv>`_