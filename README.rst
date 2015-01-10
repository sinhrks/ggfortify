
.. image:: https://travis-ci.org/sinhrks/ggfortify.svg?branch=master
    :target: https://travis-ci.org/sinhrks/ggfortify

ggfortify
=========

Define ``fortify`` and ``autoplot`` functions to allow ``ggplot2`` to handle some popular R packages.

This covers following classes:

- ``base::matrix``
- ``base::table`` (supports ``fortify`` only)
- ``cluster::clara``
- ``cluster::fanny``
- ``cluster::pam``
- ``changepoint::cpt``
- ``fGarch::fGARCH``
- ``forecast::bats``
- ``forecast::forecast``
- ``forecast::ets``
- ``fracdiff::fracdiff``
- ``MASS::isoMDS``  (indirectly, see the `doc <http://rpubs.com/sinhrks/plot_mds>`_)
- ``MASS::sammon``  (indirectly, see the `doc <http://rpubs.com/sinhrks/plot_mds>`_)
- ``stats::acf``
- ``stats::ar``
- ``stats::Arima``
- ``stats::cmdscale`` (indirectly, see the `doc <http://rpubs.com/sinhrks/plot_mds>`_)
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

* `Plotting Diagnostics for LM and GLM with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_lm>`_
* `Plotting Time Series with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_ts>`_
* `Plotting Time Series Statistics with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_tsstats>`_
* `Plotting PCA/clustering results using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_pca>`_
* `Plotting Multidimensional Scaling using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_mds>`_
* `Plotting Survival Curves using ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_surv>`_
* `Plotting Probability Distributions with ggplot2 and ggfortify <http://rpubs.com/sinhrks/plot_dist>`_