## Test environments
* local OS X install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## Notes

Notable changes are:

* Added support of Silhouette plot for `cluster::silhouette` objects, thanks to @damirpolat.
* Fixed issue with `autoplot.lm(which = 2)` when residuals have `AsIs` type, thanks to @richierocks.
