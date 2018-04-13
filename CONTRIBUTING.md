# Contributing guidelines

## How to become a contributor and submit your own code

### Contributing code

Make sure you understand how the code base is organized before you contribute any code. Check out our [R Journal paper](https://journal.r-project.org/archive/2016-2/tang-horikoshi-li.pdf) for more details on the architecture. Basically, if you want to extend the functionalities to more classes, you need to implement the following S3 generic functions for the classes you want to support:

* `autoplot()`, which enables plotting a custom object with `ggplot2`, and
* `fortify()`, which enables converting a custom object to a tidy `data.frame`

For example, if you want to implement visualizations for `some_r_class` objects, you need to implement the following:

* `fortify.some_r_class()` to extract neccessary information into a `data.frame` object
* `autoplot.some_r_class()` that takes what `fortify.some_r_class()` returns and create suitable visualization using `ggplot2` syntax


### Contribution guidelines and standards

Before sending your pull request for
[review](https://github.com/sinhrks/ggfortify/pulls),
make sure your changes are consistent with the guidelines and follow the
coding style described in this document.

#### General guidelines and philosophy for contribution

* Include unit tests when you contribute new features, as they help to
  a) prove that your code works correctly, and b) guard against future breaking
  changes to lower the maintenance cost.
* Bug fixes also generally require unit tests, because the presence of bugs
  usually indicates insufficient test coverage.

#### Unit tests and Continuous integration

* Run `devtools::test("pathToPackage")` to execute all the unit tests locally.
* Unit tests are triggered automatically on [Travis CI](https://travis-ci.org) so you should be able to locate your test result [here](https://travis-ci.org/sinhrks/ggfortify/pull_requests). 

#### Coding style

We are following Google's R style guide that can be found [here](https://google.github.io/styleguide/Rguide.xml).
