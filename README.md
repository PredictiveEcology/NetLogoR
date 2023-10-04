# NetLogoR

<!-- badges: start -->
[![R build status](https://github.com/PredictiveEcology/NetLogoR/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/NetLogoR/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/NetLogoR)](https://cran.r-project.org/package=NetLogoR)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/NetLogoR)](https://cran.r-project.org/package=NetLogoR)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/NetLogoR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/NetLogoR?branch=master)
<!-- badges: end -->

## Build and run spatially explicit agent-based models in R

`NetLogoR` is an R package to build and run spatially explicit agent-based models using only the R platform ([Bauduin et al., 2019](https://onlinelibrary.wiley.com/doi/10.1111/ecog.04516)).
It follows the same framework as NetLogo ([Wilensky, 1999](http://ccl.northwestern.edu/netlogo/)) and is a translation in R language of the structure and functions of NetLogo ([NetLogo primitives](https://ccl.northwestern.edu/netlogo/docs/dictionary.html)).
`NetLogoR` provides new R classes to define model agents and functions to implement spatially explicit agent-based models in the R environment.
This package allows benefiting of the fast and easy coding phase from the highly developed NetLogo's framework, coupled with the versatility, power and massive resources of the R software.

## Getting Started

Examples of three models ([Ants](http://ccl.northwestern.edu/netlogo/models/Ants), Butterfly (Railsback and Grimm, 2012) and [Wolf-Sheep-Predation](http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation)) written using `NetLogoR` are available. The NetLogo code of the original version of these models is provided alongside.
A programming guide inspired from the [NetLogo Programming Guide](https://ccl.northwestern.edu/netlogo/docs/programming.html) and a dictionary of [NetLogo primitives](https://ccl.northwestern.edu/netlogo/docs/dictionary.html) equivalences are also available.
A model simulating the wolf life cycle written using `NetLogoR` has been published ([Bauduin et al., 2020](https://www.sciencedirect.com/science/article/pii/S0304380020302799?via%3Dihub)) with the ([code available on GitHub](https://github.com/SarahBauduin/appendix_wolfIBM)).

## Installing `NetLogoR`

### From CRAN

Currently, the package is not on CRAN due to some dependencies that were kicked off CRAN. It will be three soon.

In the mean time, please use:
```r
# install.packages("NetLogoR")
install.packages("NetLogoR", repos = c(https://predictiveecology.r-universe.dev, getOption("repos")))
```

### From GitHub

```r
#install.packages("devtools")
devtools::install_github("PredictiveEcology/NetLogoR")
```

## Getting help

We have created a Google group for users to get help implementing their models using the package.
Please see the discussions at <https://groups.google.com/g/netlogor>.
