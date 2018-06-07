# NetLogoR

[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/k65nup6cuqr5p2hy/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/netlogor/branch/master)
[![Build Status](https://travis-ci.org/PredictiveEcology/NetLogoR.svg?branch=master)](https://travis-ci.org/PredictiveEcology/NetLogoR)
[![Coverage Status](https://coveralls.io/repos/PredictiveEcology/NetLogoR/badge.svg?branch=master)](https://coveralls.io/r/PredictiveEcology/NetLogoR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/NetLogoR)](https://cran.r-project.org/package=NetLogoR)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/NetLogoR)](https://cran.r-project.org/package=NetLogoR)

## Build and run spatially explicit agent-based models in R

`NetLogoR` is an R package to build and run spatially explicit agent-based models using only the R platform.
It follows the same framework as NetLogo ([Wilensky, 1999](http://ccl.northwestern.edu/netlogo/)) and is a translation in R language of the structure and functions of NetLogo ([NetLogo primitives](https://ccl.northwestern.edu/netlogo/docs/dictionary.html)).
`NetLogoR` provides new R classes to define model agents and functions to implement spatially explicit agent-based models in the R environment.
This package allows benefiting of the fast and easy coding phase from the highly developed NetLogo's framework, coupled with the versatility, power and massive resources of the R software.

## Getting Started

Examples of three models ([Ants](http://ccl.northwestern.edu/netlogo/models/Ants), Butterfly (Railsback and Grimm, 2012) and [Wolf-Sheep-Predation](http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation)) written using `NetLogoR` are available. The NetLogo code of the original version of these models is provided alongside.
A programming guide inspired from the [NetLogo Programming Guide](https://ccl.northwestern.edu/netlogo/docs/programming.html) and a dictionary of [NetLogo primitives](https://ccl.northwestern.edu/netlogo/docs/dictionary.html) equivalences are also available. 

## Installing `NetLogoR`

### From GitHub

```r
#install.packages("devtools")
devtools::install_github("PredictiveEcology/NetLogoR")
```
