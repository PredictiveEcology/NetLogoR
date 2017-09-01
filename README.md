# NetLogoR

[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/k65nup6cuqr5p2hy/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/netlogor/branch/master)
[![Build Status](https://travis-ci.org/PredictiveEcology/NetLogoR.svg?branch=master)](https://travis-ci.org/PredictiveEcology/NetLogoR)
[![Coverage Status](https://coveralls.io/repos/PredictiveEcology/NetLogoR/badge.svg?branch=master)](https://coveralls.io/r/PredictiveEcology/NetLogoR?branch=master)

## A Port of NetLogo Functions and Language to R

Easily create agent-based models in R following the NetLogo framework ([Wilensky, 1999](http://ccl.northwestern.edu/netlogo/)).
`NetLogoR` provides classes to represent "patches" (raster cells) and "turtles" (individuals), the necessary [NetLogo primitives](https://ccl.northwestern.edu/netlogo/docs/dictionary.html), and complementary functions to easily build agent-based models or translate NetLogo models in R.


## Getting Started

A programming guide derived from the [NetLogo Programming Guide](https://ccl.northwestern.edu/netlogo/docs/programming.html) and a dictionary of [NetLogo primitives](https://ccl.northwestern.edu/netlogo/docs/dictionary.html) equivalences are available.
As examples, three NetLogo models ([Ants](http://ccl.northwestern.edu/netlogo/models/Ants), Butterfly (Railsback and Grimm, 2012) and [Wolf-Sheep-Predation](http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation)) were translated in R language using `NetLogoR`.

## Installing the R package

To do this, you will require the devtools package.

```r
library(devtools)
install_github("PredictiveEcology/NetLogoR")
```
