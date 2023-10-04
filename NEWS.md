Known issues: <https://github.com/PredictiveEcology/NetLogoR/issues>

# NetLogoR 0.3.11.9000

## Dependency changes
* removed dependency: `sp` and `raster`. There is still the possibility to convert `sp` and `raster` objects into `NetLogoR` objects and vice versa but except for these functions, all the others do not rely anymore on `sp` and `raster` objects and functions.
* added dependency: `terra`

# NetLogoR 0.3.11

## Dependency changes
* removed dependency: `rgeos`
* added dependency: `sf`

# NetLogoR 0.3.10

## Dependency changes
* removed support for R 3.6. To our knowledge, `NetLogoR` will still work with R 3.6, but we no longer support it.
* removed dependencies: `car`, `Hmisc`, `plyr`

## Bugfixes
* using sample in several agent functions would have given the wrong sample (see `?sample`) when the length of the argument `x` is 1.
  This occurred under some (but now all) conditions where there are multiple agents with the same `id`, along with some agents with only one member within an `id`, and where the objective is to select one of the using `oneOf` or `nOf(agent, n = 1)`. Now we use `resample` as defined in the examples of `?base::sample`.
* minor bugfixes

# NetLogoR 0.3.9

## Bugfixes
* Fix for GDAL/PROJ crs issue (#43, @rsbivand)

# NetLogoR 0.3.8

* Drop support for R 3.5 (several dependencies require R >= 3.6)
* `NLwith()` now handles `NA` values (#36)
* fixed bug in Programming Guide vignette (@DataStrategist, #42)
* improved documentation
* added link to new discussion forum: <https://groups.google.com/g/netlogor>

# NetLogoR 0.3.7

* Updates to fix documentation and CRAN check problems
* change maintainer to Eliot

# NetLogoR 0.3.6

* R 3.5 is now minimum R version required. Too many dependencies are not maintaining their backwards compatibility.
* Added new citation for the Ecography paper describing the package.

# NetLogoR 0.3.5

* Updates to fix CRAN check problems.

# NetLogoR 0.3.4

* Set random seed in two tests.

# NetLogoR 0.3.3

* Add `sf` to Suggests, as it is used in tests.

# NetLogoR 0.3.2

* Fix bug in `turtlesOn()`. Error when the world was not square.
* Fix use of suggested packages in tests.

# NetLogoR 0.3.1

* First CRAN release.

# NetLogoR 0.3.0

* `inRadius` now multiplies the `width` by a tiny amount so that the function returns an inclusive result.
* Add `quickPlot >= 0.1.1.9000` dependency.
* Update dependencies on `SpaDES`-related packages to only include the ones actually used.
* Define [[ and $ for `worldArray` to extract subset layers.
* Add `show` methods for `worldMatrix` and `worldArray`, similar to `RasterLayer` and `RasterStack`.
* Add tools so `quickPlot::Plot` works, e.g., `Plot(agentMatrixObj)` or `Plot(worldMatrixObj)` or `Plot(worldArrayObj)`, `Plot(worldArrayObj$layer1)`. `addTo` argument implemented for layering, `Plot(worldMatrixObj); Plot(agentMatrixObj, addTo="worldMatrixObj")` will plot the agents on the map.

# NetLogoR 0.2.0

* Minimum R version increased from `3.2.5` to `3.3.0` as required by dependency `RandomFieldsUtils`.

# NetLogoR 0.1.0

* All key NetLogo functions rewritten in R, except visualizations and "links" agents.
* Adapted plot methods.
