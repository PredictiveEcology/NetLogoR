Known issues: <https://github.com/PredictiveEcology/NetLogoR/issues>

Version 0.3.9
=============
* none

# Bugfixes
* Fix for issue #43 opened by Roger Bivand.

Version 0.3.8
=============
* Drop support for R 3.5 (several dependencies require R >= 3.6)
* `NLwith()` now handles `NA` values (#36)
* fixed bug in Programming Guide vignette (@DataStrategist, #42)
* improved documentation
* added link to new discussion forum: <https://groups.google.com/g/netlogor>

Version 0.3.7
=============
* Updates to fix documentation and CRAN check problems
* change maintainer to Eliot

Version 0.3.6
=============
* R 3.5 is now minimum R version required. Too many dependencies are not maintaining their backwards compatibility.
* Added new citation for the Ecography paper describing the package.

Version 0.3.5
=============
* Updates to fix CRAN check problems.

Version 0.3.4
=============
* Set random seed in two tests.

Version 0.3.3
=============
* Add `sf` to Suggests, as it is used in tests.

Version 0.3.2
=============
* Fix bug in `turtlesOn()`. Error when the world was not square.
* Fix use of suggested packages in tests.

Version 0.3.1
=============
* First CRAN release.

Version 0.3.0
=============
* `inRadius` now multiplies the `width` by a tiny amount so that the function returns an inclusive result.
* Add `quickPlot >= 0.1.1.9000` dependency.
* Update dependencies on `SpaDES`-related packages to only include the ones actually used.
* Define [[ and $ for `worldArray` to extract subset layers.
* Add `show` methods for `worldMatrix` and `worldArray`, similar to `RasterLayer` and `RasterStack`.
* Add tools so `quickPlot::Plot` works, e.g., `Plot(agentMatrixObj)` or `Plot(worldMatrixObj)` or `Plot(worldArrayObj)`, `Plot(worldArrayObj$layer1)`. `addTo` argument implemented for layering, `Plot(worldMatrixObj); Plot(agentMatrixObj, addTo="worldMatrixObj")` will plot the agents on the map.

Version 0.2.0
=============
* Minimum R version increased from `3.2.5` to `3.3.0` as required by dependency `RandomFieldsUtils`.

Version 0.1.0
=============
* All key NetLogo functions rewritten in R, except visualizations and "links" agents.
* Adapted plot methods.
