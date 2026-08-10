# NetLogoR 1.0.6.9000

## Dependency changes
* removed dependency: `quickPlot`, which is no longer maintained (#54). `NetLogoR` now defines its own `coordinates()`, `extent()`, `layerNames()`, and `numLayers()` generics, so these continue to work unchanged. As a consequence, `NetLogoR` objects can no longer be plotted with `quickPlot::Plot()`; use `plot()` and `points()`, which draw through `terra`.
* added dependency: `dbscan`, used to speed up the radius searches in `inRadius()`;
* `inRadius()` and `inCone()` no longer require `sf`, which is now only needed by `sf2turtles()` and `turtles2sf()`;

## Enhancements
* speed improvements to `NLset()`, `NLwith()`, `inRadius()`, `of()`, and `turtle()`, achieved by indexing the underlying `matrix`/`array` directly instead of going through coordinate-conversion helpers.

## Deprecations
* the `sp` package is being retired in favour of `sf`, so everything in `NetLogoR` that still depends on it is now deprecated and will be removed in a future release. This affects `spdf2turtles()` (use `sf2turtles()`), `turtles2spdf()` (use `turtles2sf()`), `bbox()` on `sp` and `raster` objects, `wrap()` on `SpatialPoints` and `SpatialPointsDataFrame` objects, and passing a `SpatialPointsDataFrame` to `agentMatrix()`. The full transition to `sf` will follow in a separate release.

## Bugfixes
* `[` on a `worldArray` assumed the array had exactly two layers, so extracting patch values from a `worldArray` with any other number of layers failed with "dims [product 2] do not match the length of object". It now uses the actual number of layers.
* stacking a `worldMatrix` of character values alongside a numeric one no longer corrupts both layers (#49). An `array` holds a single type, so the character values used to turn every layer into character, and were then coerced back to `NA` on the way out. `worldArray` now stores character layers as integer codes with their categories in a new `levels` slot, exactly as `agentMatrix` already does for character columns. `of()` reports the character values, `plot()` draws the layer as a categorical raster instead of warning "NAs introduced by coercion", `NLwith()` and `NLset()` accept and return the characters, and `[[` and `$` give back a character `worldMatrix`. Numeric layers stacked alongside a character one keep their own type.
* `inRadius()` with `torus = TRUE` no longer reports patches that were not among the `agents2` supplied;
* `of()` on an `agentMatrix` now returns columns in the order given by `var` when a mix of factor and numeric variables is requested.

## Documentation changes
* the plot methods are documented as producing a `SpatRaster` via `terra::plot()`, which is what they have done since `terra` replaced `raster`;
* `bbox()` and `extent()` now cross-reference `terra::ext()` rather than `sp::bbox()` and `raster::extent()`;
* removed the unused internal `.projNowhere` object, which described a `sp`/`raster` buffering workflow the package no longer has.

# NetLogoR 1.0.6

## Dependency changes
* drop support for R 4.1 and 4.2 due to changes in dependency packages;
* remove unmaintained dependency package `fastshp` (#55);
* update NetLogo website URLs;

## Documentation changes
* add package anchors to help files (#53);

# NetLogoR 1.0.5

## Dependency changes
* moved dependencies: `sp` and `raster` to Suggests.
  There is still the possibility to convert `sp` and `raster` objects into `NetLogoR` objects
  and _vice versa_, but except for these functions, all the others do not rely anymore on
  `sp` and `raster` objects and functions.
* added dependency: `terra`;
* many changes to accommodate these connections to `terra`;

## Documentation changes
* several minor fixes for CRAN requirements;

## Bugfixes
* several minor, including some speed improvements;
* several corrections in documentation;
* Deal with CRAN incoming check: "Running R code in ... had CPU time 3 times elapsed time" by setting OMP_THREAD_LIMIT = 1 during tests on CRAN;

# NetLogoR 0.3.11

## Dependency changes
* removed dependency: `rgeos`;
* added dependency: `sf`;

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
