Known issues: https://github.com/PredictiveEcology/NetLogoR/issues

version 0.3.0
=============
* `inRadius` now multiplies the `width` by a tiny amount so that the function returns an inclusive result.
* add `quickPlot >= 0.1.1.9000` dependency
* update dependencies on `SpaDES`-related packages to only include the ones actually used.
* Define [[ and $ for `worldArray` to extract subset layers.
* Add `show` methods for `worldMatrix` and `worldArray`, similar to `RasterLayer` and `RasterStack`
* Add tools so `quickPlot::Plot` works, e.g., `Plot(agentMatrixObj)` or `Plot(worldMatrixObj)` or `Plot(worldArrayObj)`, `Plot(worldArrayObj$layer1)`. `addTo` argument implemented for layering, `Plot(worldMatrixObj); Plot(agentMatrixObj, addTo="worldMatrixObj")` will plot the agents on the map.

version 0.2.0
=============
* minimum R version increased from `3.2.5` to `3.3.0` as required by dependency `RandomFieldsUtils`.

version 0.1.0
=============
* all key NetLogo functions rewritten in R, except visualizations and "links" agents
* adapted plot methods
