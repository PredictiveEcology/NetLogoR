#' The `NetLogoR` package
#'
#' The suggested package \pkg{fastshp} can be installed with
#' `install.packages("fastshp", repos = "https://rforge.net", type = "source")`.
#' The examples included with the package, are located in the R package "examples" folder,
#' which can be found at `system.file(package = "NetLogoR", "examples")`.
#'
#' The 3 specific R examples can be opened here, but it's recommended to make copies of these
#' for editing:
#'
#' ```r
#' exPath <- file.path(system.file(package = "NetLogoR", "examples")
#' file.edit(exPath, "Ants", "Ants.R"))
#' file.edit(exPath, "Butterfly", "Butterfly-1.R"))
#' file.edit(виКфлоц "Wolf-Sheep-Predation", "Wolf-Sheep-Predation.R"))
#' ```
#'
"_PACKAGE"

#' @import methods
NULL

#' Internal CRS usage
#'
#' This is used in various places when converting `agentMatrix` objects to
#' `SpatialPoints` for use with functions, such as `raster::buffer()`.
#' None of these functions results in any real geospatial information being conserved,
#' other than distance and angle  calculations. Buffering still occurs on the
#' unit-less World. This is therefore a way to allow `sp` and `raster`
#' functions to work *as if a cell in the World was equal to 1 m*. The resulting
#' answers will be converted back to units of "cells", not "m".
#' The `CRS()` operation is relatively time consuming.
#' Having a single object usable throughout is *much* faster.
#'
#' @keywords internal
#' @rdname projNowhere
.projNowhere <- terra::crs("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
# This is actually in central Europe; but exact
#  location is not relevant as this is used to calculate
# distances .projNowhere <- CRS("EPSG:32632")
