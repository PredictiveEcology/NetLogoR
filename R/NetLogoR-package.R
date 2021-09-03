#' The \code{NetLogoR} package
#'
#' The suggested package \pkg{fastshp} can be installed with
#' \code{install.packages("fastshp", repos = "https://rforge.net", type = "source")}.
#' The examples included with the package, are located in the R package "examples" folder,
#' which can be found at \code{system.file(package = "NetLogoR", "examples")}. The 3 specific
#' R examples can be opened here:
#' \code{file.edit(file.path(system.file(package = "NetLogoR", "examples"), "Ants", "Ants.R"))},
#' \code{file.edit(file.path(system.file(package = "NetLogoR", "examples"), "Butterfly", "Butterfly-1.R"))},
#' or
#' \code{file.edit(file.path(system.file(package = "NetLogoR", "examples"), "Wolf-Sheep-Predation", "Wolf-Sheep-Predation.R"))}.
"_PACKAGE"

#' @import methods
#' @import raster
NULL

#' Internal CRS usage
#'
#' This is used in various places when converting \code{agentMatrix} objects to
#' \code{SpatialPoints} for use with functions, such as \code{\link[raster]{buffer}}.
#' None of these functions results in any real geospatial information being conserved,
#' other than distance and angle  calculations. Buffering still occurs on the
#' unit-less World. This is therefore a way to allow \code{sp} and \code{raster}
#' functions to work \emph{as if a cell in the World was equal to 1 m}. The resulting
#' answers will be converted back to units of "cells", not "m".
#' The \code{CRS()} operation is relatively time consuming.
#' Having a single object usable throughout is \emph{much} faster.
#'
#' @keywords internal
#' @rdname projNowhere
.projNowhere <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
# This is actually in central Europe; but exact
#  location is not relevant as this is used to calculate
# distances .projNowhere <- CRS("EPSG:32632")
