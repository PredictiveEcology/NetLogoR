# `quickPlot` classes
#
# \pkg{quickPlot} offers a type of plotting that is modular.
# Users of NetLogoR may find this useful for simulation modeling.
# We have put in place the required methods and imported the appropriate classes
# to use the `quickPlot::Plot` function.
# Users can still use `plot` from the \pkg{graphics} package, but it is not modular.
#
# This adds `agentMatrix` to the `.quickPlottables`, `.quickObjects`,
# and `spatialObjects`.
#
# This adds `worldMatrix` to the `.quickPlottables`, `.quickObjects`,
# `spatialObjects` and `griddedClasses`.
#
# @seealso [quickPlotClasses()]
#
# @slot members [.quickPlotObjects()] and [.quickPlot()]
#
# @aliases quickPlottables
# @author Eliot McIntire
# @importClassesFrom quickPlot .quickPlottables
# @importClassesFrom quickPlot griddedClasses spatialObjects
# @include Agent-classes.R
# @keywords internal
# @name .quickPlottables-class
# @rdname quickPlottables-class
#
# setIs("agentMatrix", ".quickPlottables")
# setIs("agentMatrix", ".quickPlotObjects")
# setIs("agentMatrix", "spatialObjects")
# setIs("worldMatrix", ".quickPlottables")
# setIs("worldMatrix", ".quickPlotObjects")
# setIs("worldMatrix", "griddedClasses")
# setIs("worldMatrix", "spatialObjects")
# setIs("worldArray", ".quickPlottables")
# setIs("worldArray", ".quickPlotObjects")
# setIs("worldArray", "griddedClasses")
# setIs("worldArray", "spatialObjects")

#' Methods for `quickPlot`
#'
#' These are required to create plotting methods to work with \pkg{quickPlot}.
#'
#' @export
#' @inheritParams quickPlot::numLayers
#' @importFrom quickPlot numLayers
#' @rdname quickPlot-methods
#' @include Agent-classes.R
numLayers.worldArray <-
  #  signature = "worldArray",
  #definition =
  function(x) {
    return(dim(x)[3])
  }

#' @export
#' @inheritParams quickPlot::numLayers
#' @importFrom quickPlot numLayers
#' @rdname quickPlot-methods
numLayers.worldMatrix <-
  #  signature = "worldArray",
  #definition =
  function(x) {
    return(1L)
  }

############## grobs
# if (!isGeneric(".plotGrob")) {
#   setGeneric(
#     ".plotGrob",
#     function(object, objects, compareRasterFileLength = 1e6, algo = "xxhash64") {
#       standardGeneric(".plotGrob")
#   })
# }

# The suggested package `fastshp` can be installed with:
# `install.packages("fastshp", repos = "https://rforge.net", type = "source")`.
#
# @export
# @importFrom quickPlot .plotGrob
# @method .plotGrob agentMatrix
# @importFrom grid gList gpar grid.draw gTree pointsGrob unit
# @include world-functions.R
# @inheritParams quickPlot::.plotGrob
# @rdname quickPlot-methods
# .plotGrob.agentMatrix <-
#   function(grobToPlot, col, size, legend, gp = gpar(), pch, speedup, name, vp, ...) {
#     browser()
#     speedupScale <- 40
#     xyOrd <- coordinates(grobToPlot)
#
#     if (!is.null(col)) {
#       if (!is.null(gp)) {
#         gp$col <- col # Accept col argument
#       } else {
#         gp <- gpar(col) #
#       }
#     }
#
#     if (NROW(xyOrd) > 1e3) {
#       # thin if greater than 1000 pts
#       if (speedup > 0.1) {
#         if (requireNamespace("fastshp", quietly = TRUE)) {
#           thinned <- data.table(
#             thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
#                                  tolerance = speedupScale * speedup)
#           )
#           xyOrd <- xyOrd[thinned$thin, ]
#         } else {
#           message(
#             paste(
#               "To speed up Polygons plotting using Plot install the fastshp package:\n",
#               "install.packages(\"fastshp\", repos=\"https://rforge.net\", type=\"source\")."
#             )
#           )
#           if (Sys.info()[["sysname"]] == "Windows") {
#             message(
#               paste(
#                 "You may also need to download and install Rtools from:\n",
#                 " https://cran.r-project.org/bin/windows/Rtools/"
#               )
#             )
#           }
#         }
#       }
#     }
#
#     pntGrob <- gTree(
#       grobToPlot = grobToPlot,
#       children = gList(
#         pointsGrob(
#           x = xyOrd[, 1], y = xyOrd[, 2],
#           pch = pch, size = size
#         )
#       ),
#       gp = gp,
#       cl = "plotPoint"
#     )
#     grid.draw(pntGrob)
#     return(invisible(pntGrob))
# }


#' @export
#' @importFrom quickPlot .plotGrob
#' @method .plotGrob worldMatrix
#' @inheritParams quickPlot::.plotGrob
#' @include world-functions.R
#' @rdname quickPlot-methods
.plotGrob.worldMatrix <-
  function(grobToPlot, col, size, legend, gp = gpar(), pch, speedup, name, vp, ...) {
    browser()
    dots <- list(...)
    dots$minv <- min(grobToPlot)
    dots$maxv <- max(grobToPlot)
    dots$real <- TRUE

    do.call(.plotGrob, append(dots, list(grobToPlot = grobToPlot@.Data,
                                         col = col, size = size, legend = legend,
                                        # minv = min(grobToPlot), maxv = max(grobToPlot),
                                         gp = gp, pch = pch, speedup = speedup, name = name)))
  }

#' @export
#' @importFrom quickPlot .plotGrob
#' @method .plotGrob worldArray
#' @include world-functions.R
#' @rdname quickPlot-methods
.plotGrob.worldArray <-
  function(grobToPlot, col, size, legend, gp = gpar(), pch, speedup, name, vp, ...) {
    browser()
    dots <- list(...)
    dots$minv <- min(grobToPlot)
    dots$maxv <- max(grobToPlot)
    dots$real <- TRUE

    do.call(.plotGrob, append(dots, list(grobToPlot = grobToPlot@.Data,
                                         col = col, size = size, legend = legend,
                                         # minv = min(grobToPlot), maxv = max(grobToPlot),
                                         gp = gp, pch = pch, speedup = speedup, name = name)))
  }


setGeneric("layerNames", quickPlot::layerNames)

# @inheritParams quickPlot::layerNames
#' @export
#' @importFrom quickPlot layerNames
#' @rdname quickPlot-methods
setMethod(
  "layerNames",
  signature = "worldArray",
  definition = function(object) {
    dimnames(object)[[3]]
})

setGeneric(".identifyGrobToPlot", quickPlot::.identifyGrobToPlot)

#' @rdname quickPlot-methods
#' @inheritParams quickPlot::.identifyGrobToPlot
#' @importFrom quickPlot .identifyGrobToPlot
#' @importMethodsFrom quickPlot .identifyGrobToPlot
setMethod(
  ".identifyGrobToPlot",
  signature = c("worldArray", ".quickPlotGrob"),
  function(toPlot, sGrob, takeFromPlotObj) {
    ## get the object name associated with this grob

    # Does it already exist on the plot device or not
    if (!takeFromPlotObj) {
      toPlot <- eval(parse(text = sGrob@objName),
                     sGrob@envir)
    }
    grobToPlot <- .emptyWorldMatrix()
    sns <- slotNames(toPlot);
    for (sn in sns[sns != ".Data"]) {
      slot(grobToPlot, sn, check = FALSE) <- slot(toPlot, sn)
    }
    grobToPlot@.Data <- toPlot@.Data[,,sGrob@layerName];
    return(grobToPlot)
})
