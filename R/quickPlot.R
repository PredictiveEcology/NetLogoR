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
#' @return `numLayers` returns an integer representing the number of
#' layers in a `worldArray` or `worldMatrix` (which is always `1L`)
numLayers.worldArray <-
  #  signature = "worldArray",
  # definition =
  function(x) {
    return(dim(x)[3])
  }

#' @export
#' @inheritParams quickPlot::numLayers
#' @importFrom quickPlot numLayers
#' @rdname quickPlot-methods
numLayers.worldMatrix <-
  #  signature = "worldArray",
  # definition =
  function(x) {
    return(1L)
  }


setGeneric("layerNames", quickPlot::layerNames)

# @inheritParams quickPlot::layerNames
#' @export
#' @param object An object from which to extract the layer names.
#' @importFrom quickPlot layerNames
#' @rdname quickPlot-methods
#' @return `layerNames` returns an character vector representing the names
#' of the layers in a `worldArray`
setMethod(
  "layerNames",
  signature = "worldArray",
  definition = function(object) {
    dimnames(object)[[3]]
  }
)

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
      toPlot <- eval(parse(text = sGrob@objName), sGrob@envir)
    }
    grobToPlot <- .emptyWorldMatrix()
    sns <- slotNames(toPlot)
    for (sn in sns[sns != ".Data"]) {
      slot(grobToPlot, sn, check = FALSE) <- slot(toPlot, sn)
    }
    grobToPlot@.Data <- toPlot@.Data[, , sGrob@layerName]
    return(grobToPlot)
  }
)
