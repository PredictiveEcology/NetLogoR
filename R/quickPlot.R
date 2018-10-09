################################################################################
#' quickPlot classes -- for using `quickPlot::Plot``
#'
#' \pkg{quickPlot} offers a type of plotting that is modular.
#' Users of NetLogoR may find this useful for simulation modeling.
#' We have put in place the required methods and imported the appropriate classes
#' to use the `quickPlot::Plot` function.
#' Users can still use `plot` from the `graphics` package, but it is not modular.
#'
#' This adds \code{agentMatrix} to the \code{.quickPlottables}, \code{.quickObjects},
#' and \code{spatialObjects}.
#'
#' This adds \code{worldMatrix} to the \code{.quickPlottables}, \code{.quickObjects},
#' \code{spatialObjects} and \code{griddedClasses}.
#'
#' @seealso \code{\link{quickPlotClasses}}
#'
#' @slot members \code{\link{.quickPlotObjects}} and \code{\link{.quickPlot}}
#'
#' @aliases quickPlottables
#' @author Eliot McIntire
#' @importClassesFrom quickPlot .quickPlottables
#' @importClassesFrom quickPlot griddedClasses spatialObjects
#' @include Agent-classes.R
#' @keywords internal
#' @name .quickPlottables-class
#' @rdname quickPlottables-class
#'
setIs("agentMatrix", ".quickPlottables")
setIs("agentMatrix", ".quickPlotObjects")
setIs("agentMatrix", "spatialObjects")
setIs("worldMatrix", ".quickPlottables")
setIs("worldMatrix", ".quickPlotObjects")
setIs("worldMatrix", "griddedClasses")
setIs("worldMatrix", "spatialObjects")
setIs("worldArray", ".quickPlottables")
setIs("worldArray", ".quickPlotObjects")
setIs("worldArray", "griddedClasses")
setIs("worldArray", "spatialObjects")

###### quickPlot methods
#' Methods for quickPlot
#'
#' These are required to create plotting methods to work with quickPlot.
#'
#' @export
#' @inheritParams quickPlot::numLayers
#' @importFrom quickPlot numLayers
#' @rdname quickPlot-methods
#' @include Classes.R
#' @include Agent-classes.R
setMethod(
  "numLayers",
  signature = "worldArray",
  definition = function(x) {
    return(dim(x)[3])
})

############## grobs
if (!isGeneric(".plotGrob")) {
  setGeneric(
    ".plotGrob",
    function(object, objects, compareRasterFileLength = 1e6, algo = "xxhash64") {
      standardGeneric(".plotGrob")
    })
}

#' The suggested package \code{fastshp} can be installed with:
#' \code{install.packages("fastshp", repos = "https://rforge.net", type = "source")}.
#' @export
#' @exportMethod .plotGrob
#' @importFrom quickPlot .plotGrob
#' @importFrom grid gpar gTree gList grid.draw pointsGrob unit
#' @importMethodsFrom quickPlot .plotGrob
#' @include world-functions.R
#' @inheritParams quickPlot::.plotGrob
#' @rdname quickPlot-methods
setMethod(
  ".plotGrob",
  signature = c("agentMatrix"),
  definition = function(grobToPlot, col, size, legend, gp = gpar(), pch, speedup,
                        name, vp, ...) {

    speedupScale <- 40
    xyOrd <- coordinates(grobToPlot)

    if (!is.null(col)) {
      if (!is.null(gp)) {
        gp$col <- col # Accept col argument
      } else {
        gp <- gpar(col) #
      }
    }

    if (NROW(xyOrd) > 1e3) {
      # thin if greater than 1000 pts
      if (speedup > 0.1) {
        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- data.table(
            thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                                 tolerance = speedupScale * speedup)
          )
          xyOrd <- xyOrd[thinned$thin, ]
        } else {
          message(
            paste(
              "To speed up Polygons plotting using Plot install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"https://rforge.net\", type=\"source\")."
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    pntGrob <- gTree(
      grobToPlot = grobToPlot,
      children = gList(
        pointsGrob(
          x = xyOrd[, 1], y = xyOrd[, 2],
          pch = pch, size = size
        )
      ),
      gp = gp,
      cl = "plotPoint"
    )
    grid.draw(pntGrob)
    return(invisible(pntGrob))
})


#' @export
#' @inheritParams quickPlot::layerNames
#' @importFrom quickPlot layerNames
#' @rdname quickPlot-methods
setMethod(
  "layerNames",
  signature = "worldArray",
  definition = function(object) {
    dimnames(object)[[3]]
})


if (!isGeneric(".identifyGrobToPlot")) {
  setGeneric(
    ".identifyGrobToPlot",
    function(toPlot, sGrob, takeFromPlotObj) {
      standardGeneric(".identifyGrobToPlot")
  })
}

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
    grobToPlot <- .emptyWorldMatrix
    sns <- slotNames(toPlot);
    for (sn in sns[sns != ".Data"]) {
      slot(grobToPlot, sn, check = FALSE) <- slot(toPlot, sn)
    }
    grobToPlot@.Data <- toPlot@.Data[,,sGrob@layerName];
    return(grobToPlot)
})
