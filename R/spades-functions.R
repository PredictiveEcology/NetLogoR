###############################################################################
#' Wrap coordinates or pixels in a torus-like fashion
#'
#' Generally for model development purposes.
#'
#' If \code{withHeading} used, then X must be a \code{SpatialPointsDataFrame}
#' that contains two columns, \code{x1} and \code{y1}, with the immediately previous
#' agent locations.
#'
#' @param X A \code{SpatialPoints*} object, or matrix of coordinates.
#'
#' @param bounds Either a \code{Raster*}, \code{Extent}, or \code{bbox} object
#'               defining bounds to wrap around.
#'
#' @param withHeading Logical. If \code{TRUE}, then the previous points must be
#' wrapped also so that the subsequent heading calculation will work.
#' Default \code{FALSE}. See details.
#'
#' @return Same class as \code{X}, but with coordinates updated to reflect the wrapping.
#'
#' @author Eliot McIntire
#' @docType methods
#' @export
#' @importFrom sp coordinates
#' @rdname wrap
#'
#' @examples
#' library(quickPlot)
#' library(raster)
#'
#' xrange <- yrange <- c(-50, 50)
#' hab <- raster(extent(c(xrange, yrange)))
#' hab[] <- 0
#'
#' # initialize agents
#' N <- 10
#'
#' # previous points
#' x1 <- rep(0, N)
#' y1 <- rep(0, N)
#' # initial points
#' starts <- cbind(x = stats::runif(N, xrange[1], xrange[2]),
#'                 y = stats::runif(N, yrange[1], yrange[2]))
#'
#' # create the agent object
#' agent <- SpatialPointsDataFrame(coords = starts, data = data.frame(x1, y1))
#'
#'
#' ln <- rlnorm(N, 1, 0.02) # log normal step length
#' sd <- 30 # could be specified globally in params
#'
#' if (interactive()) {
#'   clearPlot()
#'   Plot(hab, zero.color = "white", axes = "L")
#' }
#' if (requireNamespace("SpaDES.tools")) {
#'   for (i in 1:10) {
#'
#'     agent <- SpaDES.tools::crw(agent = agent,
#'                                extent = extent(hab), stepLength = ln,
#'                                stddev = sd, lonlat = FALSE, torus = TRUE)
#'    if (interactive()) Plot(agent, addTo = "hab", axes = TRUE)
#'  }
#' }
setGeneric("wrap", function(X, bounds, withHeading) {
  standardGeneric("wrap")
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "Extent", withHeading = "missing"),
  definition = function(X, bounds) {
    if (identical(colnames(X), c("x", "y"))) {
      return(cbind(
        x = (X[, "x"] - bounds@xmin) %% (bounds@xmax - bounds@xmin) + bounds@xmin,
        y = (X[, "y"] - bounds@ymin) %% (bounds@ymax - bounds@ymin) + bounds@ymin
      ))
    } else {
      stop("When X is a matrix, it must have 2 columns, x and y,",
           "as from say, coordinates(SpatialPointsObj)")
    }
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPoints", bounds = "ANY", withHeading = "missing"),
  definition = function(X, bounds) {
    X@coords <- wrap(X@coords, bounds = bounds)
    return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "Raster", withHeading = "missing"),
  definition = function(X, bounds) {
    X <- wrap(X, bounds = extent(bounds))
    return(X)
  })

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "Raster", withHeading = "missing"),
  definition = function(X, bounds) {
    X <- wrap(X, bounds = extent(bounds))
    return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "matrix", bounds = "matrix", withHeading = "missing"),
  definition = function(X, bounds) {
    if (identical(colnames(bounds), c("min", "max")) &
        (identical(rownames(bounds), c("s1", "s2")))) {
      X <- wrap(X, bounds = extent(bounds))
      return(X)
    } else {
      stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
    }
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPointsDataFrame", bounds = "Extent", withHeading = "logical"),
  definition = function(X, bounds, withHeading) {
    if (withHeading) {
      # This requires that previous points be "moved" as if they are
      #  off the bounds, so that the heading is correct
      X@data[coordinates(X)[, "x"] < bounds@xmin, "x1"] <-
        (X@data[coordinates(X)[, "x"] < bounds@xmin, "x1"] - bounds@xmin) %%
        (bounds@xmax - bounds@xmin) + bounds@xmax
      X@data[coordinates(X)[, "x"] > bounds@xmax, "x1"] <-
        (X@data[coordinates(X)[, "x"] > bounds@xmax, "x1"] - bounds@xmax) %%
        (bounds@xmin - bounds@xmax) + bounds@xmin
      X@data[coordinates(X)[, "y"] < bounds@ymin, "y1"] <-
        (X@data[coordinates(X)[, "y"] < bounds@ymin, "y1"] - bounds@ymin) %%
        (bounds@ymax - bounds@ymin) + bounds@ymax
      X@data[coordinates(X)[, "y"] > bounds@ymax, "y1"] <-
        (X@data[coordinates(X)[, "y"] > bounds@ymax, "y1"] - bounds@ymax) %%
        (bounds@ymin - bounds@ymax) + bounds@ymin
    }
    return(wrap(X, bounds = bounds))
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPointsDataFrame", bounds = "Raster", withHeading = "logical"),
  definition = function(X, bounds, withHeading) {
    X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
    return(X)
})

#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(X = "SpatialPointsDataFrame", bounds = "matrix", withHeading = "logical"),
  definition = function(X, bounds, withHeading) {
    if (identical(colnames(bounds), c("min", "max")) &
         identical(rownames(bounds), c("s1", "s2"))) {
      X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
      return(X)
    } else {
      stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
    }
})


################################################################################
#' Update elements of a named list with elements of a second named list
#'
#' Merge two named list based on their named entries.
#' Where any element matches in both lists, the value from the second list is
#' used in the updated list.
#' Subelements are not examined and are simply replaced. If one list is empty,
#' then it returns the other one, unchanged.
#'
#' @param x,y   a named list
#'
#' @return A named list, with elements sorted by name.
#'          The values of matching elements in list \code{y}
#'          replace the values in list \code{x}.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @rdname updateList
#'
#' @examples
#' L1 <- list(a = "hst", b = NA_character_, c = 43)
#' L2 <- list(a = "gst", c = 42, d = list(letters))
#' updateList(L1, L2)
#'
#' updateList(L1, NULL)
#' updateList(NULL, L2)
#' updateList(NULL, NULL) # should return empty list
#'
setGeneric("updateList", function(x, y) {
  standardGeneric("updateList")
})

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("list", "list"),
  definition = function(x, y) {
    if (any(is.null(names(x)), is.null(names(y)))) {
      # If one of the lists is empty, then just return the other, unchanged
      if (length(y) == 0) return(x)
      if (length(x) == 0) return(y)
      stop("All elements in lists x,y must be named.")
    } else {
      x[names(y)] <- y
      return(x[order(names(x))])
    }
})

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("NULL", "list"),
  definition = function(x, y) {
    if (is.null(names(y))) {
      if (length(y) == 0) return(x)
      stop("All elements in list y must be named.")
    }
    return(y[order(names(y))])
})

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("list", "NULL"),
  definition = function(x, y) {
    if (is.null(names(x))) {
      if (length(x) == 0) return(x)
      stop("All elements in list x must be named.")
    }
    return(x[order(names(x))])
})

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("NULL", "NULL"),
  definition = function(x, y) {
    return(list())
})
