###############################################################################
#' Wrap coordinates or pixels in a torus-like fashion
#'
#' Generally for model development purposes.
#'
#' If `withHeading` used, then obj must be a `SpatialPointsDataFrame`
#' that contains two columns, `x1` and `y1`, with the immediately previous
#' agent locations.
#'
#' @param obj A `SpatialPoints*` object, or matrix of coordinates.
#'
#' @param bounds Either a `Raster*`, `Extent`, or `bbox` object
#'               defining bounds to wrap around.
#'
#' @param withHeading Logical. If `TRUE`, then the previous points must be
#' wrapped also so that the subsequent heading calculation will work.
#' Default `FALSE`. See details.
#'
#' @return Same class as `obj`, but with coordinates updated to reflect the wrapping.
#'
#' @author Eliot McIntire
#' @export
#' @rdname wrap
#'
#' @examples
#' if (requireNamespace("terra")) {
#'   xrange <- yrange <- c(-50, 50)
#'   hab <- terra::rast(terra::ext(c(xrange, yrange)))
#'   hab[] <- runif(terra::ncell(hab))
#'
#'   # initialize agents
#'   N <- 10
#'
#'   # previous points
#'   x1 <- rep(0, N)
#'   y1 <- rep(0, N)
#'   # initial points
#'   starts <- cbind(
#'     x = stats::runif(N, xrange[1], xrange[2]),
#'     y = stats::runif(N, yrange[1], yrange[2])
#'   )
#'
#'   # create the agent object
#'   agent <- agentMatrix(coords = starts, data = data.frame(x1 = x1, y1 = y1))
#'
#'   ln <- rlnorm(N, 1, 0.02) # log normal step length
#'   sd <- 30 # could be specified globally in params
#'
#'   if (interactive()) {
#'     library(quickPlot)
#'     clearPlot()
#'     Plot(hab, zero.color = "white", axes = "L")
#'     Plot(agent, addTo = "hab")
#'   }
#'   if (requireNamespace("SpaDES.tools") &&
#'     requireNamespace("CircStats")) {
#'     for (i in 1:10) {
#'       agent <- SpaDES.tools::crw(
#'         agent = agent,
#'         extent = terra::ext(hab), stepLength = ln,
#'         stddev = sd, lonlat = FALSE, torus = TRUE
#'       )
#'       if (interactive()) Plot(agent, addTo = "hab", axes = TRUE)
#'     }
#'   }
#' }
setGeneric("wrap", function(obj, bounds, withHeading) {
  standardGeneric("wrap")
})


#' @export
#' @rdname wrap
setMethod(
  "wrap",
  signature(obj = "ANY", bounds = "ANY"),
  definition = function(obj, bounds, withHeading) {
    if (requireNamespace("SpaDES.tools")) {
      # browser()
      if (missing(withHeading)) {
        obj <- SpaDES.tools::wrap(obj, bounds)
      } else {
        obj <- SpaDES.tools::wrap(obj, bounds, withHeading)
      }
      return(obj)
    } else {
      if (is.matrix(obj) && inherits(bounds, c("Extent", "SpatExtent"))) {
        if (identical(colnames(obj), c("x", "y"))) {
          xmn <- terra::xmin(bounds)
          xmx <- terra::xmax(bounds)
          ymn <- terra::ymin(bounds)
          ymx <- terra::ymax(bounds)

          return(cbind(
            x = (obj[, "x"] - xmn) %% (xmx - xmn) + xmn,
            y = (obj[, "y"] - ymn) %% (ymx - ymn) + ymn
          ))
        } else {
          stop(
            "When obj is a matrix, it must have 2 columns, x and y,",
            "as from say, coordinates(SpatialPointsObj)"
          )
        }
      } else if (is(obj, "SpatialPointsDataFrame")) {
        if (is(bounds, "Raster") || is.matrix(bounds)) {
          bounds <- extent(bounds)
        }
        if (isTRUE(withHeading)) {
          # This requires that previous points be "moved" as if they are
          #  off the bounds, so that the heading is correct
          obj@data[coordinates(obj)[, "x"] < bounds@xmin, "x1"] <-
            (obj@data[coordinates(obj)[, "x"] < bounds@xmin, "x1"] - bounds@xmin) %%
            (bounds@xmax - bounds@xmin) + bounds@xmax
          obj@data[coordinates(obj)[, "x"] > bounds@xmax, "x1"] <-
            (obj@data[coordinates(obj)[, "x"] > bounds@xmax, "x1"] - bounds@xmax) %%
            (bounds@xmin - bounds@xmax) + bounds@xmin
          obj@data[coordinates(obj)[, "y"] < bounds@ymin, "y1"] <-
            (obj@data[coordinates(obj)[, "y"] < bounds@ymin, "y1"] - bounds@ymin) %%
            (bounds@ymax - bounds@ymin) + bounds@ymax
          obj@data[coordinates(obj)[, "y"] > bounds@ymax, "y1"] <-
            (obj@data[coordinates(obj)[, "y"] > bounds@ymax, "y1"] - bounds@ymax) %%
            (bounds@ymin - bounds@ymax) + bounds@ymin
        }
        return(wrap(obj, bounds = bounds, withHeading = withHeading))
      } else if (is(obj, "SpatialPoints")) {
        obj@coords <- wrap(obj@coords, bounds = bounds)
        return(obj)
      } else if (is(obj, "Raster") && is(bounds, "Raster")) {
        obj <- wrap(obj, bounds = extent(bounds))
        return(obj)
      } else if (is.matrix(obj) && is.matrix(bounds)) {
        if (identical(colnames(bounds), c("min", "max")) &
          (identical(rownames(bounds), c("s1", "s2")))) {
          obj <- wrap(obj, bounds = extent(bounds))
          return(obj)
        } else {
          stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
        }
      }
    }
  }
)

# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "matrix", bounds = "Extent", withHeading = "missing"),
#   definition = function(obj, bounds) {
#     if (identical(colnames(obj), c("x", "y"))) {
#       return(cbind(
#         x = (obj[, "x"] - bounds@xmin) %% (bounds@xmax - bounds@xmin) + bounds@xmin,
#         y = (obj[, "y"] - bounds@ymin) %% (bounds@ymax - bounds@ymin) + bounds@ymin
#       ))
#     } else {
#       stop("When obj is a matrix, it must have 2 columns, x and y,",
#            "as from say, coordinates(SpatialPointsObj)")
#     }
# })

# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "SpatialPoints", bounds = "ANY", withHeading = "missing"),
#   definition = function(obj, bounds) {
#     obj@coords <- wrap(obj@coords, bounds = bounds)
#     return(obj)
# })

# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "matrix", bounds = "Raster", withHeading = "missing"),
#   definition = function(obj, bounds) {
#     obj <- wrap(obj, bounds = extent(bounds))
#     return(obj)
#   })


# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "matrix", bounds = "matrix", withHeading = "missing"),
#   definition = function(obj, bounds) {
#     if (identical(colnames(bounds), c("min", "max")) &
#         (identical(rownames(bounds), c("s1", "s2")))) {
#       obj <- wrap(obj, bounds = extent(bounds))
#       return(obj)
#     } else {
#       stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
#     }
# })

# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "SpatialPointsDataFrame", bounds = "Extent", withHeading = "logical"),
#   definition = function(obj, bounds, withHeading) {
#     if (withHeading) {
#       # This requires that previous points be "moved" as if they are
#       #  off the bounds, so that the heading is correct
#       obj@data[coordinates(obj)[, "x"] < bounds@xmin, "x1"] <-
#         (obj@data[coordinates(obj)[, "x"] < bounds@xmin, "x1"] - bounds@xmin) %%
#         (bounds@xmax - bounds@xmin) + bounds@xmax
#       obj@data[coordinates(obj)[, "x"] > bounds@xmax, "x1"] <-
#         (obj@data[coordinates(obj)[, "x"] > bounds@xmax, "x1"] - bounds@xmax) %%
#         (bounds@xmin - bounds@xmax) + bounds@xmin
#       obj@data[coordinates(obj)[, "y"] < bounds@ymin, "y1"] <-
#         (obj@data[coordinates(obj)[, "y"] < bounds@ymin, "y1"] - bounds@ymin) %%
#         (bounds@ymax - bounds@ymin) + bounds@ymax
#       obj@data[coordinates(obj)[, "y"] > bounds@ymax, "y1"] <-
#         (obj@data[coordinates(obj)[, "y"] > bounds@ymax, "y1"] - bounds@ymax) %%
#         (bounds@ymin - bounds@ymax) + bounds@ymin
#     }
#     return(wrap(obj, bounds = bounds))
# })

# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "SpatialPointsDataFrame", bounds = "Raster", withHeading = "logical"),
#   definition = function(obj, bounds, withHeading) {
#     obj <- wrap(obj, bounds = extent(bounds), withHeading = withHeading)
#     return(obj)
# })

# @export
# @rdname wrap
# setMethod(
#   "wrap",
#   signature(obj = "SpatialPointsDataFrame", bounds = "matrix", withHeading = "logical"),
#   definition = function(obj, bounds, withHeading) {
#     if (identical(colnames(bounds), c("min", "max")) &
#         identical(rownames(bounds), c("s1", "s2"))) {
#       obj <- wrap(obj, bounds = extent(bounds), withHeading = withHeading)
#       return(obj)
#     } else {
#       stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
#     }
# })


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
#'          The values of matching elements in list `y`
#'          replace the values in list `x`.
#'
#' @author Alex Chubaty
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
      if (length(y) == 0) {
        return(x)
      }
      if (length(x) == 0) {
        return(y)
      }
      stop("All elements in lists x,y must be named.")
    } else {
      x[names(y)] <- y
      return(x[order(names(x))])
    }
  }
)

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("NULL", "list"),
  definition = function(x, y) {
    if (is.null(names(y))) {
      if (length(y) == 0) {
        return(x)
      }
      stop("All elements in list y must be named.")
    }
    return(y[order(names(y))])
  }
)

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("list", "NULL"),
  definition = function(x, y) {
    if (is.null(names(x))) {
      if (length(x) == 0) {
        return(x)
      }
      stop("All elements in list x must be named.")
    }
    return(x[order(names(x))])
  }
)

#' @rdname updateList
setMethod(
  "updateList",
  signature = c("NULL", "NULL"),
  definition = function(x, y) {
    return(list())
  }
)
