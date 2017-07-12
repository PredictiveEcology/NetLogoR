###############################################################################
#' Wrap coordinates or pixels in a torus-like fashion
#'
#' Generally for model development purposes.
#'
#' If \code{withHeading} used, then X must be a \code{SpatialPointsDataFrame}
#' that contains two columns, x1 and y1, with the immediately previous agent
#' locations.
#'
#' @param X A SpatialPoints* object, or matrix of coordinates
#'
#' @param bounds Either a Raster*, Extent, or bbox object defining bounds to wrap around
#'
#' @param withHeading logical. If TRUE, then the previous points must be wrapped also
#' so that the subsequent heading calculation will work. Default FALSE. See details.
#'
#' @return Same class as X, but with coordinates updated to reflect the wrapping
#'
#' @export
#' @rdname wrap
#'
#' @author Eliot McIntire
#' @examples
#' library(raster)
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
    if ( identical(colnames(bounds), c("min", "max")) &
         identical(rownames(bounds), c("s1", "s2"))) {
      X <- wrap(X, bounds = extent(bounds), withHeading = withHeading)
      return(X)
    } else {
      stop("Must use either a bbox, Raster*, or Extent for 'bounds'")
    }
  })


##############################################################
#' Fast `adjacent` function, and Just In Time compiled version
#'
#' Faster function for determining the cells of the 4, 8 or bishop
#'  neighbours of the \code{cells}. This is a hybrid function that uses
#'  matrix for small numbers of loci (<1e4) and data.table for larger numbers of loci
#'
#' Between 4x (large number loci) to 200x (small number loci) speed gains over
#' \code{adjacent} in raster package. There is some extra speed gain if
#' \code{NumCol} and \code{NumCells} are passed rather than a raster.
#' Efficiency gains come from:
#'  1. use \code{data.table} internally
#'     - no need to remove NAs because wrapped or outside points are
#'       just removed directly with data.table
#'     - use data.table to sort and fast select (though not fastest possible)
#'  2. don't make intermediate objects; just put calculation into return statement
#'
#' The steps used in the algorithm are:
#' 1. Calculate indices of neighbouring cells
#' 2. Remove "to" cells that are
#'    - <1 or >numCells (i.e., they are above or below raster), using a single modulo
#'      calculation
#'    - where the modulo of "to" cells is equal to 1 if "from" cells are 0 (wrapped right
#'      to left)
#'    - or where the modulo of the "to" cells is equal to 0 if "from" cells are 1 (wrapped
#'      left to right)
#'
#' @param x Raster* object for which adjacency will be calculated.
#'
#' @param cells vector of cell numbers for which adjacent cells should be found. Cell
#'              numbers start with 1 in the upper-left corner and increase from left
#'              to right and from top to bottom
#'
#' @param directions the number of directions in which cells should be connected: 4
#'                   (rook's case), 8 (queen's case), or 'bishop' to connect cells
#'                   with one-cell diagonal moves. Or a neigborhood matrix (see Details)
#'
#' @param sort logical. Whether the outputs should be sorted or not, using Cell IDs of the
#'             from cells (and to cells, if \code{match.adjacent} is TRUE.
#'
#' @param pairs logical. If TRUE, a matrix of pairs of adjacent cells is returned.
#'              If FALSE, a vector of cells adjacent to cells is returned
#'
#' @param include logical. Should the focal cells be included in the result?
#'
#' @param target a vector of cells that can be spread to. This is the inverse of a mask.
#'
#' @param numCol numeric indicating number of columns in the raster. Using this with
#'               numCell is a bit faster execution time.
#'
#' @param numCell numeric indicating number of cells in the raster. Using this
#'                with numCol is a bit faster execution time.
#'
#' @param match.adjacent logical. Should the returned object be the same as the \code{adjacent}
#'                       function in the raster package.
#'
#' @param cutoff.for.data.table numeric. If the number of cells is above this value,
#'                              the function uses data.table which is
#'                              faster with large numbers of cells.
#'
#' @param torus Logical. Should the spread event wrap around to the other side of the raster.
#' Default is FALSE.
#'
#' @param id numeric If not NULL, then function will return "id" column. Default NULL.
#'
#' @return a matrix of one or two columns, from and to.
#'
#' @seealso \code{\link[raster]{adjacent}}
#'
#' @importFrom data.table data.table set key setcolorder setkeyv ':='
#' @importFrom raster ncell ncol nrow
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @rdname adj
#'
#' @author Eliot McIntire
#'
#' @examples
#' library(raster)
#' a <- raster(extent(0, 1000, 0, 1000), res = 1)
#' sam <- sample(1:length(a), 1e4)
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' adj.new <- adj(numCol = numCol, numCell = numCell, cells = sam, directions = 8)
#' adj.new <- adj(numCol = numCol, numCell = numCell, cells = sam, directions = 8,
#'   include = TRUE)
#'
adj <- function(x = NULL, cells, directions = 8, sort = FALSE, pairs = TRUE,
                    include = FALSE, target = NULL, numCol = NULL, numCell = NULL,
                    match.adjacent = FALSE, cutoff.for.data.table = 1e4,
                    torus = FALSE, id = NULL) {
  to = NULL
  J = NULL
  cells <- as.integer(cells)

  if (is.null(numCol) | is.null(numCell)) {
    if (is.null(x)) stop("must provide either numCol & numCell or a x")
    numCol <- as.integer(ncol(x))
    numCell <- as.integer(ncell(x))
  }

  if (directions == "bishop")  {
    dirs <- 4
    needCorners <- TRUE
  } else {
    needCorners <- if (directions == 8) TRUE else FALSE
    dirs <- directions
  }

  numToCells <- dirs + include
  fromCells <- rep.int(cells, times = numToCells)

  if (is.numeric(directions)) {
    top <- cells - numCol
    lef <- cells - 1
    rig <- cells + 1
    bot <- cells + numCol
  }
  if (needCorners) {
    topl <- cells - numCol - 1L
    topr <- cells - numCol + 1L
    botl <- cells + numCol - 1L
    botr <- cells + numCol + 1L
  }

  toCells <- if (directions == 8) {
    if (match.adjacent)
      if (include)
        c(cells, topl, lef, botl, topr, rig, botr, top, bot)
    else
      c(topl, lef, botl, topr, rig, botr, top, bot)
    else
      if (include)
        c(topl, top, topr, lef, cells, rig, botl, bot, botr)
    else
      c(topl, top, topr, lef, rig, botl, bot, botr)
  } else if (directions == 4) {
    if (match.adjacent)
      if (include)
        c(cells, lef, rig, top, bot)
    else
      c(lef, rig, top, bot)
    else
      if (include)
        c(top, lef, cells, rig, bot)
    else
      c(top, lef, rig, bot)
  } else if (directions == "bishop") {
    if (match.adjacent)
      if (include)
        c(cells, topl, botl, topr, botr)
    else
      c(topl, botl, topr, botr)
    else
      if (include)
        c(topl, topr, cells, botl, botr)
    else
      c(topl, topr, botl, botr)
  } else {
    stop("directions must be 4 or 8 or \'bishop\'")
  }

  useMatrix <- (length(cells) < cutoff.for.data.table)
  if (useMatrix) {
    adj <- cbind(from = fromCells, to = toCells)
    if (!is.null(id)) adj <- cbind(adj, id = rep.int(id, times = numToCells))
  } else {
    adj <- data.table(from = fromCells, to = toCells)
    if (!is.null(id)) set(adj, , "id", rep.int(id, times = numToCells)) #adj[,id:=rep.int(id, times = 4)]
  }

  if (useMatrix) {

    ################################################
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      adj <- adj[na.omit(adj[, "to"] %in% target), , drop = FALSE]
    }

    if (sort) {
      if (pairs) {
        if (match.adjacent) {
          adj <- adj[order(adj[, "from"], adj[, "to"]), , drop = FALSE]
        } else {
          adj <- adj[order(adj[, "from"]), , drop = FALSE]
        }
      } else {
        adj <- adj[order(adj[, "to"]), , drop = FALSE]
      }
    }

    # Remove the "from" column if pairs is FALSE
    # Good time savings if no intermediate object is created
    keepCols <- if (is.null(id)) "to" else c("to", "id")
    if (!torus) {
      if (pairs) {
        return(adj[
          !((((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]) |  #top or bottom of raster
              ((adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          , , drop = FALSE])
      } else {
        adj <- adj[
          !((((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]) |  #top or bottom of raster
              ((adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          , keepCols, drop = FALSE]
        if (match.adjacent) {
          adj <- unique(adj[, "to"])
        }
        return(adj)
      }
    } else {
      whLefRig <- (adj[, "from"] %% numCol + adj[, "to"] %% numCol) == 1
      adj[whLefRig, "to"] <- adj[whLefRig, "to"] + numCol*(adj[whLefRig, "from"] - adj[whLefRig, "to"])
      whBotTop <- ((adj[, "to"] - 1) %% numCell + 1) != adj[, "to"]
      adj[whBotTop, "to"] <- adj[whBotTop, "to"] + sign(adj[whBotTop, "from"] - adj[whBotTop, "to"])*numCell
      if (pairs) {
        return(adj)
      } else {
        if (match.adjacent) {
          adj <- unique(adj[, "to", drop = TRUE])
        } else {
          adj <- adj[, keepCols, drop = FALSE]
        }
        return(adj)
      }
    }
  } else {
    #################################################
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      set(adj, , "ord", seq_len(NROW(adj)))
      setkeyv(adj, "to")
      adj <- adj[J(target)]
      adj <- na.omit(adj)
      setkeyv(adj, "ord")
      set(adj, , "ord", NULL)
    }

    if (sort) {
      if (pairs) {
        if (match.adjacent) {
          setkeyv(adj, c("from", "to"))
        } else {
          setkeyv(adj, "from")
        }
      } else {
        setkeyv(adj, "to")
      }
    }

    # Remove the "from" column if pairs is FALSE
    if (!pairs) {
      from <- as.integer(adj$from)
      set(adj, , "from", NULL)
    }

    if (!torus) {
      if (!pairs) {
        adj <- adj[
          !((((to - 1) %% numCell + 1) != to) |  #top or bottom of raster
              ((from %% numCol + to %% numCol) == 1))# | #right & left edge cells, with neighbours wrapped
          ]
        if (match.adjacent) {
          return(unique(adj$to))
        }
        return(as.matrix(adj))
      } else {
        return(as.matrix(adj[
          !((((to - 1) %% numCell + 1) != to) | #top or bottom of raster
              ((from %% numCol + to %% numCol) == 1)) # | #right & left edge cells, with neighbours wrapped
          ]))
      }
    } else {
      if (!pairs) {
        whLefRig <- (from %% numCol + adj$to %% numCol) == 1
        toWhLefRig <- adj$to[whLefRig]
        set(adj, which(whLefRig), "to", toWhLefRig + numCol*(from[whLefRig] - toWhLefRig))
        whBotTop <- ((adj$to - 1) %% numCell + 1) != adj$to
        toWhBotTop <- adj$to[whBotTop]
        set(adj, which(whBotTop), "to", toWhBotTop +
              as.integer(sign(from[whBotTop] - toWhBotTop)*numCell))

        if (match.adjacent) {
          adj <- unique(adj$to)
          return(adj)
        }
      } else {
        whLefRig <- (adj$from %% numCol + adj$to %% numCol) == 1
        toWhLefRig <- adj$to[whLefRig]
        set(adj, which(whLefRig), "to", toWhLefRig + numCol*(adj$from[whLefRig] - toWhLefRig))
        whBotTop <- ((adj$to - 1) %% numCell + 1) != adj$to
        toWhBotTop <- adj$to[whBotTop]
        set(adj, which(whBotTop), "to", toWhBotTop +
              as.integer(sign(adj$from[whBotTop] - toWhBotTop)*numCell))
      }
      return(as.matrix(adj))
    }
  }
}


################################################################################
#' Update elements of a named list with elements of a second named list
#'
#' Merge two named list based on their named entries. Where
#' any element matches in both lists, the value from the
#' second list is used in the updated list.
#' Subelements are not examined and are simply replaced. If one list is empty, then
#' it returns the other one, unchanged.
#'
#' @param x   a named list
#' @param y   a named list
#'
#' @return A named list, with elements sorted by name.
#'          The values of matching elements in list \code{y}
#'          replace the values in list \code{x}.
#'
#' @export
#' @docType methods
#' @rdname updateList
#'
#' @author Alex Chubaty
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
setMethod("updateList",
          signature = c("list", "list"),
          definition = function(x, y) {
            if (any(is.null(names(x)), is.null(names(y)))) {
              # If one of the lists is empty, then just return the other, unchanged
              if (length(y)==0) return(x)
              if (length(x)==0) return(y)
              stop("All elements in lists x,y must be named.")
            } else {
              x[names(y)] <- y
              return(x[order(names(x))])
            }
          })

#' @rdname updateList
setMethod("updateList",
          signature = c("NULL", "list"),
          definition = function(x, y) {
            if (is.null(names(y))) {
              if (length(y) == 0) return(x)
              stop("All elements in list y must be named.")
            }
            return(y[order(names(y))])
          })

#' @rdname updateList
setMethod("updateList",
          signature = c("list", "NULL"),
          definition = function(x, y) {
            if (is.null(names(x))) {
              if (length(x) == 0) return(x)
              stop("All elements in list x must be named.")
            }
            return(x[order(names(x))])
          })

#' @rdname updateList
setMethod("updateList",
          signature = c("NULL", "NULL"),
          definition = function(x, y) {
            return(list())
          })
