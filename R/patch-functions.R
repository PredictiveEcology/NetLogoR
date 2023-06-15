if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("from", "id", "to"))
}

################################################################################
#' Diffuse values in a `world`
#'
#' Each `patch` gives an equal share of a portion of its value to its neighbor `patches`.
#'
#' @inheritParams fargs
#'
#' @param share      Numeric. Value between 0 and 1 representing the portion of
#'                   the `patches` values to be diffused among the neighbors.
#'
#' @return `WorldMatrix` or `worldArray` object with `patches` values updated.
#'
#' @details What is given is lost for the patches.
#'
#'          If `torus = TRUE`, all `patches` have `nNeighbors` `patches` around
#'          them, which
#'          some may be on the other sides of the `world`. If `torus = FALSE`,
#'          `patches` located on the edges of the `world` have less than
#'          `nNeighbors` `patches` around them.
#'          However, each neighbor still gets 1/4 or 1/8 of the shared amount
#'          and the diffusing
#'          patch keeps the leftover.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#diffuse>
#'
#'          <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#diffuse4>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:3, size = 25, replace = TRUE))
#' plot(w1)
#' # Diffuse 50% of each patch value to its 8 neighbors
#' w2 <- diffuse(world = w1, share = 0.5, nNeighbors = 8)
#' plot(w2)
#'
#'
#' @export
#' @importFrom data.table data.table ':='
#' @importFrom data.table setkey
#' @rdname diffuse
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "diffuse",
  function(world, pVar, share, nNeighbors, torus = FALSE) {
    standardGeneric("diffuse")
})

#' @export
#' @rdname diffuse
setMethod(
  "diffuse",
  signature = c(world = "worldMatrix", pVar = "missing", share = "numeric",
                nNeighbors = "numeric"),
  definition = function(world, share, nNeighbors, torus) {

    val <- as.numeric(t(world@.Data))
    cellNum <- 1:length(val)
    toGive <- (val * share) / nNeighbors

    if (!requireNamespace("SpaDES.tools"))
      stop("Please install.packages('SpaDES.tools')")
    df <- SpaDES.tools::adj(world@.Data, cells = cellNum, directions = nNeighbors, torus = torus)
    # nNeigh <- plyr::count(df[, "from"])
    nNeigh <- as.data.frame(table(df[, "from"]))
    # if (!identical(nNeigh2[[2]], nNeigh[[2]])) stop("count and table error 1")
    toGiveNeigh <- rep(toGive, nNeigh$Freq)
    df <- df[order(df[, "from"]), ]
    dt <- data.table(df, toGiveNeigh)
    setkey(dt, from)
    dt <- dt[, loose := sum(toGiveNeigh), by = from] # how much each patch give
    loose <- unique(dt[, c(1, 4), with = FALSE]) # from and loose
    setkey(dt, to)
    dt <- dt[, win := sum(toGiveNeigh), by = to] # how much each patch receive
    win <- unique(dt[, c(2, 5), with = FALSE]) # to and win

    newVal <- val - loose[, loose] + win[, win]
    world[] <- newVal
    return(world)

})

#' @export
#' @rdname diffuse
setMethod(
  "diffuse",
  signature = c(world = "worldArray", pVar = "character", share = "numeric",
                nNeighbors = "numeric"),
  definition = function(world, pVar, share, nNeighbors, torus) {

    layer <- match(pVar, dimnames(world)[[3]])
    val <- as.numeric(t(world@.Data[, , layer]))
    cellNum <- 1:length(val)
    toGive <- (val * share) / nNeighbors

    if (!requireNamespace("SpaDES.tools"))
      stop("Please install.packages('SpaDES.tools')")
    df <- SpaDES.tools::adj(world@.Data[, , layer], cells = cellNum, directions = nNeighbors,
              torus = torus)
    # nNeigh <- plyr::count(df[, "from"])
    nNeigh <- as.data.frame(table(df[, "from"]))
    # if (!identical(nNeigh2[[2]], nNeigh[[2]])) stop("count and table error 2")

    toGiveNeigh <- rep(toGive, nNeigh$Freq)
    df <- df[order(df[, "from"]), ]
    dt <- data.table(df, toGiveNeigh)
    setkey(dt, from)
    dt <- dt[, loose := sum(toGiveNeigh), by = from] # how much each patch give
    loose <- unique(dt[, c(1, 4), with = FALSE]) # from and loose
    setkey(dt, to)
    dt <- dt[, win := sum(toGiveNeigh), by = to] # how much each patch receive
    win <- unique(dt[, c(2, 5), with = FALSE]) # to and win

    newVal <- val - loose[, loose] + win[, win]
    world@.Data[, , layer] <- matrix(newVal, ncol = dim(world)[2], byrow = TRUE)

    return(world)
})

################################################################################
#' Distances between agents
#'
#' Report the distances between `agents` and `agents2`.
#'
#' @inheritParams fargs
#'
#' @param allPairs Logical. Only relevant if the number of agents/locations in
#'                 `agents` and in `agents2` are the same. If
#'                 `allPairs = FALSE`,
#'                 the distance between each `agents` with the
#'                 corresponding `agents2` is returned. If
#'                 `allPairs = TRUE`, a full
#'                 distance matrix is returned. Default is `allPairs = FALSE`.
#'
#' @return Numeric. Vector of distances between `agents` and `agents2` if
#'         `agents` and/or `agents2` contained
#'         one agent/location, or if `agents` and `agents2` contained the same
#'         number of agents/locations and `allPairs = FALSE`, or
#'
#'         Matrix of distances between `agents` (rows) and
#'         `agents2` (columns)
#'         if `agents` and `agents2` are of different lengths,
#'         or of same length
#'         and `allPairs = TRUE`.
#'
#' @details Distances from/to a patch are measured from/to its center.
#'
#'          If `torus = FALSE`, `world` does not need to be provided.
#'
#'          If `torus = TRUE`, a distance around the sides of the `world` is
#'          reported only if smaller than the one across the `world`.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#distance>
#'
#'          <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#distancexy>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' NLdist(agents = patch(w1, 0, 0), agents2 = patch(w1, c(1, 9), c(1, 9)))
#' NLdist(agents = patch(w1, 0, 0), agents2 = patch(w1, c(1, 9), c(1, 9)),
#'        world = w1, torus = TRUE)
#' t1 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2))
#' NLdist(agents = t1, agents2 = patch(w1, c(1,9), c(1,9)), allPairs = TRUE)
#'
#'
#' @export
#' @rdname NLdist
#' @aliases dist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLdist",
  function(agents, agents2, world, torus = FALSE, allPairs = FALSE) {
    standardGeneric("NLdist")
})

#' @export
#' @rdname NLdist
setMethod(
  "NLdist",
  signature = c(agents = "matrix", agents2 = "matrix"),
  definition = function(agents, agents2, world, torus, allPairs) {

    if (inherits(agents, "agentMatrix")) {
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    if (inherits(agents2, "agentMatrix")) {
      agents2 <- agents2@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    # dist <- pointDistance(p1 = agents, p2 = agents2, lonlat = FALSE, allpairs = allPairs)

    dist <- terra::distance(x = agents, y = agents2, lonlat = FALSE, pairwise = !allPairs)
    # dist <- raster::pointDistance(p1 = agents, p2 = agents2, lonlat = FALSE, allpairs = FALSE)
    if (torus == TRUE) {
      if (missing(world)) {
        stop("A world must be provided as torus = TRUE")
      }

      # Need to create coordinates for "agents2" in a wrapped world
      # For all the 8 possibilities of wrapping (to the left, right, top, bottom and 4 corners)
      exts <- extents(world@extent)

      to1 <- cbind(pxcor = agents2[, 1] - (exts$xmax - exts$xmin),
                   pycor = agents2[, 2] + (exts$ymax - exts$ymin))
      to2 <- cbind(pxcor = agents2[, 1], pycor = agents2[, 2] +
                     (exts$ymax - exts$ymin))
      to3 <- cbind(pxcor = agents2[, 1] + (exts$xmax - exts$xmin),
                   pycor = agents2[, 2] + (exts$ymax - exts$ymin))
      to4 <- cbind(pxcor = agents2[, 1] - (exts$xmax - exts$xmin),
                   pycor = agents2[, 2])
      to5 <- cbind(pxcor = agents2[, 1] + (exts$xmax - exts$xmin),
                   pycor = agents2[, 2])
      to6 <- cbind(pxcor = agents2[, 1] - (exts$xmax - exts$xmin),
                   pycor = agents2[, 2] - (exts$ymax - exts$ymin))
      to7 <- cbind(pxcor = agents2[, 1], pycor = agents2[, 2] -
                     (exts$ymax - exts$ymin))
      to8 <- cbind(pxcor = agents2[, 1] + (exts$xmax - exts$xmin),
                   pycor = agents2[, 2] - (exts$ymax - exts$ymin))


      # dist1 <- raster::pointDistance(p1 = agents, p2 = to1, lonlat = FALSE, allpairs = allPairs)
      # dist2 <- raster::pointDistance(p1 = agents, p2 = to2, lonlat = FALSE, allpairs = allPairs)
      # dist3 <- raster::pointDistance(p1 = agents, p2 = to3, lonlat = FALSE, allpairs = allPairs)
      # dist4 <- raster::pointDistance(p1 = agents, p2 = to4, lonlat = FALSE, allpairs = allPairs)
      # dist5 <- raster::pointDistance(p1 = agents, p2 = to5, lonlat = FALSE, allpairs = allPairs)
      # dist6 <- raster::pointDistance(p1 = agents, p2 = to6, lonlat = FALSE, allpairs = allPairs)
      # dist7 <- raster::pointDistance(p1 = agents, p2 = to7, lonlat = FALSE, allpairs = allPairs)
      # dist8 <- raster::pointDistance(p1 = agents, p2 = to8, lonlat = FALSE, allpairs = allPairs)

      dist1 <- terra::distance(x =agents, y =to1, lonlat = FALSE, pairwise = !allPairs)
      dist2 <- terra::distance(x =agents, y =to2, lonlat = FALSE, pairwise = !allPairs)
      dist3 <- terra::distance(x =agents, y =to3, lonlat = FALSE, pairwise = !allPairs)
      dist4 <- terra::distance(x =agents, y =to4, lonlat = FALSE, pairwise = !allPairs)
      dist5 <- terra::distance(x =agents, y =to5, lonlat = FALSE, pairwise = !allPairs)
      dist6 <- terra::distance(x =agents, y =to6, lonlat = FALSE, pairwise = !allPairs)
      dist7 <- terra::distance(x =agents, y =to7, lonlat = FALSE, pairwise = !allPairs)
      dist8 <- terra::distance(x =agents, y =to8, lonlat = FALSE, pairwise = !allPairs)

      dist <- pmin(dist, dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8)
    }

    return(dist)
})

################################################################################
#' Do the patches exist?
#'
#' Report `TRUE` if a patch exists inside the `world`'s extent, report
#' `FALSE` otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#member>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pExist(world = w1, pxcor = -1, pycor = 2)
#'
#'
#' @export
#' @rdname pExist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "pExist",
  function(world, pxcor, pycor) {
    standardGeneric("pExist")
})

#' @export
#' @rdname pExist
setMethod(
  "pExist",
  signature = c("worldNLR", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {

    if (length(pxcor) == 1 & length(pycor) != 1) {
      pxcor <- rep(pxcor, length(pycor))
    }
    if (length(pycor) == 1 & length(pxcor) != 1) {
      pycor <- rep(pycor, length(pxcor))
    }

    pxcorIn <- pxcor >= world@minPxcor & pxcor <= world@maxPxcor
    pycorIn <- pycor >= world@minPycor & pycor <= world@maxPycor
    pExist <- pxcorIn & pycorIn

    return(pExist)
})

################################################################################
#' Neighbors `patches`
#'
#' Report the coordinates of the neighbors `patches` around the `agents`.
#'
#' @inheritParams fargs
#'
#' @return Matrix (`ncol` = 3) with the first column `pxcor`
#'         and the second column `pycor` representing the coordinates of the neighbors
#'         `patches` around the `agents` and the third column `id` representing
#'         the `id` of the `agents` in the order provided.
#'
#' @details The `patch` around which the neighbors are identified, or the `patch` where
#'          the `turtle` is located on around which the neighbors are identified, is not
#'          returned.
#'
#'          If `torus = FALSE`, `agents` located on the edges of the
#'          `world` have less than `nNeighbors` patches around them.
#'          If `torus = TRUE`, all `agents` located on the edges of the
#'          `world` have `nNeighbors` patches around them,
#'          which some may be on the other sides of the `world`.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#neighbors>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' neighbors(world = w1, agents = patch(w1, c(0,9), c(0,7)), nNeighbors = 8)
#' t1 <- createTurtles(n = 3, coords = randomXYcor(w1, n = 3))
#' neighbors(world = w1, agents = t1, nNeighbors = 4)
#'
#'
#' @export
#' @importFrom data.table data.table setkey
#' @rdname neighbors
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "neighbors",
  function(world, agents, nNeighbors, torus = FALSE) {
    standardGeneric("neighbors")
})

#' @export
#' @rdname neighbors
setMethod(
  "neighbors",
  signature = c(world = "worldNLR", agents = "matrix", nNeighbors = "numeric"),
  definition = function(world, agents, nNeighbors, torus) {

    if (inherits(agents, "agentMatrix")) {
      agents <- patch(world = world, x = agents@.Data[, "xcor"],
                      y = agents@.Data[, "ycor"], duplicate = TRUE)
    }

    # To be used with adj()
    if (inherits(world, "worldMatrix")) {
      worldMat <- world@.Data
    } else {
      # worldArray
      worldMat <- world@.Data[, , 1]
    }
    if (!requireNamespace("SpaDES.tools"))
      stop("Please install.packages('SpaDES.tools')")

    if (nrow(agents) < 100000) {
      # df is faster below 100 agents, DT faster above
      cellNum <- cellFromPxcorPycor(world = world, pxcor = agents[, 1], pycor = agents[, 2])
      neighbors <- SpaDES.tools::adj(worldMat, cells = cellNum, directions = nNeighbors,
                       torus = torus, id = seq_along(cellNum))
      pCoords <- PxcorPycorFromCell(world = world, cellNum = neighbors[, 2])
      neighborsDf <- data.frame(neighbors, pCoords)

      # Output as a matrix
      neighborsDf <- neighborsDf[order(neighborsDf$id), ]
      neighborsID <- cbind(pxcor = neighborsDf$pxcor, pycor = neighborsDf$pycor,
                           id = neighborsDf$id)

    } else {
      cellNum <- cellFromPxcorPycor(world = world, pxcor = agents[, 1], pycor = agents[, 2])
      neighbors <- data.table(SpaDES.tools::adj(worldMat, cells = cellNum, directions = nNeighbors,
                                  torus = torus, id = seq_along(cellNum)))
      cellNum <- data.table(cellNum = cellNum, id = seq_along(cellNum))
      pCoords <- PxcorPycorFromCell(world = world, cellNum = neighbors[, to])
      neighbors[, `:=`(pxcor = pCoords[, 1], pycor = pCoords[, 2])]
      setkey(neighbors, id)
      neighborsID <- cbind(pxcor = neighbors$pxcor,
                           pycor = neighbors$pycor,
                           id = neighbors$id) # %>% as.factor %>% as.numeric)
    }

    return(neighborsID)
})

################################################################################
#' `Patches` coordinates
#'
#' Report the coordinates of the `patches` at the given `[x, y]` locations.
#'
#' @inheritParams fargs
#'
#' @param x          Numeric. Vector of `x` coordinates. Must be of same
#'                   length as `y`.
#'
#' @param y          Numeric. Vector of `y` coordinates. Must be of same
#'                   length as `x`.
#'
#' @param duplicate  Logical. If more than one location `[x, y]`
#'                   fall into the same `patch` and `duplicate == TRUE`, the
#'                   `patch` coordinates are returned the number of times the locations.
#'                   If `duplicate == FALSE`, the `patch` coordinates
#'                   are only returned once.
#'                   Default is `duplicate == FALSE`.
#'
#' @param out        Logical. If `out = FALSE`, no `patch` coordinates are returned
#'                   for `patches` outside of the `world`'s extent, if `out = TRUE`,
#'                   `NA` are returned.
#'                   Default is `out = FALSE`.
#'
#' @return Matrix (`ncol` = 2) with the first column `pxcor` and the second column
#'         `pycor` representing the `patches` coordinates at `[x, y]`.
#'
#' @details If a location `[x, y]` is outside the `world`'s extent and
#'          `torus = FALSE` and `out = FALSE`, no `patch` coordinates are returned;
#'          if `torus = FALSE` and `out = TRUE`, `NA` are returned;
#'          if `torus = TRUE`, the `patch` coordinates from a wrapped `world` are
#'          returned.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4))
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4),
#'       duplicate = TRUE)
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4),
#'       torus = TRUE)
#' patch(world = w1, x = c(0, 9.1, 8.9, 5, 5.3), y = c(0, 0, -0.1, 12.4, 12.4),
#'       torus = TRUE, duplicate = TRUE)
#'
#'
#' @export
#' @rdname patch
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patch",
  function(world, x, y, duplicate = FALSE, torus = FALSE, out = FALSE) {
    standardGeneric("patch")
})

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c(world = "worldNLR", x = "numeric", y = "numeric"),
  definition = function(world, x, y, duplicate, torus, out) {

    pxcor <- round(x)
    pycor <- round(y)

    if (torus == TRUE) {
      pCoords <- wrap(cbind(x = pxcor, y = pycor), world@extent)
      pxcor <- pCoords[, 1]
      pycor <- pCoords[, 2]
    }

    pxcor[pxcor < world@minPxcor | pxcor > world@maxPxcor] <- NA
    pycor[pycor < world@minPycor | pycor > world@maxPycor] <- NA
    pxcor[is.na(pycor)] <- NA
    pycor[is.na(pxcor)] <- NA

    if (out == FALSE) {
      pxcor <- pxcor[!is.na(pxcor)]
      pycor <- pycor[!is.na(pycor)]
    }

    pCoords <- matrix(data = c(pxcor, pycor), ncol = 2,
                      nrow = length(pxcor), dimnames = list(NULL, c("pxcor", "pycor")))

    if (duplicate == FALSE) {
      pCoords <- unique(pCoords)
    }
    return(pCoords)
})

################################################################################
#' No `patches`
#'
#' Report an empty `patch` `agentset`.
#'
#' @return Matrix (`ncol` = 2, `nrow` = 0) with the first column `pxcor` and the
#'         second column `pycor`.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#no-patches>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' p1 <- noPatches()
#' NLcount(p1)
#'
#' @export
#' @rdname noPatches
#'
#' @author Sarah Bauduin
#'
noPatches <- function() {
  return(matrix(0, nrow = 0, ncol = 2, dimnames = list(NULL, c("pxcor", "pycor"))))
}

################################################################################
#' `Patches` at
#'
#' Report the coordinates of the `patches` at `(dx, dy)` distances of the `agents`.
#'
#' @inheritParams fargs
#'
#' @return Matrix (`ncol` = 2) with the first column `pxcor` and the second column
#'         `pycor` representing the coordinates of the `patches` at `(dx, dy)`
#'         distances of the `agents`. The order of the `patches` follows the order
#'         of the `agents`.
#'
#' @details If the `patch` at distance `(dx, dy)`
#'          of an `agent` is outside of the `world`'s extent and `torus = FALSE`,
#'          `NA` are returned
#'          for the `patch` coordinates;
#'          if `torus = TRUE`, the `patch` coordinates from a wrapped `world` are
#'          returned.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-at>
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#at-points>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patchCorner <- patchAt(world = w1, agents = patch(w1, 0, 0), dx = 1, dy = 1)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
#' patchCorner <- patchAt(world = w1, agents = t1, dx = 1, dy = 1)
#'
#'
#' @export
#' @rdname patchAt
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchAt",
  function(world, agents, dx, dy, torus = FALSE) {
    standardGeneric("patchAt")
})

#' @export
#' @rdname patchAt
setMethod(
  "patchAt",
  signature = c(world = "worldNLR", agents = "matrix", dx = "numeric", dy = "numeric"),
  definition = function(world, agents, dx, dy, torus) {

    if (inherits(agents, "agentMatrix")) {
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    pxcor <- agents[, 1] + dx
    pycor <- agents[, 2] + dy
    pAt <- patch(world = world, x = pxcor, y = pycor, duplicate = TRUE,
                 torus = torus, out = TRUE)

    return(pAt)
})

################################################################################
#' `Patches` at given distances and directions
#'
#' Report the coordinates of the `patches` at the given
#' distances and directions from the `agents`.
#'
#' @inheritParams fargs
#'
#' @param dist   Numeric. Vector of distances from the `agents`. Must be
#'               of length 1 or of the same length as the number of `agents`.
#'
#' @param angle  Numeric. Absolute directions from the `agents`. `angle`
#'               must be of length 1 or of the same length as the number of
#'               `agents`. Angles are in degrees with 0 being North.
#'
#' @return Matrix (`ncol` = 2) with the first column `pxcor` and the second column
#'         `pycor` representing the coordinates of the `patches` at the distances
#'         `dist` and directions `angle` of `agents`.
#'         The order of the `patches` follows the order of the `agents`.
#'
#' @details If `torus = FALSE` and the `patch` at distance `dist` and
#'          direction `angle` of an `agent` is outside the `world`'s extent,
#'          `NA` are returned for the `patch` coordinates.
#'          If `torus = TRUE`, the `patch` coordinates from a wrapped `world`
#'          are returned.
#'
#'          If `agents` are `turtles`, their `headings` are not taken into account;
#'          the given directions `angle` are used. To find a `patch` at certain
#'          distance from a `turtle` using the `turtle`'s `heading`, look at `pacthAhead()`,
#'          `patchLeft()` or `patchRight()`.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-at-heading-and-distance>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchDistDir(world = w1, agents = patch(w1, 0, 0), dist = 1, angle = 45)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 315)
#' p2 <- patchDistDir(world = w1, agents = t1, dist = 1, angle = 45)
#'
#' @export
#' @importFrom CircStats rad
#' @rdname patchDistDir
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchDistDir",
  function(world, agents, dist, angle, torus = FALSE) {
    standardGeneric("patchDistDir")
})

#' @export
#' @rdname patchDistDir
setMethod(
  "patchDistDir",
  signature = c(world = "worldNLR", agents = "matrix", dist = "numeric", angle = "numeric"),
  definition = function(world, agents, dist, angle, torus) {
    if (inherits(agents, "agentMatrix")) {
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
    }

    radAngle <- rad(angle)
    pxcor <- agents[, 1] + sin(radAngle) * dist
    pycor <- agents[, 2] + cos(radAngle) * dist
    pDistHead <- patch(world = world, x = pxcor, y = pycor, torus = torus,
                       duplicate = TRUE, out = TRUE)

    return(pDistHead)
})

################################################################################
#' All the `patches` in a `world`
#'
#' Report the coordinates of all the `patches` in the `world`.
#'
#' @inheritParams fargs
#'
#' @return Matrix (`ncol` = 2) with the first column `pxcor` and the second column
#'         `pycor` representing the `patches` coordinates. The order of the `patches`
#'         follows the order of the cells numbers as defined for a `Raster*` object.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patches>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' allPatches <- patches(world = w1)
#' NLcount(allPatches) # 100 patches
#'
#' @author Sarah Bauduin
#' @export
#' @rdname patches
setGeneric(
  "patches",
  function(world) {
    standardGeneric("patches")
})

#' @export
#' @rdname patches
setMethod(
  "patches",
  signature = "worldNLR",
  definition = function(world) {
    return(world@pCoords)
})

################################################################################
#' `Patch` set
#'
#' Report the `patch` coordinates of all the unique `patches` contained in the inputs.
#'
#' @param ... Matrices (`ncol` = 2) of `patches` coordinates with the first column
#'            `pxcor` and the second column `pycor`.
#'
#' @return Matrix (`ncol` = 2) with the first column `pxcor` and the second column
#'         `pycor` representing the `patches` coordinates.
#'
#' @details Duplicate `patches` among the inputs are removed in the returned matrix.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-set>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchAt(world = w1, agents = patch(w1, c(0,1,2), c(0,0,0)), dx = 1, dy = 1)
#' p2 <- patchDistDir(world = w1, agents = patch(w1, 0, 0), dist = 1, angle = 45)
#' p3 <- patch(world = w1, x = 4.3, y = 8)
#' p4 <- patchSet(p1, p2, p3)
#'
#'
#' @export
#' @rdname patchSet
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchSet",
  function(...) {
    standardGeneric("patchSet")
})

#' @export
#' @rdname patchSet
setMethod(
  "patchSet",
  signature = "matrix",
  definition = function(...) {

    dots <- list(...)
    pCoords <- unique(do.call(rbind, dots))
    return(pCoords)
})

################################################################################
#' Random `pxcor`
#'
#' Report `n` random `pxcor` coordinates within the `world`'s extent.
#'
#' @inheritParams fargs
#'
#' @return Integer. Vector of length `n` of `pxcor` coordinates.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pxcor <- randomPxcor(world = w1, n = 10)
#'
#'
#' @export
#' @rdname randomPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomPxcor",
  function(world, n) {
    standardGeneric("randomPxcor")
})

#' @export
#' @rdname randomPxcor
setMethod(
  "randomPxcor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {
    pxcor <- sample(minPxcor(world):maxPxcor(world), size = n, replace = TRUE)
    return(pxcor)
})

################################################################################
#' Random `pycor`
#'
#' Report `n` random `pycor` coordinates within the `world`'s extent.
#'
#' @inheritParams fargs
#'
#' @return Integer. Vector of length `n` of `pycor` coordinates.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor>
#'
#' @references Wilensky, U. 1999. NetLogo. <http://ccl.northwestern.edu/netlogo/>.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pycor <- randomPycor(world = w1, n = 10)
#'
#'
#' @export
#' @rdname randomPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomPycor",
  function(world, n) {
    standardGeneric("randomPycor")
})

#' @export
#' @rdname randomPycor
setMethod(
  "randomPycor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {
    pycor <- sample(minPycor(world):maxPycor(world), size = n, replace = TRUE)
    return(pycor)
})
