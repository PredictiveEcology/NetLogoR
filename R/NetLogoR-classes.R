################################################################################
#' The worldMatrix class
#'
#' This is an s4 class extension of \code{matrix} with 7 additional slots.
#' A \code{worldMatrix} object can be viewed as a grid composed of squared patches
#' (i.e., matrix cells). Patches have two spatial coordinates \code{pxcor} and
#' \code{pycor}, representing the location of their center. \code{pxcor} and
#' \code{pycor} are always integer and increment by 1. \code{pxcor} increases as
#' you move right and \code{pycor} increases as you move up.  \code{pxcor} and
#' \code{pycor} can be negative if there are patches to the left or below the patch
#' \code{[pxcor = 0, pycor = 0]}.
#'
#' The first four slots of the \code{worldMatrix} are: \code{minPxcor}, \code{maxPxcor},
#' \code{minPycor}, \code{maxPycor} which represent the minimum and maximum patches
#' coordinates in the \code{worldMatrix}.
#' The slot \code{extent} is similar to a \code{Raster*} extent. Because \code{pxcor}
#' and \code{pycor} represent the spatial location at the center of the patches and the
#' resolution of them is 1, the extent of the \code{worldMatrix} is equal to
#' \code{xmin = minPxcor - 0.5}, \code{xmax = maxPxcor + 0.5}, \code{ymin = minPycor - 0.5},
#' and \code{ymax = maxPycor + 0.5}.
#' The number of patches in a \code{worldMatrix} is equal to
#' \code{((maxPxcor - minPxcor) + 1) * ((maxPycor - minPycor) + 1)}.
#' The slot \code{res} is equal to \code{1} as it is the spatial resolution of the patches.
#' The last slot \code{pCoords} is a \code{matrix} representing the patches coordinates
#' of all the matrix cells in the order of cells in a \code{Raster*} (i.e., by rows).
#'
#' Careful: The methods \code{[]} and \code{[] <-} retrieve or assign values for
#' the patches in the given order of the patches coordinates provided.
#' When no patches coordinates are provided, the values retrieved or assigned
#' is done in the order of the cell numbers as defined in in \code{Raster*} objects 
#' (i.e., by rows).
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @aliases worldMatrix
#' @name worldMatrix-class
#' @rdname worldMatrix-class
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass worldMatrix
#' @importClassesFrom raster Extent
#'
setClass(
  "worldMatrix",
  representation(
    .Data = "matrix",
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    extent = "Extent",
    res = "numeric",
    pCoords = "matrix"
  )
)

#' @export
#' @name [
#' @aliases [,worldMatrix,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("worldMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {

    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    cellValues <- x[cbind(rowMat, colMat)]

    return(cellValues)
})

#' @export
#' @name [
#' @aliases [,worldMatrix,missing,missing,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature("worldMatrix", "missing", "missing", "ANY"),
  definition = function(x, ..., drop) {
    return(as.numeric(t(x@.Data)))
})

#' @export
#' @name [<-
#' @aliases [<-,worldMatrix,numeric,numeric,ANY-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("worldMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, value) {

    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    x@.Data[cbind(rowMat, colMat)] <- value

    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,worldMatrix,missing,missing,ANY-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("worldMatrix", "missing", "missing", "ANY"),
  definition = function(x, i, j, value) {

    nCell <- dim(x@.Data)[1] * dim(x@.Data)[2]
    if (length(value) != nCell) {
      value <- rep(value, nCell)
    }
    x@.Data <- matrix(data = value, ncol = dim(x@.Data)[2], byrow = TRUE)
    validObject(x)
    return(x)
})


################################################################################
#' Create a world
#'
#' Create a world of patches of class worldMatrix.
#'
#' @inheritParams fargs
#'
#' @param data Vector of length 1 or length
#'            \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}.
#'             Default is \code{NA}.
#'
#' @return WorldMatrix object composed of
#'         \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches (i.e., matrix cells).
#'
#' @details If \code{data} is provided, values are assigned by rows.
#'
#'          If no parameters value are provided, default values are:
#'          \code{minPxcor = -16},
#'          \code{maxPxcor = 16}, \code{minPycor = -16}, and \code{maxPycor = 16}.
#'
#'          See \code{help("worldMatrix-class")} for more details on the worldMatrix class.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
#' plot(w1)
#'
#'
#' @export
#' @importFrom raster extent
#' @docType methods
#' @rdname createWorld
#'
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#'
setGeneric(
  "createWorld",
  function(minPxcor, maxPxcor, minPycor, maxPycor, data = NA) {
    standardGeneric("createWorld")
})

#' @export
#' @rdname createWorld
setMethod(
  "createWorld",
  signature = c(minPxcor = "numeric", maxPxcor = "numeric", minPycor = "numeric",
                maxPycor = "numeric"),
  definition = function(minPxcor, maxPxcor, minPycor, maxPycor, data) {

    numX <- (maxPxcor - minPxcor + 1)
    numY <- (maxPycor - minPycor + 1)
    data <- matrix(ncol = numX,
                    nrow = numY, data = data, byrow = TRUE)
    # byrow = TRUE to be similar as a raster when assigning data

    world <- new("worldMatrix",
                 .Data = data,
                 minPxcor = minPxcor, maxPxcor = maxPxcor,
                 minPycor = minPycor, maxPycor = maxPycor,
                 extent = extent(minPxcor - 0.5, maxPxcor + 0.5, minPycor - 0.5, maxPycor + 0.5),
                 res = c(1, 1),
                 pCoords = cbind(pxcor = rep_len(minPxcor:maxPxcor, length.out = numX * numY),
                                 pycor = rep(maxPycor:minPycor, each = numX))
                 )

    return(world)
})

#' @export
#' @rdname createWorld
setMethod(
  "createWorld",
  signature = c("missing", "missing", "missing", "missing", "missing"),
  definition = function() {
    createWorld(-16, 16, -16, 16, data = NA)
})


################################################################################
#' The worldArray class
#'
#' This is an s4 class extension of \code{array}. It is a collection of several
#' \code{worldMatrix} objects with the same extent (i.e., same values for all their
#' slots) stacked together. It is used to keep more than one value per patch.
#'
#' @name worldArray-class
#' @rdname worldArray-class
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass worldArray
#' @importClassesFrom raster Extent
#'
setClass(
  "worldArray",
  representation(
    .Data = "array",
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    extent = "Extent",
    res = "numeric",
    pCoords = "matrix"
  )
)

#' @export
#' @name [
#' @aliases [,worldArray,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("worldArray", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {
    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    pCoords <- cbind(rowMat, colMat)
    cellValues <- unlist(lapply(1:dim(x)[3], function(z){
      as.numeric(t(x@.Data[cbind(pCoords, z)]))
    }))
    dim(cellValues) <- c(NROW(pCoords), 2L)
    colnames(cellValues) <- dimnames(x@.Data)[[3]]
    return(cellValues)
})

#' @export
#' @name [
#' @aliases [,worldArray,missing,missing,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("worldArray", "missing", "missing", "ANY"),
  definition = function(x, ..., drop) {
    cellValues <- unlist(lapply(1:dim(x)[3], function(z){as.numeric(t(x@.Data[, , z]))}))
    dim(cellValues) <- c(dim(x)[1] * dim(x)[2], dim(x)[3])
    colnames(cellValues) <- dimnames(x@.Data)[[3]]
    return(cellValues)
})

#' @export
#' @name [<-
#' @aliases [<-,worldArray,numeric,numeric,matrix-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("worldArray", "numeric", "numeric", "matrix"),
  definition = function(x, i, j, value) {
    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    coords <- cbind(rowMat, colMat)
    for (k in 1:dim(x)[3]) {
      x@.Data[cbind(coords, k)] <- value[, k]
    }
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,worldArray,missing,missing,matrix-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("worldArray", "missing", "missing", "matrix"),
  definition = function(x, i, j, value) {
    nCell <- dim(x@.Data)[1] * dim(x@.Data)[2]
    if (NROW(value) != nCell) { # assuming value has one row
      value <- value[rep(1, nCell),]
    }
    for (k in 1:dim(x)[3]) {
      x@.Data[, , k] <- matrix(data = value[, k], ncol = dim(x@.Data)[2], byrow = TRUE)
    }
    validObject(x)
    return(x)
})


################################################################################
#' Stack worlds
#'
#' Stack multiple worldMatrix into a worldArray.
#'
#' @param ... worldMatrix objects.
#'
#' @return WorldArray object.
#'
#' @details The worldMatrix objects must all have the same extents.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
#' w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 25:1)
#' w3 <- stackWorlds(w1, w2)
#' plot(w3)
#'
#' @export
#' @importFrom abind abind
#' @docType methods
#' @rdname stackWorlds
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "stackWorlds",
  signature = "...",
  function(...) {
    standardGeneric("stackWorlds")
})

#' @export
#' @rdname stackWorlds
setMethod(
  "stackWorlds",
  signature = "worldMatrix",
  definition = function(...) {
    NLwMs <- list(...)
    # similar dimensions can have different extent
    if (length(unique(lapply(NLwMs, FUN = function(x){x@extent}))) == 1) {
      out <- abind::abind(NLwMs@.Data, along = 3)
    } else {
      stop("worldMatrix extents must all be equal")
    }
    objNames <- as.character(substitute(deparse(...))[-1])
    dimnames(out) <- list(NULL, NULL, objNames)

    world <- new("worldArray",
                 .Data = out,
                 minPxcor = NLwMs[[1]]@minPxcor, maxPxcor = NLwMs[[1]]@maxPxcor,
                 minPycor = NLwMs[[1]]@minPycor, maxPycor = NLwMs[[1]]@maxPycor,
                 extent = NLwMs[[1]]@extent,
                 res = c(1, 1),
                 pCoords = NLwMs[[1]]@pCoords
    )

    return(world)
})


################################################################################
#' The worldNLR class
#'
#'
#' The \code{worldNLR} class is the union of the \code{worldMatrix} and \code{worldArray}
#' classes. Mostly used for building function purposes.
#'
#' @aliases worldNLR
#' @author Sarah Bauduin, and Eliot McIntire
#' @exportClass worldNLR
#' @name worldNLR-class
#' @rdname worldNLR-class
#'
setClassUnion(name = "worldNLR",
              members = c("worldMatrix", "worldArray")
)


################################################################################
#' Cells numbers from patches coordinates
#'
#' Report the cells numbers as defined for a Raster* object given the patches
#' coordinates \code{pxcor} and \code{pycor}.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of cells number.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' cellFromPxcorPycor(world = w1, pxcor = 0, pycor = 9)
#' cellFromPxcorPycor(world = w1, pxcor = c(0, 1, 2), pycor = 0)
#'
#' @export
#' @docType methods
#' @rdname cellFromPxcorPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "cellFromPxcorPycor",
  function(world, pxcor, pycor) {
    standardGeneric("cellFromPxcorPycor")
})

#' @export
#' @rdname cellFromPxcorPycor
setMethod(
  "cellFromPxcorPycor",
  signature = c("worldNLR", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    j <- pxcor - world@minPxcor + 1
    i <- world@maxPycor - pycor + 1
    (i - 1) * ncol(world) + j # Faster
})


################################################################################
#' Patches coordinates from cells numbers
#'
#' Report the patches coordinates \code{pxcor} and \code{pycor} given the cells
#' numbers as defined for a Raster* object.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" in the order of the given \code{cellNum}.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' cellNum <- cellFromPxcorPycor(world = w1, pxcor = 0, pycor = 9)
#' PxcorPycorFromCell(world = w1, cellNum = cellNum)
#' cellNum <- cellFromPxcorPycor(world = w1, pxcor = c(0, 1, 2), pycor = 0)
#' PxcorPycorFromCell(world = w1, cellNum = cellNum)
#'
#'
#' @export
#' @docType methods
#' @rdname PxcorPycorFromCell
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "PxcorPycorFromCell",
  function(world, cellNum) {
    standardGeneric("PxcorPycorFromCell")
})

#' @export
#' @rdname PxcorPycorFromCell
setMethod(
  "PxcorPycorFromCell",
  signature = c("worldNLR", "numeric"),
  definition = function(world, cellNum) {
    pCoords <- world@pCoords[cellNum, , drop = FALSE]
    return(pCoords)
  }
)


################################################################################
#' WorldMatrix indices from vector indices
#'
#' Convert vector indices or Raster* cellnumbers into worldMatrix indices.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of worldMatrix indices.
#'
#' @export
#' @docType methods
#' @rdname NLworldIndex
#'
#' @author Eliot McIntire
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = 1:100)
#' w1Ras <- world2raster(w1)
#' index <- 24
#' pxpy <- PxcorPycorFromCell(world = w1, cellNum = index)
#'
#' rasValue <- as.integer(unname(w1Ras[index]))
#' # Not correct index:
#' identical(w1[index], rasValue)
#'
#' # Correct index
#' identical(w1[NLworldIndex(w1, index)], rasValue)
#'
#'
setGeneric(
  "NLworldIndex",
  function(world, cellNum) {
    standardGeneric("NLworldIndex")
})

#' @export
#' @rdname NLworldIndex
setMethod(
  "NLworldIndex",
  signature = c("worldMatrix", "numeric"),
  definition = function(world, cellNum) {
    b <- dim(world)
    floor((cellNum - 1) / b[2]) + seq.int(from = 1, to = prod(b), 
                                          by = b[1])[(cellNum - 1) %% b[2] + 1]
  }
)
