##########################################################
#' Initialize for `agentMatrix` Class
#'
#' To create a new `agentMatrix` object.
#'
#' @docType methods
#' @include Agent-classes.R
#' @include helpers.R
#' @inheritParams methods::initialize
#' @param coords 2 column matrix of coordinates
#' @param levelsAM A list with named character vectors. Each name should
#'                 match with elements in `...`, and each character vector
#'                 should be the length of unique elements in the `...` element.
#'
#' @return An `agentMatrix` object.
#' @aliases initialize,agentMatrix-method
#' @exportMethod initialize
setMethod(
  "initialize",
  "agentMatrix",
  function(.Object = "agentMatrix", coords, ..., levelsAM) {
    Coords <- TRUE
    if (missing(coords)) {
      coords <- NULL
    }

    dotCols <- list(...)

    if (is.null(coords)) {
      if (length(dotCols) == 0) {
        coords <- cbind(xcor = integer(), ycor = integer())
      } else {
        coords <- matrix(c(NA, NA), ncol = 2)
      }
      Coords <- FALSE
    } else {
      coords <- unname(coords)
    }
    if (is.data.frame(coords)) {
      coords <- as.matrix(coords)
    }

    colnames(coords) <- .coordsColNames

    if (missing(levelsAM)) {
      if (all(sapply(dotCols, is.numeric))) {
        isMatrix <- sapply(dotCols, is.matrix)

        singleMatrix <- if (length(isMatrix) > 0) sum(isMatrix) == 1 else FALSE
        if (singleMatrix) {
          otherCols <- do.call(cbind, list(xcor = coords[, 1], ycor = coords[, 2], dotCols[[1]]))
        } else {
          otherCols <- append(list(xcor = coords[, 1], ycor = coords[, 2]), dotCols)
          otherCols <- do.call(cbind, otherCols)
        }
        rownames(otherCols) <- NULL
        if (length(otherCols) > 0) {
          .Object@.Data <- otherCols
          .Object@levels <- list(NULL)
          if (Coords) {
            .Object@bbox <- .bboxCoords(coords)
          } else {
            .Object@bbox <- matrix(rep(NA_real_, 4), ncol = 2)
          }
        }
      } else {
        isDF <- sapply(dotCols, function(x) is(x, "data.frame"))
        if (any(names(dotCols) == "stringsAsFactors")) {
          dotCols$stringsAsFactors <- NULL
        }
        if (any(isDF)) {
          dotCols <- unlist(lapply(unname(dotCols[isDF]), as.list), recursive = FALSE)
        } else {
          # can't just do "do.call(cbind, dotCols)" because some may be numerics,
          # others not... would coerce to all character
          dotCols <- unlist(lapply(seq_len(length(dotCols)), function(x) {
            isMat <- is.matrix(dotCols[[x]])
            if (isMat) {
              innerMats <- lapply(seq_len(ncol(dotCols[[x]])), function(y) dotCols[[x]][, y])
              names(innerMats) <- colnames(dotCols[[x]])
            } else {
              innerMats <- dotCols[x]
            }
            return(innerMats)
          }), recursive = FALSE)
        }
        otherCols <- append(list(xcor = coords[, 1], ycor = coords[, 2]), dotCols)
        charCols <- sapply(otherCols, is.character)
        numCols <- sapply(otherCols, is.numeric)
        facCols <- sapply(otherCols, is.factor)
        charCols <- facCols | charCols
        otherCols[charCols] <- lapply(otherCols[charCols], function(x) {
          factor(x, levels = sort(unique(x)))
        })

        if (length(otherCols[[1]]) == 1) names(otherCols[[1]]) <- 1
        if (length(otherCols) > 0) {
          .Object@.Data <- do.call(cbind, otherCols)
          .Object@levels <- lapply(
            otherCols[charCols],
            function(x) if (is.factor(x)) levels(x) else NULL
          )
          if (Coords) {
            .Object@bbox <- .bboxCoords(coords)
          } else {
            .Object@bbox <- matrix(rep(NA_real_, 4), ncol = 2)
          }
        }
      }
    } else {
      if ((is.matrix(dotCols[[1]]) & is.numeric(dotCols[[1]])) | is(dotCols[[1]], "agentMatrix")) {
        .Object@.Data <- cbind(xcor = coords[, 1], ycor = coords[, 2], dotCols[[1]])
      } else {
        stop("if passing levelsAM, then ... must be a numeric matrix")
      }
      .Object@levels <- levelsAM
      if (Coords) {
        .Object@bbox <- .bboxCoords(coords)
      } else {
        .Object@bbox <- matrix(rep(NA_real_, 4), ncol = 2)
      }
    }
    .Object
  }
)

################################################################################
#' Create a new `agentMatrix` object
#'
#' This is a fast alternative to the `SpatialPointsDataFrame`.
#' It is meant to replace that functionality, though there are not as many methods (yet).
#' The object is primarily a numeric matrix.
#' Any character column passed to `...` will be converted to a numeric, using `as.factor`
#' internally, and stored as a numeric.
#' Methods using this class will automatically convert character queries to the correct numeric
#' alternative.
#'
#' @param coords  A matrix with 2 columns representing `x` and `y` coordinates
#' @param ... Vectors, a data.frame, or a matrix of extra columns to add to the coordinates,
#'            or a `SpatialPointsDataFrame`.
#'
#' @docType methods
#' @return An `agentMatrix` object
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-turtles>
#'
#' @examples
#' newAgent <- agentMatrix(
#'   coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'   char = letters[c(1, 2, 6)],
#'   nums2 = c(4.5, 2.6, 2343),
#'   char2 = LETTERS[c(4, 24, 3)],
#'   nums = 5:7
#' )
#'
#' w1 <- createWorld(
#'   minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'   data = runif(25)
#' )
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' @author Eliot McIntire
#' @export
#' @rdname agentMatrix
setGeneric(
  "agentMatrix",
  function(..., coords) {
    standardGeneric("agentMatrix")
  }
)

#' @export
#' @rdname agentMatrix
#' @exportMethod agentMatrix
setMethod(
  "agentMatrix",
  signature = c(coords = "matrix"),
  definition = function(..., coords) {
    new("agentMatrix", coords = coords, ...)
  }
)

#' @export
#' @rdname agentMatrix
#' @exportMethod agentMatrix
setMethod(
  "agentMatrix",
  signature = c(coords = "missing"),
  definition = function(...) {
    dots <- list(...)
    if (all(unlist(lapply(dots, is, "SpatialPointsDataFrame"))) & length(dots) == 1) {
      if (!requireNamespace("sp", quietly = TRUE)) {
        stop("Please install.packages('sp') to use sp objects")
      }
      dots <- list(...)
      new("agentMatrix", coords = sp::coordinates(dots[[1]]), dots[[1]]@data)
    } else {
      new("agentMatrix", coords = NULL, ...)
    }
  }
)


setGeneric("coordinates", quickPlot::coordinates)

#' Spatial accessors and setters for NetLogoR classes
#'
#' @param obj object deriving from class "agentMatrix"
#' @param ...  additional arguments that may be used by particular methods
#'
#' @return `coordinates` returns a matrix of coordinates of the `obj`.
#'
#' @export
#' @rdname coordinates
#' @aliases coordinates,agentMatrix-method
#' @seealso [bbox()], [extent()]
#' @exportMethod coordinates
setMethod(
  "coordinates",
  signature("agentMatrix"),
  definition = function(obj, ...) {
    obj@.Data[, 1:2, drop = FALSE]
  }
)

#' @export
setAs(
  "matrix", "agentMatrix",
  function(from) {
    tmp <- new("agentMatrix",
      coords = from[, 1:2, drop = FALSE], from[, -(1:2), drop = FALSE]
    )
    tmp
  }
)

#' @export
setAs(
  "data.frame", "agentMatrix",
  function(from) {
    tmp <- new("agentMatrix",
      coords = from[, 1:2, drop = FALSE], from[, -(1:2), drop = FALSE]
    )
    tmp
  }
)

#' @export
setAs(
  "agentMatrix", "data.frame",
  function(from) {
    tmp <- data.frame(from@.Data)
    rownames(tmp) <- seq_len(NROW(tmp))
    nam <- names(from@levels)
    tmp[, nam] <- lapply(nam, function(n) from@levels[[n]][tmp[, n]])
    tmp
  }
)

#' Extract or Replace Parts of an Object
#'
#' Operators acting on vectors, matrices, arrays and lists to extract or replace parts.
#'
#' @note Extract methods for `agentMatrix` class will generally maintain the `agentMatrix` class.
#' This means that there will still be coordinates, character columns represented as numerics etc.
#' `$` is for extracting the raw columns and does not maintain the `agentMatrix` class.
#' `[]` will extract all values, and result in a data.frame with the correct character and
#' numeric columns.
#'
#' @param x     A `agentMatrix` object from which to extract element(s) or
#'                in which to replace element(s).
#' @param i     Indices specifying elements to extract or replace.
#' @param j     see `i`.
#' @param ...   other named arguments
#' @param drop  not implemented
#'
#' @return An `agentMatrix` when full row(s), full column(s) or element(s)
#'         at specific row(s) and column(s) is/are extracted.
#'
#' @export
#' @name [
#' @aliases [,agentMatrix,numeric,numeric,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    colNames <- colnames(x@.Data)[j]
    levelInd <- match(colNames, names(x@levels))
    if (isTRUE(drop) && length(i) == 1) {
      x <- x@.Data[i, , drop = TRUE]
    } else {
      x@.Data <- x@.Data[i, unique(c(1:2, j)), ..., drop = FALSE]
      if (all(is.na(levelInd))) {
        x@levels <- list(NULL)
      } else {
        x@levels <- x@levels[colNames[!is.na(levelInd)]]
      }
      x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
    }
    x
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,logical,missing,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "logical", "missing", "ANY"),
  definition = function(x, i, ..., drop = FALSE) {
    if (isTRUE(drop) && length(i) == 1) {
      x <- x@.Data[i, , drop = TRUE]
    } else {
      x@.Data <- x@.Data[i, , drop = FALSE]
      if (length(x@.Data) > 0) {
        x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
      }
    }
    x
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,numeric,missing,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "numeric", "missing", "ANY"),
  definition = function(x, i, ..., drop = FALSE) {
    if (isTRUE(drop) && length(i) == 1) {
      x <- x@.Data[i, , drop = TRUE]
    } else {
      x@.Data <- x@.Data[i, , drop = FALSE]
      if (length(x@.Data) > 0) {
        x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
      }
    }
    x
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,missing,missing,missing-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "missing", "missing", "missing"),
  definition = function(x, i, j, ..., drop = FALSE) {
    as(x, "data.frame")
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,missing,character,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "missing", "character", "ANY"),
  definition = function(x, j, ..., drop = FALSE) {
    cols <- match(j, colnames(x@.Data))
    x[, cols, ..., drop = drop]
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,numeric,character,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "numeric", "character", "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    cols <- match(j, colnames(x@.Data))
    x[i, cols, ..., drop = drop]
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,missing,numeric,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "missing", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop = FALSE) {
    colNames <- colnames(x@.Data)[j]
    levelInd <- match(colNames, names(x@levels))
    if (isTRUE(drop)) {
      x <- x@.Data[, j, ..., drop = drop]
    } else {
      x@.Data <- x@.Data[, unique(c(1:2, j)), ..., drop = drop]
      if (all(is.na(levelInd))) {
        x@levels <- list(NULL)
      } else {
        x@levels <- x@levels[colNames[!is.na(levelInd)]]
      }
      x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
    }
    x
  }
)

#' @param value  Any R object
#'
#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,numeric,numeric-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "numeric", "numeric"),
  definition = function(x, i, j, value) {
    x@.Data[i, j] <- value
    validObject(x)
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,missing,numeric,numeric-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "missing", "numeric", "numeric"),
  definition = function(x, i, j, value) {
    x@.Data[, j] <- value
    validObject(x)
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,missing,numeric-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "missing", "numeric"),
  definition = function(x, i, j, value) {
    x@.Data[i, ] <- value
    validObject(x)
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,character,data.frame-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "character", "data.frame"),
  definition = function(x, i, j, value) {
    nam <- names(x@levels)
    charCols <- unlist(lapply(value, is.character))
    # charCols <- match(j, nam)
    # charCols <- match(nam, j)
    numCols <- which(!charCols) # colNums[!(colNums %in% charCols)]
    newCols <- match(j, colnames(x))
    if (any(is.na(newCols))) {
      stop("Can only replace columns that are existing. Use cbind.")
    }

    if (any(numCols)) {
      # numColsJ <- which(colNums %in% numCols)
      x@.Data[i, j[numCols]] <- as.matrix(value[, j[numCols]])
    }

    if (any(charCols)) {
      # charColsJ <- which(colNums %in% charCols)
      for (y in j[charCols]) {
        x[i, y] <- value[, y]
      }
    }
    validObject(x)
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,numeric,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "numeric", "character"),
  definition = function(x, i, j, value) {
    colNames <- colnames(x@.Data)[j]
    levelInd <- match(colNames, names(x@levels))
    levelExists <- all(value %in% x@levels[[levelInd]])
    if (!levelExists) {
      uniqueLevels <- unique(c(x@levels[[levelInd]], value))
      x@levels[[levelInd]] <- as.character(factor(
        x = uniqueLevels,
        levels = uniqueLevels
      ))
    }
    rmLevels <- c(unique(x@.Data[, j]), match(value, x@levels[[levelInd]]))
    if (length(unique(rmLevels)) < length(x@levels[[levelInd]])) {
      uniqueLevels <- x@levels[[levelInd]][unique(rmLevels)]
      x@levels[[levelInd]] <- as.character(factor(
        x = uniqueLevels,
        levels = uniqueLevels
      ))
    }

    x@.Data[i, j] <- match(value, x@levels[[levelInd]])
    validObject(x)
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,missing,numeric,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "missing", "numeric", "character"),
  definition = function(x, i, j, value) {
    x[seq_len(NROW(x)), j] <- value
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,missing,character,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "missing", "character", "character"),
  definition = function(x, i, j, value) {
    cols <- match(j, colnames(x@.Data))
    x[seq_len(NROW(x)), cols] <- value
    return(x)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,character,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "character", "character"),
  definition = function(x, i, j, value) {
    cols <- match(j, colnames(x@.Data))
    x[i, cols] <- value
    return(x)
  }
)

#' @param name  A literal character string or a [name()] (possibly backtick quoted).
#'
#' @export
#' @rdname extract-methods
setMethod(
  "$",
  signature(x = "agentMatrix"),
  definition = function(x, name) {
    if (name %in% names(x@levels)) {
      x@levels[[name]][x@.Data[, name]]
    } else {
      x@.Data[, name]
    }
  }
)

#' Relational Operators
#'
#' Binary operators which allow the comparison of values in an `agentMatrix`.
#'
#' @param e1  An `agentMatrix` object.
#' @param e2  atomic vector, symbol, call, or other object for which methods have been written.
#'
#' @return A logical vector indicating the result of the element by element comparison.
#'
#' @export
#' @importFrom stats na.omit
#' @rdname agentMatrix-compare-methods
setMethod(
  "==",
  signature("agentMatrix", "character"),
  definition = function(e1, e2) {
    colNames <- colnames(e1@.Data)
    if (length(colNames) < 3) {
      warning("Coordinates are not characters, returning test for both coordinates")
      return(matrix(rep(FALSE, length(e1)), ncol = 2))
    }
    levelInd <- match(colNames, names(e1@levels))
    levelIndNoNA <- na.omit(levelInd)
    whInd <- which(!is.na(levelInd))
    if (all(is.na(levelInd))) {
      (e1@.Data == e2)[, -(1:2)]
    } else {
      logic <- e1@.Data == e2
      logic[, whInd] <- sapply(levelIndNoNA, function(z) {
        e1@levels[[z]][e1@.Data[, whInd[z]]]
      }) == e2
      logic[, -(1:2)]
    }
  }
)

#' @export
#' @importFrom stats na.omit
#' @rdname agentMatrix-compare-methods
setMethod(
  "==",
  signature("agentMatrix", "numeric"),
  definition = function(e1, e2) {
    colNames <- colnames(e1@.Data)
    if (length(colNames) < 3) {
      warning("Coordinates are not characters, returning test for both coordinates")
      return(matrix(rep(FALSE, length(e1)), ncol = 2))
    }
    levelInd <- match(colNames, names(e1@levels))
    whInd <- which(!is.na(levelInd))

    if (length(whInd)) {
      ind <- c(1, 2, whInd)
    } else {
      ind <- c(1, 2)
    }

    (e1@.Data[, -ind] == e2)
  }
)

#' Key base R functions for `agentMatrix` class
#'
#' Slight modifications from the default versions.
#'
#' @param object  An `agentMatrix` object.
#'
#' @return `show` returns an invisible `NULL`.
#'         `length` returns a non-negative integer of length 1,
#'         except for vectors of more than 2^31 - 1 elements, when it returns a double.
#'         `nrow` returns an integer of length 1 or `NULL`.

#'
#' @export
#' @rdname agentMatrix-show-methods
setMethod(
  "show",
  signature(object = "agentMatrix"),
  definition = function(object) {
    if (NROW(object@.Data) > 0) {
      tmp <- data.frame(object@.Data, row.names = seq_len(NROW(object@.Data)))
      colNames <- colnames(tmp[, names(object@levels), drop = FALSE])
      tmp[, names(object@levels)] <-
        sapply(seq_along(names(object@levels)), function(x) {
          curLevels <- sort(unique(tmp[, names(object@levels)[x]]))
          as.character(factor(
            tmp[, names(object@levels)[x]], curLevels,
            object@levels[[colNames[x]]][curLevels]
          ))
        })
    } else {
      tmp <- object@.Data
    }
    show(tmp[, -1:-2, drop = FALSE])
  }
)

#' @param x  An `agentMatrix` object
#'
#' @export
#' @rdname agentMatrix-show-methods
setMethod(
  "length",
  signature(x = "agentMatrix"),
  definition = function(x) {
    length(x@.Data)
  }
)

#'
#' @export
#' @rdname agentMatrix-show-methods
setMethod(
  "nrow",
  signature(x = "agentMatrix"),
  definition = function(x) {
    nrow(x@.Data)
  }
)

#' @param n  an integer vector of length up to dim(x) (or 1, for non-dimensioned objects).
#' @param ...  arguments to be passed to or from other methods (currently, none used).
#'
#' @return An `agentMatrix` object, like `x`, but generally smaller.
#'
#' @method head agentMatrix
#' @export
#' @name head
#' @importFrom utils head
#' @rdname agentMatrix-show-methods
head.agentMatrix <- function(x, n = 6L, ...) {
  x[seq_len(n), , drop = FALSE]
}

#' @method tail agentMatrix
#' @export
#' @name tail
#' @importFrom utils tail
#' @rdname agentMatrix-show-methods
tail.agentMatrix <- function(x, n = 6L, ...) {
  len <- NROW(x@.Data)
  ind <- (len - n + 1):len
  out <- x[ind, , drop = FALSE]
  rownames(out@.Data) <- ind
  out
}

#' Combine R Objects by Rows or Columns
#'
#' Take a sequence of `agentMatrix` arguments and combine by columns or rows, respectively.
#' This will take the coordinates of the first argument and remove the coordinates
#' of the second object.
#'
#' @param deparse.level See [base::cbind()].
#' @param ... Two `agentMatrix` objects.
#'
#' @return An `agentMatrix` object.
#'
#' @export
#' @method cbind agentMatrix
#' @name cbind
#' @rdname agentMatrix-bind-methods
cbind.agentMatrix <- function(..., deparse.level) {
  tmp <- list(...)
  if (length(tmp) != 2) stop("cbind for agentMatrix is only defined for 2 agentMatrices")
  notAM <- sapply(tmp, function(x) all(is.na(x@.Data[, 1:2])))

  if (NROW(tmp[[2]]@.Data) == 1) {
    tmp[[2]]@.Data <- tmp[[2]]@.Data[rep_len(1, length.out = NROW(tmp[[1]]@.Data)), ]
  }

  if (any(colnames(tmp[[1]]@.Data)[-1:-2] %in% colnames(tmp[[2]]@.Data)[-1:-2])) {
    stop("There are duplicate columns in the two agentMatrix objects. Please remove duplicates.")
  }
  newMat <- cbind(tmp[[1]]@.Data, tmp[[2]]@.Data[, -1:-2, drop = FALSE])
  tmp[[1]]@.Data <- newMat
  colnames(newMat)
  tmp[[1]]@levels <- updateList(tmp[[2]]@levels, tmp[[1]]@levels)
  tmp[[1]]
}

#' @method rbind agentMatrix
#' @export
#' @importFrom data.table rbindlist
#' @name rbind
#' @rdname agentMatrix-bind-methods
rbind.agentMatrix <- function(..., deparse.level = 1) {
  dots <- list(...)
  levelsSame <- isTRUE(do.call(all.equal, lapply(dots, function(x) x@levels)))
  if (levelsSame) {
    # if same, then faster rbind of the matrices
    if (isTRUE(do.call(all.equal, lapply(dots, colnames)))) {
      mat <- do.call(rbind, lapply(dots, function(x) x@.Data)) # Fastest option...
      # i.e., pass agentMatrix with known levels
    } else {
      mat <- as.matrix(rbindlist(lapply(dots, function(x) data.frame(x@.Data)), fill = TRUE))
    }
    levels <- dots[[1]]@levels
    if (any(!unlist(lapply(levels, is.null)))) {
      new("agentMatrix",
        coords = mat[, 1:2, drop = FALSE],
        mat[, -1:-2],
        levelsAM = levels
      )
    } else {
      new("agentMatrix",
        coords = mat[, 1:2, drop = FALSE],
        mat[, -1:-2, drop = FALSE]
      )
    }
  } else {
    # if levels are not the same, then need to take the "slow" option: convert to data.frame
    mat <- as.data.frame(do.call(rbindlist,
      args = list(lapply(dots, function(x) as(x, "data.frame")),
        fill = TRUE
      )
    ))
    new("agentMatrix", coords = mat[, 1:2], mat[, -1:-2])
  }
}

setGeneric("extent", quickPlot::extent)

#' Bounding box and extent methods for NetLogoR classes
#'
#' Same as `sp::bbox` and `raster::extent`.
#'
#' @include worldNLR-classes-methods.R
#' @param x object deriving from class "agentMatrix",
#'    or a "worldMatrix" or "worldArray"
#' @param ... Ignored.
#'
#' @return `bbox` returns a two-column matrix; the first column has the minimum,
#'         the second the maximum values; rows represent the spatial dimensions.
#'         `extent` returns an `SpatExtent` object from the package `terra`.
#' @rdname extent
#' @docType methods
#' @seealso [bbox()], [coordinates()]
#' @exportMethod extent
setMethod(
  "extent",
  signature("worldNLR"),
  definition = function(x, ...) {
    attr(x, "extent")
  }
)

#' @include worldNLR-classes-methods.R
#' @rdname extent
#' @exportMethod extent
setMethod(
  "extent",
  signature("agentMatrix"),
  definition = function(x, ...) {
    if (sum(attr(x, "bbox") != 0)) {
      exts <- attr(x, "bbox")
      do.call(terra::ext, append(as.list(exts), list(xy = TRUE)))
    } else {
      terra::ext(as.numeric(bbox(x)), xy = TRUE)
    }
  }
)

#' `.bboxCoords` is a drop in replacement for `raster::.bboxCoords`.
#'
#' @param coords xy coordinates for all cells, e.g., produced by `raster::coordinates`.
#'
.bboxCoords <- function(coords) {
  stopifnot(length(coords) > 0)
  # bbox <- matrixStats::colRanges(coords)
  bbox <- rbind(range(coords[, 1]), range(coords[, 2]))
  dimnames(bbox)[[2]] <- c("min", "max")
  dimnames(bbox)[[1]] <- c("xcor", "ycor")
  bbox
}

#' Extract or set bounding box
#'
#' These are methods for classes in NetLogoR, i.e., `agentMatrix`, `worldMatrix`,
#' and `worldArray`.
#'
#' @include worldNLR-classes-methods.R
#' @docType methods
#' @param obj object deriving from class "agentMatrix",
#'    or for `bbox` and `extent`, a "worldMatrix" or "worldArray"
#' @rdname bbox
#' @name bbox
#' @seealso [extent()], [coordinates()], `sp::bbox`
#' @examples
#' newAgent <- agentMatrix(
#'   coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'   char = letters[c(1, 2, 6)],
#'   nums2 = c(4.5, 2.6, 2343),
#'   char2 = LETTERS[c(4, 24, 3)],
#'   nums = 5:7
#' )
#' bbox(newAgent)
#' extent(newAgent)
#' coordinates(newAgent)
setGeneric(
  "bbox",
  function(obj) {
    standardGeneric("bbox")
  }
)

#' @name bbox
#' @aliases bbox,agentMatrix-method
#' @rdname bbox
#' @export
setMethod(
  "bbox",
  signature("agentMatrix"),
  definition = function(obj) {
    if (sum(attr(obj, "bbox") != 0)) {
      attr(obj, "bbox")
    } else {
      cbind(attr(obj, "bbox")[, 1] - 1, attr(obj, "bbox")[, 2] + 1)
    }
  }
)

#' @name bbox
#' @rdname bbox
#' @aliases bbox,ANY-method
#' @export
setMethod(
  "bbox",
  signature("ANY"),
  definition = function(obj) {
    if (!requireNamespace("sp")) {
      stop("Please install.packages('sp') to use raster or sp class objects")
    }
    sp::bbox(obj)
  }
)

#' Replacement method for `bbox`
#'
#' Replacement method sets the bbox attribute of an `agentMatrix`.
#'
#' @export
#' @rdname bbox
#' @param value 2x2 matrix representing the bounding box. See `sp::bbox`.
#' @return The replacement method returns the same object as supplied to
#' obj, i.e., an `agentMatrix`, with the `bbox` attribute set to `value`.
#' @name bbox<-
setGeneric(
  "bbox<-",
  function(obj, value) {
    standardGeneric("bbox<-")
  }
)

#' @include worldNLR-classes-methods.R
#' @rdname bbox
#' @aliases bbox<-,agentMatrix,matrix-method
#' @name bbox<-
setReplaceMethod(
  "bbox",
  signature("agentMatrix", "matrix"),
  definition = function(obj, value) {
    attr(obj, "bbox") <- value
    obj
  }
)

#' @include worldNLR-classes-methods.R
#' @aliases bbox,worldNLR-method
#' @export
#' @name bbox
setMethod(
  "bbox",
  signature("worldNLR"),
  definition = function(obj) {
    bbox(attr(obj, "extent"))
  }
)

#' @include worldNLR-classes-methods.R
#' @aliases bbox,SpatExtent-method
#' @export
#' @name bbox
setMethod(
  "bbox",
  signature("SpatExtent"),
  definition = function(obj) {
    obj <- as.vector(obj)
    obj <- rbind(obj[1:2], obj[3:4])
    rownames(obj) <- c("s1", "s2")
    obj
  }
)

################################################################################
.emptyWorldMatrix <- function() createWorld()
.emptyAgentMatrix <- function() agentMatrix()
