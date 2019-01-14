#' Basic plot methods for \code{agentMatrix}, \code{worldMatrix}, \code{worldArray}
#'
#' These pass to plot, as a matrix of points (\code{agentMatrix}), as a \code{raster} (\code{worldMatrix}),
#' or a \code{rasterStack} (\code{worldArray}).
#' They can be modified.
#'
#' @param x an \code{agentMatrix}, \code{worldMatrix} or \code{worldArray} object
#' @param ... arguments passed to plot methods for matrix (\code{agentMatrix}) or \code{raster} (\code{world*})
#'
#' @export
#' @method plot agentMatrix
#' @rdname plotMethods
#'
#' @examples
#' # agentMatrix
#'  newAgent <- new("agentMatrix",
#'    coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'    char = letters[c(1, 2, 6)],
#'    nums2 = c(4.5, 2.6, 2343),
#'    char2 = LETTERS[c(4, 24, 3)],
#'    nums = 5:7)
#'  plot(newAgent)
plot.agentMatrix <- function(x, ...) {
  plot(x@.Data, ...)
}

#' @export
#' @method plot worldMatrix
#' @rdname plotMethods
#'
#' @examples
#'
#' ## worldMatrix
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = 1:100)
#' plot(w1)
plot.worldMatrix <- function(x, ...) {
  Ras <- world2raster(x)
  plot(Ras, ...)
}

#' @export
#' @method plot worldArray
#' @rdname plotMethods
#'
#' @examples
#'
#' ## worldArray
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
#' w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 25:1)
#' w3 <- stackWorlds(w1, w2)
#' plot(w3)
plot.worldArray <- function(x, ...) {
  Ras <- world2raster(x)
  plot(Ras, ...)
}

#' @export
#' @importFrom graphics points
#' @method points agentMatrix
#' @rdname plotMethods
#'
#' @examples
#'
#' # agentMatrix
#'  newAgent <- new("agentMatrix",
#'    coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'    char = letters[c(1, 2, 6)],
#'    nums2 = c(4.5, 2.6, 2343),
#'    char2 = LETTERS[c(4, 24, 3)],
#'    nums = 5:7)
#'  points(newAgent)
points.agentMatrix <- function(x, ...) {
  points(x@.Data, ...)
}
