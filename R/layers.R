#' Number of layers and layer names
#'
#' Accessors for the layers of a `worldMatrix` or `worldArray`.
#'
#' @param x       A `worldMatrix` or `worldArray` object.
#' @param object  A `worldArray` object.
#'
#' @export
#' @rdname layers
#' @include Agent-classes.R
#' @seealso [plot.agentMatrix()]
#' @return `numLayers` returns an integer representing the number of
#' layers in a `worldArray` or `worldMatrix` (which is always `1L`)
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
#' w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 25:1)
#' numLayers(w1)
#'
#' a1 <- stackWorlds(w1, w2)
#' numLayers(a1)
#' layerNames(a1)
numLayers <- function(x) {
  UseMethod("numLayers")
}

#' @export
#' @rdname layers
numLayers.worldArray <- function(x) {
  return(dim(x)[3])
}

#' @export
#' @rdname layers
numLayers.worldMatrix <- function(x) {
  return(1L)
}

setGeneric("layerNames", function(object) {
  standardGeneric("layerNames")
})

#' @export
#' @rdname layers
#' @aliases layerNames
#' @return `layerNames` returns an character vector representing the names
#' of the layers in a `worldArray`
setMethod(
  "layerNames",
  signature = "worldArray",
  definition = function(object) {
    dimnames(object)[[3]]
  }
)
