if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("xcor", "ycor"))
}

#' The \code{agentMatrix} class
#'
#' Documentation needed.
#'
#' @name agentMatrix-class
#' @rdname agentMatrix-class
#' @author Eliot McIntire
#' @exportClass agentMatrix
#' @examples
#' newAgent <- new("agentMatrix",
#'       coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'       char = letters[c(1, 2, 6)],
#'       nums2 = c(4.5, 2.6, 2343),
#'       char2 = LETTERS[c(4, 24, 3)],
#'       nums = 5:7)
#'
setClass("agentMatrix", contains = "matrix",
         slots = c(x = "matrix", levels = "list", bbox = "matrix"),
         prototype = prototype(
           x = matrix(numeric()), levels = list(), bbox = matrix(numeric())
         )
)

#' A meta class for \code{agentMatrix} and \code{SpatialPointsDataFrame}
#'
#' Both these types can be used by NetLogoR to describe turtle agents.
#'
#' @aliases agentClasses
#' @author Eliot McIntire
#' @exportClass agentClasses
#' @importClassesFrom sp SpatialPixelsDataFrame SpatialPointsDataFrame
#' @name agentClasses-class
#' @rdname agentClasses-class
setClassUnion(name = "agentClasses",
              members = c("agentMatrix", "SpatialPointsDataFrame", "SpatialPixelsDataFrame")
)
