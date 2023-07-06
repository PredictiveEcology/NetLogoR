utils::globalVariables(c("xcor", "ycor"))

#' The `agentMatrix` class
#'
#' @name agentMatrix-class
#' @rdname agentMatrix-class
#' @author Eliot McIntire
#' @exportClass agentMatrix
#' @examples
#' newAgent <- new("agentMatrix",
#'   coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'   char = letters[c(1, 2, 6)],
#'   nums2 = c(4.5, 2.6, 2343),
#'   char2 = LETTERS[c(4, 24, 3)],
#'   nums = 5:7
#' )
#'
#' # compare speeds -- about 5x faster
#' if (requireNamespace("microbenchmark", quietly = TRUE) &&
#'   requireNamespace("sp", quietly = TRUE)) {
#'   microbenchmark::microbenchmark(
#'     times = 499,
#'     spdf = {
#'       sp::SpatialPointsDataFrame(
#'         coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'         data = data.frame(
#'           char = letters[c(1, 2, 6)],
#'           nums2 = c(4.5, 2.6, 2343),
#'           char2 = LETTERS[c(4, 24, 3)],
#'           nums = 5:7
#'         )
#'       )
#'     },
#'     agentMat = {
#'       agentMatrix(
#'         coords = cbind(
#'           pxcor = c(1, 2, 5),
#'           pycor = c(3, 4, 6)
#'         ),
#'         char = letters[c(1, 2, 6)],
#'         nums2 = c(4.5, 2.6, 2343),
#'         char2 = LETTERS[c(4, 24, 3)],
#'         nums = 5:7
#'       )
#'     },
#'     agentMatDirect = {
#'       new("agentMatrix",
#'         coords = cbind(
#'           pxcor = c(1, 2, 5),
#'           pycor = c(3, 4, 6)
#'         ),
#'         char = letters[c(1, 2, 6)],
#'         nums2 = c(4.5, 2.6, 2343),
#'         char2 = LETTERS[c(4, 24, 3)],
#'         nums = 5:7
#'       )
#'     }
#'   )
#' }
setClass("agentMatrix",
  contains = "matrix",
  slots = c(x = "matrix", levels = "list", bbox = "matrix"),
  prototype = prototype(
    x = matrix(numeric()), levels = list(), bbox = matrix(numeric())
  )
)
