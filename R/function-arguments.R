################################################################################
#' Function arguments
#'
#' @keywords integral
#'
#' @name fargs
#'
#'
#' @param n Integer.
#'
#' @param world \code{WorldMatrix} or \code{worldArray} object.
#'
#' @param torus Logical to determine if the \code{world} is wrapped. Default is
#'              \code{torus = FALSE}.
#'
#' @param minPxcor Integer. Minimum \code{pxcor} for the \code{patches} (\code{world}'s left border).
#'
#' @param maxPxcor Integer. Maximum \code{pxcor} for the \code{patches} (\code{world}'s right border).
#'
#' @param minPycor Integer. Minimum \code{pycor} for the \code{patches} (\code{world}'s bottom border).
#'
#' @param maxPycor Integer. Maximum \code{pycor} for the \code{patches} (\code{world}'s top border).
#'
#' @param pxcor Integer. Vector of patches \code{pxcor} coordinates. Must be of length 1
#'              or of the same length as \code{pycor}.
#'
#' @param pycor Integer. Vector of patches \code{pycor} coordinates. Must be of length 1
#'              or of the same length as \code{pxcor}.
#'
#' @param cellNum Integer. Vector of cells number.
#'
#' @param pVar Character. If the \code{world} is a \code{worldArray} object, \code{pVar}
#'             is the name of the layer to use to define the \code{patches} values.
#'             \code{pVar} must not be provided if the \code{world} is a \code{worldMatrix} object.
#'
#' @param turtles \code{AgentMatrix} object representing the moving \code{agents}.
#'
#' @param patches Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second
#'                column \code{pycor} representing the \code{patches} coordinates.
#'
#' @param agents Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second
#'               column \code{pycor} representing the \code{patches} coordinates, or
#'
#'               \code{AgentMatrix} object representing the moving \code{agents}.
#'
#' @param agents2 Matrix (\code{ncol} = 2) with the first column \code{pxcor} and the second
#'                column \code{pycor} representing the \code{patches} coordinates, or
#'
#'                \code{AgentMatrix} object representing the moving \code{agents}, or
#'
#'                Matrix (\code{ncol} = 2) with the first column \code{x} and the second column
#'                \code{y} representing locations coordinates.
#'
#' @param nNeighbors Integer: 4 or 8. Represents the number of neighbor \code{patches}
#'                   considered.
#'
#' @param dx Numeric. Vector of distances to the east (right) from the \code{agents}.
#'           If \code{dx} is negative, the distance to the west (left) is computed.
#'           \code{dx} must be of length 1 or of the same length as number of \code{patches}
#'           or \code{turtles} in \code{agents}.
#'
#' @param dy Numeric. Vector of distances to the north (up) from the \code{agents}.
#'           If \code{dy} is negative, the distance to the south is computed (down).
#'           \code{dy} must be of length 1 or of the same length as number of \code{patches}
#'           or \code{turtles} in \code{agents}.
#'
#' @param color Character. Vector of \code{color} names. Must be of length \code{n}.
#'              If missing, \code{colors} are assigned using the function \code{rainbow(n)}.
#'
#' @param who Integer. Vector of the \code{who} numbers for the selected \code{turtles}.
#'
#' @param breed Characters. Vector of \code{breed} names for the selected \code{turtles}.
#'              If missing, there is no distinction based upon \code{breed}.
#'
#' @param var Character. The name of the selected \code{agents} variable.
#'            If \code{agents} are \code{patches} and the \code{world} is a
#'            \code{worldMatrix} object, \code{var} must not be provided. If
#'            \code{agents} are \code{patches} and the \code{world} is a \code{worldArray}
#'            object, \code{var} is the name of the layer to use to define the \code{patches}
#'            values. If \code{agents} are \code{turtles}, \code{var} is one of
#'            the \code{turtles}' variable and can be equal to \code{xcor},
#'            \code{ycor}, any of the variables created when \code{turtles} were created,
#'            as well as any variable created using \code{turtlesOwn()}.
#'
#' @param val Numeric or character. Vector of any length.
#'
#'
NULL
