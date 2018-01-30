#' The \code{NetLogoR} package
#'
#' The suggested package \pkg{fastshp} can be installed with
#' \code{install.packages("fastshp", repos = "https://rforge.net", type = "source")}.
"_PACKAGE"

#' @import methods
#' @import raster
NULL

#' Internal CRS usage
#'
#' This is used in various places when converting \code{agentMatrix} objects to
#' \code{SpatialPoints} for use with functions, such as \code{\link[raster]{buffer}}.
#' The \code{CRS()} operation is relatively time consuming.
#' Having a single object useable throughout is \emph{much} faster.
#'
#' @keywords internal
#' @rdname projNowhere
.projNowhere <- CRS("+proj=utm")
