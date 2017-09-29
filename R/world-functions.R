################################################################################
#' Maximum pxcor
#'
#' Report the patches maximum pxcor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' maxPxcor(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname maxPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPxcor",
  function(world) {
    standardGeneric("maxPxcor")
  })

#' @export
#' @rdname maxPxcor
setMethod(
  "maxPxcor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@maxPxcor)
  }
)


################################################################################
#' Maximum pycor
#'
#' Report the patches maximum pycor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' maxPycor(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname maxPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPycor",
  function(world) {
    standardGeneric("maxPycor")
  })

#' @export
#' @rdname maxPycor
setMethod(
  "maxPycor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@maxPycor)
  }
)


################################################################################
#' Minimum pxcor
#'
#' Report the patches minimum pxcor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' minPxcor(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname minPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minPxcor",
  function(world) {
    standardGeneric("minPxcor")
  })

#' @export
#' @rdname minPxcor
setMethod(
  "minPxcor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@minPxcor)
  }
)


################################################################################
#' Minimum pycor
#'
#' Report the patches minimum pycor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' minPycor(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname minPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minPycor",
  function(world) {
    standardGeneric("minPycor")
  })

#' @export
#' @rdname minPycor
setMethod(
  "minPycor",
  signature = "worldNLR",
  definition = function(world) {
    return(world@minPycor)
  }
)


################################################################################
#' World width
#'
#' Report the width of the \code{world} in patch number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' worldWidth(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname worldWidth
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "worldWidth",
  function(world) {
    standardGeneric("worldWidth")
  })

#' @export
#' @rdname worldWidth
setMethod(
  "worldWidth",
  signature = "worldNLR",
  definition = function(world) {
    wWidth <- maxPxcor(world) - minPxcor(world) + 1
    return(wWidth)
  }
)


################################################################################
#' World height
#'
#' Report the height of the \code{world} in patch number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' worldHeight(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname worldHeight
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "worldHeight",
  function(world) {
    standardGeneric("worldHeight")
  })

#' @export
#' @rdname worldHeight
setMethod(
  "worldHeight",
  signature = "worldNLR",
  definition = function(world) {
    wHeight <- maxPycor(world) - minPycor(world) + 1
    return(wHeight)
  }
)


################################################################################
#' Clear world's patches
#'
#' Reset all patches values to \code{NA}.
#'
#' @inheritParams fargs
#'
#' @return WorldMatrix object with \code{NA} values for all patches.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-patches}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld()
#' w1 <- NLset(world = w1, agents = patches(w1), val = runif(NLcount(patches(w1))))
#' w1Val <- of(world = w1, agents = patches(w1))
#' summary(w1Val)
#'
#' w1 <- clearPatches(w1)
#' w1Val <- of(world = w1, agents = patches(w1))
#' summary(w1Val)
#'
#'
#' @export
#' @docType methods
#' @rdname clearPatches
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "clearPatches",
  function(world) {
    standardGeneric("clearPatches")
  }
)

#' @export
#' @rdname clearPatches
setMethod(
  "clearPatches",
  signature = c("worldMatrix"),
  definition = function(world) {
    world@.Data[] <- NA
    return(world)
  }
)

#' @export
#' @rdname clearPatches
setMethod(
  "clearPatches",
  signature = c("worldArray"),
  definition = function(world) {
    worldNA <- createWorld(minPxcor = minPxcor(world), maxPxcor = maxPxcor(world),
                                   minPycor = minPycor(world), maxPycor = maxPycor(world))
    return(worldNA)
  }
)


################################################################################
#' Convert a Raster* object into a worldMatrix or worldArray object
#'
#' Convert a RasterLayer object into a worldMatrix object or a RasterStack object
#' into a worldArray object.
#'
#' @param raster RasterLayer or RasterStack object.
#'
#' @param method "ngb" or "bilinear" for the resample method.
#'
#' @return WorldMatrix or worldArray object depending on the input \code{raster}.
#'         Patches value are retained from the \code{raster}.
#'
#' @details See \code{help("worldMatrix-class")} or \code{help("worldArray-class")}
#'          for more details on the classes.
#'
#'          The \code{raster} is resampled to match the coordinates system and
#'          resolution of a worldMatrix or worldArray using the chosen \code{method}. The
#'          extent will be bigger by 1 on the width and on the height.
#'
#' @examples
#' r1 <- raster(extent(c(0,10,0,10)), nrows = 10, ncols = 10)
#' r1[]<-runif(100)
#' w1 <- raster2world(r1, method = "ngb")
#' plot(r1)
#' plot(w1)
#'
#'
#' @export
#' @importFrom abind abind
#' @docType methods
#' @rdname raster2world
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "raster2world",
  function(raster, method) {
    standardGeneric("raster2world")
  })


#' @export
#' @rdname raster2world
setMethod(
  "raster2world",
  signature = c("RasterLayer", "character"),
  definition = function(raster, method) {

    minPxcor <- round(raster@extent@xmin)
    maxPxcor <- round(raster@extent@xmax)
    minPycor <- round(raster@extent@ymin)
    maxPycor <- round(raster@extent@ymax)
    world <- createWorld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                                 minPycor = minPycor, maxPycor = maxPycor)
    worldRaster <- raster(world@extent)
    res(worldRaster) <- c(1, 1)

    worldR <- resample(raster, worldRaster, method = method)

    world[] <- values(worldR)

    return(world)
  })


#' @export
#' @rdname raster2world
setMethod(
  "raster2world",
  signature = c("RasterStack", "character"),
  definition = function(raster, method) {

    minPxcor <- round(raster@extent@xmin)
    maxPxcor <- round(raster@extent@xmax)
    minPycor <- round(raster@extent@ymin)
    maxPycor <- round(raster@extent@ymax)
    world <- createWorld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                                 minPycor = minPycor, maxPycor = maxPycor)
    worldRaster <- raster(world@extent)
    res(worldRaster) <- c(1, 1)

    worldR <- lapply(1:nlayers(raster), function(x) {
      layer <- resample(raster[[x]], worldRaster, method = method)
      matrix(values(layer), ncol = dim(world)[2], byrow = TRUE)
    })

    out <- abind::abind(worldR, along = 3)
    dimnames(out) <- list(NULL, NULL, names(raster))

    wArray <- new("worldArray",
                      .Data = out,
                      minPxcor = minPxcor, maxPxcor = maxPxcor,
                      minPycor = minPycor, maxPycor = maxPycor,
                      extent = world@extent,
                      res = c(1, 1),
                      pCoords = world@pCoords
    )

    return(wArray)
  })


################################################################################
#' Convert a worldMatrix or worldArray object into a Raster* object
#'
#' Convert a worldMatrix object into a RasterLayer object or a
#' worldArray object into a RasterStack object
#'
#' @inheritParams fargs
#'
#' @return RasterLayer or RasterStack object depending on the input \code{world}.
#'         Patches value are retained from the \code{world}.
#'
#' @details The Raster* returned has the same extent and resolution as the \code{world}
#'          with round coordinates at the center of the cells and coordinates \code{x.5}
#'          at the edges of the cells.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = runif(100))
#' r1 <- world2raster(w1)
#' plot(r1)
#'
#'
#' @export
#' @docType methods
#' @rdname world2raster
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "world2raster",
  function(world) {
    standardGeneric("world2raster")
  })


#' @export
#' @rdname world2raster
setMethod(
  "world2raster",
  signature = c("worldMatrix"),
  definition = function(world) {
    ras <- raster(world@.Data, xmn = world@extent@xmin, xmx = world@extent@xmax,
                     ymn = world@extent@ymin, ymx = world@extent@ymax)

    return(ras)
  })


#' @export
#' @rdname world2raster
setMethod(
  "world2raster",
  signature = c("worldArray"),
  definition = function(world) {

    listRaster <- lapply(1:dim(world)[3], function(x) {
      raster(world@.Data[, , x], xmn = world@extent@xmin, xmx = world@extent@xmax,
             ymn = world@extent@ymin, ymx = world@extent@ymax)
    })
    rasterStack <- stack(listRaster)
    return(rasterStack)
})
