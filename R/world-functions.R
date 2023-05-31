################################################################################
#' Maximum `pxcor`
#'
#' Report the patches maximum `pxcor` in the `world`.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor>
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
#' Maximum `pycor`
#'
#' Report the patches maximum `pycor` in the `world`.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor>
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
#' Minimum `pxcor`
#'
#' Report the patches minimum `pxcor` in the `world`.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor>
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
#' Minimum `pycor`
#'
#' Report the patches minimum `pycor` in the `world`.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor>
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
#' `World` width
#'
#' Report the width of the `world` in `patch` number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim>
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
#' `World` height
#'
#' Report the height of the `world` in `patch` number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim>
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
#' Clear `world`'s `patches`
#'
#' Reset all `patches` values to `NA`.
#'
#' @inheritParams fargs
#'
#' @return `WorldMatrix` object with `NA` values for all `patches`.
#'
#' @seealso <https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-patches>
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
#' @rdname clearPatches
#'
#' @author Sarah Bauduin
#'
#' @include worldNLR-classes-methods.R
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
#' Convert a `Raster*` object into a `worldMatrix` or `worldArray` object
#'
#' Convert a `RasterLayer` object into a `worldMatrix` object or a `RasterStack` object
#' into a `worldArray` object.
#'
#' @param raster `RasterLayer` or `RasterStack` object.
#'
#' @return `WorldMatrix` or `worldArray` object depending on the input `raster`.
#'         `Patches` value are retained from the `raster`.
#'
#' @details See `help("worldMatrix-class")` or `help("worldArray-class")`
#'          for more details on the classes.
#'
#'          The number of rows and columns, as well as the cell values of the `raster`
#'          are kept the same. However, to match the coordinates system and resolution of a
#'          `worldMatrix` or `worldArray`, the grid is shifted by a 1/2 cell to have
#'          round coordinate values at the center of the patches and patch size is equal to (1,1).
#'          The bottom left corner cell coordinates of the `worldMatrix` or `worldArray`
#'          will be (pxcor = 0, pycor = 0).
#'
#' @examples
#' r1 <- raster(extent(c(0, 10, 0, 10)), nrows = 10, ncols = 10)
#' r1[]<-runif(100)
#' w1 <- raster2world(r1)
#' plot(r1)
#' plot(w1)
#'
#'
#' @export
#' @rdname raster2world
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "raster2world",
  function(raster) {
    standardGeneric("raster2world")
})

#' @export
#' @rdname raster2world
setMethod(
  "raster2world",
  signature = c("RasterLayer"),
  definition = function(raster) {

    world <- createWorld(minPxcor = 0, maxPxcor = raster@ncols - 1,
                         minPycor = 0, maxPycor = raster@nrows - 1,
                         data = values(raster))

    return(world)
})

#' @export
#' @rdname raster2world
#' @importFrom raster unstack
setMethod(
  "raster2world",
  signature = c("RasterStack"),
  definition = function(raster) {

    rasList <- raster::unstack(raster)
    names(rasList) <- names(raster)
    worldList <- lapply(rasList, function(ras) {
      raster2world(ras)
    })
    wArray <- do.call(stackWorlds, worldList)

    return(wArray)
})

################################################################################
#' Convert a `SpatRaster` object into a `worldMatrix` or `worldArray` object
#'
#' Convert a `SpatRaster` object into a `worldMatrix` 
#' object or a `worldArray` object depending on the number of layers of the
#' `SpatRaster` object.
#'
#' @param raster `SpatRaster` object.
#'
#' @return `WorldMatrix` or `worldArray` object depending on the number of layers
#' of the input `raster`.
#'         `Patches` value are retained from the `raster`.
#'
#' @details See `help("worldMatrix-class")` or `help("worldArray-class")`
#'          for more details on the classes.
#'          
#'          If the `SpatRaster` object has only one layer, a `worldMatrix` object
#'          will be returned. If the `SpatRaster` object has more than one layer,
#'          layers must have unique names and a `worldArray` object will be returned.
#'
#'          The number of rows and columns, as well as the cell values of the `raster`
#'          are kept the same. However, to match the coordinates system and resolution of a
#'          `worldMatrix` or `worldArray`, the grid is shifted by a 1/2 cell to have
#'          round coordinate values at the center of the patches and patch size is equal to (1,1).
#'          The bottom left corner cell coordinates of the `worldMatrix` or `worldArray`
#'          will be (pxcor = 0, pycor = 0).
#'
#' @examples
#' r1 <- rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10, nrows = 10, ncols = 10)
#' r1[]<-runif(100)
#' w1 <- spatRast2world(r1)
#' plot(r1)
#' plot(w1)
#' 
#' r2 <- rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10, nrows = 10, ncols = 10)
#' r2[]<- 0
#' r3 <- c(r1, r2)
#' names(r3) <- c("layer1", "layer2")
#' w3 <- spatRast2world(r3)
#' plot(r3)
#' plot(w3)
#'
#' @export
#' @rdname spatRast2world
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "spatRast2world",
  function(raster) {
    standardGeneric("spatRast2world")
  })

#' @export
#' @rdname spatRast2world
setMethod(
  "spatRast2world",
  signature = c("SpatRaster"),
  definition = function(raster) {
    
    if(dim(raster)[3] == 1){ # one layer raster
      world <- createWorld(minPxcor = 0, maxPxcor = ncol(raster) - 1,
                           minPycor = 0, maxPycor = nrow(raster) - 1,
                           data = values(raster))
    } else { # multiple layer raster
      worldList <- list()
      for(lay in 1:dim(raster)[3]){
        worldList[[lay]] <- createWorld(minPxcor = 0, maxPxcor = ncol(raster) - 1,
                                        minPycor = 0, maxPycor = nrow(raster) - 1,
                                        data = values(raster)[, lay])
      }
      if(any(duplicated(names(raster)))){
        stop("Each layer of the SpatRaster must have a unique name")
      }
      names(worldList) <- names(raster)
      world <- do.call(stackWorlds, worldList)
    }
    
    return(world)
  })

################################################################################
#' Convert a `worldMatrix` or `worldArray` object into a `Raster*` object
#'
#' Convert a `worldMatrix` object into a `RasterLayer` object or a
#' `worldArray` object into a `RasterStack` object
#'
#' @inheritParams fargs
#'
#' @return `RasterLayer` or `RasterStack` object depending on the input `world`.
#'         `Patches` value are retained from the `world`.
#'
#' @details The `Raster*` returned has the same extent and resolution as the `world`
#'          with round coordinates at the center of the cells and coordinates `x.5`
#'          at the edges of the cells.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = runif(100))
#' r1 <- world2raster(w1)
#' plot(r1)
#'
#'
#' @export
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

################################################################################
#' Convert a `worldMatrix` or `worldArray` object into a `SpatRaster` object
#'
#' Convert a `worldMatrix` object or a
#' `worldArray` object into a `SpatRaster` object
#'
#' @inheritParams fargs
#'
#' @return `SpatRaster` object.
#'         `Patches` value are retained from the `world`.
#'
#' @details The `SpatRaster` returned has the same extent and resolution as the `world`
#'          with round coordinates at the center of the cells and coordinates `x.5`
#'          at the edges of the cells.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = runif(100))
#' r1 <- world2spatRast(w1)
#' plot(r1)
#' 
#' w2 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = 0)
#' w3 <- stackWorlds(w1, w2)
#' r3 <- world2spatRast(w3)
#' plot(r3)
#'
#'
#' @export
#' @rdname world2spatRast
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "world2spatRast",
  function(world) {
    standardGeneric("world2spatRast")
  })


#' @export
#' @rdname world2spatRast
setMethod(
  "world2spatRast",
  signature = c("worldMatrix"),
  definition = function(world) {
    ras <- rast(xmin = world@extent@xmin, xmax = world@extent@xmax,
                ymin = world@extent@ymin, ymax = world@extent@ymax,
                ncol = ncol(world), nrow = nrow(world))
    values(ras) <- world@.Data
    
    return(ras)
  })


#' @export
#' @rdname world2spatRast
setMethod(
  "world2spatRast",
  signature = c("worldArray"),
  definition = function(world) {
    
    listRaster <- lapply(1:dim(world)[3], function(x) {
      ras <- rast(xmin = world@extent@xmin, xmax = world@extent@xmax,
                  ymin = world@extent@ymin, ymax = world@extent@ymax,
                  ncol = ncol(world), nrow = nrow(world), vals = world@.Data[, , x])
    })
    rasterStack <- rast(listRaster)
    names(rasterStack) <- colnames(world[,,])
    return(rasterStack)
  })


################################################################################
#' Key base R functions for `worldNLR` classes
#'
#' Slight modifications from the default versions.
#'
#' @param object  An `agentMatrix` object.
#'
#' @export
#' @importFrom quickPlot numLayers
#' @rdname show-methods
setMethod(
  "show",
  signature(object = "worldArray"),
  definition = function(object) {
    cat("class       :", class(object), "\n")
    cat("resolution  :", paste(object@res, collapse = ", "), "(x, y)\n")
    cat("dimensions  : Pxcor: ", object@minPxcor, ",", object@maxPxcor, "\n",
        "             Pycor: ", object@minPycor, ",", object@maxPycor, "\n")

    # Copied and modified from show method in Raster
      minv <- format(apply(object@.Data, 3, min))
      maxv <- format(apply(object@.Data, 3, max))
      minv <- gsub("Inf", "?", minv)
      maxv <- gsub("-Inf", "?", maxv)
      nl <- numLayers(object)
      mnr <- 15

      if (nl > mnr) {
        minv <- c(minv[1:mnr], "...")
        maxv <- c(maxv[1:mnr], "...")
      }

      ln <- dimnames(object)[[3]]
      n <- nchar(ln)
      if (nl > 5) {
        b <- n > 26
        if (any(b)) {
          mid <- floor(n/2)
          ln[b] <- paste(substr(ln[b], 1, 9), "//",
                         substr(ln[b], nchar(ln[b]) - 9, nchar(ln[b])), sep = "")
        }
      }

      w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
      m <- rbind(ln, minv, maxv)
      # a loop because 'width' is not recycled by format
      for (i in 1:ncol(m)) {
        m[, i]   <- format(m[, i], width = w[i], justify = "right")
      }
      cat("names       :", paste(m[1, ], collapse = ", "), "\n")
      cat("min values  :", paste(m[2, ], collapse = ", "), "\n")
      cat("max values  :", paste(m[3, ], collapse = ", "), "\n")

    # } else {
    #   cat("names       :", paste(ln, collapse=", "), "\n")
    # }
      dims <- dim(object@.Data)
      dims <- pmin(dims, 4)
      if (any(dims >= 4))
        cat("First",dims[1],"rows and ",dims[2],"columns:\n")

    # cat("First 4 rows and columns:\n")
      print(object@.Data[1:dims[1], 1:dims[2], ])
})

#' @export
#' @rdname show-methods
setMethod(
  "show",
  signature(object = "worldMatrix"),
  definition = function(object) {
    cat("class       :", class(object), "\n")
    cat("resolution  :", paste(object@res, collapse = ", "), "(x, y)\n")
    cat("dimensions  : Pxcor: ", object@minPxcor, ",", object@maxPxcor, "\n",
        "             Pycor: ", object@minPycor, ",", object@maxPycor, "\n")

    # Copied and modified from show method in Raster
    minv <- format(min(object@.Data))
    maxv <- format(max(object@.Data))
    minv <- gsub("Inf", "?", minv)
    maxv <- gsub("-Inf", "?", maxv)

    ln <- "layer"
    w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
    m <- rbind(ln, minv, maxv)
    # a loop because 'width' is not recycled by format
    m   <- format(m, width = w, justify = "right")
    cat("names       :", paste(m[1, ], collapse = ", "), "\n")
    cat("min values  :", paste(m[2, ], collapse = ", "), "\n")
    cat("max values  :", paste(m[3, ], collapse = ", "), "\n")

    dims <- dim(object@.Data)
    dims <- pmin(dims, 4)
    if (any(dims >= 4))
      cat("First",dims[1],"rows and ",dims[2],"columns:\n")
    print(object@.Data[1:dims[1], 1:dims[2]])
})
