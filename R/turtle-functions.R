################################################################################
#' Create turtles
#'
#' Create \code{n} moving agents with a set of defined variables.
#'
#' @inheritParams fargs
#'
#' @param coords  Matrix (ncol = 2) with the first column "xcor" and the second
#'                column "ycor" representing the turtles inital locations.
#'                \code{nrow(coords)} must be equal to 1 or to \code{n}.
#'                Given coordinates must be inside the \code{world}'s extent. If missing,
#'                turtles are put in the center of the \code{world}.
#'
#' @param heading Numeric. Vector of values between 0 and 360. Must be of length 1 or
#'                of length \code{n}. If missing, a random heading is assigned to
#'                each turtle.
#'
#' @param breed   Character. Vector of "breed" names. Must be of length 1 or of length
#'                \code{n}. If missing, \code{breed = "turtle"} for all turtles.
#'
#' @return AgentMatrix object of length \code{n} with data for the
#'         turtles being: "xcor", "ycor", "who", "heading", "prevX", "prevY",
#'         "breed", and "color".
#'
#' @details If \code{coords} is provided, \code{world} must not be provided.
#'
#'          The identity of the turtles is defined by their "who" number. This
#'          numbering starts at 0 and increments by 1.
#'
#'          The coordinates from the previous time step are stored in "prevX" and
#'          "prevY". The initial values are \code{NA}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname createTurtles
#'
#' @author Sarah Bauduin
setGeneric(
  "createTurtles",
  function(n, coords, world, heading, breed, color) {
    standardGeneric("createTurtles")
})

#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#' @rdname createTurtles
setMethod(
  "createTurtles",
  signature = c("numeric", "matrix", "missing", "ANY", "ANY", "ANY"),
  definition = function(n, coords, world, heading, breed, color) {

    if (missing(heading)) heading <- runif(n = n, min = 0, max = 360)

    if (missing(breed)) breed <- "turtle"

    if (missing(color)) color <- rainbow(n)

    repNA <- rep(NA, n)
    turtles <- new("agentMatrix",
                 coords = coords,
                 who = seq_len(n) - 1,
                 heading = heading,
                 prevX = repNA,
                 prevY = repNA,
                 breed = breed,
                 color = color)
    return(turtles)
})

#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#' @rdname createTurtles
setMethod(
  "createTurtles",
  signature = c("numeric", "missing", "ANY", "ANY", "ANY", "ANY"),
  definition = function(n, coords, world, heading, breed, color) {
    coords <- cbind(xcor = rep(((maxPxcor(world) - minPxcor(world)) / 2) + minPxcor(world), n),
                    ycor = rep(((maxPycor(world) - minPycor(world)) / 2) + minPycor(world), n))

    if (missing(heading))
      heading <- runif(n = n, min = 0, max = 360)

    if (missing(breed))
      breed <- "turtle"

    if (missing(color))
      color <- rainbow(n)

    turtles <- new("agentMatrix", coords = coords, who = seq(from = 0, to = n - 1, by = 1),
                   heading = heading, prevX = rep(NA, n), prevY = rep(NA, n),
                   breed = breed, color = color)
    return(turtles)
  }
)


################################################################################
#' Create ordered turtles
#'
#' Create \code{n} turtles at the center of the \code{world} with their headings evenly
#' distributed.
#'
#' @inheritParams createTurtles
#'
#' @return AgentMatrix object of length \code{n} with data for the
#'         turtles being: "xcor", "ycor", "who", "heading", "prevX", "prevY", "breed",
#'         and "color".
#'
#' @details The identity of the turtles is defined by their "who" number. This
#'          numbering starts at 0 and increments by 1.
#'
#'          The coordinates from the previous time step are stored in "prevX" and
#'          "prevY". The initial values are \code{NA}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-ordered-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createOTurtles(n = 10, world = w1)
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- fd(turtles = t1, dist = 1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname createOTurtles
#'
#' @author Sarah Bauduin and Eliot McIntire
#'
setGeneric(
  "createOTurtles",
  function(n, world, breed, color) {
    standardGeneric("createOTurtles")
})

#' @export
#' @importFrom grDevices rainbow
#' @rdname createOTurtles
setMethod(
  "createOTurtles",
  signature = c(n = "numeric", world = "ANY"),
  definition = function(n, world, breed, color) {

    heading <- numeric(n)
    heading[1] <- 0
    if (n > 1) {
      heading[2:n] <- heading[1:(n - 1)] + (360 / n) * (1:(n - 1))
    }

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]

    if (missing(breed)) {
      li$breed <- rep("turtle", n)
    }

    if (length(li$breed) == 1) {
      li$breed <- rep(li$breed, n)
    }

    if (missing(color)) {
      li$color <- rainbow(n)
    }

    createTurtles(n = n, world = world, heading = heading, breed = li$breed,
                    color = li$color)
})


################################################################################
#' Move forward
#'
#' Move the \code{turtles} forward with their headings as directions.
#'
#' @inheritParams fargs
#'
#' @param dist    Numeric. Vector of distances to move. Must
#'                be of length 1 or of length \code{turtles}.
#'
#' @param out     Logical. Determine if a turtle should move when
#'                \code{torus = FALSE} and its ending position will be outside of
#'                the \code{world}'s extent. Default is \code{out = TRUE}.
#'
#' @return AgentMatrix representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If \code{torus = FALSE} and \code{out = TRUE}, \code{world}
#'          does not need to be provided.
#'
#'          If a distance to move leads a turtle outside of the \code{world}'s extent
#'          and \code{torus = TRUE}, the turtle is
#'          relocated on the other side of the \code{world}, inside its extent; if
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          \code{world}'s extent; if \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{fd()} (i.e., its current position).
#'
#'          If a given \code{dist} value is negative, then the turtle moves
#'          backward.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#forward}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#jump}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createOTurtles(n = 10, world = w1)
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- fd(turtles = t1, dist = 1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname fd
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "fd",
  function(turtles, dist, world, torus = FALSE, out = TRUE) {
    standardGeneric("fd")
})

#' @export
#' @importFrom CircStats rad
#' @rdname fd
setMethod(
  "fd",
  signature = c(turtles = "agentMatrix", dist = "numeric"),
  definition = function(turtles, dist, world, torus, out) {

    turtles@.Data[, c("prevX", "prevY")] <- turtles@.Data[, 1:2]

    inRads <- rad(turtles@.Data[, "heading"])
    fdXcor <- turtles@.Data[, "prevX"] + sin(inRads) * dist
    fdYcor <- turtles@.Data[, "prevY"] + cos(inRads) * dist

    if (torus == TRUE) {
      if (missing(world)) {
        stop("A world must be provided as torus = TRUE")
      }
      coords <- c(fdXcor, fdYcor)
      dim(coords) <- c(length(fdXcor), 2L)
      colnames(coords) <- c("x", "y")
      tCoords <- wrap(coords, extent(world))
      fdXcor <- tCoords[, 1]
      fdYcor <- tCoords[, 2]
    }

    if (torus == FALSE & out == FALSE) {
      if (missing(world)) {
        stop("A world must be provided as torus = FALSE and out = FALSE")
      }
      outX <- fdXcor < world@extent@xmin | fdXcor > world@extent@xmax
      outY <- fdYcor < world@extent@ymin | fdYcor > world@extent@ymax
      outXY <- which(outX | outY) # position of turtles out of the world's extent
      fdXcor[outXY] <- turtles@.Data[, "prevX"][outXY]
      fdYcor[outXY] <- turtles@.Data[, "prevY"][outXY]
    }

    turtles@.Data[, 1:2] <- c(round(fdXcor, digits = 5),
                             round(fdYcor, digits = 5))


    return(turtles)
  }
)


################################################################################
#' Move backward
#'
#' Move the \code{turtles} backward of their headings' directions.
#'
#' @inheritParams fargs
#'
#' @inheritParams fd
#'
#' @return AgentMatrix representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If \code{torus = FALSE} and \code{out = TRUE}, \code{world}
#'          does not need to be provided.
#'
#'          If a distance to move leads a turtle outside of the \code{world}'s extent
#'          and \code{torus = TRUE}, the turtle is
#'          relocated on the other side of the \code{world}, inside its extent; if
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          \code{world}'s extent; if \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{bk()} (i.e., its current position).
#'
#'          If a given \code{dist} value is negative, then the turtle moves
#'          forward.
#'
#'          The \code{turtles}' headings are not affected by the function (i.e., the
#'          \code{turtles} do not face backward).
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#back}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#jump}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createOTurtles(n = 10, world = w1)
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- fd(turtles = t1, dist = 2)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#' t1 <- bk(turtles = t1, dist = 1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#' t1 <- fd(turtles = t1, dist = 0.5)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname bk
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "bk",
  function(turtles, dist, world, torus = FALSE, out = TRUE) {
    standardGeneric("bk")
})

#' @export
#' @rdname bk
setMethod(
  "bk",
  signature = c(turtles = "agentMatrix", dist = "numeric"),
  definition = function(turtles, dist, world, torus, out) {
    fd(turtles = turtles, dist = -dist, world = world, torus = torus, out = out)
  }
)


################################################################################
#' Return home
#'
#' Move the \code{turtles} back \code{home}.
#'
#' @inheritParams fargs
#'
#' @param home    Character. Can take one of the following options to define where
#'                to relocate the \code{turtles}:
#'
#'                \code{home = "home0"} will place the \code{turtles} at the location
#'                \code{x = 0, y = 0}.
#'
#'                \code{home = "center"} will place the \code{turtles} at the center of
#'                the \code{world}.
#'
#'                \code{home = "pCorner"} will place the \code{turtles} at the center of
#'                the patch located in the left bottom corner of the \code{world}.
#'
#'                \code{home = "corner"} will place the \code{turtles} at the left bottom
#'                corner of the \code{world}.
#'
#' @return AgentMatrix representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#home}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' plot(w1)
#' points(t1, col = "black", pch = 16)
#'
#' t1 <- home(world = w1, turtles = t1, home = "pCorner")
#' points(t1, col = "red", pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname home
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "home",
  function(world, turtles, home) {
    standardGeneric("home")
  })

#' @export
#' @rdname home
setMethod(
  "home",
  signature = c("worldNLR", "agentMatrix", "character"),
  definition = function(world, turtles, home) {

    if (home == "home0") {
      if (world@extent@xmin <= 0 & world@extent@xmax >= 0 & world@extent@ymin <= 0
          & world@extent@ymax >= 0) {
        newTurtles <- setXY(turtles = turtles, xcor = 0, ycor = 0, world = world, torus = FALSE)
      } else {
        stop("The world provided does not contain the location [x = 0, y = 0]")
      }
    }

    if (home == "center") {
      newTurtles <- setXY(turtles = turtles,
                          xcor = ((world@extent@xmax - world@extent@xmin) / 2) +
                            world@extent@xmin,
                          ycor = ((world@extent@ymax - world@extent@ymin) / 2) +
                            world@extent@ymin,
                          world = world, torus = FALSE)
    }

    if (home == "pCorner") {
      newTurtles <- setXY(turtles = turtles, xcor = world@minPxcor,
                          ycor = world@minPycor, world = world, torus = FALSE)
    }

    if (home == "corner") {
      newTurtles <- setXY(turtles = turtles, xcor = world@extent@xmin,
                          ycor = world@extent@ymin, world = world, torus = FALSE)
    }

    return(newTurtles)
  }
)


################################################################################
#' x-increment
#'
#' Report the amount by which the \code{turtles}' coordinates xcor would change
#' if the \code{turtles} were
#' to move forward the given distances with their current headings.
#'
#' @inheritParams fargs
#'
#' @param dist    Numeric. Vector of distances the \code{turtles} would have to
#'                move forward to
#'                compute the increment values. Must be of length 1 or of length
#'                \code{turtles}. The default value is \code{dist = 1}.
#'
#' @return Numeric. Vector of length \code{turtles}.
#'
#' @details Report the sine of the \code{turtles}' heading multiplied by the \code{dist}
#'          values. Heading 0 is north and angles are calculated in degrees in a
#'          clockwise manner.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#dxy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createOTurtles(world = w1, n = 10)
#' dx(turtles = t1)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname dx
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "dx",
  function(turtles, dist = 1) {
    standardGeneric("dx")
  })

#' @export
#' @rdname dx
setMethod(
  "dx",
  signature = c("agentMatrix", "numeric"),
  definition = function(turtles, dist) {
    xIncr <- round(sin(rad(turtles@.Data[, "heading"])) * dist, digits = 5)
    return(xIncr)
  }
)

#' @export
#' @rdname dx
setMethod(
  "dx",
  signature = c("agentMatrix", "missing"),
  definition = function(turtles) {
    dx(turtles = turtles, dist = 1)
  }
)


################################################################################
#' y-increment
#'
#' Report the amount by which the \code{turtles}' coordinates ycor would change
#' if the \code{turtles} were
#' to move forward the given distances with their current headings.
#'
#' @inheritParams fargs
#'
#' @inheritParams dx
#'
#' @return Numeric. Vector of length \code{turtles}.
#'
#' @details Report the cosine of the \code{turtles}' heading multiplied by the \code{dist}
#'          values. Heading 0 is north and angles are calculated in degrees in a
#'          clockwise manner.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#dxy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createOTurtles(world = w1, n = 10)
#' dy(turtles = t1)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname dy
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "dy",
  function(turtles, dist = 1) {
    standardGeneric("dy")
  })

#' @export
#' @rdname dy
setMethod(
  "dy",
  signature = c("agentMatrix", "numeric"),
  definition = function(turtles, dist) {
    yIncr <- round(cos(rad(turtles@.Data[, "heading"])) * dist, digits = 5)
    return(yIncr)
  }
)

#' @export
#' @rdname dy
setMethod(
  "dy",
  signature = c("agentMatrix", "missing"),
  definition = function(turtles) {
    dy(turtles = turtles, dist = 1)
  }
)


################################################################################
#' Kill turtles
#'
#' Kill selected turtles.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix representing the \code{turtles} with the selected
#'         ones removed.
#'
#' @details The "who" numbers of the remaining \code{turtles} are unchanged.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#die}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' NLcount(t1)
#' t1 <- die(turtles = t1, who = c(2, 3, 4))
#' NLcount(t1)
#'
#'
#' @export
#' @docType methods
#' @rdname die
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "die",
  function(turtles, who) {
    standardGeneric("die")
  })

#' @export
#' @importFrom stats na.omit
#' @rdname die
setMethod(
  "die",
  signature = c("agentMatrix", "numeric"),
  definition = function(turtles, who) {
    if (length(who) != 0) {
      turtles <- turtles[-na.omit(match(who, turtles@.Data[, "who"])), ]
    }
    return(turtles)
})


################################################################################
#' Hatch new turtles
#'
#' Create new turtles from parent turtles.
#'
#' @inheritParams fargs
#'
#' @param n Integer. Vector of length 1 or of length \code{who}. Number of new turtles
#'          to create for each parent.
#'
#' @param breed   Character. One "breed" name. If missing,
#'                the created turtles are of the same "breed" as their parent turtle.
#'
#' @return AgentMatrix representing the \code{turtles} with the new
#'         hatched ones.
#'
#' @details The parent turtle must be contained in the \code{turtles}.
#'
#'          The created turtles inherit of all the data from the parent turtle,
#'          except for the "breed" if specified otherwise, and for the "who" numbers.
#'          The "who" numbers of the turtles created take on following the highest
#'          "who" number among the \code{turtles}.
#'
#'          All new hatched turtles are placed at the end of the agentMatrix object.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#hatch}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' NLcount(t1)
#' t1 <- hatch(turtles = t1, who = 0, n = 2)
#' NLcount(t1)
#'
#'
#' @export
#' @docType methods
#' @rdname hatch
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "hatch",
  function(turtles, who, n, breed) {
    standardGeneric("hatch")
  })

#' @export
#' @rdname hatch
setMethod(
  "hatch",
  signature = c("agentMatrix", "numeric", "numeric", "ANY"),
  definition = function(turtles, who, n, breed) {

    iTurtle <- match(who, turtles@.Data[, "who"])
    newData <- turtles@.Data[iTurtle, , drop = FALSE]

    # Allow n to be different
    if (length(n) != length(iTurtle)) {
      n <- rep(n, length(iTurtle))
    }
    if (any(n == 0)) {
      iTurtle <- iTurtle[n != 0]
      newData <- newData[n != 0, , drop = FALSE]
      n <- n[n != 0]
    }

    newData <- newData[rep(seq_len(nrow(newData)), n), , drop = FALSE]
    newData[, "who"] <- (max(turtles@.Data[, "who"]) + 1):(max(turtles@.Data[, "who"]) + sum(n))

    if (!missing(breed)) {
      if (!breed %in% turtles@levels$breed) {
        turtles@levels$breed <- c(turtles@levels$breed, breed)
      }
      newData[, "breed"] <- match(breed, turtles@levels$breed)
    }

    turtles@.Data <- rbind(turtles@.Data, newData)

    return(turtles)
  }
)


################################################################################
#' Can the turtles move?
#'
#' Report \code{TRUE} if a turtle can move the given distance without leaving
#' the \code{world}'s extent, report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @inheritParams fd
#'
#' @return Logical. Vector of length \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#can-move}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' canMove(world = w1, turtles = t1, dist = 1:10)
#'
#'
#' @export
#' @docType methods
#' @rdname canMove
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "canMove",
  function(world, turtles, dist) {
    standardGeneric("canMove")
  })

#' @export
#' @rdname canMove
setMethod(
  "canMove",
  signature = c("worldNLR", "agentMatrix", "numeric"),
  definition = function(world, turtles, dist) {
    wrapFalse <- fd(world = world, turtles = turtles, dist = dist, torus = FALSE)
    wrapTrue <- fd(world = world, turtles = turtles, dist = dist, torus = TRUE)
    testX <- wrapFalse@.Data[, "xcor"] == wrapTrue@.Data[, "xcor"]
    testY <- wrapFalse@.Data[, "ycor"] == wrapTrue@.Data[, "ycor"]
    return(testX & testY)
  }
)


################################################################################
#' Random xcor
#'
#' Report \code{n} random xcor coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of length \code{n} of xcor coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-cor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname randomXcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomXcor",
  function(world, n) {
    standardGeneric("randomXcor")
})

#' @export
#' @importFrom stats runif
#' @rdname randomXcor
setMethod(
  "randomXcor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {
    if (n == 0) {
      return(xcor = numeric())
    } else {
      xcor <- round(runif(n = n, min = world@extent@xmin, max = world@extent@xmax), digits = 5)
      return(xcor)
    }
})

################################################################################
#' Random ycor
#'
#' Report \code{n} random ycor coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of length \code{n} of ycor coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-cor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname randomYcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomYcor",
  function(world, n) {
    standardGeneric("randomYcor")
  })

#' @export
#' @importFrom stats runif
#' @rdname randomYcor
setMethod(
  "randomYcor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {

    if (n == 0) {
      return(ycor = numeric())
    } else {
      ycor <- round(runif(n = n, min = world@extent@ymin, max = world@extent@ymax), digits = 5)
      return(ycor)
    }
})


################################################################################
#' Directions towards
#'
#' Report the directions of each \code{agents} towards each corresponding \code{agents2}.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of angles in degrees of length equal to the largest
#'         number of agents/locations between \code{agents} and \code{agents2}.
#'
#' @details \code{agents} and \code{agents2} must have the same number of agents/locations
#'          or if different, one of them must have only one agent/location. If
#'          \code{agents} and \code{agents2} have the same number of agents/locations,
#'          the directions are calculated for each pair \code{agents[i]} and \code{agents2[i]}
#'          and not for each \code{agents} towards every single \code{agents2}.
#'
#'          If \code{torus = FALSE}, \code{world} does not need to be provided.
#'
#'          If \code{torus = TRUE} and the distance from one \code{agents} to
#'          its corresponding \code{agents2} is smaller around the
#'          sides of the \code{world} than across it, then the direction to \code{agents2}
#'          going around the sides of the \code{world} is returned.
#'
#'          The direction from a patch to its location returns 0; the direction from
#'          a turtle to its location returns the turtle's heading.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#towards}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#towardsxy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' towards(agents = patches(w1), agents2 = cbind(x = 0, y = 0))
#' t1 <- createTurtles(n = 10, world = w1)
#' towards(agents = t1, agents2 = cbind(x = 0, y = 0))
#'
#'
#' @export
#' @importFrom CircStats deg
#' @docType methods
#' @rdname towards
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "towards",
  function(agents, agents2, world, torus = FALSE) {
    standardGeneric("towards")
  })

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(agents = "matrix", agents2 = "matrix"),
  definition = function(agents, agents2, world, torus) {

    if (!inherits(agents, "agentMatrix") & !inherits(agents2, "agentMatrix")) {
      # patches to patches
      if (torus == FALSE) {
        heading <- deg(atan2(agents2[, 1] - agents[, 1], agents2[, 2] - agents[, 2]))
        # angles between -180 and 180
        heading[heading < 0] <- heading[heading < 0] + 360
      } else {
        if (missing(world)) {
          stop("A world must be provided as torus = TRUE")
        }

        if (NROW(agents2) == 1 & NROW(agents) != 1) {
          agents2 <- c(rep(agents2[, 1], NROW(agents)), rep(agents2[, 2], NROW(agents)))
          dim(agents2) <- c(NROW(agents), 2L)
        }
        if (NROW(agents) == 1 & NROW(agents2) != 1) {
          agents <- c(rep(agents[, 1], NROW(agents2)), rep(agents[, 2], NROW(agents2)))
          dim(agents) <- c(NROW(agents2), 2L)
        }

        # Need to create coordinates for "agents2" in a wrapped world
        # For all the 8 possibilities of wrapping (to the left, right, top, bottom and 4 corners)
        # Find the smallest distances across or around the world

        to1 <- cbind(agents2[, 1] - (world@extent@xmax - world@extent@xmin),
                     agents2[, 2] + (world@extent@ymax - world@extent@ymin))
        to2 <- cbind(agents2[, 1], agents2[, 2] + (world@extent@ymax - world@extent@ymin))
        to3 <- cbind(agents2[, 1] + (world@extent@xmax - world@extent@xmin),
                     agents2[, 2] + (world@extent@ymax - world@extent@ymin))
        to4 <- cbind(agents2[, 1] - (world@extent@xmax - world@extent@xmin), agents2[, 2])
        to5 <- cbind(agents2[, 1] + (world@extent@xmax - world@extent@xmin), agents2[, 2])
        to6 <- cbind(agents2[, 1] - (world@extent@xmax - world@extent@xmin),
                     agents2[, 2] - (world@extent@ymax - world@extent@ymin))
        to7 <- cbind(agents2[, 1], agents2[, 2] - (world@extent@ymax - world@extent@ymin))
        to8 <- cbind(agents2[, 1] + (world@extent@xmax - world@extent@xmin),
                     agents2[, 2] - (world@extent@ymax - world@extent@ymin))

        # All distances in a wrapped world
        dist_agents2 <- pointDistance(p1 = agents, p2 = agents2, lonlat = FALSE, allpairs = FALSE)
        dist_to1 <- pointDistance(p1 = agents, p2 = to1, lonlat = FALSE, allpairs = FALSE)
        dist_to2 <- pointDistance(p1 = agents, p2 = to2, lonlat = FALSE, allpairs = FALSE)
        dist_to3 <- pointDistance(p1 = agents, p2 = to3, lonlat = FALSE, allpairs = FALSE)
        dist_to4 <- pointDistance(p1 = agents, p2 = to4, lonlat = FALSE, allpairs = FALSE)
        dist_to5 <- pointDistance(p1 = agents, p2 = to5, lonlat = FALSE, allpairs = FALSE)
        dist_to6 <- pointDistance(p1 = agents, p2 = to6, lonlat = FALSE, allpairs = FALSE)
        dist_to7 <- pointDistance(p1 = agents, p2 = to7, lonlat = FALSE, allpairs = FALSE)
        dist_to8 <- pointDistance(p1 = agents, p2 = to8, lonlat = FALSE, allpairs = FALSE)

        # Which distance is the minimum
        allDist <- cbind(dist_agents2, dist_to1, dist_to2, dist_to3, dist_to4, dist_to5,
                         dist_to6, dist_to7, dist_to8)
        distMin <- apply(allDist, 1, min)

        toShortest <- agents2
        for (i in 1:NROW(agents)) {
          # All the possibilities for each agents (i.e., agents2 and the wrapped agents2)
          allToCoords <- rbind(agents2[i, ], to1[i, ], to2[i, ], to3[i, ], to4[i, ], to5[i, ],
                               to6[i, ], to7[i, ], to8[i, ])
          toShortest[i, ] <- allToCoords[match(distMin[i], allDist[i, ]), ]
          # if ties, take the first match (good because favor the non wrapped distances)
        }

        heading <- deg(atan2(toShortest[, 1] - agents[, 1], toShortest[, 2] - agents[, 2]))
        # angles between -180 and 180
        heading[heading < 0] <- heading[heading < 0] + 360
      }

    } else if (inherits(agents, "agentMatrix") & !inherits(agents2, "agentMatrix")) {
      # turtles to patches
      tCoords <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
      heading <- towards(agents = tCoords, agents2 = agents2, world = world, torus = torus)
      sameLoc <- tCoords[, 1] == agents2[, 1] & tCoords[, 2] == agents2[, 2]
      if (NROW(tCoords) == 1) {
        heading[sameLoc] <- agents@.Data[, "heading"]
      } else {
        heading[sameLoc] <- agents@.Data[, "heading"][sameLoc]
      }

    } else if (!inherits(agents, "agentMatrix") & inherits(agents2, "agentMatrix")) {
      # patches to turtles
      heading <- towards(agents = agents,
                         agents2 = agents2@.Data[, c("xcor", "ycor"), drop = FALSE],
                         world = world, torus = torus)

    } else if (inherits(agents, "agentMatrix") & inherits(agents2, "agentMatrix")) {
      # turtles to turtles
      t1Coords <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
      t2Coords <- agents2@.Data[, c("xcor", "ycor"), drop = FALSE]
      heading <- towards(agents = t1Coords, agents2 = t2Coords, world = world, torus = torus)
      sameLoc <- t1Coords[, 1] == t2Coords[, 1] & t1Coords[, 2] == t2Coords[, 2]
      if (NROW(t1Coords) == 1) {
        heading[sameLoc] <- agents@.Data[, "heading"]
      } else {
        heading[sameLoc] <- agents@.Data[, "heading"][sameLoc]
      }
    }

    return(heading)
  }
)

################################################################################
#' Face something
#'
#' Set the \code{turtles}' heading towards \code{agents2}.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix representing the \code{turtles} with updated headings.
#'
#' @details The number of agents/locations in \code{agents2} must be equal to 1 or
#'          to the length of \code{turtles}.
#'
#'          If \code{torus = FALSE}, \code{world} does not need to be provided.
#'
#'          If \code{torus = TRUE} and the distance from one \code{turtles} to
#'          its corresponding agent/location \code{agents2} is smaller around the
#'          sides of the \code{world} than across it, then the direction to the agent/location
#'          \code{agents2} going around the sides of the \code{world} is given to the turtle.
#'
#'          If a turtle is facing its own location, its heading does not change.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#face}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#facexy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- face(turtles = t1, agents2 = cbind(x = 0, y = 0))
#' t1 <- fd(turtles = t1, dist = 0.5)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname face
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "face",
  function(turtles, agents2, world, torus = FALSE) {
    standardGeneric("face")
  })

#' @export
#' @rdname face
setMethod(
  "face",
  signature = c(turtles = "agentMatrix", agents2 = "matrix"),
  definition = function(turtles, agents2, world, torus) {
    newHeading <- towards(agents = turtles, agents2 = agents2, world = world, torus = torus)
    turtles@.Data[, "heading"] <- newHeading
    return(turtles)

  }
)


################################################################################
#' Rotate to the left
#'
#' Rotate the \code{turtles}'s headings to the left of \code{angle} degrees.
#'
#' @inheritParams fargs
#'
#' @param angle   Numeric. Vector of angles in degrees by which to rotate the \code{turtles}'
#'                headings. Must be of length 1 or of length \code{turtles}.
#'
#' @return AgentMatrix representing the \code{turtles} with updated "heading" values.
#'
#' @details If a given \code{angle} value is negative, then the turtle rotates to the right.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#left}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' of(agents = t1, var = "heading")
#' t1 <- left(turtles = t1, angle = 180)
#' of(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname left
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "left",
  function(turtles, angle) {
    standardGeneric("left")
  })

#' @export
#' @rdname left
setMethod(
  "left",
  signature = c("agentMatrix", "numeric"),
  definition = function(turtles, angle) {
    newHeading <- turtles@.Data[, "heading"] - angle
    newHeading[newHeading < 0] <- newHeading[newHeading < 0] + 360
    newHeading[newHeading >= 360] <- newHeading[newHeading >= 360] - 360

    turtles@.Data[, "heading"] <- newHeading
    return(turtles)
  }
)


################################################################################
#' Rotate to the right
#'
#' Rotate the \code{turtles}'s headings to the right of \code{angle} degrees.
#'
#' @inheritParams fargs
#'
#' @inheritParams left
#'
#' @return AgentMatrix representing the \code{turtles} with updated "heading" values.
#'
#' @details If a given \code{angle} value is negative, then the turtle rotates to the left.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#right}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' of(agents = t1, var = "heading")
#' t1 <- right(turtles = t1, angle = 180)
#' of(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname right
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "right",
  function(turtles, angle) {
    standardGeneric("right")
  })

#' @export
#' @rdname right
setMethod(
  "right",
  signature = c("agentMatrix", "numeric"),
  definition = function(turtles, angle) {
    left(turtles = turtles, angle = -angle)
  }
)


################################################################################
#' Move downhill
#'
#' Move the \code{turtles} to their neighboring patch with the lowest value.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix representing the \code{turtles} with updated
#'         coordinates and updated data for their "heading" values and
#'         previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If no neighboring patch has a smaller value than the patch where the
#'          turtle is currently located on, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'
#'          If there are multiple neighboring patches with the same lowest value,
#'          the turtle chooses one patch randomly.
#'
#'          If a turtle is located on a patch on the edge
#'          of the \code{world} and \code{torus = FALSE}, it has fewer
#'          neighborhing patches as options to move than \code{nNeighbors}; if
#'          \code{torus = TRUE}, the turtle can move on the other side of the \code{world} to
#'          move downhill and its choice of neighborhing patches is always equals to
#'          \code{nNeighbors}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#downhill}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10,
#'                           data = runif(100))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- downhill(world = w1, turtles = t1, nNeighbors = 8)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @importFrom car some
#' @docType methods
#' @rdname downhill
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "downhill",
  function(world, pVar, turtles, nNeighbors, torus = FALSE) {
    standardGeneric("downhill")
  })

#' @export
#' @rdname downhill
setMethod(
  "downhill",
  signature = c(world = "worldMatrix", pVar = "missing", turtles = "agentMatrix",
                nNeighbors = "numeric"),
  definition = function(world, turtles, nNeighbors, torus) {

    # Output neighbors() as a matrix
    pNeighbors <- neighbors(world = world, agents = turtles, nNeighbors = nNeighbors,
                            torus = torus)
    pValues <- as.numeric(t(world@.Data)) # ordered by cellNumbers
    tDF <- data.frame(patchHere(world, turtles), id = 1:NLcount(turtles))
    allPatches <- rbind(pNeighbors, tDF) # neighbors patches + patches under the turtles

    allPatches$cellNum <- cellFromPxcorPycor(world = world, pxcor = allPatches$pxcor,
                                             pycor = allPatches$pycor)
    allPatches$pVal <- pValues[allPatches$cellNum]

    rows <- split(1:nrow(allPatches), allPatches$id)
    rowMin <- sapply(rows, function(rowi) rowi[which.min(allPatches$pVal[rowi])])
    # minimum patch value per id
    pMinCoords <- allPatches[rowMin, ]
    pMinCoords1 <- pMinCoords[tapply(1:nrow(pMinCoords), pMinCoords$id, some, 1), ]
    # select randomly one row per id
    pMinCoords1 <- pMinCoords1[order(pMinCoords1$id), ] # order by turtles
    pMinCoords2 <- cbind(pxcor = pMinCoords1[, 1], pycor = pMinCoords1[, 2])

    newTurtles <- face(world = world, turtles = turtles, agents2 = pMinCoords2, torus = torus)
    newTurtles <- moveTo(turtles = newTurtles, agents = pMinCoords2)
    return(newTurtles)
  }
)

#' @export
#' @rdname downhill
setMethod(
  "downhill",
  signature = c(world = "worldArray", pVar = "character", turtles = "agentMatrix",
                nNeighbors = "numeric"),
  definition = function(world, pVar, turtles, nNeighbors, torus) {

    # Output neighbors() as a matrix
    pNeighbors <- neighbors(world = world, agents = turtles, nNeighbors = nNeighbors,
                            torus = torus)

    ## Only difference with method for worldMatrix
    layer <- match(pVar, dimnames(world)[[3]])
    pValues <- as.numeric(t(world@.Data[, , layer])) # ordered by cellNumbers
    ##

    tDF <- data.frame(patchHere(world, turtles), id = 1:NLcount(turtles))
    allPatches <- rbind(pNeighbors, tDF) # neighbors patches + patches under the turtles

    allPatches$cellNum <- cellFromPxcorPycor(world = world, pxcor = allPatches$pxcor,
                                             pycor = allPatches$pycor)
    allPatches$pVal <- pValues[allPatches$cellNum]

    rows <- split(1:nrow(allPatches), allPatches$id)
    rowMin <- sapply(rows, function(rowi) rowi[which.min(allPatches$pVal[rowi])])
    # minimum patch value per id
    pMinCoords <- allPatches[rowMin, ]
    pMinCoords1 <- pMinCoords[tapply(1:nrow(pMinCoords), pMinCoords$id, some, 1), ]
    # select randomly one row per id
    pMinCoords1 <- pMinCoords1[order(pMinCoords1$id), ] # order by turtles
    pMinCoords2 <- cbind(pxcor = pMinCoords1[, 1], pycor = pMinCoords1[, 2])

    newTurtles <- face(world = world, turtles = turtles, agents2 = pMinCoords2, torus = torus)
    newTurtles <- moveTo(turtles = newTurtles, agents = pMinCoords2)
    return(newTurtles)
  }
)


################################################################################
#' Move uphill
#'
#' Move the \code{turtles} to their neighboring patch with the highest value.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix representing the \code{turtles} with updated
#'         coordinates and updated data for their "heading" values and
#'         previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If no neighboring patch has a larger value than the patch where the
#'          turtle is currently located on, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'
#'          If there are multiple neighboring patches with the same highest value,
#'          the turtle chooses one patch randomly.
#'
#'          If a turtle is located on a patch on the edge
#'          of the \code{world} and \code{torus = FALSE}, it has fewer
#'          neighborhing patches as options to move than \code{nNeighbors}; if
#'          \code{torus = TRUE}, the turtle can move on the other side of the \code{world} to
#'          move uphill and its choice of neighborhing patches is always equals to
#'          \code{nNeighbors}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#uphill}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10,
#'                           data = runif(100))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- uphill(world = w1, turtles = t1, nNeighbors = 8)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname uphill
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "uphill",
  function(world, pVar, turtles, nNeighbors, torus = FALSE) {
    standardGeneric("uphill")
  })

#' @export
#' @rdname uphill
setMethod(
  "uphill",
  signature = c(world = "worldMatrix", pVar = "missing", turtles = "agentMatrix",
                nNeighbors = "numeric"),
  definition = function(world, turtles, nNeighbors, torus) {
    # Uphill is the inverse of downhill
    world[] <- 1 / world[]
    downhill(world = world, turtles = turtles, nNeighbors = nNeighbors, torus = torus)
  }
)

#' @export
#' @rdname uphill
setMethod(
  "uphill",
  signature = c(world = "worldArray", pVar = "character", turtles = "agentMatrix",
                nNeighbors = "numeric"),
  definition = function(world, pVar, turtles, nNeighbors, torus) {
    world[] <- 1 / world[]
    downhill(world = world, pVar = pVar, turtles = turtles, nNeighbors = nNeighbors,
             torus = torus)
  }
)


################################################################################
#' Patches ahead
#'
#' Report the coordinates of the patches at the given
#' distances of the \code{turtles} in the direction of their headings.
#'
#' @inheritParams fargs
#'
#' @param dist   Numeric. Vector of distances from the \code{turtles}. \code{dist} must be
#'               of length 1 or of length \code{turtles}.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches at the distances \code{dist}
#'         and \code{turtles}'s headings directions
#'         of \code{turtles}. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
#'          is outside the \code{world}'s extent, \code{NA}
#'          are returned for the patch coordinates. If \code{torus = TRUE}, the patch
#'          coordinates from a wrapped \code{world} are returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-ahead}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' patchAhead(world = w1, turtles = t1, dist = 1)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname patchAhead
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchAhead",
  function(world, turtles, dist, torus = FALSE) {
    standardGeneric("patchAhead")
  })

#' @export
#' @rdname patchAhead
setMethod(
  "patchAhead",
  signature = c(world = "worldNLR", turtles = "agentMatrix", dist = "numeric"),
  definition = function(world, turtles, dist, torus) {

    radHeading <- rad(turtles@.Data[, "heading"])
    xcor <- round(turtles@.Data[, "xcor"] + sin(radHeading) * dist, digits = 5)
    ycor <- round(turtles@.Data[, "ycor"] + cos(radHeading) * dist, digits = 5)
    pAhead <- patch(world = world, x = xcor, y = ycor, duplicate = TRUE,
                    torus = torus, out = TRUE)
    return(pAhead)

  }
)


################################################################################
#' Patches here
#'
#' Report the coordinates of the patches under the \code{turtles}
#' locations.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches at the \code{turtles}
#'         location. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a turtle is located outside of the \code{world}'s extent,
#'          \code{NA} are returned
#'          for the patch coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-here}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' patchHere(world = w1, turtles = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname patchHere
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchHere",
  function(world, turtles) {
    standardGeneric("patchHere")
  })

#' @export
#' @rdname patchHere
setMethod(
  "patchHere",
  signature = c("worldNLR", "agentMatrix"),
  definition = function(world, turtles) {

    pTurtles <- patch(world = world, x = turtles@.Data[, 1], y = turtles@.Data[, 2],
                      duplicate = TRUE, out = TRUE)
    return(pTurtles)

  }
)


################################################################################
#' Patches on the left
#'
#' Report the coordinates of the patches at the given distances of the \code{turtles}
#' and given \code{angle} left of their headings.
#'
#' @inheritParams fargs
#'
#' @inheritParams patchAhead
#'
#' @param angle   Numeric. Vector of angles in degrees by which the \code{turtle}'s
#'                headings should rotate to locate the patches. Must be of length 1 or of
#'                length \code{turtles}.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" representing the coordinates of the patches at \code{dist}
#'         distances of the \code{turtles} and \code{angle} to the left of their headings.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a given \code{dist} value is negative, then the turtle would look backward.
#'          If a given \code{angle} value is negative, then the turtle would look to the right.
#'
#'          If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
#'          and \code{angle} degrees to the left of its heading is outside the
#'          \code{world}'s extent, \code{NA}
#'          are returned for the patch coordinates. If \code{torus = TRUE}, the patch
#'          coordinates from a wrapped \code{world} are returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-lr-and-ahead}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2), heading = 90)
#' patchLeft(world = w1, turtles = t1, dist = 2, angle = 90)
#'
#'
#' @export
#' @docType methods
#' @rdname patchLeft
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchLeft",
  function(world, turtles, dist, angle, torus = FALSE) {
    standardGeneric("patchLeft")
  })

#' @export
#' @rdname patchLeft
setMethod(
  "patchLeft",
  signature = c(world = "worldNLR", turtles = "agentMatrix", dist = "numeric",
                angle = "numeric"),
  definition = function(world, turtles, dist, angle, torus) {

    tLeft <- left(turtles = turtles, angle = angle)
    tFd <- fd(world = world, turtles = tLeft, dist = dist, torus = torus)
    pLeftFd <- patchHere(world = world, turtles = tFd)

    return(pLeftFd)
  }
)


################################################################################
#' Patches on the right
#'
#' Report the coordinates of the patches at the given distances of the \code{turtles}
#' and given \code{angle} right of their headings.
#'
#' @inheritParams fargs
#'
#' @inheritParams patchLeft
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" representing the coordinates of the patches at \code{dist}
#'         distances of the \code{turtles} and \code{angle} to the right of their headings.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a given \code{dist} value is negative, then the turtle would look backward.
#'          If a given \code{angle} value is negative, then the turtle would
#'          look to the left.
#'
#'          If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
#'          and \code{angle} degrees to the right of its heading is outside the
#'          \code{world}'s extent, \code{NA}
#'          are returned for the patch coordinates. If \code{torus = TRUE}, the patch
#'          coordinates from a wrapped \code{world} are returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-lr-and-ahead}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2), heading = 90)
#' patchRight(world = w1, turtles = t1, dist = 2, angle = 90)
#'
#'
#' @export
#' @docType methods
#' @rdname patchRight
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchRight",
  function(world, turtles, dist, angle, torus = FALSE) {
    standardGeneric("patchRight")
  })

#' @export
#' @rdname patchRight
setMethod(
  "patchRight",
  signature = c(world = "worldNLR", turtles = "agentMatrix", dist = "numeric",
                angle = "numeric"),
  definition = function(world, turtles, dist, angle, torus) {
    patchLeft(world = world, turtles = turtles, dist = dist, angle = -angle,
              torus = torus)
  }
)


################################################################################
#' Set turtles' locations
#'
#' Set the turtles \code{xcor} and \code{ycor} coordinates.
#'
#' @inheritParams fargs
#'
#' @param xcor    Numeric. Vector of x coordinates. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @param ycor    Numeric. Vector of y coordinates. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @return AgentMatrix representing the \code{turtles} with updated coordinates
#'         and updated data for their previous coordinates "prevX" and "prevY"
#'
#' @details \code{world} must be provided only if \code{torus = TRUE}.
#'
#'          If the given coordinates \code{[xcor, ycor]}
#'          are located outside of the \code{world}'s extent and \code{torus = TRUE},
#'          then the coordinates assigned to the turtle
#'          are the ones from a wrapped \code{word}; if \code{torus = FALSE}, the turtle
#'          is located outside of the \code{world}'s extent with the given coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#setxy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9,
#'                           data = runif(100))
#' t1 <- createTurtles(n = 5, coords = randomXYcor(w1, n = 5))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t1 <- setXY(turtles = t1, xcor = 1:5, ycor = 1:5)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname setXY
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "setXY",
  function(turtles, xcor, ycor, world, torus = FALSE) {
    standardGeneric("setXY")
  })


#' @export
#' @rdname setXY
setMethod(
  "setXY",
  signature = c("agentMatrix", "numeric", "numeric", "missing", "ANY"),
  definition = function(turtles, xcor, ycor, torus) {

    turtles@.Data[, "prevX"] <- turtles@.Data[, "xcor"]
    turtles@.Data[, "prevY"] <- turtles@.Data[, "ycor"]

    if (length(xcor) == 1 & NROW(turtles) != 1) {
      xcor <- as.numeric(rep(xcor, NROW(turtles)))
    }
    if (length(ycor) == 1 & NROW(turtles) != 1) {
      ycor <- as.numeric(rep(ycor, NROW(turtles)))
    }
    turtles@.Data[, "xcor"] <- xcor
    turtles@.Data[, "ycor"] <- ycor

    return(turtles)
  }
)

#' @export
#' @rdname setXY
setMethod(
  "setXY",
  signature = c("agentMatrix", "numeric", "numeric", "worldNLR", "logical"),
  definition = function(turtles, xcor, ycor, world, torus) {

    wrapCoords <- wrap(cbind(x = xcor, y = ycor), world@extent)
    setXY(turtles = turtles, xcor = wrapCoords[, 1], ycor = wrapCoords[, 2],
          torus = FALSE)

  }
)


################################################################################
#' Sprout new turtles
#'
#' Create \code{n} new turtles on specific \code{patches}.
#'
#' @param n Integer. Vector of length 1 or of length the number of \code{patches}.
#'          Number of new turtles
#'          to create on each patch.
#'
#' @param heading	Numeric. Vector of values between 0 and 360.
#'                Must be of length 1 or of length the number of \code{patches}.
#'                If missing, a random heading is assigned to each sprouted turtle.
#'
#' @param breed	Character. Vector of "breed" names.
#'              Must be of length 1 or of length the number of \code{patches}.
#'              If missing, breed = "turtle" for all the sprouted turtles.
#'
#' @param color	Character. Vector of color names.
#'              Must be of length 1, of length the number of \code{patches} or
#'              of length \code{sum(n)}.
#'              If missing, colors are assigned using the function rainbow(n)
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix including the new
#'         sprouted turtles.
#'
#' @details \code{nrow(patches)} must be equal to 1 or to \code{n}.
#'
#'          If \code{turtles} is provided, the new turtles are added to
#'          the \code{turtles} when returned. The who numbers of the sprouted turtles
#'          therefore follow the ones from the \code{turtles}.
#'          All new sprouted turtles are placed at the end of the agentMatrix object.
#'          If no \code{turtles}
#'          is provided, a new agentMatrix is created and the who numbers
#'          start at 0.
#'
#'          If \code{turtles} is provided and had additional variables created
#'          with \code{turtlesOwn()}, \code{NA} is given for these variables
#'          for the new sprouted turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#sprout}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- sprout(patches = cbind(pxcor = 2, pycor = 2), n = 3)
#' t2 <- sprout(patches = cbind(pxcor = 3, pycor = 3), n = 3, turtles = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname sprout
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "sprout",
  function(n, patches, breed, heading, color, turtles) {
    standardGeneric("sprout")
})

#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#' @rdname sprout
setMethod(
  "sprout",
  signature = c(n = "numeric", patches = "matrix"),
  definition = function(n, patches, breed, heading, color, turtles) {
    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text = x)))
    names(li) <- names(match.call())[-1]
    if (length(n) == 1) {
      if (nrow(li$patches) == 1 & n != 1) {
        li$patches <- cbind(as.numeric(rep(li$patches[, 1], n)),
                            as.numeric(rep(li$patches[, 2], n)))
      }
      colnames(li$patches) <- c("xcor", "ycor")

      if (missing(breed))
        li$breed <- rep("turtle", n)

      if (length(li$breed) == 1) {
        li$breed <- rep(li$breed, n)
      }

      if (missing(heading)) li$heading <- runif(n = n, min = 0, max = 360)

      if (length(li$heading) == 1) {
        li$heading <- rep(li$heading, n)
      }

      if (missing(color)) li$color <- rainbow(n)

      newTurtles <- createTurtles(n = n, coords = li$patches, heading = li$heading,
                                    breed = li$breed, color = li$color)

    } else {
      # if length(n) != 0
      li$patches <- cbind(as.numeric(rep(li$patches[, 1], n)),
                          as.numeric(rep(li$patches[, 2], n)))
      colnames(li$patches) <- c("xcor", "ycor")

      if (missing(breed)) li$breed <- rep("turtle", sum(n))
      if (length(li$breed) == 1) {
        li$breed <- rep(li$breed, sum(n))
      } else if (length(li$breed) != sum(n)) {
        li$breed <- rep(li$breed, n)
      }

      if (missing(heading)) li$heading <- runif(n = sum(n), min = 0, max = 360)
      if (length(li$heading) == 1) {
        li$heading <- rep(li$heading, sum(n))
      } else if (length(li$heading) != sum(n)) {
        li$heading <- rep(li$heading, n)
      }

      if (missing(color)) li$color <- rainbow(sum(n))
      if (length(li$color) == 1) {
        li$color <- rep(li$color, sum(n))
      } else if (length(li$color) != sum(n)) {
        li$color <- rep(li$color, n)
      }

      newTurtles <- createTurtles(n = sum(n), coords = li$patches, heading = li$heading,
                                    breed = li$breed, color = li$color)
    }

    if (missing(turtles)) {
      return(newTurtles)
    } else {
      turtles <- hatch(turtles = turtles, who = max(turtles@.Data[, "who"]),
                       n = NLcount(newTurtles))
      # Replace the locations and headings of newTurtles inside turtles
      ids <- (nrow(turtles@.Data) - NLcount(newTurtles) + 1):nrow(turtles@.Data)
      turtles@.Data[ids, c(1, 2, 4)] <- newTurtles@.Data[, c(1, 2, 4)]
      # Replace the breed and color of the newTurtles inside turtles
      ids <- (nrow(turtles@.Data) - NLcount(newTurtles) + 1):nrow(turtles@.Data)
      whoNewTurtles <- turtles@.Data[ids, 3]
      turtles <- NLset(turtles = turtles, agents = turtle(turtles, who = whoNewTurtles),
                       var = "breed", val = of(agents = newTurtles, var = "breed"))
      turtles <- NLset(turtles = turtles, agents = turtle(turtles, who = whoNewTurtles),
                         var = "color", val = of(agents = newTurtles, var = "color"))
      # Replace any other additional variables
      if (ncol(turtles@.Data) > 8) {
        valToReplace <- matrix(NA, ncol = (ncol(turtles@.Data) - 8), nrow = NLcount(newTurtles))
        colnames(valToReplace) <- colnames(turtles@.Data)[9:ncol(turtles@.Data)]
        turtles <- NLset(turtles = turtles, agents = turtle(turtles, who = whoNewTurtles),
                         var = colnames(turtles@.Data)[9:ncol(turtles@.Data)],
                         val = valToReplace)
      }

      # newTurtles@.Data[,"who"] <- (max(turtles@.Data[,"who"]) + 1):(NLcount(newTurtles) +
      #                                                               max(turtles@.Data[,"who"]))
      # # unique who number
      # newTurtlesBreed <- of(agents = newTurtles, var = "breed")
      # newTurtlesColor <- of(agents = newTurtles, var = "color")
      #
      # turtles@.Data <- rbind(turtles@.Data, newTurtles@.Data)
      # turtles <- NLset(turtles = turtles, agents = newTurtles, var = "breed",
      #                  val = newTurtlesBreed)
      # turtles <- NLset(turtles = turtles, agents = newTurtles, var = "color",
      #                  val = newTurtlesColor)

      return(turtles)
    }
})

################################################################################
#' Inspect turtles
#'
#' Display all variables values for the selected individuals among the \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @return Dataframe (nrow = \code{length(who)}) of the variables of the selected
#'         individuals among the \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#inspect}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createOTurtles(world = w1, n = 10)
#' inspect(turtles = t1, who = c(2, 3))
#'
#'
#' @export
#' @importFrom plyr mapvalues
#' @docType methods
#' @rdname inspect
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "inspect",
  function(turtles, who) {
    standardGeneric("inspect")
  })

#' @export
#' @rdname inspect
setMethod(
  "inspect",
  signature = c("agentMatrix", "numeric"),
  definition = function(turtles, who) {
    tData <- as.data.frame(turtles@.Data[turtles@.Data[, "who"] %in% who, , drop = FALSE],
                           stringsAsFactors = FALSE)
    tData[, names(turtles@levels)] <- do.call(cbind, lapply(1:length(turtles@levels), function(x){
      unlist(mapvalues(tData[, names(turtles@levels)[x]],
                       from = unique(tData[, names(turtles@levels)[x]]),
                       to = turtles@levels[names(turtles@levels)[x]][[1]][
                         unique(tData[, names(turtles@levels)[x]])]))}))

    return(tData)
  }
)


################################################################################
#' Move to
#'
#' Move the \code{turtles} to the \code{agents}' locations.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix representing the \code{turtles} with updated coordinates
#'         and updated data for their previous coordinates "prevX" and "prevY".
#'
#' @details The number of \code{agents} must be equal to 1 or to
#'          length \code{turtles}.
#'
#'          The \code{turtle}'s headings are not affected with this function.
#'
#'          If a turtle is moving to a patch location, it will be located at
#'          the patch center.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#move-to}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9,
#'                           data = runif(100))
#' t1 <- createTurtles(n = 5, coords = randomXYcor(w1, n = 5))
#' plot(w1)
#' points(t1, col = "black", pch = 16)
#'
#' t1 <- moveTo(turtles = t1, agents = turtle(t1, who = 0))
#' points(t1, col = "red", pch = 16)
#'
#' t1 <- moveTo(turtles = t1, agents = patch(w1, 9, 9))
#' points(t1, col = "blue", pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname moveTo
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "moveTo",
  function(turtles, agents) {
    standardGeneric("moveTo")
  })

#' @export
#' @rdname moveTo
setMethod(
  "moveTo",
  signature = c("agentMatrix", "matrix"),
  definition = function(turtles, agents) {
    if (!inherits(agents, "agentMatrix")) {
      setXY(turtles = turtles, xcor = as.numeric(agents[, 1]),
            ycor = as.numeric(agents[, 2]), torus = FALSE)

    } else{
      setXY(turtles = turtles, xcor = agents@.Data[, "xcor"],
            ycor = agents@.Data[, "ycor"], torus = FALSE)

    }
  }
)


################################################################################
#' Random turtles coordinates
#'
#' Report \code{n} random xcor and ycor coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "xcor" and the second
#'         column "ycor".
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = runif(25))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname randomXYcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomXYcor",
  function(world, n) {
    standardGeneric("randomXYcor")
  })

#' @export
#' @rdname randomXYcor
setMethod(
  "randomXYcor",
  signature = c("worldNLR", "numeric"),
  definition = function(world, n) {
    xycor <- cbind(xcor = randomXcor(world = world, n = n),
                   ycor = randomYcor(world = world, n = n))
    return(xycor)
  }
)


################################################################################
#' Do the turtles exist?
#'
#' Report \code{TRUE} if a turtle exists inside the \code{turtles}, report
#' \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical. Vector of \code{TRUE} or \code{FALSE} if the \code{who} numbers
#'         with any of the \code{breed}, if provided, exist or not
#'         inside the \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#member}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     breed = c(rep("sheep", 5), rep("wolf", 5)))
#' tExist(turtles = t1, who = 3, breed = "sheep")
#' tExist(turtles = t1, who = 9, breed = "sheep")
#' tExist(turtles = t1, who = 9, breed = c("sheep", "wolf"))
#' tExist(turtles = t1, who = c(3, 9))
#'
#'
#' @export
#' @docType methods
#' @rdname tExist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "tExist",
  function(turtles, who, breed) {
    standardGeneric("tExist")
  })

#' @export
#' @rdname tExist
setMethod(
  "tExist",
  signature = c("agentMatrix", "numeric", "missing"),
  definition = function(turtles, who) {

    tExist <- who %in% turtles@.Data[, "who"]
    return(tExist)

  }
)

#' @export
#' @rdname tExist
setMethod(
  "tExist",
  signature = c("agentMatrix", "numeric", "character"),
  definition = function(turtles, who, breed) {

    breedFactor <- match(breed, turtles@levels$breed)
    tExist <- who %in% turtles@.Data[turtles@.Data[, "breed"] %in% breedFactor, "who"]
    return(tExist)

  }
)


################################################################################
#' Select turtles
#'
#' Report the individuals among \code{turtles} based on their \code{who} numbers
#' and \code{breed}.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix of the selected \code{turtles} sorted in the order of
#'         the \code{who} numbers requested. If \code{breed} was provided, the
#'         \code{turtles} selected are of one of the \code{breed}.
#'
#' @details If no turtle matches the given \code{who} numbers, with potentially
#'          one of the given
#'          \code{breed}, inside \code{turtles}, then an empty agentMatrix is returned.
#'
#'          If there are duplicates "who" numbers among the \code{turtles}, the first
#'          matching turtle with the requested \code{who} number is returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtle}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' t2 <- turtle(t1, who = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname turtle
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtle",
  function(turtles, who, breed) {
    standardGeneric("turtle")
})

#' @export
#' @importFrom stats na.omit
#' @rdname turtle
setMethod(
  "turtle",
  signature = c("agentMatrix", "numeric", "missing"),
  definition = function(turtles, who) {
    turtles[na.omit(match(who, turtles@.Data[, "who"])), ] # %>% na.omit %>% sort
})

#' @export
#' @rdname turtle
setMethod(
  "turtle",
  signature = c("agentMatrix", "numeric", "character"),
  definition = function(turtles, who, breed) {

    breedFactor <- which(turtles@levels$breed %in% breed)
    if (length(breedFactor) == 0) {
      noTurtles()
    } else {
      tBreed <- turtles[which(turtles@.Data[, "breed"] %in% breedFactor), ]
      turtle(tBreed, who)
    }
})


################################################################################
#' Turtles on
#'
#' Report the individuals among \code{turtles} that are on the same patches as
#' the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @param simplify Logical. If \code{simplify = TRUE}, all \code{turtles} on the same
#'                 patches as any \code{agents} are returned; if \code{simplify = FALSE},
#'                 the \code{turtles} are evaluated for each \code{agents}'s patches
#'                 individually.
#'
#' @return AgentMatrix representing any individuals from \code{turtles} of
#'         any of the given \code{breed}, if speficied,
#'         located on the same patches as any of the \code{agents}, if \code{simplify = TRUE}, or
#'
#'         Matrix (ncol = 2) with the first column "whoTurtles" and the second column
#'         "id" showing which \code{turtles} are on the same
#'         patches as which \code{agents} represented by "id", if \code{simplify = FALSE}.
#'         "id" represents and follows the order of the \code{agents}. "id" does not represent
#'         the "who" numbers
#'         of the \code{agents} if \code{agents} are turtles.
#'
#' @details The \code{agents} must be located inside the
#'          \code{world}'s extent.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-on}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9,
#'                           data = runif(100))
#' t1 <- createTurtles(n = 500, coords = randomXYcor(w1, n = 500))
#' plot(w1)
#' points(t1, col = of(agents = t1, var = "color"), pch = 16)
#'
#' t2 <- turtlesOn(world = w1, turtles = t1, agents = patch(w1, 2, 2))
#'
#'
#' @export
#' @docType methods
#' @rdname turtlesOn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtlesOn",
  function(world, turtles, agents, breed, simplify = TRUE) {
    standardGeneric("turtlesOn")
  })

#' @export
#' @importFrom stats na.omit
#' @rdname turtlesOn
setMethod(
  "turtlesOn",
  signature = c(world = "worldNLR", turtles = "agentMatrix",
                agents = "matrix", breed = "missing"),
  definition = function(world, turtles, agents, simplify) {
    if (inherits(agents, "agentMatrix")) {
      agents <- patchHere(world = world, turtles = agents)
    }

    pTurtles <- round(turtles@.Data[, c("xcor", "ycor", "who"), drop = FALSE])
    colnames(pTurtles)[1:2] <- c("pxcor", "pycor") # awkward column name change

    if (simplify == TRUE) {
      # Instead of merge, which is slow, make an empty matrix (of NAs)
      # and use the agents coordinates directly
      a <- matrix(ncol = ncol(world), nrow = nrow(world))
      a[agents - (c(attr(world, "minPxcor"), attr(world, "minPycor")) - 1)] <- 1
      # change this 26 to the dimensions of the world, and next line
      pOn <- na.omit(a[pTurtles[, 1:2, drop = FALSE] -
                         (c(attr(world, "minPxcor"), attr(world, "minPycor")) - 1)] * pTurtles)

      if (nrow(pOn) == 0) {
        return(noTurtles())
      } else {
        return(turtle(turtles = turtles, who = pOn[, 3]))
      }
    } else {
      if (any(is.na(agents)))
        agents <- na.omit(agents) # There shouldn't be any NAs passed in here, probably
      agents <- cbind(agents, id = 1:dim(agents)[1])

      b <- a <- matrix(ncol = ncol(world), nrow = nrow(world))
      a[agents[, 1:2, drop = FALSE] - (c(attr(world, "minPxcor"),
                                         attr(world, "minPycor")) - 1)] <- 1
      # change this 26 to the dimensions of the world, and next line
      b[agents[, 1:2, drop = FALSE] - (c(attr(world, "minPxcor"),
                                         attr(world, "minPycor")) - 1)] <- agents[, 3]
      # change this 26 to the dimensions of the world, and next line
      pOn <- na.omit(a[pTurtles[, 1:2, drop = FALSE] -
                         (c(attr(world, "minPxcor"), attr(world, "minPycor")) - 1)] * pTurtles)
      dims <- c(nrow(pOn), ncol(pOn))
      colNames <- colnames(pOn)
      length(pOn) <- length(pOn) + dims[1]
      dim(pOn) <- dims + c(0, 1)
      colnames(pOn) <- c(colNames, "id")
      pOn[, "id"] <- na.omit(b[pTurtles[, 1:2, drop = FALSE] -
                                 (c(attr(world, "minPxcor"), attr(world, "minPycor")) - 1)])

      pOn <- pOn[order(pOn[, "id"]), , drop = FALSE]
      turtlesID <- pOn[, c("who", "id"), drop = FALSE]
      colnames(turtlesID)[1] <- "whoTurtles"
      return(turtlesID)
    }
})

#' @export
#' @rdname turtlesOn
setMethod(
  "turtlesOn",
  signature = c(world = "worldNLR", turtles = "agentMatrix",
                agents = "matrix", breed = "character"),
  definition = function(world, turtles, agents, breed, simplify) {

    breedFactor <- which(turtles@levels$breed %in% breed)
    if (length(breedFactor) == 0) {
      tBreed <- noTurtles()
    } else {
      tBreed <- turtles[which(turtles@.Data[, "breed"] %in% breedFactor), ]
    }
    turtlesOn(world = world, turtles = tBreed, agents = agents, simplify = simplify)
})

################################################################################
#' No turtles
#'
#' Report an empty turtle agentset.
#'
#' @return AgentMatrix with the turtle variables defined as when using
#'         \code{createTurtles()} but with 0 turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#no-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- noTurtles()
#' NLcount(t1)
#'
#' @export
#' @docType methods
#' @rdname noTurtles
#'
#' @author Sarah Bauduin
#'
noTurtles <- function() {
  t0 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
  empty <- t0[which(t0@.Data[, "who"] == 1), ]
  empty@levels$breed <- character(0)
  empty@levels$color <- character(0)
  return(empty)
}

################################################################################
#' Turtles at
#'
#' Report the indviduals among \code{turtles} that are located on the patches at
#' \code{(dx, dy)} distances of the
#' \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return AgentMatrix representing the individuals among \code{turtles}
#'         of any of the given \code{breed}, if specified,
#'         which are located on the patches at \code{(dx, dy)} distances of the
#'         \code{agents}.
#'
#' @details If the patch at distance \code{(dx, dy)}
#'          of an agent is outside of the \code{world}'s extent and \code{torus = FALSE},
#'          no turtle is returned;
#'          if \code{torus = TRUE}, the turtle located on the patch whose coordinates
#'          are defined from the wrapped \code{world} is returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-at}
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#at-points}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = 0:9, ycor = 0:9),
#'                     breed = c(rep("sheep", 5), rep("wolf", 5)))
#' t2 <- turtlesAt(world = w1, turtles = t1, agents = turtle(t1, who = 0),
#'                 dx = 1, dy = 1)
#' t3 <- turtlesAt(world = w1, turtles = t1,
#'                 agents = patch(w1, c(3,4,5), c(3,4,5)), dx = 1, dy = 1,
#'                 breed = "sheep")
#'
#'
#' @export
#' @docType methods
#' @rdname turtlesAt
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtlesAt",
  function(world, turtles, agents, dx, dy, breed, torus = FALSE) {
    standardGeneric("turtlesAt")
  })

#' @export
#' @rdname turtlesAt
setMethod(
  "turtlesAt",
  signature = c("worldNLR", "agentMatrix", "matrix", "numeric", "numeric",
                "missing", "ANY"),
  definition = function(world, turtles, agents, dx, dy, torus) {
    pAt <- patchAt(world = world, agents = agents, dx = dx, dy = dy)
    turtlesOn(world = world, turtles = turtles, agents = pAt)
  }
)

#' @export
#' @rdname turtlesAt
setMethod(
  "turtlesAt",
  signature = c("worldNLR", "agentMatrix", "matrix", "numeric", "numeric",
                "character", "ANY"),
  definition = function(world, turtles, agents, dx, dy, breed, torus) {
    pAt <- patchAt(world = world, agents = agents, dx = dx, dy = dy)
    turtlesOn(world = world, turtles = turtles, agents = pAt, breed = breed)
  }
)


################################################################################
#' Create a turtle agenset
#'
#' Report a turtle agentset containing all unique turtles provided in the inputs.
#'
#' @param ... AgentMatrix objects representing the moving agents.
#'
#' @return AgentMatrix object containing all the unique turtles.
#'
#' @details Duplicated turtles are identified based only on their who numbers.
#'          The turtle chosen for a who number is the first one given in the inputs.
#'          To keep all turtles from the inputs, use NLset() to
#'          reassign who numbers in some of the inputs, prior using
#'          turtleSet(), to avoid turtles with duplicated who numbers.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtle-set}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), breed = "sheep")
#' t2 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2), breed = "wolf")
#' t2 <- NLset(turtles = t2, agents = t2, var = "who", val = c(10, 11))
#' t3 <- createTurtles(n = 1, coords = randomXYcor(w1, n = 1), breed = "sheperd")
#' t3 <- NLset(turtles = t3, agents = t3, var = "who", val = 12)
#' t4 <- turtleSet(t1, t2, t3)
#'
#'
#' @export
#' @docType methods
#' @rdname turtleSet
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtleSet",
  function(...) {
    standardGeneric("turtleSet")
  })

#' @export
#' @rdname turtleSet
setMethod(
  "turtleSet",
  signature = "agentMatrix",
  definition = function(...) {
    dots <- list(...)
    nTurtles <- lapply(dots, function(x) NROW(x))

    if (sum(unlist(nTurtles)) == 0) {
      return(noTurtles())
    } else {
      if (any(nTurtles == 0)) {
        dots <- dots[which(nTurtles != 0)] # remove the empty agentMatrix
        if (length(dots) == 1) {
          # if there is only one list element left
          return(dots[[1]])
        }
      }

      # if (do.call(all.equal, lapply(dots, colnames))) {
         allTurtles <- do.call(rbind, lapply(dots, function(x) x))
      # } else {
      #   allTurtles <- as.data.frame(rbindlist(lapply(dots, function(x) {
      #     inspect(x, who = of(agents = x, var = "who"))}), fill = TRUE))
      # }

      if (anyDuplicated(allTurtles$who) != 0) {
        warning("Duplicated turtles based on who numbers are present among the inputs.")
        allTurtles <- allTurtles[match(unique(allTurtles$who), allTurtles$who), ]
      }

      if (!is(allTurtles, "agentMatrix"))
        allTurtles <- as(allTurtles, "agentMatrix")

      return(allTurtles)
    }
})

################################################################################
#' New turtles variable
#'
#' Create a new variable for the \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @param tVar    Character. the name of the \code{turtles} variable to create.
#'
#' @param tVal    Vector representing the values of \code{tVar}.
#'                Must be of length 1 or of length \code{turtles}.
#'                If missing, \code{NA} is given.
#'
#' @return AgentMatrix representing the \code{turtles} with the new
#'         variable \code{tVar} added.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-own}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = 0, ycor = 0))
#' t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c("F", "F", "F", "M", "M"))
#'
#'
#' @export
#' @docType methods
#' @rdname turtlesOwn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtlesOwn",
  function(turtles, tVar, tVal) {
    standardGeneric("turtlesOwn")
  })

#' @export
#' @rdname turtlesOwn
setMethod(
  "turtlesOwn",
  signature = c("agentMatrix", "character", "missing"),
  definition = function(turtles, tVar) {

    turtles@.Data <- cbind(turtles@.Data, newCol = NA)
    colnames(turtles@.Data)[ncol(turtles@.Data)] <- tVar
    return(turtles)
  }
)

#' @export
#' @rdname turtlesOwn
setMethod(
  "turtlesOwn",
  signature = c("agentMatrix", "character", "ANY"),
  definition = function(turtles, tVar, tVal) {

  if (inherits(tVal, "numeric") | inherits(tVal, "integer")) {

    turtles@.Data <- cbind(turtles@.Data, newCol = tVal)
    colnames(turtles@.Data)[ncol(turtles@.Data)] <- tVar

  } else {

    turtles@.Data <- cbind(turtles@.Data, newCol = as.factor(tVal))
    colnames(turtles@.Data)[ncol(turtles@.Data)] <- tVar
    nameLevels <- names(turtles@levels)
    listLevels <- c(turtles@levels, list(levels(as.factor(tVal))))
    names(listLevels) <- c(nameLevels, tVar)
    turtles@levels <- listLevels
  }

  return(turtles)
})


################################################################################
#' Substract headings
#'
#' Compute the difference between headings.
#'
#' @param angle1 AgentMatrix object representing the moving agents, or
#'
#'               Numeric. Vector of angles.
#'
#' @param angle2 AgentMatrix object representing the moving agents, or
#'
#'               Numeric. Vector of angles.
#'
#' @param range360  Logical. If \code{range360 = TRUE}, returned values are
#'                  between 0 and 360 degrees;
#'                  if \code{range360 = FALSE}, returned values are between
#'                  -180 and 180 degrees.
#'                  Default is \code{range360 = FALSE}.
#'
#' @return Numeric. Vector of the smallest angles in degrees
#'         by which \code{angle1} could be rotated to produce \code{angle2}
#'         (i.e., the target heading).
#'
#' @details This function does the opposite as the one in NetLogo where
#'          \code{angle1} is the target heading.
#'
#'         \code{angle1} and \code{angle2} must be of the same length or if different,
#'         one of them must be of length 1.
#'
#'          Positive values mean clockwise rotations, negative value mean
#'          counterclockwise rotations.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#subtract-headings}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createOTurtles(n = 10, world = w1)
#' subHeadings(angle1 = t1, angle2 = 0)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @importFrom CircStats deg
#' @docType methods
#' @rdname subHeadings
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "subHeadings",
  function(angle1, angle2, range360 = FALSE) {
    standardGeneric("subHeadings")
})

#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "numeric", angle2 = "numeric"),
  definition = function(angle1, angle2, range360) {
    len2 <- length(angle2)
    len1 <- length(angle1)
    if (len2 != len1) {
      if (len2 == 1) {
        angle2 <- rep(angle2, len1)
      } else if (len1 == 1) {
        angle1 <- rep(angle1, len2)
      } else {
        stop("angle1 and angle2 must be of the same length or one must be of length 1")
      }
    }
    rad2 <- rad(angle2)
    rad1 <- rad(angle1)
    rads <- rad2 - rad1
    angles <- deg(atan2(sin(rads), cos(rads)))

    if (range360 == TRUE) {
      anglesNeg <- angles < 0
      angles[anglesNeg] <- angles[anglesNeg] + 360
    }

    return(angles)
})

#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "agentMatrix", angle2 = "numeric"),
  definition = function(angle1, angle2, range360) {
    subHeadings(angle1 = angle1@.Data[, "heading"], angle2 = angle2,
                range360 = range360)
  }
)
#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "numeric", angle2 = "agentMatrix"),
  definition = function(angle1, angle2, range360) {
    subHeadings(angle1 = angle1, angle2 = angle2@.Data[, "heading"],
                range360 = range360)
  }
)
#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "agentMatrix", angle2 = "agentMatrix"),
  definition = function(angle1, angle2, range360) {
    subHeadings(angle1 = angle1@.Data[, "heading"], angle2 = angle2@.Data[, "heading"],
                range360 = range360)
  }
)


################################################################################
#' Others
#'
#' Report an agentset of the \code{agents} except specific ones.
#'
#' @inheritParams fargs
#'
#' @param except Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               AgentMatrix object representing the moving agents.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" representing the patches in \code{agents} without
#'         the ones in \code{except}, or
#'
#'         AgentMatrix representing the turtles in \code{agents} without
#'         the ones in \code{except}.
#'
#' @details Both \code{agents} and \code{except} must be of the same class (e.g., both
#'          patches or both turtles).
#'
#'          Carefull: this function removes turtles only based on similar "who" numbers
#'          and "breed" names.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#other}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- other(agents = patches(w1), except = patch(w1, 0, 0))
#' NLcount(p1) # 99 patches
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = 0, ycor = 0))
#' t2 <- other(agents = t1, except = turtle(t1, who = 0))
#' NLcount(t2) # 9 turtles
#'
#'
#' @export
#' @docType methods
#' @rdname other
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "other",
  function(agents, except) {
    standardGeneric("other")
  })

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("matrix", "matrix"),
  definition = function(agents, except) {

    if (inherits(agents, "agentMatrix") & inherits(except, "agentMatrix")) {

      matchWho <- match(except@.Data[, "who"], agents@.Data[, "who"])
      matchWho <- matchWho[!is.na(matchWho)]
      matchBreed <- which(agents@.Data[matchWho, "breed"] ==
                            except@.Data[except@.Data[, "who"] ==
                                           agents@.Data[matchWho, "who"], "breed"])
      if (length(matchBreed) != 0) {
        agents <- agents[-matchWho[matchBreed], ]
      }

      return(agents)

    } else {
      pCoords <- agents[!duplicated(rbind(except, agents))[-(1:nrow(except))], ]
      return(pCoords)
    }
  }
)


################################################################################
#' Layout turtles on a circle
#'
#' Relocate the \code{turtles} on a circle centered on the \code{world}.
#'
#' @inheritParams fargs
#'
#' @param radius  Numeric. Radius of the circle.
#'
#' @return AgentMatrix representing the \code{turtles} with updated
#'         coordinates and updated data for their "heading" values and
#'         previous coordinates "prevX"
#'         and "prevY".
#'
#' @details The \code{turtles} point outwards.
#'
#'          If the
#'          \code{radius} value leads turtles outside of the \code{world}'s extent
#'          and \code{torus = TRUE}, they are
#'          relocated on the other sides of the \code{world}, inside its extent; if
#'          \code{torus = FALSE}, the turtles are located past
#'          the world's extent.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#layout-circle}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9,
#'                           data = runif(100))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' plot(w1)
#' points(t1, col = "black", pch = 16)
#'
#' t1 <- layoutCircle(world = w1, turtles = t1, radius = 3)
#' points(t1, col = "red", pch = 16)
#'
#'
#' @export
#' @docType methods
#' @rdname layoutCircle
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "layoutCircle",
  function(world, turtles, radius, torus = FALSE) {
    standardGeneric("layoutCircle")
  })

#' @export
#' @rdname layoutCircle
setMethod(
  "layoutCircle",
  signature = c(world = "worldNLR", turtles = "agentMatrix", radius = "numeric"),
  definition = function(world, turtles, radius, torus) {
    tSurrogates <- createOTurtles(n = NLcount(turtles), world = world)
    turtles@.Data[, c("xcor", "ycor")] <- tSurrogates@.Data[, c("xcor", "ycor")]
    turtles@.Data[, "heading"] <- tSurrogates@.Data[, "heading"]
    fd(world = world, turtles = turtles, dist = radius, torus = torus, out = TRUE)
  }
)


################################################################################
#' Values of an agents variable
#'
#' Report the \code{agents} values for the requested variable.
#'
#' @inheritParams fargs
#'
#' @param var Character. Vector of the names of the selected \code{agents} variables.
#'            If \code{agents} are patches and the \code{world} is a
#'            \code{worldMatrix} object, \code{var} must not be provided. If
#'            \code{agents} are patches and the \code{world} is a
#'            \code{worldArray} object, \code{var} is the name of the layers to
#'            use to define the patches
#'            values. If \code{agents} are turtles, \code{var} is some of
#'            the turtles' variable and can be any of the variables created
#'            when turtles were created,
#'            as well as any variable created with \code{turtlesOwn()}.
#'
#' @return Vector of values for the \code{agents} if one variable is
#'         requested. The class depends
#'         of the variable class. The order of the vector follows the order
#'         of the \code{agents}, or
#'
#'         Matrix or Dataframe (ncol = \code{length(var)}, nrow = \code{NLcount(agents)})
#'         if more than one variable is requested. The row order
#'         follows the order of the \code{agents}.
#'
#' @details \code{world} must be provided only if \code{agents} are patches.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = 1:25)
#' of(world = w1, agents = patch(w1, c(0, 0), c(4, 0)))
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' of(agents = t1, var = "heading")
#'
#'
#' @export
#' @importFrom plyr mapvalues
#' @docType methods
#' @rdname of
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "of",
  function(world, agents, var) {
    standardGeneric("of")
})

#' @export
#' @rdname of
setMethod(
  "of",
  signature = c("missing", "agentMatrix", "character"),
  definition = function(agents, var) {

    if (any(names(agents@levels) %in% var)) {

      wh <- var %in% names(agents@levels)
      #if (any(wh)) {
        newNames <- var[wh]
        df <- do.call(data.frame, args = append(list(stringsAsFactors = FALSE),
                                                lapply(which(wh), function(w)
            agents@levels[[var[w]]][agents@.Data[, var[w]]])))
        if (!all(wh))  {
          df <- data.frame(agents@.Data[, var[!wh], drop = FALSE], df)
          newNames <- c(var[!wh], newNames)
        }
        colnames(df) <- newNames
        return(df[, match(newNames, var)])
    } else {
      if (length(var) == 1) {
        return(agents@.Data[, var])
      } else {
        return(agents@.Data[, var, drop = FALSE])
      }
    }
})

#' @export
#' @rdname of
setMethod(
  "of",
  signature = c("worldMatrix", "matrix", "missing"),
  definition = function(world, agents) {

    if (identical(patches(world), agents)) {
      return(as.numeric(t(world@.Data))) # values must be returned by row
    } else {

      return(world[agents[, 1], agents[, 2]])
    }
})

#' @export
#' @rdname of
setMethod(
  "of",
  signature = c("worldArray", "matrix", "character"),
  definition = function(world, agents, var) {

    if (identical(patches(world), agents)) {
      allValues <- world[]
      return(allValues[, var])

    } else {
      cellNum <- cellFromPxcorPycor(world = world, pxcor = agents[, 1], pycor = agents[, 2])
      allValues <- world[]
      if (length(var) == 1) {
        return(allValues[cellNum, var])
      } else {
        return(allValues[cellNum, var, drop = FALSE])
      }
    }
})


################################################################################
#' From SpatialPointsDataFrame to agentMatrix
#'
#' Convert a SpatialPointsDataFrame object into an agentMatrix object.
#'
#' @param spdf SpatialPointsDataFrame object representing moving agents.
#'
#' @return AgentMatrix object representing the moving agents (coordinates and data)
#'         as contained in \code{spdf}.
#'
#' @details If the \code{spdf} does not contain the variables created with
#'          \code{createTurtles()}, these variables will be created with the
#'          default values as in \code{createTurtles()}.
#'
#' @examples
#' sp1 <- SpatialPointsDataFrame(coords = cbind(x = c(1, 2, 3), y = c(1, 2, 3)),
#'                               data = cbind.data.frame(age = c(0, 0, 3),
#'                                                       sex = c("F", "F", "M")))
#' t1 <- spdf2turtles(spdf = sp1)
#'
#'
#' @export
#' @docType methods
#' @rdname spdf2turtles
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "spdf2turtles",
  function(spdf) {
    standardGeneric("spdf2turtles")
  })


#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats runif
#' @rdname spdf2turtles
setMethod(
  "spdf2turtles",
  signature = c("SpatialPointsDataFrame"),
  definition = function(spdf) {

    spdfData <- spdf@data
    n <- length(spdf)

    if (!is.na(match("who", names(spdfData)))) {
      who <- spdfData$who
    } else {
      who <- seq(from = 0, to = n - 1, by = 1)
    }

    if (!is.na(match("heading", names(spdfData)))) {
      heading <- spdfData$heading
    } else {
      heading <- runif(n = n, min = 0, max = 360)
    }

    if (!is.na(match("prevX", names(spdfData)))) {
      prevX <- spdfData$prevX
    } else {
      prevX <- rep(NA, n)
    }

    if (!is.na(match("prevY", names(spdfData)))) {
      prevY <- spdfData$prevY
    } else {
      prevY <- rep(NA, n)
    }

    if (!is.na(match("breed", names(spdfData)))) {
      breed <- spdfData$breed
    } else {
      breed <- rep("turtle", n)
    }

    if (!is.na(match("color", names(spdfData)))) {
      color <- spdfData$color
    } else {
      color <- rainbow(n)
    }

    turtles <- new("agentMatrix",
                   coords = cbind(xcor = spdf@coords[, 1], ycor = spdf@coords[, 2]),
                   who = who,
                   heading = heading,
                   prevX = prevX,
                   prevY = prevY,
                   breed = breed,
                   color = color)

    for (i in which(!names(spdfData) %in% c("who", "heading", "prevX", "prevY",
                                            "breed", "color", "stringsAsFactors"))) {
      turtles <- turtlesOwn(turtles = turtles, tVar = names(spdfData)[i], tVal = spdfData[, i])
    }

    return(turtles)
})

################################################################################
#' From agentMatrix to SpatialPointsDataFrame
#'
#' Convert an agentMatrix object into a SpatialPointsDataFrame object.
#'
#' @inheritParams fargs
#'
#' @return SpatialPointsDataFrame object representing the moving agents
#'        (coordinates and data)
#'         as contained in \code{turtles}.
#'
#' @examples
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 1:10))
#' sp1 <- turtles2spdf(turtles = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname turtles2spdf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtles2spdf",
  function(turtles) {
    standardGeneric("turtles2spdf")
  })

#' @export
#' @importFrom sp SpatialPointsDataFrame
#' @rdname turtles2spdf
setMethod(
  "turtles2spdf",
  signature = c("agentMatrix"),
  definition = function(turtles) {
    spdf <- SpatialPointsDataFrame(coords = turtles@.Data[, c("xcor", "ycor"), drop = FALSE],
                                   data = inspect(turtles, who = turtles@.Data[, "who"])
                                   [3:ncol(turtles@.Data)])
    return(spdf)
  })
