################################################################################
#' All agents?
#'
#' Report \code{TRUE} if all \code{agents} have their variable equal to a given value,
#' report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical. \code{TRUE} if all the \code{agents} have their variable equal to
#'         \code{val}, \code{FALSE} otherwise.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#all}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = runif(25))
#' NLall(agents = patches(w1), world = w1, val = 5)
#' w2 <- w1
#' w2 <- NLset(world = w1, agents = patches(w1), val = 5)
#' NLall(agents = patches(w2), world = w2, val = 5)
#'
#' # Turtles
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
#' NLall(agents = t1, var = "xcor", val = 1)
#' NLall(agents = t1, var = "heading", val = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname NLall
#' @aliases all
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLall",
  function(agents, world, var, val) {
    standardGeneric("NLall")
  })

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("matrix", "worldMatrix", "missing", "ANY"),
  definition = function(agents, world, val) {
    withVal <- NLwith(agents = agents, world = world, val = val)
    allTrue <- ifelse(nrow(agents) == nrow(withVal), TRUE, FALSE)
    return(allTrue)
  }
)

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("matrix", "worldArray", "character", "ANY"),
  definition = function(agents, world, var, val) {
    withVal <- NLwith(agents = agents, world = world, var = var, val = val)
    allTrue <- ifelse(nrow(agents) == nrow(withVal), TRUE, FALSE)
    return(allTrue)
  }
)

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("agentMatrix", "missing", "character", "ANY"),
  definition = function(agents, var, val) {
    withVal <- NLwith(agents = agents, var = var, val = val)
    allTrue <- ifelse(length(agents) == length(withVal), TRUE, FALSE)
    return(allTrue)
  }
)


################################################################################
#' Any agents?
#'
#' Report \code{TRUE} if \code{agents} is non empty, report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical. \code{TRUE} if there is at least one patch or one turtle in the
#'         \code{agents}, \code{FALSE} otherwise.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#any}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' p1 <- noPatches()
#' p2 <- patch(w1, 0, 0)
#' NLany(p1)
#' NLany(p2)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' t2 <- noTurtles()
#' NLany(t1)
#' NLany(t2)
#'
#'
#' @export
#' @docType methods
#' @rdname NLany
#' @aliases any
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLany",
  function(agents) {
    standardGeneric("NLany")
  })

#' @export
#' @rdname NLany
setMethod(
  "NLany",
  signature = c("matrix"),
  definition = function(agents) {

    if (inherits(agents, "agentMatrix")) {
      anyAgents <- ifelse(NROW(agents) == 0, FALSE, TRUE)

    } else {

      anyAgents <- ifelse(NROW(agents) == 0, FALSE, TRUE)

      if (anyAgents == TRUE) {
        nonNAs <- apply(agents, 2, function(x) length(which(!is.na(x))))
        if (sum(nonNAs) == 0) {
          anyAgents <- FALSE
        }
      }
    }

    return(anyAgents)
  }
)


################################################################################
#' Count agents
#'
#' Report the number of patches or turtles inside \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#count}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' p1 <- patches(w1)
#' NLcount(p1) # 25 patches
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' NLcount(t1) # 10 turtles
#'
#'
#' @export
#' @docType methods
#' @rdname NLcount
#' @aliases count
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLcount",
  function(agents) {
    standardGeneric("NLcount")
  })

#' @export
#' @rdname NLcount
setMethod(
  "NLcount",
  signature = c("matrix"),
  definition = function(agents) {
    return(NROW(agents))
  }
)


################################################################################
#' Sort agents
#'
#' Return the \code{agents} sorted according to their value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches sorted according to
#'         their values, if \code{agents}
#'         are patches, or
#'
#'         AgentMatrix representing the turtles sorted according
#'         to their \code{var} values, if \code{agents} are
#'         turtles.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          The sorting of the \code{agents} is done in a increasing order.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#sort-on}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:5, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- sortOn(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' sortHeadingT1 <- sortOn(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname sortOn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "sortOn",
  function(agents, world, var) {
    standardGeneric("sortOn")
  })

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("matrix", "worldMatrix", "missing"),
  definition = function(agents, world) {
    agentsVal <- world[agents[, 1], agents[, 2]]
    pCoords <- agents[order(agentsVal), ]
    return(pCoords)
  }
)

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("matrix", "worldArray", "character"),
  definition = function(agents, world, var) {

    agentsVal <- world[agents[, 1], agents[, 2]]
    pCoords <- agents[order(agentsVal[, var]), ]
    return(pCoords)
  }
)

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("agentMatrix", "missing", "character"),
  definition = function(agents, var) {

    if (NROW(agents) > 1) {
      agents@.Data <- agents@.Data[order(agents@.Data[, var]), ]
    }
    return(agents)
  }
)


################################################################################
#' Agents with
#'
#' Report the patches or the turtles among \code{agents} which have their variable
#' equals to specific values.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches among the \code{agents}
#'         which have their variable
#'         equals to any \code{val}, or
#'
#'         AgentMatrix representing the turtles among the \code{agents}
#'         which have their variable
#'         \code{var} equals to any \code{val}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          This is equivalent in R to subsetting.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#with}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:5, size = 25, replace = TRUE))
#' plot(w1)
#' p2 <- NLwith(agents = patches(w1), world = w1, val = 2)
#'
#' # Turtles
#' t1 <- createTurtles(n = 5, coords = randomXYcor(w1, n = 5),
#'                     breed = c("sheep", "sheep", "wolf", "sheep", "sheperd"))
#' t2 <- NLwith(agents = t1, var = "breed", val = "sheep")
#' t3 <- NLwith(agents = t1, var = "breed", val = c("sheep", "wolf"))
#'
#'
#' @export
#' @docType methods
#' @rdname NLwith
#' @aliases with
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLwith",
  function(agents, world, var, val) {
    standardGeneric("NLwith")
  })

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("matrix", "worldMatrix", "missing", "ANY"),
  definition = function(agents, world, val) {

    agentsValues <- world[agents[, 1], agents[, 2]]
    pVal <- which(agentsValues %in% val)
    return(agents[pVal, , drop = FALSE])
  }
)

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("matrix", "worldArray", "character", "ANY"),
  definition = function(agents, world, var, val) {
    agentsCell <- cellFromPxcorPycor(world = world, pxcor = agents[, 1], pycor = agents[, 2])
    allVal <- as.numeric(t(world@.Data[, , var])) # t() to retrieve the values by rows
    agentsValues <- allVal[agentsCell]
    pVal <- which(agentsValues %in% val)
    return(agents[pVal, , drop = FALSE])
  }
)

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("agentMatrix", "missing", "character", "ANY"),
  definition = function(agents, var, val) {
    # simpler for speed if only 1 val
    if (length(val) == 1) {
      if (!is.numeric(val)) {
        toReturn <- (agents[agents@levels[[var]][agents@.Data[, var]] == val, ])
      } else {
        toReturn <- agents[agents@.Data[, var] == val, ]
      }
    } else {
      if (!is.numeric(val)) {
        toReturn <- (agents[agents@levels[[var]][agents@.Data[, var]] %in% val, ])
      } else {
        toReturn <- agents[agents@.Data[, var] %in% val, ]
      }
    }
    return(toReturn[!is.na(toReturn@.Data[, "who"]), ])
  }
)

################################################################################
#' Agents with maximum
#'
#' Report the patches or turtles among \code{agents} which have their variable
#' equals to the maximum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches among the \code{agents}
#'         which have their variable
#'         equal to the maximum value among the \code{agents}, or
#'
#'         AgentMatrix representing the turtles among the \code{agents}
#'         which have their variable
#'         \code{var} equal to the maximum value among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#with-max}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:5, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- withMax(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- withMax(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname withMax
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "withMax",
  function(agents, world, var) {
    standardGeneric("withMax")
  })

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("matrix", "worldMatrix", "missing"),
  definition = function(agents, world) {

    val <- of(world = world, agents = agents)

    if (length(val[is.na(val)]) == length(val)) {
      stop("patches' values are all NAs")
    } else {
      maxVal <- max(val, na.rm = TRUE)
      posMax <- which(val %in% maxVal)
      return(agents[posMax, , drop = FALSE])
    }
  }
)

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("matrix", "worldArray", "character"),
  definition = function(agents, world, var) {

    val <- of(world = world, agents = agents, var = var)

    if (length(val[is.na(val)]) == length(val)) {
      stop("patches' values are all NAs")
    } else {
      maxVal <- max(val, na.rm = TRUE)
      posMax <- which(val %in% maxVal)
      return(agents[posMax, , drop = FALSE])
    }
  }
)

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("agentMatrix", "missing", "character"),
  definition = function(agents, var) {

    val <- of(agents = agents, var = var)

    if (length(val[is.na(val)]) == length(val)) {
      stop("patches' values are all NAs")
    } else {
      maxVal <- max(val, na.rm = TRUE)
      posMax <- val %in% maxVal
      return(agents[posMax, , drop = FALSE])
    }
  }
)


################################################################################
#' Agents with minimum
#'
#' Report the patches or turtles among \code{agents} which have their variable
#' equals to the minimum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches among the \code{agents}
#'         which have their variable
#'         equal to the minimum value among the \code{agents}, or
#'
#'         AgentMatrix representing the turtles among the \code{agents}
#'         which have their variable
#'         \code{var} equal to the minimum value among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#with-min}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:5, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- withMin(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- withMin(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname withMin
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "withMin",
  function(agents, world, var) {
    standardGeneric("withMin")
  })

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("matrix", "worldMatrix", "missing"),
  definition = function(agents, world) {

    val <- of(world = world, agents = agents)

    if (length(val[is.na(val)]) == length(val)) {
      stop("patches' values are all NAs")
    } else {
      minVal <- min(val, na.rm = TRUE)
      posMin <- which(val %in% minVal)
      return(agents[posMin, , drop = FALSE])
    }
  }
)

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("matrix", "worldArray", "character"),
  definition = function(agents, world, var) {

    val <- of(world = world, agents = agents, var = var)

    if (length(val[is.na(val)]) == length(val)) {
      stop("patches' values are all NAs")
    } else {
      minVal <- min(val, na.rm = TRUE)
      posMin <- which(val %in% minVal)
      return(agents[posMin, , drop = FALSE])
    }
  }
)

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("agentMatrix", "missing", "character"),
  definition = function(agents, var) {

    val <- of(agents = agents, var = var)

    if (length(val[is.na(val)]) == length(val)) {
      stop("patches' values are all NAs")
    } else {
      minVal <- min(val, na.rm = TRUE)
      posMin <- which(val %in% minVal)
      return(agents[posMin, , drop = FALSE])
    }
  }
)


################################################################################
#' One agent with maximum
#'
#' Report one patch or one turtle among \code{agents} which has its variable equals
#' to the maximum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = 1) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the patch
#'         (or of one of the patches) among the \code{agents} which has its variable
#'         equals to the maximum value
#'         among the \code{agents}, or
#'
#'         AgentMatrix of length 1 representing the turtle (or one of
#'         the turtles) among the \code{agents} which has its variable \code{var}
#'         equals to the maximum value
#'         among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there are several patches or turtles among \code{agents} with their
#'          variable equal to the maximum
#'          value, one is chosen randomly. To access to all patches or turtles among
#'          \code{agents} which have their variable equal
#'          to the maximum value, use \code{withMax()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-one-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:5, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- maxOneOf(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- maxOneOf(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname maxOneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxOneOf",
  function(agents, world, var) {
    standardGeneric("maxOneOf")
  })

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("matrix", "worldMatrix", "missing"),
  definition = function(agents, world) {
    maxAgents <- withMax(world = world, agents = agents)
    row <- sample(1:NROW(maxAgents), size = 1)
    return(maxAgents[row, , drop = FALSE])
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("matrix", "worldArray", "character"),
  definition = function(agents, world, var) {
    maxAgents <- withMax(world = world, agents = agents, var = var)
    row <- sample(1:NROW(maxAgents), size = 1)
    return(maxAgents[row, , drop = FALSE])
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("agentMatrix", "missing", "character"),
  definition = function(agents, var) {
    maxAgents <- withMax(agents = agents, var = var)
    row <- sample(1:NLcount(maxAgents), size = 1)
    return(maxAgents[row, ])
  }
)


################################################################################
#' One agent with minimum
#'
#' Report one patch or one turtle among \code{agents} which has its variable equals
#' to the minimum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = 1) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the patch
#'         (or of one of the patches) among the \code{agents} which has its variable
#'         equals to the minimum value
#'         among the \code{agents}, or
#'
#'         AgentMatrix of length 1 representing the turtle (or one of
#'         the turtles) among the \code{agents} which has its variable \code{var}
#'         equals to the minimum value
#'         among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there are several patches or turtles among \code{agents} with their
#'          variable equal to the minimum
#'          value, one is chosen randomly. To access to all patches or turtles among
#'          \code{agents} which have their variable equal
#'          to the minimum value, use \code{withMin()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-one-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:5, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- minOneOf(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- minOneOf(agents = t1, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname minOneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minOneOf",
  function(agents, world, var) {
    standardGeneric("minOneOf")
  })

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("matrix", "worldMatrix", "missing"),
  definition = function(agents, world) {
    minAgents <- withMin(world = world, agents = agents)
    row <- sample(1:NROW(minAgents), size = 1)
    return(minAgents[row, , drop = FALSE])
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("matrix", "worldArray", "character"),
  definition = function(agents, world, var) {
    minAgents <- withMin(world = world, agents = agents, var = var)
    row <- sample(1:NROW(minAgents), size = 1)
    return(minAgents[row, , drop = FALSE])
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("agentMatrix", "missing", "character"),
  definition = function(agents, var) {
    minAgents <- withMin(agents = agents, var = var)
    row <- sample(1:NLcount(minAgents), size = 1)
    return(minAgents[row, ])
  }
)


################################################################################
#' Type of object
#'
#' Report \code{TRUE} if the \code{agents} is of the \code{class} tested,
#' report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @param class  Character. Can take one of the following options to define
#'               the \code{class}: \code{"agent"}, \code{"agentset"},
#'               \code{"patch"}, \code{"patchset"}. \code{"turtle"} or \code{"turtleset"}.
#'
#' @return Logical. \code{TRUE} if \code{agents} is of the \code{class} tested.
#'
#' @details Careful! The \code{class} tested does not correspond to actual R classes.
#'
#'          \code{agents} is \code{"patch"} if it is a matrix (ncol = 2) with the
#'          first column "pxcor" and the second column "pycor" with only
#'          one row. \code{agents} is \code{"patcheset"} if the matrix has more than
#'          one row.
#'
#'          \code{agents} is \code{"turtle"} if it is an agentMatrix
#'          containing only one turtle.
#'          \code{agents} is \code{"turtleset"} if the
#'          agentMatrix contains more than one turtle.
#'
#'          \code{agents} is \code{"agent"} if it is either \code{"patch"} or
#'          \code{"turtle"}. \code{agents} is \code{"agentset"} if it is either
#'          \code{"patcheset"} or \code{"turtleset"}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#is-of-type}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:3, size = 10, replace= TRUE))
#' isNLclass(agents = patches(w1), class = "patch")
#' isNLclass(agents = patches(w1), class = "patcheset")
#' isNLclass(agents = t1, class = "agentset")
#' isNLclass(agents = t1, class = "turtleset")
#'
#'
#' @export
#' @docType methods
#' @rdname isNLclass
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "isNLclass",
  function(agents, class) {
    standardGeneric("isNLclass")
  })

#' @export
#' @rdname isNLclass
setMethod(
  "isNLclass",
  signature = c("matrix", "character"),
  definition = function(agents, class) {
    # turtles
    if (inherits(agents, "agentMatrix")) {
      if (class == "agent") {
        class <- "turtle"
      }
      if (class == "agentset") {
        class <- "turtleset"
      }

      if (all(colnames(agents@.Data)[1:8] == c("xcor", "ycor", "who", "heading",
                                               "prevX", "prevY", "breed", "color") &
              NLcount(agents) != 0)) {
        if (NLcount(agents) == 1) {
          agentsClass <- "turtle"
        } else {
          agentsClass <- "turtleset"
        }
      } else {
        agentsClass <- "nothing"
      }

      matchClass <- ifelse(class == agentsClass, TRUE, FALSE)
      return(matchClass)
    } else {
      # If it is this signature, it is a matrix, therefore patch or patches
      if (class == "agent") {
        class <- "patch"
      }
      if (class == "agentset") {
        class <- "patchset"
      }

      if (all(colnames(agents) == c("pxcor", "pycor") & NROW(agents) != 0)) {
        if (NROW(agents) == 1) {
          agentsClass <- "patch"
        } else {
          agentsClass <- "patchset"
        }
      } else {
        agentsClass <- "nothing"
      }

      matchClass <- ifelse(class == agentsClass, TRUE, FALSE)
      return(matchClass)
    }
  }
)


################################################################################
#' N random agents
#'
#' Report \code{n} patches or turtles randomly selected among \code{agents}.
#'
#' @inheritParams oneOf
#'
#' @param n  Integer. Number of patches or turtles to select from \code{agents}.
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "pxcor"
#'         and the second  column "pycor" representing the coordinates of the
#'         selected patches from \code{agents}, or
#'
#'         Matrix (ncol = 2) with the first column "pxcor"
#'         and the second  column "pycor" representing the coordinates of the
#'         selected patches from \code{agents}, \code{n} per individual "id", or
#'
#'         AgentMatrix (nrow = \code{n}) representing the turtles
#'         selected from \code{agents},
#'
#'         Integer. Vector of "who" numbers for the selected turtles from
#'         \code{agents}, \code{n} per individual "id".
#'
#' @details \code{n} must be less or equal the number of patches
#'          or turtles in \code{agents}.
#'
#'          If \code{agents} is a matrix with ncol = 3, the selection of \code{n}
#'          random patches is done per individual "id". The order of the patches
#'          coordinates returned follow the order of "id".
#'          If \code{agents} is a matrix (ncol = 2) with columns "whoTurtles" and
#'          "id", the selection of \code{n} random turtles (defined by their "whoTurtles")
#'          is done per individual "id". The order of the "who" numbers returned
#'          follow the order of "id".
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#n-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' pSelect <- nOf(agents = patches(w1), n = 5)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' tSelect <- nOf(agents = t1, n = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname nOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "nOf",
  function(agents, n) {
    standardGeneric("nOf")
  })

#' @export
#' @rdname nOf
setMethod(
  "nOf",
  signature = c("matrix", "numeric"),
  definition = function(agents, n) {


    if (inherits(agents, "agentMatrix")) {
      row <- sample(1:NROW(agents), size = n, replace = FALSE)
      row <- row[order(row)]
      turtles <- agents[row, ]
      return(turtles)
    } else {
      if (ncol(agents) == 2 & colnames(agents)[1] == "pxcor") {
        # patches
        row <- sample(1:NROW(agents), size = n, replace = FALSE)
        row <- row[order(row)]
        patches <- agents[row, , drop = FALSE]
        return(patches)
      } else {
        # patches
        if (min(table(agents[, "id"])) < n) {
          stop("n is larger than the number of agents per id")
        } else {
          if (ncol(agents) == 3) {
            # patches with id
            row <- tapply(X = 1:nrow(agents), INDEX = as.factor(agents[, "id"]),
                          FUN = function(x) sample(x, size = n, replace = FALSE))
            patches <- agents[unlist(row), c("pxcor", "pycor")]
            return(patches)
          } else {
            # whoNUmbers of turtles with id
            row <- tapply(X = 1:nrow(agents), INDEX = as.factor(agents[, "id"]),
                          FUN = function(x) sample(x, size = n, replace = FALSE))
            turtles <- agents[unlist(row), "whoTurtles"]
            return(turtles)
          }
        }
      }
    }
})

################################################################################
#' One random agent
#'
#' Report one patch or turtle randomly selected among \code{agents}.
#'
#' @param agents Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               Matrix (ncol = 3) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates and the
#'               third column "id", or
#'
#'               AgentMatrix object representing the moving agents, or
#'
#'               Matrix (ncol = 2) with the first column "whoTurtles" and the
#'               second column "id".
#'
#' @return Matrix (ncol = 2, nrow = 1) with the first column "pxcor"
#'         and the second  column "pycor" representing the coordinates of the
#'         selected patch from \code{agents}, or
#'
#'         Matrix (ncol = 2) with the first column "pxcor"
#'         and the second  column "pycor" representing the coordinates of the
#'         selected patches from \code{agents}, one per individual "id", or
#'
#'         AgentMatrix object representing the turtle
#'         selected from \code{agents}, or
#'
#'         Integer. Vector of "who" numbers for the selected turtles from
#'         \code{agents}, one per individual "id".
#'
#' @details If \code{agents} is a matrix with ncol = 3, the selection of one
#'          random patch is done per individual "id". The order of the patches
#'          coordinates returned follow the order of "id".
#'          If \code{agents} is a matrix (ncol = 2) with columns "whoTurtles" and
#'          "id", the selection of one random turtle (defined by their "whoTurtles")
#'          is done per individual "id". The order of the "who" numbers returned
#'          follow the order of "id".
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#one-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' pSelect <- oneOf(agents = patches(w1))
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' tSelect <- oneOf(agents = t1)
#'
#'
#' @export
#' @importFrom Hmisc mApply
#' @docType methods
#' @rdname oneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "oneOf",
  function(agents) {
    standardGeneric("oneOf")
  })

#' @export
#' @rdname oneOf
setMethod(
  "oneOf",
  signature = c("matrix"),
  definition = function(agents) {
    if (inherits(agents, "agentMatrix")) {
      nOf(agents = agents, n = 1)
    } else {
      if (ncol(agents) == 2 & colnames(agents)[1] == "pxcor") {
        # patches
        nOf(agents = agents, n = 1)
      } else if (ncol(agents) == 3) {
        # patches with id
        mApply(X = agents[, c("pxcor", "pycor")], INDEX = as.factor(agents[, "id"]),
               FUN = oneOf, keepmatrix = TRUE)
      } else {
        # whoNUmbers of turtles with id
        whoTurtles <- tapply(X = agents[, "whoTurtles"], INDEX = as.factor(agents[, "id"]),
                             FUN = function(x) ifelse(length(x) == 1, x, sample(x, size = 1)))
        return(as.numeric(whoTurtles))
      }
    }
  }
)

################################################################################
#' N agents with maximum
#'
#' Report the \code{n} patches or turtles among \code{agents} which have their variable
#' among the maximum values.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the \code{n}
#'         patches among the \code{agents} which have their variable values among
#'         the maximum values among the
#'         \code{agents}, or
#'
#'         AgentMatrix of length \code{n} representing the turtles among the
#'         \code{agents} which
#'         have their \code{var} values among the maximum values among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there is a tie that would make the number of returned patches or turtles larger
#'          than \code{n}, it is broken randomly.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-n-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                           data = sample(1:10, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- maxNof(agents = patches(w1), n = 6, world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:5, size = 10, replace= TRUE))
#' t2 <- maxNof(agents = t1, n = 5, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname maxNof
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxNof",
  function(agents, n, world, var) {
    standardGeneric("maxNof")
  })

#' @export
#' @rdname maxNof
setMethod(
  "maxNof",
  signature = c("matrix", "numeric", "worldMatrix", "missing"),
  definition = function(agents, n, world) {

    if (n == 1) {
      maxOneOf(agents = agents, world = world)
    } else if (n == 0) {
      noPatches()
    } else if (n == NROW(agents)) {
      return(agents)
    } else {

      val <- of(world = world, agents = agents)
      agentsVal <- cbind(val, agents)
      agentsVal <- agentsVal[order(-agentsVal[, "val"]), ] # decreasing order

      minVal <- min(agentsVal[1:n, "val"], na.rm = TRUE)
      maxAgents <- agentsVal[agentsVal[, "val"] >= minVal, , drop = FALSE]

      # To break ties randomly
      if (NROW(maxAgents) != n) {
        nToRemove <- NROW(maxAgents) - n # how many ties to remove
        toKeep <- sample(1:NROW(maxAgents[maxAgents[, "val"] == minVal, ]),
                         size = NROW(maxAgents[maxAgents[, "val"] == minVal, ]) - nToRemove)
        maxAgents <- rbind(maxAgents[maxAgents[, "val"] > minVal, ],
                           maxAgents[maxAgents[, "val"] == minVal, ][toKeep, ])
      }

      return(maxAgents[, c("pxcor", "pycor"), drop = FALSE])
    }
  }
)

#' @export
#' @rdname maxNof
setMethod(
  "maxNof",
  signature = c("matrix", "numeric", "worldArray", "character"),
  definition = function(agents, n, world, var) {
    if (n == 1) {
      maxOneOf(agents = agents, world = world, var = var)
    } else if (n == 0) {
      noPatches()
    } else if (n == NROW(agents)) {
      return(agents)
    } else {
      val <- of(world = world, agents = agents, var = var)
      agentsVal <- cbind(val, agents)
      agentsVal <- agentsVal[order(-agentsVal[, "val"]), ] # decreasing order

      minVal <- min(agentsVal[1:n, "val"], na.rm = TRUE)
      maxAgents <- agentsVal[agentsVal[, "val"] >= minVal, , drop = FALSE]

      # To break ties randomly
      if (NROW(maxAgents) != n) {
        nToRemove <- NROW(maxAgents) - n # how many ties to remove
        toKeep <- sample(1:NROW(maxAgents[maxAgents[, "val"] == minVal, ]),
                         size = NROW(maxAgents[maxAgents[, "val"] == minVal, ]) - nToRemove)
        maxAgents <- rbind(maxAgents[maxAgents[, "val"] > minVal, ],
                           maxAgents[maxAgents[, "val"] == minVal, ][toKeep, ])
      }

      return(maxAgents[, c("pxcor", "pycor"), drop = FALSE])
    }
  }
)

#' @export
#' @rdname maxNof
setMethod(
  "maxNof",
  signature = c("agentMatrix", "numeric", "missing", "character"),
  definition = function(agents, n, var) {
    if (n == 1) {
      maxOneOf(agents = agents, var = var)
    } else if (n == 0) {
      noTurtles()
    } else if (n == NLcount(agents)) {
      return(agents)
    } else {
      tData <- inspect(agents, who = agents@.Data[, "who"])
      rownames(tData) <- 1:NROW(tData)
      tData <- tData[order(-tData[, var]), ] # decreasing order

      minVal <- min(tData[1:n, var], na.rm = TRUE)
      maxAgents <- tData[tData[, var] >= minVal, ]

      # To break ties randomly
      if (NROW(maxAgents) != n) {
        nToRemove <- NROW(maxAgents) - n # how many ties to remove
        toKeep <- sample(1:NROW(maxAgents[maxAgents[, var] == minVal, ]),
                         size = NROW(maxAgents[maxAgents[, var] == minVal, ]) - nToRemove)
        maxAgents <- rbind(maxAgents[maxAgents[, var] > minVal, ],
                           maxAgents[maxAgents[, var] == minVal, ][toKeep, ])
      }

      tSelect <- as.numeric(rownames(maxAgents))
      tSelect <- sort(tSelect)
      maxTurtles <- agents[tSelect, ]
      return(maxTurtles)
    }
  }
)

################################################################################
#' N agents with minimum
#'
#' Report the \code{n} patches or turtles among \code{agents} which have their variable
#' among the minimum values.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the \code{n}
#'         patches among the \code{agents} which have their variable values among
#'         the minimum values among the
#'         \code{agents}, or
#'
#'         AgentMatrix of length \code{n} representing the turtles among the
#'         \code{agents} which
#'         have their \code{var} values among the minimum values among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there is a tie that would make the number of returned patches or turtles larger
#'          than \code{n}, it is broken randomly.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-n-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
#'                   data = sample(1:10, size = 25, replace = TRUE))
#' plot(w1)
#' p1 <- minNof(agents = patches(w1), n = 6, world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10),
#'                     heading = sample(1:5, size = 10, replace= TRUE))
#' t2 <- minNof(agents = t1, n = 5, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname minNof
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minNof",
  function(agents, n, world, var) {
    standardGeneric("minNof")
})

#' @export
#' @rdname minNof
setMethod(
  "minNof",
  signature = c("matrix", "numeric", "worldMatrix", "missing"),
  definition = function(agents, n, world) {
    if (n == 1) {
      minOneOf(agents = agents, world = world)
    } else if (n == 0) {
      noPatches()
    } else if (n == NROW(agents)) {
      return(agents)
    } else {

      val <- of(world = world, agents = agents)
      agentsVal <- cbind(val, agents)
      agentsVal <- agentsVal[order(agentsVal[, "val"]), ] # increasing order

      maxVal <- max(agentsVal[1:n, "val"], na.rm = TRUE)
      minAgents <- agentsVal[agentsVal[, "val"] <= maxVal, , drop = FALSE]

      # To break ties randomly
      if (NROW(minAgents) != n) {
        nToRemove <- NROW(minAgents) - n # how many ties to remove
        toKeep <- sample(1:NROW(minAgents[minAgents[, "val"] == maxVal, ]),
                         size = NROW(minAgents[minAgents[, "val"] == maxVal, ]) - nToRemove)
        minAgents <- rbind(minAgents[minAgents[, "val"] < maxVal, ],
                           minAgents[minAgents[, "val"] == maxVal, ][toKeep, ])
      }

      return(minAgents[, c("pxcor", "pycor"), drop = FALSE])
    }
  }
)

#' @export
#' @rdname minNof
setMethod(
  "minNof",
  signature = c("matrix", "numeric", "worldArray", "character"),
  definition = function(agents, n, world, var) {
    if (n == 1) {
      minOneOf(agents = agents, world = world, var = var)
    } else if (n == 0) {
      noPatches()
    } else if (n == NROW(agents)) {
      return(agents)
    } else {

      val <- of(world = world, agents = agents, var = var)
      agentsVal <- cbind(val, agents)
      agentsVal <- agentsVal[order(agentsVal[, "val"]), ] # increasing order

      maxVal <- max(agentsVal[1:n, "val"], na.rm = TRUE)
      minAgents <- agentsVal[agentsVal[, "val"] <= maxVal, , drop = FALSE]

      # To break ties randomly
      if (NROW(minAgents) != n) {
        nToRemove <- NROW(minAgents) - n # how many ties to remove
        toKeep <- sample(1:NROW(minAgents[minAgents[, "val"] == maxVal, ]),
                         size = NROW(minAgents[minAgents[, "val"] == maxVal, ]) - nToRemove)
        minAgents <- rbind(minAgents[minAgents[, "val"] < maxVal, ],
                           minAgents[minAgents[, "val"] == maxVal, ][toKeep, ])
      }

      return(minAgents[, c("pxcor", "pycor"), drop = FALSE])
    }
  }
)

#' @export
#' @rdname minNof
setMethod(
  "minNof",
  signature = c("agentMatrix", "numeric", "missing", "character"),
  definition = function(agents, n, var) {
    if (n == 1) {
      minOneOf(agents = agents, var = var)
    } else if (n == 0) {
      noTurtles()
    } else if (n == NLcount(agents)) {
      return(agents)
    } else {

      tData <- inspect(agents, who = agents@.Data[, "who"])
      rownames(tData) <- 1:NROW(tData)
      tData <- tData[order(tData[, var]), ] # increasing order

      maxVal <- max(tData[1:n, var], na.rm = TRUE)
      minAgents <- tData[tData[, var] <= maxVal, ]

      # To break ties randomly
      if (NROW(minAgents) != n) {
        nToRemove <- NROW(minAgents) - n # how many ties to remove
        toKeep <- sample(1:NROW(minAgents[minAgents[, var] == maxVal, ]),
                         size = NROW(minAgents[minAgents[, var] == maxVal, ]) - nToRemove)
        minAgents <- rbind(minAgents[minAgents[, var] < maxVal, ],
                           minAgents[minAgents[, var] == maxVal, ][toKeep, ])
      }

      tSelect <- as.numeric(rownames(minAgents))
      tSelect <- sort(tSelect)
      minTurtles <- agents[tSelect, ]
      return(minTurtles)
    }
  }
)


################################################################################
#' Agents in radius
#'
#' Report the patches or turtles among \code{agents2} within given distances of
#' each of the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @param radius  Numeric. Vector of distances from \code{agents} to locate
#'                \code{agents2}. Must be of length 1 or of length \code{agents}.
#'
#' @param agents2 Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               AgentMatrix object representing the moving agents.
#'
#' @return Matrix (ncol = 3) with the first column "pxcor"
#'         and the second column "pycor" representing the coordinates of the
#'         patches among \code{agents2} within \code{radius} distances for each \code{agents}
#'         which are reprensented by the "id" column, if
#'         \code{agents2} are patches, or
#'
#'         Matrix (ncol = 2) with the first column "who"
#'         representing the "who" numbers of the
#'         turtles among \code{agents2} within \code{radius} distances for each \code{agents}
#'         which are reprensented by the "id" column, if
#'         \code{agents2} are turtles.
#'
#' @details Distances from/to patches are calculated from/to their center.
#'
#'          If \code{torus = FALSE}, \code{world} does not need to be provided.
#'
#'          If \code{torus = TRUE}, the \code{radius} distances are calculared
#'          around the sides of the \code{world} to select \code{agents2}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#in-radius}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' p1 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patches(w1))
#' t2 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = t1)
#' p2 <- inRadius(agents = t1, radius = 2, agents2 = patches(w1))
#' t3 <- inRadius(agents = turtle(t1, who = 0), radius = 2, agents2 = t1)
#'
#'
#' @export
#' @importFrom rgeos gBuffer
#' @importFrom sp over
#' @importFrom sp SpatialPoints
#' @docType methods
#' @rdname inRadius
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "inRadius",
  function(agents, radius, agents2, world, torus = FALSE) {
    standardGeneric("inRadius")
})

#' @export
#' @rdname inRadius
setMethod(
  "inRadius",
  signature = c(agents = "matrix", radius = "numeric", agents2 = "matrix"),
  definition = function(agents, radius, agents2, world, torus) {
    if (inherits(agents, "agentMatrix") & inherits(agents2, "agentMatrix")) {
      # Transform agent into a matrix
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
      inRadius(agents = agents, radius = radius, agents2 = agents2, world = world, torus = torus)
    } else if (inherits(agents, "agentMatrix") & !inherits(agents2, "agentMatrix")) {
      # Transform agent into a matrix
      agents <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
      inRadius(agents = agents, radius = radius, agents2 = agents2, world = world, torus = torus)
    } else if (!inherits(agents, "agentMatrix") & inherits(agents2, "agentMatrix")) {
      # Transform the agents into SP to use gBuffer
      agentsSP <- SpatialPoints(coords = agents)

      # Create buffers around the locations of agents
      pBuffer <- gBuffer(agentsSP, byid = TRUE, id = 1:NROW(agents), width = radius, quadsegs = 50)

      if (torus == TRUE) {
        if (missing(world)) {
          stop("A world must be provided as torus = TRUE")
        }

        agents2c <- agents2@.Data[, c("xcor", "ycor"), drop = FALSE]
        agents2c1 <- cbind(agents2c[, 1] - (world@extent@xmax - world@extent@xmin),
                           agents2c[, 2] + (world@extent@ymax - world@extent@ymin))
        agents2c2 <- cbind(agents2c[, 1], agents2c[, 2] + (world@extent@ymax - world@extent@ymin))
        agents2c3 <- cbind(agents2c[, 1] + (world@extent@xmax - world@extent@xmin),
                           agents2c[, 2] + (world@extent@ymax - world@extent@ymin))
        agents2c4 <- cbind(agents2c[, 1] - (world@extent@xmax - world@extent@xmin), agents2c[, 2])
        agents2c5 <- cbind(agents2c[, 1] + (world@extent@xmax - world@extent@xmin), agents2c[, 2])
        agents2c6 <- cbind(agents2c[, 1] - (world@extent@xmax - world@extent@xmin),
                           agents2c[, 2] - (world@extent@ymax - world@extent@ymin))
        agents2c7 <- cbind(agents2c[, 1], agents2c[, 2] - (world@extent@ymax - world@extent@ymin))
        agents2c8 <- cbind(agents2c[, 1] + (world@extent@xmax - world@extent@xmin),
                           agents2c[, 2] - (world@extent@ymax - world@extent@ymin))
        agents2cAll <- rbind(agents2c, agents2c1, agents2c2, agents2c3, agents2c4,
                             agents2c5, agents2c6, agents2c7, agents2c8)

        # Extract the locations of agents2 under the buffers
        pOverL <- over(pBuffer, SpatialPoints(coords = agents2cAll), returnList = TRUE)
        pOver <- unlist(pOverL)
        lengthID <- unlist(lapply(pOverL, length))
        colnames(agents2cAll) <- c("x", "y")
        agentsWrap <- wrap(agents2cAll[pOver, , drop = FALSE], world@extent)
        agentsXY <- unique(cbind(agentsWrap, id = rep(as.numeric(names(pOverL)), lengthID)))

        tOn <- merge(agentsXY, agents2@.Data[, c("xcor", "ycor", "who"), drop = FALSE],
                     by.x = c("x", "y"), by.y = c("xcor", "ycor"))
        return(tOn[order(tOn[, "id"]), c("who", "id")])
      } else {
        pOverL <- over(pBuffer,
                       SpatialPoints(coords = agents2@.Data[, c("xcor", "ycor"), drop = FALSE]),
                       returnList = TRUE)
        pOver <- unlist(pOverL)
        lengthID <- unlist(lapply(pOverL, length))
        agentsXY <- unique(cbind(agents2@.Data[pOver, c("xcor", "ycor"), drop = FALSE],
                                 id = rep(as.numeric(names(pOverL)), lengthID)))

        tOn <- merge(agentsXY, agents2@.Data[, c("xcor", "ycor", "who"), drop = FALSE])
        return(tOn[order(tOn[, "id"]), c("who", "id")])
      }
    } else {
      # Transform the agents into SP to use gBuffer
      agentsSP <- SpatialPoints(coords = agents)

      # Create buffers around the locations of agents
      pBuffer <- raster::buffer(agentsSP, dissolve = FALSE, width = radius)

      if (torus == TRUE) {
        if (missing(world)) {
          stop("A world must be provided as torus = TRUE")
        }

        worldWrap <- createWorld(minPxcor = minPxcor(world) - radius,
                                 maxPxcor = maxPxcor(world) + radius,
                                 minPycor = minPycor(world) - radius,
                                 maxPycor = maxPycor(world) + radius)
        pAllWrap <- patches(worldWrap)

        # Extract the locations of agents2 under the buffers
        pOverL <- over(pBuffer, SpatialPoints(coords = pAllWrap), returnList = TRUE)
        pOver <- unlist(pOverL)
        lengthID <- unlist(lapply(pOverL, length))
        colnames(pAllWrap) <- c("x", "y")
        agentsWrap <- wrap(pAllWrap[pOver, , drop = FALSE], world@extent)
        agentsXY <- unique(cbind(agentsWrap, id = rep(as.numeric(names(pOverL)), lengthID)))
        colnames(agentsXY)[1:2] <- c("pxcor", "pycor")
        return(agentsXY)
      } else {
        sp1 <- SpatialPoints(coords = agents2)
        pOverL <- over(pBuffer, sp1, returnList = TRUE)
        pOver <- unlist(pOverL)
        lengthID <- unlist(lapply(pOverL, length))
        agentsXY <- cbind(agents2[pOver, , drop = FALSE],
                          id = rep(as.numeric(names(pOverL)), lengthID))

        return(agentsXY)
      }
    }
  }
)

################################################################################
#' Agents in cone
#'
#' Report the \code{agents} within the "cone of vision" in front of each one of the
#' \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @param radius  Numeric. Vector of distances from \code{turtles} to locate
#'                \code{agents}. Must be of length 1 or of length \code{turtles}.
#'
#' @param angle   Numeric. Vector of angles to define the size of the cone of vision
#'                for the \code{turtles}. The cone of vision is defined between the
#'                direction of their headings minus \code{angle / 2}
#'                to the direction of their headings plus \code{angle / 2}. Must be of
#'                length 1 or
#'                of length \code{turtles}.
#'
#' @return Matrix (ncol = 3) with the first column "pxcor"
#'         and the second column "pycor" representing the coordinates of the
#'         patches among \code{agents2} within the cone of vision of each of the
#'         \code{turtles}
#'         which are reprensented by the "id" column, if
#'         \code{agents} are patches, or
#'
#'         Matrix (ncol = 2) with the first column "who"
#'         representing the "who" numbers of the
#'         turtles among \code{agents2} within the cone of vision of each of  the
#'         \code{turtles}
#'         which are reprensented by the "id" column, if
#'         \code{agents} are turtles.
#'
#' @details \code{agents} are reported if there are within \code{radius}
#'          distance of the turtle and their direction from the turtle is within
#'          \code{[-angle, + angle]} of the turtle's heading.
#'
#'          Distances to patches are calculated to their center.
#'
#'          If \code{torus = FALSE}, \code{world} does not need to be provided.
#'
#'          If \code{torus = TRUE}, the \code{radius} distances are calculated
#'          around the sides of the \code{world} to select \code{agents}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#in-cone}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' p1 <- inCone(turtles = t1, radius = 2, agents = patches(w1), angle = 90)
#' t2 <- inCone(turtles = turtle(t1, who = 0), radius = 2, angle = 90, agents = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname inCone
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "inCone",
  function(turtles, radius, angle, agents, world, torus = FALSE) {
    standardGeneric("inCone")
})

#' @export
#' @rdname inCone
setMethod(
  "inCone",
  signature = c(turtles = "agentMatrix", radius = "numeric", angle = "numeric", agents = "matrix"),
  definition = function(turtles, radius, angle, agents, world, torus) {
    if (inherits(agents, "agentMatrix")) {
      agentsCoords <- agents@.Data[, c("xcor", "ycor"), drop = FALSE]
      colnames(agentsCoords) <- c("pxcor", "pycor")
      patchInCone <- inCone(turtles = turtles, radius = radius, angle = angle,
                            agents = agentsCoords, world = world, torus = torus)
      colnames(patchInCone)[1:2] <- c("xcor", "ycor")
      agentsInCone <- merge(patchInCone, agents@.Data[, c("xcor", "ycor", "who"), drop = FALSE])
      return(agentsInCone[order(agentsInCone[, "id"]), c("who", "id")])
    } else {
      # Find the patches within distances
      agentsInRadius <- inRadius(agents = turtles, radius = radius, agents2 = agents,
                                 world = world, torus = torus)
      if (NROW(agentsInRadius) == 0) {
        # No patches are within radius distances for any turtles
        return(agentsInRadius)
      } else {
        turtlesWith <- turtles[unique(agentsInRadius[, "id"]), ]

        # Direction from the turtle to each of their patches within radius distance
        tDir <- lapply(unique(agentsInRadius[, "id"]), function(x) {
          towards(world = world, agents = turtles[x, ],
                  agents2 = agentsInRadius[agentsInRadius[, "id"] == x,
                                           c("pxcor", "pycor"), drop = FALSE],
                  torus = torus)
          })
        # Define the rotation angle between the turtle heading and the direction to each patches
        tCone <- lapply(1:length(tDir), function(x) {
          subHeadings(angle1 = tDir[[x]], angle2 = turtles[x, ], range360 = FALSE)
        })

        angle <- angle / 2
        if (length(angle) == 1) {
          angle <- rep(angle, NROW(turtles))
        }
        # Remove the angle for the turtles which do not have patches within radius distance
        angle <- angle[unique(agentsInRadius[, "id"])]

        # Is the rotation to face the patches smaller than the maximum rotation allowed
        pWithin <- lapply(1:length(unique(agentsInRadius[, "id"])), function(x) {
          posPatch <- which(abs(tCone[[x]]) < angle[x])
          unique(agentsInRadius[agentsInRadius[, "id"] == unique(agentsInRadius[, "id"])[x],
                                c("pxcor", "pycor"), drop = FALSE][posPatch, , drop = FALSE])
        })

        nAgents <- unlist(lapply(pWithin, NROW))
        agentsInCone <- cbind(do.call(rbind, pWithin),
                              id = rep(unique(agentsInRadius[, "id"]), nAgents))

        return(agentsInCone)
      }
    }
  }
)

################################################################################
#' Set an agents variable
#'
#' Assign values to the \code{agents} for the selected variables.
#'
#' @inheritParams of
#'
#' @inheritParams fargs
#'
#' @param val Numeric or character. Vector of length 1 or length \code{NLcount(agents)}
#'            if \code{length(var) == 1}, or
#'
#'            Matrix or Dataframe (ncol = \code{length(var)}, nrow = \code{NLcount(agents)}).
#'            Columns must be in the same order as \code{var}.
#'
#' @return WorldMatrix or worldArray object with the values \code{val} assigned to the patches
#'         variables \code{var}
#'         for the \code{agents}, or
#'
#'         AgentMatrix representing the \code{turtles} with
#'         the values \code{val} assigned to the variables \code{var} for the \code{agents}.
#'
#' @details If \code{agents} are patches, \code{world} must be provided and \code{turtles}
#'          must not be provided. If \code{agents} are turtles, \code{turtles} must be
#'          provided and \code{world} must not be provided.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#set}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1 <- NLset(world = w1, agents = patches(w1), val = 1)
#' # Set the patch[0,4] to 0
#' w1 <- NLset(world = w1, agents = patch(w1, 0, 4), val = 0)
#' of(world = w1, agents = patches(w1))
#'
#' t1 <- createTurtles(n = 3, world = w1, heading = 0)
#' # Set the heading of turtle 0 to 180
#' t2 <- NLset(turtles = t1, agents = turtle(t1, who = 0), var = "heading", val = 180)
#' of(agents = t2, var = "heading") # c(180, 0, 0)
#'
#'
#' @export
#' @docType methods
#' @rdname NLset
#' @aliases set
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLset",
  function(world, turtles, agents, var, val) {
    standardGeneric("NLset")
  })

#' @export
#' @rdname NLset
setMethod(
  "NLset",
  signature = c(world = "missing", turtles = "agentMatrix", agents = "agentMatrix",
                var = "character", val = "ANY"),
  definition = function(turtles, agents, var, val) {

    if (NROW(agents) != 0) {

      if (length(var) == 1) {

        if (!is.na(match(var, names(turtles@levels)))) {

          # Update the levels as some may have disapeared
          turtlesLevelsVar <- turtles@levels[[var]] # old levels
          turtlesVar <- turtles@.Data[, var] # old levels number
          turtlesVarUnique <- unique(turtlesVar) # unique old levels numbers
          # levels in the order of the unique old levels numbers
          turtlesLevelsVarUpdated <- turtlesLevelsVar[unique(turtlesVar)[order(turtlesVarUnique)]]
          turtles@levels[[var]] <- turtlesLevelsVarUpdated
          # replace the levels number starting to 1 and increasing by 1
          turtlesVarUpdated <- mapvalues(x = turtlesVar, from = turtlesVarUnique,
                                         to = rank(turtlesVarUnique))
          turtles@.Data[, var] <- turtlesVarUpdated

          if (identical(agents, turtles)) {
            turtles[, var] <- as.character(val)

          } else {

            iAgents <- match(agents@.Data[, "who"], turtles@.Data[, "who"])
            turtles[iAgents, var] <- as.character(val)
          }

        } else {

          if (identical(agents, turtles)) {
            turtles@.Data[, var] <- as.numeric(val)

          } else {

            iAgents <- match(agents@.Data[, "who"], turtles@.Data[, "who"])
            turtles@.Data[iAgents, var] <- as.numeric(val)
          }
        }
      } else {

        varLevels <- which(var %in% names(turtles@levels))
        if (length(varLevels) != 0) {
          varNum <- (1:length(var))[! (1:length(var)) %in% varLevels]
        } else {
          varNum <- 1:length(var)
        }

        if (length(varLevels) != 0) {
          if (length(varLevels) == 1) {

            # Update the levels as some may have disapeared
            turtlesLevelsVar <- turtles@levels[[var[varLevels]]] # old levels
            turtlesVar <- turtles@.Data[, var[varLevels]] # old levels number
            turtlesVarUnique <- unique(turtlesVar) # unique old levels numbers
            # levels in the order of the unique old levels numbers
            turtlesLevelsVarUpdated <- turtlesLevelsVar[unique(turtlesVar)
                                                        [order(turtlesVarUnique)]]
            turtles@levels[[var[varLevels]]] <- turtlesLevelsVarUpdated
            # replace the levels number starting to 1 and increasing by 1
            turtlesVarUpdated <- mapvalues(x = turtlesVar, from = turtlesVarUnique,
                                           to = rank(turtlesVarUnique))
            turtles@.Data[, var[varLevels]] <- turtlesVarUpdated

            if (identical(agents, turtles)) {
              turtles[, var[varLevels]] <- as.character(val[, varLevels])

            } else {

              iAgents <- match(agents@.Data[, "who"], turtles@.Data[, "who"])
              turtles[iAgents, var[varLevels]] <- as.character(val[, varLevels])
            }

          } else {

            if (identical(agents, turtles)) {

              for(i in varLevels) {

                # Update the levels as some may have disapeared
                turtlesLevelsVar <- turtles@levels[[var[i]]] # old levels
                turtlesVar <- turtles@.Data[, var[i]] # old levels number
                turtlesVarUnique <- unique(turtlesVar) # unique old levels numbers
                # levels in the order of the unique old levels numbers
                turtlesLevelsVarUpdated <- turtlesLevelsVar[unique(turtlesVar)
                                                            [order(turtlesVarUnique)]]
                turtles@levels[[var[i]]] <- turtlesLevelsVarUpdated
                # replace the levels number starting to 1 and increasing by 1
                turtlesVarUpdated <- mapvalues(x = turtlesVar, from = turtlesVarUnique,
                                               to = rank(turtlesVarUnique))
                turtles@.Data[, var[i]] <- turtlesVarUpdated

                turtles[, var[i]] <- as.character(val[, var[i]])
              }
            } else {

              iAgents <- match(agents@.Data[, "who"], turtles@.Data[, "who"])
              for(i in varLevels) {

                # Update the levels as some may have disapeared
                turtlesLevelsVar <- turtles@levels[[var[i]]] # old levels
                turtlesVar <- turtles@.Data[, var[i]] # old levels number
                turtlesVarUnique <- unique(turtlesVar) # unique old levels numbers
                # levels in the order of the unique old levels numbers
                turtlesLevelsVarUpdated <- turtlesLevelsVar[unique(turtlesVar)
                                                            [order(turtlesVarUnique)]]
                turtles@levels[[var[i]]] <- turtlesLevelsVarUpdated
                # replace the levels number starting to 1 and increasing by 1
                turtlesVarUpdated <- mapvalues(x = turtlesVar, from = turtlesVarUnique,
                                               to = rank(turtlesVarUnique))
                turtles@.Data[, var[i]] <- turtlesVarUpdated

                turtles[iAgents, var[i]] <- as.character(val[, i])
              }
            }
          }
        }

        if (length(varNum) != 0) {

          if (length(varNum) == 1) {
            if (identical(agents@.Data[, "who"], turtles@.Data[, "who"])) {

              turtles@.Data[, var[varNum]] <- as.numeric(val[, varNum])

            } else {

              iAgents <- match(agents@.Data[, "who"], turtles@.Data[, "who"])
              turtles@.Data[iAgents, var[varNum]] <- as.numeric(val[, varNum])
            }
          } else {

            if (identical(agents, turtles)) {

              if (inherits(val[, varNum], "data.frame")) {

                turtles@.Data[, var[varNum]] <- as.matrix(val[, varNum])

              } else if (inherits(val[, varNum, drop = FALSE], "matrix")) {

                turtles@.Data[, var[varNum]] <- matrix(as.numeric(val[, varNum]),
                                                        ncol = length(varNum))
              }
            } else {

              iAgents <- match(agents@.Data[, "who"], turtles@.Data[, "who"])

              if (inherits(val[, varNum], "data.frame")) {

                turtles@.Data[iAgents, var[varNum]] <- as.matrix(val[, varNum])

              } else if (inherits(val[, varNum, drop = FALSE], "matrix")) {

                turtles@.Data[iAgents, var[varNum]] <- matrix(as.numeric(val[, varNum]),
                                                               ncol = length(varNum))

              }
            }
          }
        }
      }

      if (any(var == "who")) { # if the who numbers have been modified, check for duplicates
        if (anyDuplicated(turtles@.Data[, "who"]) != 0) {
          warning("Duplicated who numbers among the resulting turtles. Please, reassign who
                  numbers to keep them unique inside the agentset.")
        }
      }

    }

    return(turtles)
})


#' @export
#' @rdname NLset
setMethod(
  "NLset",
  signature = c(world = "worldMatrix", turtles = "missing", agents = "matrix",
                var = "missing", val = "ANY"),
  definition = function(world, agents, val) {

    if (NROW(agents) != 0) {

      if (length(val) == 1 & NROW(agents) != 1) {

        val <- rep(val, NROW(agents))
      }

      if (identical(patches(world), agents)) {

        world@.Data[] <- matrix(val, ncol = dim(world)[2], byrow = TRUE)

      } else {

        agents[is.na(agents[, 1]), 2] <- NA
        agents[is.na(agents[, 2]), 1] <- NA

        val <- val[!is.na(agents[, 1])]
        i <- agents[!is.na(agents[, 1]), 1]
        j <- agents[!is.na(agents[, 1]), 2]

        world[i, j] <- val
      }
    }

    return(world)
  })

#' @export
#' @rdname NLset
setMethod(
  "NLset",
  signature = c(world = "worldArray", turtles = "missing", agents = "matrix",
                var = "character", val = "ANY"),
  definition = function(world, agents, var, val) {

    if (NROW(agents) != 0) {

      if (length(var) == 1) {

        if (length(val) == 1 & NROW(agents) != 1) {

          val <- rep(val, NROW(agents))

        }

        if (identical(patches(world), agents)) {

          world@.Data[, , var] <- matrix(val, ncol = dim(world)[2], byrow = TRUE)

        } else {

          agents[is.na(agents[, 1]), 2] <- NA
          agents[is.na(agents[, 2]), 1] <- NA

          val <- val[!is.na(agents[, 1])]
          pxcor <- agents[!is.na(agents[, 1]), 1]
          pycor <- agents[!is.na(agents[, 1]), 2]

          matj <- pxcor - world@minPxcor + 1
          mati <- world@maxPycor - pycor + 1

          var_k <- match(var, dimnames(world@.Data)[[3]])
          world@.Data[cbind(mati, matj, var_k)] <- val

        }
      } else {

        if (identical(patches(world), agents)) {

          for(i in 1:length(var)) {
            val_i <- val[, var[i]]

            if (length(val_i) == 1) {
              val_i <- rep(val_i, NROW(agents))
            }
            world@.Data[, , var[i]] <- matrix(val_i, ncol = dim(world)[2], byrow = TRUE)

          }
        } else {

          matj <- agents[, 1] - world@minPxcor + 1
          mati <- world@maxPycor - agents[, 2] + 1

          mati[is.na(matj)] <- NA
          matj[is.na(mati)] <- NA

          if (nrow(val) == 1 & length(matj) != 1) {
            val <- val[rep(1, length(matj)), ]
          }

          if (is.matrix(val)) {
            val <- val[!is.na(matj), , drop = FALSE]
          } else {
            val <- val[!is.na(matj), ]
          }

          matj <- matj[!is.na(matj)]
          mati <- mati[!is.na(mati)]

          for(i in 1:length(var)) {
             var_k <- match(var[i], dimnames(world@.Data)[[3]])
            world@.Data[cbind(mati, matj, var_k)] <- val[, var[i]]
          }
        }
      }
    }

    return(world)
})
