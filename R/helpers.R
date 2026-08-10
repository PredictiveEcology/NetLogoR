rename <- function(x, from, to) {
  to[match(x, from)]
}

resample <- function(x, ...) x[sample.int(length(x), ...)]

sampleWithin <- function(group) {
  tapply(seq_along(group), group, resample, 1)
}

.coordsColNames <- c("xcor", "ycor")

## A worldArray holds a single R array, so a character layer stacked alongside a
## numeric one would coerce the whole array to character. Character layers are
## therefore stored as integer codes with their levels kept in the @levels slot,
## exactly as agentMatrix does for its character columns.
##
## Note the test is is.character()/is.factor() rather than !is.numeric(): a world
## created with the default data = NA has a *logical* matrix, which must be left
## alone rather than encoded as a one-level factor.
.encodeLayer <- function(mat) {
  if (!(is.character(mat) || is.factor(mat))) {
    return(list(data = mat, levels = NULL))
  }
  fac <- factor(as.vector(mat))
  codes <- matrix(as.integer(fac), nrow = nrow(mat), ncol = ncol(mat))
  list(data = codes, levels = levels(fac))
}

## Map integer codes back to their character values. `levels = NULL` marks a
## numeric layer, which is returned untouched.
.decodeLayer <- function(values, levels) {
  if (is.null(levels)) {
    return(values)
  }
  decoded <- levels[values]
  if (!is.null(dim(values))) {
    dim(decoded) <- dim(values)
    dimnames(decoded) <- dimnames(values)
  }
  decoded
}

## Levels for `var`, or NULL when that layer is numeric. Works for any worldNLR;
## a worldMatrix has no @levels slot because its matrix can hold characters
## directly.
.layerLevels <- function(world, var) {
  if (!.hasSlot(world, "levels")) {
    return(NULL)
  }
  world@levels[[var]]
}

.hasSlot <- function(object, name) {
  name %in% methods::slotNames(class(object))
}

## TRUE if any of `var` is stored as codes.
.anyCoded <- function(world, var) {
  any(vapply(var, function(v) !is.null(.layerLevels(world, v)), logical(1)))
}

## Prepare `val` for assignment into worldArray layer `var`, returning the world
## with any newly seen character values appended to that layer's levels, and the
## integer codes to write. `union()` keeps the existing levels in place, so codes
## already stored in unassigned patches stay valid.
##
## Turning a numeric layer into a character one is only meaningful when every
## patch is assigned; otherwise the numbers still sitting in the layer would
## silently be reinterpreted as codes, so that case is rejected.
.encodeForLayer <- function(world, var, val, allPatches = FALSE) {
  lvls <- world@levels[[var]]
  valIsChar <- is.character(val) || is.factor(val)

  if (is.null(lvls)) {
    if (!valIsChar) {
      return(list(world = world, val = val))
    }

    ## `cbind(num = 1, hab = "ice")` is a character matrix, so numbers reach a
    ## numeric layer as strings. Put them back when they convert cleanly, rather
    ## than treating genuinely numeric data as new categories. Pass `val` as a
    ## data.frame to keep the column types distinct in the first place.
    asNum <- suppressWarnings(as.numeric(as.character(val)))
    if (!any(is.na(asNum) & !is.na(val))) {
      return(list(world = world, val = asNum))
    }

    if (!allPatches) {
      stop(
        "cannot assign character values to only some patches of the numeric ",
        "worldArray layer '", var, "'.\n",
        "Assign the whole layer instead, e.g. world[['", var, "']] <- aCharacterWorld."
      )
    }
    lvls <- character(0)
  }

  newLvls <- union(lvls, setdiff(unique(as.character(val)), NA_character_))
  world@levels[[var]] <- newLvls

  list(world = world, val = match(as.character(val), newLvls))
}

## Deprecation notice for the parts of the package that still rely on sp, which
## is being retired in favour of sf. `what` names the deprecated function or
## code path; `instead` spells out the sf-based way to do the same thing, so
## every one of these warnings hands the user a concrete migration path.
.deprecatedSp <- function(what, instead) {
  .Deprecated(msg = paste0(
    "'", what, "' is deprecated and will be removed in a future release of ",
    "NetLogoR, as the 'sp' package is being retired in favour of 'sf'.\n",
    instead
  ))
}

## Position of each `var` among the layers of a worldArray.
## Indexing the array directly with a layer name that does not exist would
## silently give NA, so the names are resolved once and checked here.
.layerIndices <- function(world, var) {
  layerIdx <- match(var, dimnames(world@.Data)[[3]])
  if (anyNA(layerIdx)) {
    stop("undefined layer(s) selected: ", paste(var[is.na(layerIdx)], collapse = ", "))
  }
  layerIdx
}

## Fixed-radius neighbour search used by inRadius().
## Returns a list, one element per row of `query`, of the row indices of `x`
## lying within `eps` of that query point, in increasing index order.
## `dbscan::frNN()` errors on NAs in `x`, but silently returns meaningless
## neighbours for NAs in `query`, so both are rejected up front.
.frNNindices <- function(x, query, eps) {
  if (anyNA(x) || anyNA(query)) {
    stop("missing values in coordinates not allowed")
  }
  nn <- dbscan::frNN(x = x, query = query, eps = eps, sort = FALSE)
  lapply(nn$id, sort)
}
