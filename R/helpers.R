rename <- function(x, from, to) {
  to[match(x, from)]
}

resample <- function(x, ...) x[sample.int(length(x), ...)]

sampleWithin <- function(group) {
  tapply(seq_along(group), group, resample, 1)
}

.coordsColNames <- c("xcor", "ycor")

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
