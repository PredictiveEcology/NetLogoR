rename <- function(x, from, to) {
  to[match(x, from)]
}

resample <- function(x, ...) x[sample.int(length(x), ...)]

sampleWithin <- function(group) {
  tapply(1:length(group), group, resample, 1)
}

.coordsColNames <- c("xcor", "ycor")
