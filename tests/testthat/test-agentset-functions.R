test_that("NLall works", {
  w1 <- createWorld(0, 4, 0, 4, data = sample(1:5, size = 25, replace = TRUE))
  expect_identical(NLall(world = w1, agents = patches(world = w1), val = 5), FALSE)
  w2 <- w1
  w2[] <- 5
  expect_identical(NLall(world = w2, agents = patches(world = w2), val = 5), TRUE)
  w3 <- w2
  w3[0, 0] <- 4
  expect_identical(NLall(world = w2, agents = patch(world = w2, x = c(0, 1, 2, 3, 4),
                                                    y = c(4, 4, 4, 4, 4)), val = 5), TRUE)

  ws <- stackWorlds(w1, w2, w3)
  expect_identical(NLall(world = ws, agents = patches(world = ws), var = "w1", val = 5), FALSE)
  expect_identical(NLall(world = ws, agents = patches(world = ws), var = "w2", val = 5), TRUE)
  expect_identical(NLall(world = ws, agents = patch(world = ws, x = c(0, 1, 2, 3, 4),
                                                    y = c(4, 4, 4, 4, 4)),
                         var = "w3", val = 5), TRUE)

  # Turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
  expect_identical(NLall(agents = t1, var = "xcor", val = 1), TRUE)
  expect_identical(NLall(agents = t1, var = "heading", val = 2), FALSE)
  expect_identical(NLall(agents = turtle(t1, who = c(1, 2, 4)), var = "heading", val = 2), TRUE)
  expect_identical(NLall(agents = t1, var = "xcor", val = 2), FALSE)
})

test_that("NLany works", {
  w1 <- createWorld(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, x = 0, y = 0)
  p3 <- patch(world = w1, x = -1, y = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, x = c(-1, 0), y = c(-1, 0))
  expect_identical(NLany(p1), FALSE)
  expect_identical(NLany(p2), TRUE)
  expect_identical(NLany(p3), FALSE)
  expect_identical(NLany(p4), TRUE)
  expect_identical(NLany(p5), TRUE)
  expect_identical(NLany(cbind(pxcor = c(NA, NA), pycor = c(NA, NA))), FALSE)

  # Turtles
  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t2 <- noTurtles()
  t3 <- NLwith(agents = t1, var = "xcor", val = 10)
  t4 <- turtle(t1, who = 0)
  expect_identical(NLany(t1), TRUE)
  expect_identical(NLany(t2), FALSE)
  expect_identical(NLany(t3), FALSE)
  expect_identical(NLany(t4), TRUE)
})

test_that("NLcount works", {
  w1 <- createWorld(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, x = 0, y = 0)
  p3 <- patch(world = w1, x = -1, y = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, x = c(-1, 0), y = c(-1, 0))
  p6 <- patch(world = w1, x = c(-1, 0), y = c(0, 0))
  expect_equivalent(NLcount(p1), 0)
  expect_equivalent(NLcount(p2), 1)
  expect_equivalent(NLcount(p3), 0)
  expect_equivalent(NLcount(p4), 25)
  expect_equivalent(NLcount(p5), 1)
  expect_equivalent(NLcount(p6), 1)

  # Turtles
  w1 <- createWorld(0, 4, 0, 4)
  t1 <- noTurtles()
  t2 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t3 <- turtle(turtles = t2, who = c(1, 2, 3))
  expect_equivalent(NLcount(t1), 0)
  expect_equivalent(NLcount(t2), 10)
  expect_equivalent(NLcount(t3), 3)
})

test_that("sortOn works", {
  w1 <- createWorld(0, 4, 0, 4, data = 25:1)
  p1 <- sortOn(world = w1, agents = patches(world = w1))
  expect_equivalent(cbind(p1[1, 1], p1[1, 2]), patch(w1, x = 4, y = 0))
  expect_equivalent(cbind(p1[25, 1], p1[25, 2]), patch(w1, x = 0, y = 4))

  w2 <- w1
  w2[] <- 1:25
  ws <- stackWorlds(w1, w2)
  p1 <- sortOn(world = ws, agents = patch(world = ws, x = c(0, 1, 2, 3, 4), y = c(4, 4, 4, 4, 4)),
               var = "w1")
  p2 <- sortOn(world = ws, agents = patch(world = ws, x = c(0, 1, 2, 3, 4), y = c(4, 4, 4, 4, 4)),
               var = "w2")
  expect_equivalent(p1, cbind(pxcor = c(4, 3, 2, 1, 0), pycor = c(4, 4, 4, 4, 4)))
  expect_equivalent(p2, cbind(pxcor = c(0, 1, 2, 3, 4), pycor = c(4, 4, 4, 4, 4)))

  #Turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 1:5, ycor = 5:1), heading = c(4, 5, 1, 3, 2))
  t2 <- sortOn(agents = t1, var = "xcor")
  t3 <- sortOn(agents = t1, var = "ycor")
  t4 <- sortOn(agents = t1, var = "heading")
  expect_equivalent(t2@.Data[, "who"], c(0, 1, 2, 3, 4))
  expect_equivalent(t3@.Data[, "who"], c(4, 3, 2, 1, 0))
  expect_equivalent(t4@.Data[, "who"], c(2, 4, 3, 0, 1))

  t5 <- sortOn(agents = turtle(turtles = t1, who = 0), var = "heading")
  expect_equivalent(t5@.Data[, "who"], 0)
  t6 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = 1)
  t7 <- sortOn(agents = t6, var = "xcor")
  expect_equivalent(t6, t7)
})

test_that("NLwith works", {
  # Patches
  valw1 <- rep(0, 25)
  valw1[c(2, 17)] <- 1
  w1 <- createWorld(data = valw1, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- NLwith(world = w1, agents = patches(world = w1), val = 1)
  expect_equivalent(cbind(pxcor = c(1, 1), pycor = c(4, 1)), p1)
  p2 <- NLwith(agents = patches(world = w1), world = w1, val = 0)
  expect_equivalent(p2, other(agents = patches(w1), except = p1))
  p3 <- NLwith(agents = patches(world = w1), world = w1, val = c(0, 1))
  expect_equivalent(patches(world = w1), p3)
  p4 <- NLwith(agents = patches(world = w1), world = w1, val = 10)
  expect_equivalent(p4, noPatches())
  # Works with NA
  valw1[c(2, 17)] <- NA
  w1 <- createWorld(data = valw1, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1NA <- NLwith(world = w1, agents = patches(world = w1), val = NA)
  expect_equivalent(p1NA, p1)
  valw1[c(3, 17)] <- 1
  w1 <- createWorld(data = valw1, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p2NA <- NLwith(world = w1, agents = patches(world = w1), val = c(1, NA))
  expect_equivalent(nrow(p2NA), 3)
  p3NA <- NLwith(world = w1, agents = patches(world = w1), val = 0)
  expect_equivalent(nrow(p3NA), 22)

  # With worldArray
  valw2 <- rep(0, 25)
  valw2[c(3, 13)] <- 1
  w2 <- createWorld(data = valw2, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w3 <- stackWorlds(w1, w2)
  p5 <- NLwith(world = w3, agents = patches(world = w3), var = "w1", val = 1)
  expect_equivalent(p5, cbind(pxcor = c(2,1), pycor = c(4,1)))
  p6 <- NLwith(agents = patches(world = w3), world = w3, var = "w1", val = 10)
  expect_equivalent(p6, noPatches())
  p7 <- NLwith(agents = patches(world = w3), world = w3, var = "w1", val = NA)
  expect_equivalent(p7, cbind(pxcor = 1, pycor = 4))

  # Turtles
  t1 <- createTurtles(n = 8, coords = cbind(xcor = c(1, 1, 1, 2, 3, NA, 6, NA),
                                            ycor = c(2, 3, 4, 4, 5, 6, NA, NA)),
                      heading = 0, breed = c("sheep", "sheep", "wolf", "sheep",
                                             "shepherd", NA, NA, "shepherd"))
  t2 <- NLwith(agents = t1, var = "xcor", val = 1)
  expect_equivalent(t2, turtle(turtles = t1, who = c(0, 1, 2)))
  t3 <- NLwith(agents = t1, var = "ycor", val = c(2, 3))
  expect_equivalent(t3, turtle(turtles = t1, who = c(0, 1)))
  t4 <- NLwith(agents = t1, var = "heading", val = 0)
  expect_equivalent(t4, t1)
  t5 <- NLwith(agents = t1, var = "breed", val = "sheep")
  expect_equivalent(t5, turtle(turtles = t1, who = c(0, 1, 3)))
  t6 <- NLwith(agents = t1, var = "breed", val = c("sheep", "wolf"))
  expect_equivalent(t6, turtle(turtles = t1, who = c(0, 1, 2, 3)))
  t7 <- NLwith(agents = t1, var = "breed", val = "moose")
  expect_equivalent(t7, noTurtles())

  # Works with NA
  t8 <- NLwith(agents = t1, var = "breed", val = NA)
  t9 <- NLwith(agents = t1, var = "xcor", val = NA)
  t10 <- NLwith(agents = t1, var = "ycor", val = NA)
  t11 <- NLwith(agents = t1, var = "ycor", val = c(NA, 2))
  t12 <- NLwith(agents = t1, var = "breed", val = c("sheep", NA))
})

test_that("withMax works", {
  w1 <- createWorld(0, 4, 0, 4)
  expect_error(withMax(agents = patches(world = w1), world = w1))
  w1[] <- 1
  expect_equivalent(withMax(agents = patches(w1), world = w1), patches(w1))
  w1[] <- runif(25)
  w1[1, c(1, 4)] <- c(2, 2)
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_equivalent(pMax, patch(w1, x = c(1, 1), y = c(4, 1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2, c(2, 4)] <- c(2, 2)
  ws <- stackWorlds(w1, w2)
  pMaxw1 <- withMax(world = ws, agents = patches(world = ws), var = "w1")
  pMaxw2 <- withMax(world = ws, agents = patches(world = ws), var = "w2")
  expect_equivalent(pMaxw1, patch(ws, x = c(1, 1), y = c(4, 1)))
  expect_equivalent(pMaxw2, patch(ws, x = c(2, 2), y = c(4, 2)))
  ws[, , 1] <- NA
  expect_error(withMax(agents = patches(world = ws), world = ws, var = "w1"))

  w1[1, 1] <- 0
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_equivalent(pMax, patch(w1, x = 1, y = 4))

  #Turtles
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1),
                      heading = c(1, 2, 3, 4, 4, 2, 3, 4, 4, 3))
  maxXcor <- withMax(agents = t1, var = "xcor")
  expect_equivalent(maxXcor, turtle(t1, who = 9))
  maxHeading <- withMax(agents = t1, var = "heading")
  expect_equivalent(maxHeading, turtle(t1, who = c(3, 4, 7, 8)))
  expect_error(withMax(agents = t1, var = "prevX"))
})

test_that("withMin works", {
  w1 <- createWorld(0, 4, 0, 4)
  expect_error(withMin(agents = patches(world = w1), world = w1))
  w1[] <- 1
  expect_equivalent(withMin(agents = patches(w1), world = w1), patches(w1))
  w1[] <- runif(25)
  w1[1, c(1, 4)] <- c(-1, -1)
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_equivalent(pMin, patch(w1, x = c(1, 1), y = c(4, 1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2, c(2, 4)] <- c(-1, -1)
  ws <- stackWorlds(w1, w2)
  pMinw1 <- withMin(world = ws, agents = patches(world = ws), var = "w1")
  pMinw2 <- withMin(world = ws, agents = patches(world = ws), var = "w2")
  expect_equivalent(pMinw1, patch(ws, x = c(1, 1), y = c(4, 1)))
  expect_equivalent(pMinw2, patch(ws, x = c(2, 2), y = c(4, 2)))
  ws[] <- NA
  expect_error(withMin(agents = patches(world = ws), world = ws, var = "w1"))


  w1[1, 1] <- 0
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_equivalent(pMin, patch(w1, x = 1, y = 4))

  # Turtles
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 10:1, ycor = 10:1),
                      heading = c(1, 2, 3, 0, 0, 2, 3, 0, 0, 3))
  maxXcor <- withMin(agents = t1, var = "xcor")
  expect_equivalent(maxXcor, turtle(t1, who = 9))
  maxHeading <- withMin(agents = t1, var = "heading")
  expect_equivalent(maxHeading, turtle(t1, who = c(3, 4, 7, 8)))
  expect_error(withMin(agents = t1, var = "prevX"))
})

test_that("maxOneOf works", {
  w1 <- createWorld(0, 4, 0, 4, data = sample(1:5, size = 25, replace = TRUE))
  allpMax <- withMax(world = w1, agents = patches(world = w1))
  onepMax <- maxOneOf(world = w1, agents = patches(world = w1))
  compare <- cbind(a = as.numeric(allpMax[, 1]) == as.numeric(onepMax[1]),
                   b = as.numeric(allpMax[, 2]) == as.numeric(onepMax[2]))
  rowTRUE <- compare[compare[, 1] == TRUE & compare[, 2] == TRUE, , drop = FALSE]
  expect_equivalent(nrow(rowTRUE), 1)

  w2 <- w1
  w2[] <- 1 / w1[]
  ws <- stackWorlds(w1, w2)
  onepMax1 <- maxOneOf(world = ws, agents = patches(world = w1), var = "w1")
  onepMax2 <- maxOneOf(world = ws, agents = patches(world = w1), var = "w2")
  compare1 <- cbind(a = as.numeric(allpMax[, 1]) == as.numeric(onepMax1[1]),
                    b = as.numeric(allpMax[, 2]) == as.numeric(onepMax1[2]))
  rowTRUE <- compare1[compare1[, 1] == TRUE & compare1[, 2] == TRUE, , drop = FALSE]
  expect_equivalent(nrow(rowTRUE), 1)
  compare2 <- onepMax2 == onepMax1
  expect_lt(length(compare2[compare2 == TRUE]), 2)

  # Turtles
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1),
                      heading = c(1, 2, 3, 4, 4, 2, 3, 4, 4, 3))
  maxXcor1 <- withMax(agents = t1, var = "xcor")
  maxXcor2 <- maxOneOf(agents = t1, var = "xcor")
  expect_equivalent(maxXcor1, maxXcor2)
  maxHeading1 <- withMax(agents = t1, var = "heading")
  maxHeading2 <- maxOneOf(agents = t1, var = "heading")
  expect_equivalent(NLcount(maxHeading2), 1)
  maxH12 <- merge(maxHeading1@.Data, maxHeading2@.Data)
  expect_equivalent(nrow(maxH12), 1)
  expect_error(maxOneOf(agents = t1, var = "prevX"))
})

test_that("minOneOf works", {
  w1 <- createWorld(0, 4, 0, 4, data = sample(1:5, size = 25, replace = TRUE))
  allpMin <- withMin(world = w1, agents = patches(world = w1))
  onepMin <- minOneOf(world = w1, agents = patches(world = w1))
  compare <- cbind(a = as.numeric(allpMin[, 1]) == as.numeric(onepMin[1]),
                   b = as.numeric(allpMin[, 2]) == as.numeric(onepMin[2]))
  rowTRUE <- compare[compare[, 1] == TRUE & compare[, 2] == TRUE, , drop = FALSE]
  expect_equivalent(nrow(rowTRUE), 1)

  w2 <- w1
  w2[] <- 1 / w1[]
  ws <- stackWorlds(w1, w2)
  onepMin1 <- minOneOf(world = ws, agents = patches(world = w1), var = "w1")
  onepMin2 <- minOneOf(world = ws, agents = patches(world = w1), var = "w2")
  compare1 <- cbind(a = as.numeric(allpMin[, 1]) == as.numeric(onepMin1[1]),
                    b = as.numeric(allpMin[, 2]) == as.numeric(onepMin1[2]))
  rowTRUE <- compare1[compare1[, 1] == TRUE & compare1[, 2] == TRUE, , drop = FALSE]
  expect_equivalent(nrow(rowTRUE), 1)
  compare2 <- onepMin2 == onepMin1
  expect_lt(length(compare2[compare2 == TRUE]), 2)

  # Turtles
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1),
                      heading = c(1, 2, 3, 4, 4, 2, 3, 4, 4, 3))
  minXcor1 <- withMin(agents = t1, var = "xcor")
  minXcor2 <- minOneOf(agents = t1, var = "xcor")
  expect_equivalent(minXcor1, minXcor2)
  minHeading1 <- withMin(agents = t1, var = "heading")
  minHeading2 <- minOneOf(agents = t1, var = "heading")
  expect_equivalent(NLcount(minHeading2), 1)
  minH12 <- merge(minHeading1@.Data, minHeading2@.Data)
  expect_equivalent(nrow(minH12), 1)
  expect_error(minOneOf(agents = t1, var = "prevX"))
})

test_that("isNLclass works", {
  w1 <- createWorld(0, 4, 0, 4)
  t1 <- createTurtles(n = 10, randomXYcor(w1, n = 10))
  expect_identical(isNLclass(agents = patch(w1, x = 0, y = 0), class = "patch"), TRUE)
  expect_identical(isNLclass(agents = patches(w1), class = "patchset"), TRUE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "turtle"), TRUE)
  expect_identical(isNLclass(agents = t1, class = "turtleset"), TRUE)
  expect_identical(isNLclass(agents = patch(w1, x = 0, y = 0), class = "agent"), TRUE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "agent"), TRUE)
  expect_identical(isNLclass(agents = patches(w1), class = "agentset"), TRUE)
  expect_identical(isNLclass(agents = t1, class = "agentset"), TRUE)

  expect_identical(isNLclass(agents = patch(w1, x = c(0, 2), y = c(1, 0)), class = "patch"),
                   FALSE)
  expect_identical(isNLclass(agents = noPatches(), class = "patchset"), FALSE)
  expect_identical(isNLclass(agents = turtle(t1, who = c(0, 2)), class = "turtle"), FALSE)
  expect_identical(isNLclass(agents = noTurtles(), class = "turtleset"), FALSE)
  expect_identical(isNLclass(agents = cbind(xcor = 2, ycor = 3), class = "agent"), FALSE)
  expect_identical(isNLclass(agents = patches(w1), class = "agent"), FALSE)
  expect_identical(isNLclass(agents = patch(w1, x = 0, y = 0), class = "agentset"), FALSE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "agentset"), FALSE)

  t2 <- turtlesOwn(turtles = t1, tVar = "age")
  expect_identical(isNLclass(agents = t2, class = "agentset"), TRUE)
  expect_identical(isNLclass(agents = t2, class = "turtleset"), TRUE)

  t1@.Data <- t1@.Data[, c(1, 2, 4:8)]
  expect_identical(isNLclass(agents = t1, class = "turtleset"), FALSE)
})

test_that("nOf works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- nOf(agents = patches(world = w1), n = 1)
  expect_equivalent(nrow(p1), 1)
  p11 <- as.matrix(merge(p1, patches(world = w1)))
  expect_equivalent(p1, p11)
  p2 <- nOf(agents = patches(w1), n = nrow(patches(w1)))
  expect_equivalent(nrow(p2), nrow(patches(w1)))
  expect_equivalent(p2, patches(w1))
  p3 <- nOf(agents = patches(w1), n = 10)
  expect_identical(nrow(p3), nrow(unique(p3)))

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t2 <- nOf(agents = t1, n = 2)
  expect_equivalent(NLcount(t2), 2)
  t2data <- merge(t2@.Data, t1@.Data)
  expect_equivalent(nrow(t2data), 2)
  expect_equivalent(nrow(unique(t2data)), 2)
  t3 <- nOf(agents = t1, n = 10)
  expect_identical(nrow(merge(t1@.Data, t3@.Data)), nrow(t1@.Data))

  # With matrix ncol = 3
  n4 <- neighbors(world = w1, agents = t1, nNeighbors = 8)
  p4 <- nOf(agents = n4, n = 1)
  expect_equivalent(nrow(p4), NLcount(t1))
  p5 <- nOf(agents = n4, n = 2)
  expect_equivalent(nrow(p5), NLcount(t1) * 2)

  # bugfix for sample vs resample -- this fails with code prior to NetLogoR v 0.3.9.9001
  wh <- tapply(1:NROW(n4), n4[, "id"], function(x) x[1]) # just take 1 per agent id
  n5 <- n4[wh,]
  p5 <- nOf(n5, 1)
  expect_true(identical(n5[, c("pxcor", "pycor")], p5))

  # With matrix ncol = 2 "whoTurtles" and "id"
  t4 <- turtlesOn(world = w1, turtles = t1, agents = patches(w1), simplify = FALSE)
  expect_error(nOf(agents = t4, n = 2))
  t5 <- nOf(agents = t4, n = 1)
  expect_equivalent(nrow(merge(as.data.frame(t5), t4, by.x = "t5", by.y = "whoTurtles")),
                    length(unique(t4[, "id"])))

})

test_that("oneOf works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- oneOf(agents = patches(world = w1))
  expect_equivalent(nrow(p1), 1)
  p11 <- as.matrix(merge(p1, patches(world = w1)))
  expect_equivalent(p1, p11)
  p2 <- oneOf(agents = patch(w1, x = 0, y = 0))
  p3 <- nOf(agents = patch(w1, x = 0, y = 0), n = 1)
  expect_identical(p2, p3)

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t2 <- oneOf(agents = t1)
  expect_equivalent(NLcount(t2), 1)
  t2data <- merge(t2@.Data, t1@.Data)
  expect_equivalent(nrow(t2data), 1)
  t3 <- nOf(agents = turtle(t1, who = 0), n = 1)
  t4 <- oneOf(agents = turtle(t1, who = 0))
  expect_identical(t3, t4)

  # With matrix ncol = 3
  n4 <- neighbors(world = w1, agents = t1, nNeighbors = 4)
  p4 <- oneOf(n4)
  expect_equivalent(nrow(p4), NLcount(t1))

  # bugfix for sample vs resample
  wh <- tapply(1:NROW(n4), n4[, "id"], function(x) x[1]) # just take 1 per agent id
  n5 <- n4[wh,]
  p5 <- oneOf(n5) # n5 only has 1 per id, so oneOf should return same
  expect_true(identical(p5, n5[, c("pxcor", "pycor")]))
  expect_equivalent(nrow(p4), NLcount(t1))

  # With matrix ncol = 2 "whoTurtles" and "id"
  t4 <- turtlesOn(world = w1, turtles = t1, agents = patches(w1), simplify = FALSE)
  t5 <- oneOf(agents = t4)
  expect_equivalent(length(t5), length(unique(t4[, "id"])))
  expect_equivalent(nrow(merge(as.data.frame(t5), t4, by.x = "t5", by.y = "whoTurtles")),
                    length(unique(t4[, "id"])))
})

test_that("maxNof works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 25:1)
  p1 <- maxNof(agents = patches(world = w1), n = 5, world = w1)
  expect_equivalent(p1, PxcorPycorFromCell(world = w1, 1:5))
  p2 <- maxNof(agents = patches(world = w1), n = 1, world = w1)
  expect_equivalent(p2, PxcorPycorFromCell(world = w1, 1))
  p3 <- maxNof(agents = patches(world = w1), n = length(w1), world = w1)
  expect_equivalent(nrow(p3), 25)
  expect_equivalent(p3, patches(w1))
  w1[] <- 25
  p4 <- maxNof(agents = patches(world = w1), n = 5, world = w1)
  expect_equivalent(nrow(p4), 5)
  p5 <- maxNof(agents = patches(world = w1), n = 0, world = w1)
  expect_equivalent(nrow(p5), 0)
  expect_equivalent(p5, noPatches())

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 1:10)
  t2 <- maxNof(agents = t1, n = 5, var = "heading")
  expect_equivalent(t2, turtle(t1, who = c(5, 6, 7, 8, 9)))
  t3 <- maxNof(agents = t1, n = 1, var = "heading")
  expect_equivalent(t3, turtle(t1, who = 9))
  t4 <- maxNof(agents = t1, n = 10, var = "heading")
  expect_equivalent(t4, t1)
  t5 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10)
  t6 <- maxNof(agents = t5, n = 5, var = "heading")
  expect_equivalent(NLcount(t6), 5)
  t7 <- maxNof(agents = t1, n = 0, var = "heading")
  expect_equivalent(t7, noTurtles())
  expect_equivalent(NLcount(t7), 0)

  w2 <- w1
  w2[] <- 25:1
  ws <- stackWorlds(w1, w2)
  p1 <- maxNof(agents = patches(world = ws), n = 5, world = ws, var = "w2")
  expect_equivalent(p1, PxcorPycorFromCell(world = w2, 1:5))
  p2 <- maxNof(agents = patches(world = ws), n = 1, world = ws, var = "w2")
  expect_equivalent(p2, PxcorPycorFromCell(world = ws, 1))
  p3 <- maxNof(agents = patches(world = ws), n = length(w1), world = ws, var = "w2")
  expect_equivalent(nrow(p3), 25)
  expect_equivalent(p3, patches(w1))
  p4 <- maxNof(agents = patches(world = ws), n = 5, world = ws, var = "w1")
  expect_equivalent(nrow(p4), 5)
  p5 <- maxNof(agents = patches(world = ws), n = 0, world = ws, var = "w1")
  expect_equivalent(nrow(p5), 0)
  expect_equivalent(p5, noPatches())
})

test_that("minNof works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  p1 <- minNof(agents = patches(world = w1), n = 5, world = w1)
  expect_equivalent(p1, PxcorPycorFromCell(world = w1, 1:5))
  p2 <- minNof(agents = patches(world = w1), n = 1, world = w1)
  expect_equivalent(p2, PxcorPycorFromCell(world = w1, 1))
  p3 <- minNof(agents = patches(world = w1), n = length(w1), world = w1)
  expect_equivalent(nrow(p3), 25)
  expect_equivalent(p3, patches(w1))
  w1[] <- 25
  p4 <- minNof(agents = patches(world = w1), n = 5, world = w1)
  expect_equivalent(nrow(p4), 5)
  p5 <- minNof(agents = patches(world = w1), n = 0, world = w1)
  expect_equivalent(nrow(p5), 0)
  expect_equivalent(p5, noPatches())

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10:1)
  t2 <- minNof(agents = t1, n = 5, var = "heading")
  expect_equivalent(t2, turtle(t1, who = c(5, 6, 7, 8, 9)))
  t3 <- minNof(agents = t1, n = 1, var = "heading")
  expect_equivalent(t3, turtle(t1, who = 9))
  t4 <- minNof(agents = t1, n = 10, var = "heading")
  expect_equivalent(t4, t1)
  t5 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10)
  t6 <- minNof(agents = t5, n = 5, var = "heading")
  expect_equivalent(NLcount(t6), 5)
  t7 <- minNof(agents = t1, n = 0, var = "heading")
  expect_equivalent(t7, noTurtles())
  expect_equivalent(NLcount(t7), 0)

  w2 <- w1
  w2[] <- 1:25
  ws <- stackWorlds(w1, w2)
  p1 <- minNof(agents = patches(world = ws), n = 5, world = ws, var = "w2")
  expect_equivalent(p1, PxcorPycorFromCell(world = w2, 1:5))
  p2 <- minNof(agents = patches(world = ws), n = 1, world = ws, var = "w2")
  expect_equivalent(p2, PxcorPycorFromCell(world = ws, 1))
  p3 <- minNof(agents = patches(world = ws), n = length(w1), world = ws, var = "w2")
  expect_equivalent(nrow(p3), 25)
  expect_equivalent(p3, patches(w1))
  p4 <- minNof(agents = patches(world = ws), n = 5, world = ws, var = "w1")
  expect_equivalent(nrow(p4), 5)
  p5 <- minNof(agents = patches(world = ws), n = 0, world = ws, var = "w1")
  expect_equivalent(nrow(p5), 0)
  expect_equivalent(p5, noPatches())
})

test_that("inRadius works", {
  # Patches to patches
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patches(w1), world = w1)
  expect_error(inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = patches(w1),
                        torus = TRUE))
  expect_equivalent(p1[p1[, "id"] == 1, c("pxcor", "pycor")], cbind(pxcor = c(0, 0, 1, 0, 1, 2),
                                                                    pycor = c(2, 1, 1, 0, 0, 0)))
  p2 <- inRadius(agents = patches(w1), radius = 2, agents2 = patch(w1, 0, 0), world = w1)
  expect_equivalent(p2[p2[, "id"] == 11, c("pxcor", "pycor"), drop = FALSE],
                    cbind(pxcor = 0, pycor = 0))
  p3 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patch(w1, 0, 0), world = w1)
  expect_equivalent(p3[p3[, "id"] == 1, c("pxcor", "pycor"), drop = FALSE], patch(w1, 0, 0))
  expect_equivalent(nrow(p3), 1)
  p4 <- inRadius(agents = patches(w1), radius = 10, agents2 = patches(w1), world = w1)
  expect_equivalent(p4[p4[, "id"] == 1, c("pxcor", "pycor")], patches(w1))
  expect_equivalent(length(unique(p4[, "id"])), nrow(patches(w1)))
  expect_equivalent(p4[p4[, "id"] == 1, c("pxcor", "pycor")], p4[p4[, "id"] == 2,
                                                                 c("pxcor", "pycor")])

  # Patches to turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4))
  t2 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 1, agents2 = t1, world = w1)
  expect_equivalent(t2[t2[, "id"] == 1, "who"], 0)
  t3 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1, world = w1)
  expect_equivalent(t3[t3[, "id"] == 1, "who"], c(0, 1))
  expect_error(inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1, torus = TRUE))
  t4 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1, world = w1,
                 torus = TRUE)
  expect_equivalent(t4[t4[, "id"] == 1, "who"], c(0, 1, 4))
  t5 <- inRadius(agents = patches(w1), radius = 1, agents2 = t1, world = w1)
  expect_equivalent(t5[t5[, "id"] == 5, "who"], 4)
  expect_equivalent(t5[t5[, "id"] == 9, "who"], 3)
  t6 <- inRadius(agents = patches(w1), radius = 10, agents2 = t1, world = w1)
  expect_equivalent(t6[t6[, "id"] == 1, "who"], t1@.Data[, "who"])
  expect_equivalent(t6[t6[, "id"] == 25, "who"], t1@.Data[, "who"])
  t7 <- inRadius(agents = patches(w1), radius = 10, agents2 = t1, world = w1, torus = TRUE)
  expect_equivalent(t7[t7[, "id"] == 1, "who"], t1@.Data[, "who"])
  expect_equivalent(t7[t7[, "id"] == 25, "who"], t1@.Data[, "who"])

  # Turtles to patches
  p5 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = patches(w1), world = w1)
  expect_equivalent(p5, p1)
  p6 <- inRadius(agents = t1, radius = 0.5, agents2 = patches(w1), world = w1)
  expect_equivalent(length(unique(p6[, "id"])), NLcount(t1))
  expect_equivalent(p6[p6[, "id"] == 1, c("pxcor", "pycor"), drop = FALSE],
                    as.matrix(inspect(t1, 0)[, c("xcor", "ycor"), drop = FALSE]))
  p7 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patches(w1), world = w1,
                 torus = TRUE)
  expect_equivalent(nrow(merge(p7[p7[, "id"] == 1, c("pxcor", "pycor")],
                               cbind(pxcor = c(0, 0, 0, 1, 4), pycor = c(4, 1, 0, 0, 0)))),
                    nrow(p7))
  p8 <- inRadius(agents = turtle(t1, c(0, 4)), radius = 1, agents2 = patches(w1), world = w1,
                 torus = TRUE)
  expect_equivalent(nrow(merge(p8[p8[, "id"] == 1, c("pxcor", "pycor")],
                               cbind(pxcor = c(0, 0, 0, 1, 4), pycor = c(4, 1, 0, 0, 0)))),
                    nrow(p8[p8[, "id"] == 1, ]))
  expect_equivalent(nrow(merge(p8[p8[, "id"] == 2, c("pxcor", "pycor")],
                               cbind(pxcor = c(0, 3, 4, 4, 4), pycor = c(4, 4, 4, 3, 0)))),
                    nrow(p8[p8[, "id"] == 2, ]))
  p9 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patch(w1, 4, 4), world = w1)
  expect_equivalent(NROW(p9), 0)

  # Turtles to turtles
  t8 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = t1, world = w1)
  expect_equivalent(t8[t8[, "id"] == 1, "who"], 0)
  t9 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1, world = w1)
  expect_equivalent(t9[t9[, "id"] == 1, "who"], c(0, 1))
  t10 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1, world = w1, torus = TRUE)
  expect_equivalent(t10[t10[, "id"] == 1, "who"], c(0, 1, 4))
  t11 <- inRadius(agents = t1, radius = 10, agents2 = t1, world = w1)
  expect_equivalent(t11[t11[, "id"] == 1, "who"], c(0, 1, 2, 3, 4))

  # Works without the world provided when torus = FALSE
  p1 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patches(w1))
  expect_equivalent(p1[p1[, "id"] == 1, c("pxcor", "pycor")], cbind(pxcor = c(0, 0, 1, 0, 1, 2),
                                                                    pycor = c(2, 1, 1, 0, 0, 0)))
  p2 <- inRadius(agents = patches(w1), radius = 2, agents2 = patch(w1, 0, 0))
  expect_equivalent(p2[p2[, "id"] == 11, c("pxcor", "pycor"), drop = FALSE],
                    cbind(pxcor = 0, pycor = 0))
  p3 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patch(w1, 0, 0))
  expect_equivalent(p3[p3[, "id"] == 1, c("pxcor", "pycor"), drop = FALSE], patch(w1, 0, 0))
  expect_equivalent(nrow(p3), 1)
  p4 <- inRadius(agents = patches(w1), radius = 10, agents2 = patches(w1))
  expect_equivalent(p4[p4[, "id"] == 1, c("pxcor", "pycor")], patches(w1))
  expect_equivalent(length(unique(p4[, "id"])), nrow(patches(w1)))
  expect_equivalent(p4[p4[, "id"] == 1, c("pxcor", "pycor")], p4[p4[, "id"] == 2,
                                                                 c("pxcor", "pycor")])
  t2 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 1, agents2 = t1)
  expect_equivalent(t2[t2[, "id"] == 1, "who"], 0)
  t3 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1)
  expect_equivalent(t3[t3[, "id"] == 1, "who"], c(0, 1))
  t5 <- inRadius(agents = patches(w1), radius = 1, agents2 = t1)
  expect_equivalent(t5[t5[, "id"] == 5, "who"], 4)
  expect_equivalent(t5[t5[, "id"] == 9, "who"], 3)
  t6 <- inRadius(agents = patches(w1), radius = 10, agents2 = t1)
  expect_equivalent(t6[t6[, "id"] == 1, "who"], t1@.Data[, "who"])
  expect_equivalent(t6[t6[, "id"] == 25, "who"], t1@.Data[, "who"])
  p5 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = patches(w1))
  expect_equivalent(p5, p1)
  p6 <- inRadius(agents = t1, radius = 0.5, agents2 = patches(w1))
  expect_equivalent(length(unique(p6[, "id"])), NLcount(t1))
  expect_equivalent(p6[p6[, "id"] == 1, c("pxcor", "pycor"), drop = FALSE],
                    as.matrix(inspect(t1, 0)[, c("xcor", "ycor"), drop = FALSE]))
  p9 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patch(w1, 4, 4))
  expect_equivalent(NROW(p9), 0)
  t8 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = t1)
  expect_equivalent(t8[t8[, "id"] == 1, "who"], 0)
  t9 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1)
  expect_equivalent(t9[t9[, "id"] == 1, "who"], c(0, 1))
  t11 <- inRadius(agents = t1, radius = 10, agents2 = t1, world = w1)
  expect_equivalent(t11[t11[, "id"] == 1, "who"], c(0, 1, 2, 3, 4))
})

test_that("inCone works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4),
                      heading = c(0, 90, 180, 270, 0))

  # Turtles to patches
  t2 <- inCone(turtles = t1, radius = 1, angle = 5, agents = patches(w1))
  expect_equivalent(length(unique(t2[, "id"])), 5)
  expect_equivalent(nrow(t2[t2[, "id"] == 5, , drop = FALSE]), 1)
  t3 <- inCone(turtles = t1, radius = 1, angle = 5, agents = patches(w1), world = w1,
               torus = TRUE)
  expect_equivalent(length(unique(t3[, "id"])), 5)
  expect_equivalent(nrow(t3[t3[, "id"] == 5, , drop = FALSE]), 2)
  t4 <- inCone(turtles = turtle(t1, who = 0), radius = 1, angle = 181, agents = patches(w1))
  expect_equivalent(t4[t4[, "id"] == 1, c("pxcor", "pycor")],
                   patch(w1, x = c(0, 0, 1), y = c(1, 0, 0)))
  t5 <- inCone(turtles = turtle(t1, who = 0), radius = 1, angle = 181, agents = patches(w1),
               world = w1, torus = TRUE)
  expect_equivalent(nrow(merge(t5[t5[, "id"] == 1, c("pxcor", "pycor")],
                               patch(w1, x = c(0, 0, 1, 4), y = c(1, 0, 0, 0)))), 4)
  t6 <- inCone(turtles = turtle(t1, who = 1), radius = 1, angle = 181,
               agents = patch(w1, x = 0, y = 0), world = w1, torus = FALSE)
  expect_equivalent(nrow(t6), 0)
  t7 <- inCone(turtles = turtle(t1, who = c(1, 2)), radius = 1, angle = 181,
               agents = patch(w1, x = 2, y = 2))
  expect_equivalent(nrow(t7[t7[, "id"] == 1, ]), 0)
  expect_equivalent(t7[t7[, "id"] == 2, c("pxcor", "pycor"), drop = FALSE],
                    patch(w1, x = 2, y = 2))

  # Turtles to turtles
  t8 <- inCone(turtles = t1, radius = 3, angle = 360, agents = t1)
  expect_equivalent(t8[t8[, "id"] == 3, "who"], t1@.Data[, "who"])
  expect_equivalent(t8[t8[, "id"] == 1, "who"], c(0, 1, 2))
  t9 <- inCone(turtles = t1, radius = 3, angle = 360, agents = t1, world = w1, torus = TRUE)
  expect_equivalent(t9[t9[, "id"] == 3, "who"], t1@.Data[, "who"])
  expect_equivalent(t9[t9[, "id"] == 1, "who"], t1@.Data[, "who"])
  t10 <- inCone(turtles = turtle(t1, 1), radius = 1, angle = 360, agents = turtle(t1, 4))
  expect_equivalent(nrow(t10), 0)
  t11 <- inCone(turtles = turtle(t1, 1), radius = 3, angle = 180, agents = t1)
  expect_equivalent(t11[t11[, "id"] == 1, "who"], c(1, 2, 3))
  t12 <- inCone(turtles = turtle(t1, 4), radius = 3, angle = 180, agents = t1)
  expect_equivalent(t12[t12[, "id"] == 1, "who"], 4)
  t13 <- inCone(turtles = turtle(t1, 4), radius = 3, angle = 180, agents = t1, world = w1,
                torus = TRUE)
  expect_equivalent(t13[t13[, "id"] == 1, "who"], c(0, 1, 4))
})

test_that("NLset works", {
  # Set work with patches
  w1 <- createWorld(data = 1:25, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1 <- NLset(world = w1, agents = patches(w1), val = 0)
  expect_equivalent(as.numeric(t(w1)), rep(0, length(w1)))
  w1 <- NLset(world = w1, agents = patches(w1), val = 1:25)
  expect_equivalent(as.numeric(t(w1)), 1:25)
  w1 <- NLset(world = w1, agents = patch(w1, 0, 0), val = 100)
  expect_equivalent(of(world = w1, agents = patch(w1, 0, 0)), 100)
  w1 <- NLset(world = w1, agents = patch(w1, c(-1, 0), c(-1, 0)), val = -10)
  expect_equivalent(of(world = w1, agents = patch(w1, 0, 0)), -10)
  w11 <- NLset(world = w1, agents = patch(w1, c(-1, -2), c(-1, 0)), val = -20)
  expect_equivalent(w1, w11)

  w1 <- NLset(world = w1, agents = patches(w1), val = 1:25)
  w2 <- createWorld(data = 25:1, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w3 <- stackWorlds(w1, w2)
  w3 <- NLset(world = w3, agents = patches(w3), var = "w1", val = 0)
  expect_equivalent(as.numeric(t(w3@.Data[, , "w1"])), rep(0, length(w1)))
  w3 <- NLset(world = w3, agents = patches(w3), var = "w1", val = 1:25)
  expect_equivalent(as.numeric(t(w3@.Data[, , "w1"])), 1:25)
  w3 <- NLset(world = w3, agents = patch(w3, 0, 0), var = "w1", val = 100)
  expect_equivalent(of(world = w3, var = "w1", agents = patch(w3, 0, 0)), 100)

  # With multiple values
  w3 <- NLset(world = w3, agents = patch(w3, 0, 0), var = c("w1", "w2"),
              val = cbind(w1 = 0, w2 = 100))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, 0, 0)),
                    cbind(w1 = 0, w2 = 100))
  w3 <- NLset(world = w3, agents = patch(w3, 0, 0), var = c("w2", "w1"),
              val = cbind(w2 = 0, w1 = 100))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, 0, 0)),
                    cbind(w1 = 100, w2 = 0))
  w3 <- NLset(world = w3, agents = patches(w3), var = c("w1", "w2"),
              val = cbind(w1 = 101:125, w2 = 125:101))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)),
                    cbind(w1 = 101:125, w2 = 125:101))
  w3 <- NLset(world = w3, agents = patches(w3), var = c("w2", "w1"),
              val = cbind(w2 = 101:125, w1 = 125:101))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)),
                    cbind(w1 = 125:101, w2 = 101:125))
  w3 <- NLset(world = w3, agents = patches(w3), var = c("w1", "w2"),
              val = cbind(w1 = 101, w2 = 125))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)),
                    cbind(w1 = rep(101, 25), w2 = rep(125, 25)))
  w3 <- NLset(world = w3, agents = patches(w3), var = c("w2", "w1"),
              val = cbind(w2 = 125, w1 = 101))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)),
                    cbind(w1 = rep(101, 25), w2 = rep(125, 25)))
  w3 <- NLset(world = w3, agents = patch(w3, c(-1, 0), c(-1, 0)), var = c("w1", "w2"),
              val = cbind(w1 = 0, w2 = 1))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, 0, 0)),
                    cbind(w1 = 0, w2 = 1))
  valW3 <- of(world = w3, var = c("w1", "w2"), agents = patches(w3))
  expect_equivalent(length(valW3[is.na(valW3[, 1]), 1]), 0)
  expect_equivalent(length(valW3[is.na(valW3[, 2]), 2]), 0)
  w3 <- NLset(world = w3, agents = patch(w3, c(0, 1), c(0, 1)), var = c("w1", "w2"),
              val = cbind(w1 = 10, w2 = 11))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, c(0, 1), c(0, 1))),
                    cbind(w1 = c(10, 10), w2 = c(11, 11)))
  w3 <- NLset(world = w3, agents = patch(w3, c(0, 1), c(0, 1)), var = "w1", val = c(100, 110))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, c(0, 1), c(0, 1))),
                    cbind(w1 = c(100, 110), w2 = c(11, 11)))
  w3 <- NLset(world = w3, agents = patch(w3, c(0, 1), c(0, 1)), var = c("w1", "w2"),
              val = cbind.data.frame(w1 = 10, w2 = 11))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, c(0, 1), c(0, 1))),
                    cbind(w1 = c(10, 10), w2 = c(11, 11)))

  # With NAs
  w4 <- NLset(world = w3, agents = cbind(pxcor = c(NA, 1, NA), pycor = c(NA, 2, NA)),
              var = c("w2", "w1"), val = cbind(w2 = c(1, 2, 3), w1 = c(1, 2, 3)))
  w5 <- NLset(world = w3, agents = cbind(pxcor = 1, pycor = 2), var = c("w2", "w1"),
              val = cbind(w2 = 2, w1 = 2))
  expect_equivalent(w4, w5)
  w6 <- NLset(world = w3, agents = cbind(pxcor = 1, pycor = 2), var = c("w2", "w1"),
              val = cbind(w2 = NA, w1 = 2))
  expect_equivalent(of(world = w6, var = "w2", agents = patch(w6, 1, 2)), as.numeric(NA))

  # Turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4),
                      heading = c(0, 90, 180, 270, 0))
  t2 <- NLset(turtles = t1, agents = t1, var = "heading", val = 0)
  expect_equivalent(t2@.Data[,  "heading"], rep(0, NLcount(t2)))
  t3 <- NLset(turtles = t1, agents = turtle(t1, 0), var = "xcor", val = 3)
  expect_equivalent(t3@.Data[, "xcor"], c(3, 1, 2, 3, 4))
  t4 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1)), var = "xcor", val = 3)
  expect_equivalent(t4@.Data[, "xcor"], c(3, 3, 2, 3, 4))

  # With multiple values
  t5 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1)), var = c("xcor", "heading"),
              val = cbind(xcor = c(100, 100), heading = c(33, 66)))
  expect_equivalent(t5@.Data[, "xcor"], c(100, 100, 2, 3, 4))
  expect_equivalent(t5@.Data[, "heading"], c(33, 66, 180, 270, 0))
  t6 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1)), var = c("heading", "xcor"),
              val = cbind(heading = c(33, 66), xcor = c(100, 100)))
  expect_identical(t5, t6)

  # Warning with who numbers
  expect_warning(NLset(turtles = t1, agents = turtle(t1, 1), var = "who", val = 0))
  expect_warning(NLset(turtles = t1, agents = turtle(t1, 1), var = c("who", "heading"),
                       val = cbind(who = 0, heading = 0)))
  t7 <- NLset(turtles = t1, agents = turtle(t1, 1), var = "who", val = 100)
  t8 <- NLset(turtles = t1, agents = turtle(t1, 1), var = c("who", "heading"),
              val = cbind(who = 100, heading = 0))

  # Non numeric value
  t9 <- NLset(turtles = t1, agents = turtle(t1, 0), var = "breed", val = "dog")
  expect_equivalent(of(agents = t9, var = "breed"), c("dog", rep("turtle", 4)))
  expect_equivalent(t9@levels$breed, c("turtle", "dog"))
  t92 <- NLset(turtles = t1, agents = t1, var = "breed", val = "cat")
  expect_equivalent(of(agents = t92, var = "breed"), rep("cat", 5))
  expect_equivalent(t92@levels$breed, c("turtle", "cat"))
  t10 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1)), var = "breed", val = c("dog", "cat"))
  expect_equivalent(of(agents = t10, var = "breed"), c("dog", "cat", rep("turtle", 3)))
  expect_equivalent(t10@levels$breed, c("turtle", "dog", "cat"))
  t11 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1)), var = c("breed", "xcor"),
             val = cbind.data.frame(breed = c("fish", "fish"), xcor = c(1, 1)))
  expect_equivalent(of(agents = t11, var = "breed"), c("fish", "fish", rep("turtle", 3)))
  expect_equivalent(of(agents = t11, var = "xcor"), c(1, 1, 2, 3, 4))
  t11 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1)), var = c("breed", "xcor"),
             val = cbind(breed = c("fish", "fish"), xcor = c(1, 1)))
  expect_equivalent(of(agents = t11, var = "breed"), c("fish", "fish", rep("turtle", 3)))
  expect_equivalent(of(agents = t11, var = "xcor"), c(1, 1, 2, 3, 4))
  t12 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1, 3)), var = c("breed", "xcor", "color",
                                                                      "heading"),
             val = cbind.data.frame(breed = c("aa", "aa", "bb"), xcor = c(10, 10, 12),
                                    color = "red", heading = 222))
  expect_equivalent(of(agents = t12, var = "breed"), c("aa", "aa", "turtle", "bb", "turtle"))
  expect_equivalent(of(agents = t12, var = "xcor"), c(10, 10, 2, 12, 4))
  expect_equivalent(of(agents = t12, var = "color")[c(1, 2, 4)], rep("red", 3))
  expect_equivalent(of(agents = t12, var = "heading"), c(222, 222, 180, 222, 0))
  t12 <- NLset(turtles = t1, agents = turtle(t1, c(0, 1, 3)), var = c("breed", "xcor", "color",
                                                                      "heading"),
             val = cbind(breed = c("aa", "aa", "bb"), xcor = c(10, 10, 12), color = "red",
                         heading = 222))
  expect_equivalent(of(agents = t12, var = "breed"), c("aa", "aa", "turtle", "bb", "turtle"))
  expect_equivalent(of(agents = t12, var = "xcor"), c(10, 10, 2, 12, 4))
  expect_equivalent(of(agents = t12, var = "color")[c(1, 2, 4)], rep("red", 3))
  expect_equivalent(of(agents = t12, var = "heading"), c(222, 222, 180, 222, 0))
  t13 <- NLset(turtles = t1, agents = t1, var = c("breed", "xcor", "color", "heading"),
               val = cbind.data.frame(breed = c("aa", "aa", "bb", "cc", "cc"), xcor = 15,
                                      color = "red", heading = 1:5))
  expect_equivalent(of(agents = t13, var = "breed"), c("aa", "aa", "bb", "cc", "cc"))
  expect_equivalent(of(agents = t13, var = "xcor"), c(15, 15, 15, 15, 15))
  expect_equivalent(of(agents = t13, var = "color"), rep("red", 5))
  expect_equivalent(of(agents = t13, var = "heading"), 1:5)
  t14 <- NLset(turtles = t1, agents = t1, var = c("xcor", "heading"),
               val = cbind(xcor = 21:25, heading = 0))
  expect_equivalent(of(agents = t14, var = "xcor"), 21:25)
  expect_equivalent(of(agents = t14, var = "heading"), rep(0, 5))
  t14 <- NLset(turtles = t1, agents = t1, var = c("xcor", "heading"),
               val = cbind.data.frame(xcor = 21:25, heading = 0))
  expect_equivalent(of(agents = t14, var = "xcor"), 21:25)
  expect_equivalent(of(agents = t14, var = "heading"), rep(0, 5))
  t15 <- NLset(turtles = t1, agents = t1, var = c("xcor", "heading", "breed"),
               val = cbind(xcor = 21:25, heading = 0, breed = "rabbit"))
  expect_equivalent(of(agents = t15, var = "xcor"), 21:25)
  expect_equivalent(of(agents = t15, var = "heading"), rep(0, 5))
  expect_equivalent(of(agents = t15, var = "breed"), rep("rabbit", 5))
  t16 <- NLset(turtles = t1, agents = t1, var = "ycor", val = 0)
  expect_equivalent(of(agents = t16, var = "ycor"), rep(0, 5))
  t17 <- NLset(turtles = t1, agents = t1, var = "ycor", val = c(1, 3, 5, 7, 9))
  expect_equivalent(of(agents = t17, var = "ycor"), c(1, 3, 5, 7, 9))
  t18 <- NLset(turtles = t1, agents = t1, var = c("xcor", "breed"),
               val = cbind(xcor = c(1, 3, 5, 7, 9), breed = "tomato"))
  expect_equivalent(of(agents = t18, var = "xcor"), c(1, 3, 5, 7, 9))
  expect_equivalent(of(agents = t18, var = "breed"), rep("tomato", 5))

  t19 <- NLset(turtles = t1, agents = t1, var = "breed", val = "wolf")
  t20 <- NLset(turtles = t19, agents = turtle(t19, 1), var = "breed", val = "sheep")
  t20Breed <- of(agents = t20, var = "breed")
  expect_equivalent(c("wolf", "sheep", "wolf", "wolf", "wolf"), t20Breed)
  t21 <- createTurtles(n = 20, coords = cbind(xcor = 1:20, ycor = 1:20))
  t22 <- die(turtles = t21, who = c(0, 1, 2, 3, 4))
  t23 <- NLset(turtles = t22, agents = turtle(t22, 10), var = "color", val = "red")
  colt23 <- of(agents = t21, var = "color")[6:20]
  colt23[6] <- "red"
  expect_equivalent(of(agents = t23, var = "color"), colt23)
 })

