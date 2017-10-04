test_that("diffuse works with 4 neighbors", {
  # worldMatrix
  w1 <- createWorld(minPxcor = 0, maxPxcor = 2, minPycor = 0, maxPycor = 2,
                    data = c(1, 3, 6, 2, 8, 10, 3, 8, 2))
  w2 <- diffuse(world = w1, share = 0.6, nNeighbors = 4)
  expect_identical(sum(w1[]), sum(w2[]))

  val02 <- 1 - (2 * (1 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4)
  # w1[0,2] - given to w1[1,2] and w1[0,1] + received from w1[1,2] and w1[0,1]
  expect_identical(as.numeric(w2[0, 2]), val02) # test a few patches, done by hand

  val12 <- 3 - (3 * (3 * 0.6 / 4)) + (1 * 0.6 / 4) + (6 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(w2[1, 2]), val12)

  val11 <- 8 - (4 * (8 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4) + (10 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(w2[1, 1]), val11)

  # With torus = TRUE
  w3 <- diffuse(world = w1, share = 0.5, nNeighbors = 4, torus = TRUE)
  expect_identical(sum(w1[]), sum(w3[]))
  val03 <- 1 - (0.5 * 1) + (3 * 0.5 / 4) + (6 * 0.5 / 4) + (3 * 0.5 / 4) + (2 * 0.5 / 4)
  expect_identical(as.numeric(w3[0, 2]), val03)

  # worldArray
  w2 <- createWorld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- stackWorlds(w1, w2)
  ws2 <- diffuse(world = ws, pVar = "w1", share = 0.6, nNeighbors = 4)
  expect_identical(sum(ws[][, 1]), sum(ws2[][, 1]))

  val02 <- 1 - (2 * (1 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4)
  # w1[0,2] - given to w1[1,2] and w1[0,1] + received from w1[1,2] and w1[0,1]
  expect_identical(as.numeric(ws2[0, 2][1]), val02)

  val12 <- 3 - (3 * (3 * 0.6 / 4)) + (1 * 0.6 / 4) + (6 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(ws2[1, 2][1]), val12)

  val11 <- 8 - (4 * (8 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4) + (10 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(ws2[1, 1][1]), val11)

  # With torus = TRUE
  w3 <- diffuse(world = ws, pVar = "w1", share = 0.5, nNeighbors = 4, torus = TRUE)
  expect_identical(sum(w1[]), sum(w3[][, 1]))
  val03 <- 1 - (0.5 * 1) + (3 * 0.5 / 4) + (6 * 0.5 / 4) + (3 * 0.5 / 4) + (2 * 0.5 / 4)
  expect_identical(as.numeric(w3[0, 2][1]), val03)
})

test_that("diffuse works with 8 neighbors", {
  # worldMatrix
  w1 <- createWorld(0, 2, 0, 2)
  w1[] <- c(1, 3, 6, 2, 8, 10, 3, 8, 2)
  w2 <- diffuse(world = w1, share = 0.6, nNeighbors = 8)
  expect_identical(sum(w1[]), sum(w2[]))

  val02 <- 1 - (3 * (1 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (8 * 0.6 / 8)
  # w1[0,2] - given to w1[1,2], w1[0,1] and w1[1,1] + received from w1[1,2], w1[0,1] and w1[1,1]
  expect_identical(as.numeric(w2[0, 2]), val02)

  val12 <- 3 - (5 * (3 * 0.6 / 8)) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (8 * 0.6 / 8) +
    (2 * 0.6 / 8) + (10 * 0.6 / 8)
  expect_identical(as.numeric(w2[1, 2]), val12)

  val11 <- 8 - (8 * (8 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8) +
    (8 * 0.6 / 8) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (3 * 0.6 / 8) + (2 * 0.6 / 8)
  expect_identical(as.numeric(w2[1, 1]), val11)

  # With torus = TRUE
  w3 <- diffuse(world = w1, share = 0.5, nNeighbors = 8, torus = TRUE)
  expect_identical(sum(w1[]), sum(w3[]))
  val03 <- 1 - (0.5 * 1) + (3 * 0.5 / 8) + (6 * 0.5 / 8) + (3 * 0.5 / 8) + (2 * 0.5 / 8) +
    (2 * 0.5 / 8) + (8 * 0.5 / 8) + (10 * 0.5 / 8) + (8 * 0.5 / 8)
  expect_identical(as.numeric(w3[0, 2]), val03)

  # worldArray
  w2 <- createWorld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- stackWorlds(w1, w2)
  ws2 <- diffuse(world = ws, pVar = "w1", share = 0.6, nNeighbors = 8)
  expect_identical(sum(ws[][, 1]), sum(ws2[][, 1]))

  val02 <- 1 - (3 * (1 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (8 * 0.6 / 8)
  # w1[0,2] - given to w1[1,2], w1[0,1] and w1[1,1] + received from w1[1,2], w1[0,1] and w1[1,1]
  expect_identical(as.numeric(ws2[0, 2][, 1]), val02)

  val12 <- 3 - (5 * (3 * 0.6 / 8)) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (8 * 0.6 / 8) +
    (2 * 0.6 / 8) + (10 * 0.6 / 8)
  expect_identical(as.numeric(ws2[1, 2][, 1]), val12)

  val11 <- 8 - (8 * (8 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8) +
    (8 * 0.6 / 8) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (3 * 0.6 / 8) + (2 * 0.6 / 8)
  expect_identical(as.numeric(ws2[1, 1][, 1]), val11)

  # With torus = TRUE
  w3 <- diffuse(world = ws, pVar = "w1", share = 0.5, nNeighbors = 8, torus = TRUE)
  expect_identical(sum(w1[]), sum(w3[][, 1]))
  val03 <- 1 - (0.5 * 1) + (3 * 0.5 / 8) + (6 * 0.5 / 8) + (3 * 0.5 / 8) + (2 * 0.5 / 8) +
    (2 * 0.5 / 8) + (8 * 0.5 / 8) + (10 * 0.5 / 8) + (8 * 0.5 / 8)
  expect_identical(as.numeric(w3[0, 2][1]), val03)
})

test_that("NLdist works for patches", {
  w1 <- createWorld(0, 9, 0, 9)
  dist <- NLdist(world = w1, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = 1, pycor = 1))
  expect_identical(as.numeric(dist), sqrt(1 ^ 2 + 1 ^ 2))
  dist <- NLdist(world = w1, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)))
  expect_identical(dist, c(1, 9))
  dist <- NLdist(world = w1, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)), torus = TRUE)
  expect_identical(dist, c(1, 1))
  dist <- NLdist(world = w1, agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)))
  expect_identical(dist, c(2, 8))
  dist <- NLdist(world = w1, agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)), allPairs = TRUE)
  expect_identical(dist[, 1], c(2, 1))
  expect_identical(dist[, 2], c(9, 8))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)
  dist <- NLdist(world = ws, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = 1, pycor = 1))
  expect_identical(as.numeric(dist), sqrt(1 ^ 2 + 1 ^ 2))
  dist <- NLdist(world = ws, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)))
  expect_identical(dist, c(1, 9))
  dist <- NLdist(world = ws, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)), torus = TRUE)
  expect_identical(dist, c(1, 1))
  dist <- NLdist(world = ws, agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)))
  expect_identical(dist, c(2, 8))
  dist <- NLdist(world = ws, agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)), allPairs = TRUE)
  expect_identical(dist[, 1], c(2, 1))
  expect_identical(dist[, 2], c(9, 8))

  w3 <- createWorld(-5, 5, -10, -2)
  dist <- NLdist(world = w3, agents = cbind(pxcor = -2, pycor = -5),
                 agents2 = cbind(pxcor = c(-1, 5), pycor = c(-5, -5)), torus = TRUE)
  expect_identical(dist, c(1, 4))

  # work without world provided when torus = false
  dist <- NLdist(agents = cbind(pxcor = 0, pycor = 0), agents2 = cbind(pxcor = 1, pycor = 1))
  expect_identical(as.numeric(dist), sqrt(1 ^ 2 + 1 ^ 2))
  dist <- NLdist(agents = cbind(pxcor = 0, pycor = 0), agents2 = cbind(pxcor = c(0, 0),
                                                                       pycor = c(1, 9)))
  expect_identical(dist, c(1, 9))
  dist <- NLdist(world = w1, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)), torus = TRUE)
  expect_identical(dist, c(1, 1))
  expect_error(NLdist(agents = cbind(pxcor = 0, pycor = 0),
                      agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)), torus = TRUE))
  dist <- NLdist(agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)))
  expect_identical(dist, c(2, 8))
  dist <- NLdist(agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)), allPairs = TRUE)
  expect_identical(dist[, 1], c(2, 1))
  expect_identical(dist[, 2], c(9, 8))
  dist <- NLdist(agents = cbind(pxcor = 0, pycor = 0), agents2 = cbind(pxcor = 1, pycor = 1))
  expect_identical(as.numeric(dist), sqrt(1 ^ 2 + 1 ^ 2))
  dist <- NLdist(agents = cbind(pxcor = 0, pycor = 0), agents2 = cbind(pxcor = c(0, 0),
                                                                       pycor = c(1, 9)))
  expect_identical(dist, c(1, 9))
  dist <- NLdist(world = ws, agents = cbind(pxcor = 0, pycor = 0),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)), torus = TRUE)
  expect_identical(dist, c(1, 1))
  expect_error(NLdist(agents = cbind(pxcor = 0, pycor = 0),
                      agents2 = cbind(pxcor = c(0, 0), pycor = c(1, 9)), torus = TRUE))
  dist <- NLdist(agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)))
  expect_identical(dist, c(2, 8))
  dist <- NLdist(agents = cbind(pxcor = c(0, 0), pycor = c(0, 1)),
                 agents2 = cbind(pxcor = c(0, 0), pycor = c(2, 9)), allPairs = TRUE)
  expect_identical(dist[, 1], c(2, 1))
  expect_identical(dist[, 2], c(9, 8))
  dist <- NLdist(world = w3, agents = cbind(pxcor = -2, pycor = -5),
                 agents2 = cbind(pxcor = c(-1, 5), pycor = c(-5, -5)), torus = TRUE)
  expect_identical(dist, c(1, 4))
  expect_error(NLdist(agents = cbind(pxcor = -2, pycor = -5),
                      agents2 = cbind(pxcor = c(-1, 5), pycor = c(-5, -5)), torus = TRUE))
})

test_that("NLdist works with turtles", {
  w1 <- createWorld(0, 9, 0, 9)
  t1 <- createTurtles(n = 4, coords = cbind(xcor = c(1, 2, 3, 4), ycor = c(1, 2, 3, 4)))
  # Patches to turtles
  distPT <- NLdist(world = w1, agents = cbind(pxcor = 2, pycor = 3), agents2 = t1)
  expect_identical(distPT, c(sqrt(1 ^ 2 + 2 ^ 2), 1, 1, sqrt(1 ^ 2 + 2 ^ 2)))
  distPT <- NLdist(world = w1, agents = cbind(pxcor = 8, pycor = 1), agents2 = t1)
  expect_identical(distPT[1], 7)
  distPT <- NLdist(world = w1, agents = cbind(pxcor = 8, pycor = 1), agents2 = t1, torus = TRUE)
  expect_identical(distPT[1], 3)

  # Turtles to patches
  distTP <- NLdist(world = w1, agents = t1, agents2 = cbind(pxcor = 2, pycor = 3))
  expect_identical(distTP, c(sqrt(1 ^ 2 + 2 ^ 2), 1, 1, sqrt(1 ^ 2 + 2 ^ 2)))

  # Turtles to turtles
  distTT <- NLdist(world = w1, agents = t1, agents2 = t1)
  expect_equivalent(distTT, rep(0, 4))
  distTT <- NLdist(world = w1, agents = t1, agents2 = t1, allPairs = TRUE)
  expect_equivalent(distTT[1, 2], sqrt(1 ^ 1 + 1 ^ 1))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)
  distPT <- NLdist(world = ws, agents = cbind(pxcor = 8, pycor = 1), agents2 = t1, torus = TRUE)
  expect_identical(distPT[1], 3)
  distTP <- NLdist(world = ws, agents = t1, agents2 = cbind(pxcor = 2, pycor = 3))
  expect_identical(distTP, c(sqrt(1 ^ 2 + 2 ^ 2), 1, 1, sqrt(1 ^ 2 + 2 ^ 2)))
  distTT <- NLdist(world = ws, agents = t1, agents2 = t1, allPairs = TRUE)
  expect_equivalent(distTT[1, 2], sqrt(1 ^ 1 + 1 ^ 1))

  # work without world provided when torus = false
  distPT <- NLdist(agents = cbind(pxcor = 2, pycor = 3), agents2 = t1)
  expect_identical(distPT, c(sqrt(1 ^ 2 + 2 ^ 2), 1, 1, sqrt(1 ^ 2 + 2 ^ 2)))
  distPT <- NLdist(agents = cbind(pxcor = 8, pycor = 1), agents2 = t1)
  expect_identical(distPT[1], 7)
  distPT <- NLdist(world = w1, agents = cbind(pxcor = 8, pycor = 1), agents2 = t1, torus = TRUE)
  expect_identical(distPT[1], 3)
  expect_error(NLdist(agents = cbind(pxcor = 8, pycor = 1), agents2 = t1, torus = TRUE))
  distTP <- NLdist(agents = t1, agents2 = cbind(pxcor = 2, pycor = 3))
  expect_identical(distTP, c(sqrt(1 ^ 2 + 2 ^ 2), 1, 1, sqrt(1 ^ 2 + 2 ^ 2)))
  distTT <- NLdist(agents = t1, agents2 = t1)
  expect_equivalent(distTT, rep(0, 4))
  distTT <- NLdist(world = w1, agents = t1, agents2 = t1, allPairs = TRUE)
  expect_equivalent(distTT[1, 2], sqrt(1 ^ 1 + 1 ^ 1))
  distPT <- NLdist(world = ws, agents = cbind(pxcor = 8, pycor = 1), agents2 = t1, torus = TRUE)
  expect_identical(distPT[1], 3)
  expect_error(NLdist(agents = cbind(pxcor = 8, pycor = 1), agents2 = t1, torus = TRUE))
  distTP <- NLdist(agents = t1, agents2 = cbind(pxcor = 2, pycor = 3))
  expect_identical(distTP, c(sqrt(1 ^ 2 + 2 ^ 2), 1, 1, sqrt(1 ^ 2 + 2 ^ 2)))
  distTT <- NLdist(agents = t1, agents2 = t1, allPairs = TRUE)
  expect_equivalent(distTT[1, 2], sqrt(1 ^ 1 + 1 ^ 1))
})

test_that("pExist works", {
  w1 <- createWorld(0, 2, 0, 2)
  expect_false(pExist(w1, 1, 3))
  expect_true(pExist(w1, 1, 1))

  w1[] <- c(1, 3, 6, 2, 8, 10, 3, 8, 2)
  expect_identical(pExist(w1, c(0, 1), c(3, 1)), c(FALSE, TRUE))

  w2 <- createWorld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- stackWorlds(w1, w2)

  # Same as for w1
  expect_false(pExist(ws, 1, 3))
  expect_true(pExist(ws, 1, 1))
  expect_identical(pExist(ws, c(0, 1), c(3, 1)), c(FALSE, TRUE))

  # With different length of inputs
  expect_equivalent(c(FALSE, TRUE), pExist(w1, c(1, 1), c(3, 1)))
  expect_equivalent(c(FALSE, TRUE), pExist(w1, 1, c(3, 1)))
  expect_equivalent(c(FALSE, TRUE), pExist(w1, c(3, 1), 1))
})

test_that("neighbors works", {
  w1 <- createWorld(0, 9, 0, 9)
  n4 <- neighbors(world = w1, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)),
                  nNeighbors = 4)
  n41 <- cbind(pxcor = c(1, 0), pycor = c(9, 8))
  n43 <- cbind(pxcor = c(0, 1), pycor = c(1, 0))
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n41)
  expect_equivalent(n4[n4[, "id"] == 3, c("pxcor", "pycor")], n43)

  n8 <- neighbors(world = w1, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)),
                  nNeighbors = 8)
  n82 <- cbind(pxcor = c(8, 8, 9), pycor = c(9, 8, 8))
  expect_equivalent(nrow(merge(n8[n8[, "id"] == 2, c("pxcor", "pycor")], n82)), 3)

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)

  # Same as for w1
  n4 <- neighbors(world = ws, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)),
                  nNeighbors = 4)
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n41)
  expect_equivalent(n4[n4[, "id"] == 3, c("pxcor", "pycor")], n43)
  n8 <- neighbors(world = ws, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)),
                  nNeighbors = 8)
  expect_equivalent(nrow(merge(n8[n8[, "id"] == 2, c("pxcor", "pycor")], n82)), 3)

  # With torus = TRUE
  nCorner <- neighbors(world = w1, agents = cbind(pxcor = 9, pycor = 9), nNeighbors = 4,
                       torus = FALSE)
  expect_equivalent(nrow(nCorner[nCorner[, "id"] == 1, c("pxcor", "pycor")]), 2)
  nCorner <- neighbors(world = w1, agents = cbind(pxcor = 9, pycor = 9), nNeighbors = 4,
                       torus = TRUE)
  expect_equivalent(nrow(nCorner[nCorner[, "id"] == 1, c("pxcor", "pycor")]), 4)
  expect_equivalent(nCorner[nCorner[, "id"] == 1, c("pxcor", "pycor")],
                    cbind(pxcor = c(9, 8, 0, 9), pycor = c(0, 9, 9, 8)))
  nCorner <- neighbors(world = ws, agents = cbind(pxcor = 9, pycor = 9), nNeighbors = 4,
                       torus = TRUE)
  expect_equivalent(nrow(nCorner[nCorner[, "id"] == 1, c("pxcor", "pycor")]), 4)
  expect_equivalent(nCorner[nCorner[, "id"] == 1, c("pxcor", "pycor")],
                    cbind(pxcor = c(9, 8, 0, 9), pycor = c(0, 9, 9, 8)))

  # Large number of agents
  w1 <- createWorld(0, 500, 0, 500)
  n4 <- neighbors(world = w1, agents = patches(w1), nNeighbors = 4)
  n4p1 <- cbind(pxcor = c(1, 0), pycor = c(500, 499))
  n4p251001 <- cbind(pxcor = c(500, 499), pycor = c(1, 0))
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n4p1)
  expect_equivalent(n4[n4[, "id"] == 251001, c("pxcor", "pycor")], n4p251001)
  # 4 neighbors, torus = FALSE
  n4double <- neighbors(world = w1, agents = rbind(patches(w1), patches(w1)), nNeighbors = 4)
  n4double1 <- neighbors(world = w1, agents = patch(w1, 0, 500), nNeighbors = 4)
  expect_equivalent(n4double[n4double[, 3] == 1, c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  expect_equivalent(n4double[n4double[, 3] == 1 + length(w1), c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  n4double503 <- neighbors(world = w1, agents = patch(w1, 1, 499), nNeighbors = 4)
  expect_equivalent(n4double[n4double[, 3] == 503 + length(w1), c("pxcor", "pycor")],
                    n4double503[, c("pxcor", "pycor")])
  # 4 neighbors, torus = TRUE
  n4double <- neighbors(world = w1, agents = rbind(patches(w1), patches(w1)), nNeighbors = 4,
                        torus = TRUE)
  n4double1 <- neighbors(world = w1, agents = patch(w1, 0, 500), nNeighbors = 4, torus = TRUE)
  expect_equivalent(n4double[n4double[, 3] == 1, c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  expect_equivalent(n4double[n4double[, 3] == 1 + length(w1), c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  n4double503 <- neighbors(world = w1, agents = patch(w1, 1, 499), nNeighbors = 4, torus = TRUE)
  expect_equivalent(n4double[n4double[, 3] == 503 + length(w1), c("pxcor", "pycor")],
                    n4double503[, c("pxcor", "pycor")])
  # 8 neighbors, torus = FALSE
  n4double <- neighbors(world = w1, agents = rbind(patches(w1), patches(w1)), nNeighbors = 8)
  n4double1 <- neighbors(world = w1, agents = patch(w1, 0, 500), nNeighbors = 8)
  expect_equivalent(n4double[n4double[, 3] == 1, c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  expect_equivalent(n4double[n4double[, 3] == 1 + length(w1), c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  n4double503 <- neighbors(world = w1, agents = patch(w1, 1, 499), nNeighbors = 8)
  expect_equivalent(n4double[n4double[, 3] == 503 + length(w1), c("pxcor", "pycor")],
                    n4double503[, c("pxcor", "pycor")])
  # 8 neighbors, torus = TRUE
  n4double <- neighbors(world = w1, agents = rbind(patches(w1), patches(w1)), nNeighbors = 8,
                        torus = TRUE)
  n4double1 <- neighbors(world = w1, agents = patch(w1, 0, 500), nNeighbors = 8, torus = TRUE)
  expect_equivalent(n4double[n4double[, 3] == 1, c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  expect_equivalent(n4double[n4double[, 3] == 1 + length(w1), c("pxcor", "pycor")],
                    n4double1[, c("pxcor", "pycor")])
  n4double503 <- neighbors(world = w1, agents = patch(w1, 1, 499), nNeighbors = 8, torus = TRUE)
  expect_equivalent(n4double[n4double[, 3] == 503 + length(w1), c("pxcor", "pycor")],
                    n4double503[, c("pxcor", "pycor")])

  # With duplicate
  w1 <- createWorld(0, 9, 0, 9)
  n4 <- neighbors(world = w1, agents = cbind(pxcor = c(0, 9, 0, 9, 0), pycor = c(9, 9, 0, 0, 9)),
                  nNeighbors = 4)
  n4p1 <- cbind(pxcor = c(1, 0), pycor = c(9, 8))
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n4p1)
  expect_equivalent(n4[n4[, "id"] == 5, c("pxcor", "pycor")], n4[n4[, "id"] == 5,
                                                                 c("pxcor", "pycor")])
  w1 <- createWorld(0, 50, 0, 50)
  n4 <- neighbors(world = w1, agents = rbind(patches(w1), patches(w1)[1, ]), nNeighbors = 4)
  n4p1 <- cbind(pxcor = c(1, 0), pycor = c(50, 49))
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n4p1)
  expect_equivalent(n4[n4[, "id"] == 2602, c("pxcor", "pycor")], n4[n4[, "id"] == 1,
                                                                    c("pxcor", "pycor")])

  # With turtles
  w1 <- createWorld(0, 9, 0, 9)
  t1 <- createTurtles(n = 4, coords = cbind(xcor = c(0, 9, 0, 9), ycor = c(9, 9, 0, 0)))
  n4 <- neighbors(world = w1, agents = t1, nNeighbors = 4)
  n41 <- cbind(pxcor = c(1, 0), pycor = c(9, 8))
  n43 <- cbind(pxcor = c(0, 1), pycor = c(1, 0))
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n41)
  expect_equivalent(n4[n4[, "id"] == 3, c("pxcor", "pycor")], n43)

  n8 <- neighbors(world = w1, agents = t1, nNeighbors = 8)
  n82 <- cbind(pxcor = c(8, 8, 9), pycor = c(9, 8, 8))
  expect_equivalent(nrow(merge(n8[n8[, "id"] == 2, c("pxcor", "pycor")], n82)), 3)

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)

  # Same as for w1
  n4 <- neighbors(world = ws, agents = t1, nNeighbors = 4)
  expect_equivalent(n4[n4[, "id"] == 1, c("pxcor", "pycor")], n41)
  expect_equivalent(n4[n4[, "id"] == 3, c("pxcor", "pycor")], n43)
  n8 <- neighbors(world = ws, agents = t1, nNeighbors = 8)
  expect_equivalent(nrow(merge(n8[n8[, "id"] == 2, c("pxcor", "pycor")], n82)), 3)

  # With torus = TRUE
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 0.2, ycor = 0.3))
  nCorner <- neighbors(world = w1, agents = t1, nNeighbors = 8, torus = TRUE)
  expect_equivalent(nrow(nCorner[nCorner[, "id"] == 1, c("pxcor", "pycor")]), 8)
  expect_equivalent(sum(nCorner[nCorner[, "id"] == 1, "pxcor"]), 3 * 9 + 3 * 1 + 2 * 0)
  expect_equivalent(sum(nCorner[nCorner[, "id"] == 1, "pycor"]), 3 * 9 + 3 * 1 + 2 * 0)
})

test_that("patch works", {
  w1 <- createWorld(data = 1:100, minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  expect_identical(patch(world = w1, x = 0.1, y = -0.4), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, x = c(1, 0), y = c(0, 0)), cbind(pxcor = c(1, 0),
                                                                      pycor = c(0, 0)))
  expect_identical(patch(world = w1, x = c(0, -1), y = c(0, 0), out = FALSE),
                   cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, x = c(0, -1), y = c(0, 0), out = TRUE),
                   cbind(pxcor = c(0, NA), pycor = c(0, NA)))
  expect_identical(patch(world = w1, x = c(0, -1), y = c(0, 0), torus = TRUE),
                   cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = w1, x = c(0, -1), y = c(0, 0), torus = TRUE, out = TRUE),
                   cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = w1, x = c(0, 0.1, 0.4), y = c(-0.4, 0, 0.2)),
                   cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, x = c(0, 0.1, 0.4), y = c(-0.4, 0, 0.2), duplicate = TRUE),
                   cbind(pxcor = c(0, 0, 0), pycor = c(0, 0, 0)))
  w2 <- createWorld(data = 100:1, minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  ws <- stackWorlds(w1, w2)
  expect_identical(patch(world = ws, x = 0.1, y = -0.4), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, x = c(1, 0), y = c(0, 0)), cbind(pxcor = c(1, 0),
                                                                      pycor = c(0, 0)))
  expect_identical(patch(world = ws, x = c(0, -1), y = c(0, 0), out = FALSE),
                   cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, x = c(0, -1), y = c(0, 0), out = TRUE),
                   cbind(pxcor = c(0, NA), pycor = c(0, NA)))
  expect_identical(patch(world = ws, x = c(0, -1), y = c(0, 0), torus = TRUE),
                   cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = ws, x = c(0, -1), y = c(0, 0), torus = TRUE, out = TRUE),
                   cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = ws, x = c(0, 0.1, 0.4), y = c(-0.4, 0, 0.2)),
                   cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, x = c(0, 0.1, 0.4), y = c(-0.4, 0, 0.2), duplicate = TRUE),
                   cbind(pxcor = c(0, 0, 0), pycor = c(0, 0, 0)))
})

test_that("noPatches works", {
  p1 <- noPatches()
  expect_equivalent(nrow(p1), 0)
  expect_equivalent(ncol(p1), 2)
})

test_that("patchAt works", {
  w1 <- createWorld(0, 9, 0, 9)
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = 1, dy = 2)
  expect_identical(p1, patch(w1, c(0 + 1, 1 + 1, 3 + 1), c(0 + 2, 1 + 2, 5 + 2)))
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)),
                dx = c(1, 3, 5), dy = c(2, 4, 6))
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)),
                dx = c(1, 3, 5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 1)))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)
  p1 <- patchAt(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = 1, dy = 2)
  expect_identical(p1, patch(ws, c(0 + 1, 1 + 1, 3 + 1), c(0 + 2, 1 + 2, 5 + 2)))
  p1 <- patchAt(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)),
                dx = c(1, 3, 5), dy = c(2, 4, 6))
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)),
                dx = c(1, 3, 5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 1)))

  w1 <- createWorld(-5, 5, -5, 5)
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)), dx = -4,
                dy = 1)
  expect_identical(p1, patch(w1, c(-4, -6, -1), c(1, 2, 6), out = TRUE, duplicate = TRUE))

  # Turtles
  w1 <- createWorld(0, 9, 0, 9)
  t1 <- createTurtles(n = 3, coords = cbind(xcor = c(0.2, 0.9, 3.1), ycor = c(-0.4, 1, 5.4)))
  p1 <- patchAt(world = w1, agents = t1, dx = 1, dy = 2)
  expect_identical(p1, patch(w1, c(0 + 1, 1 + 1, 3 + 1), c(0 + 2, 1 + 2, 5 + 2)))
  p1 <- patchAt(world = w1, agents = t1, dx = c(1, 3, 5), dy = c(2, 4, 6))
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = w1, agents = t1, dx = c(1, 3, 5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 1)))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)
  p1 <- patchAt(world = ws, agents = t1, dx = 1, dy = 2)
  expect_identical(p1, patch(ws, c(0 + 1, 1 + 1, 3 + 1), c(0 + 2, 1 + 2, 5 + 2)))
  p1 <- patchAt(world = ws, agents = t1, dx = c(1, 3, 5), dy = c(2, 4, 6))
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = ws, agents = t1, dx = c(1, 3, 5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 1)))

  w1 <- createWorld(-5, 5, -5, 5)
  t2 <- createTurtles(n = 3, coords = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)))
  p1 <- patchAt(world = w1, agents = t2, dx = -4, dy = 1)
  expect_identical(p1, patch(w1, c(-4, -6, -1), c(1, 2, 6), out = TRUE, duplicate = TRUE))
})

test_that("patchDistDir works", {
  w1 <- createWorld(0, 9, 0, 9)
  p1 <- patchDistDir(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = 45)
  expect_identical(p1, patch(w1, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistDir(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = -45, torus = TRUE)
  p2 <- patchDistDir(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = 315, torus = TRUE)
  expect_identical(p1, patch(w1, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistDir(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = -45)
  expect_identical(p1, patch(w1, c(NA, NA, 1), c(NA, NA, 7), out = TRUE, duplicate = TRUE))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)
  p1 <- patchDistDir(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = 45)
  expect_identical(p1, patch(ws, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistDir(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = -45, torus = TRUE)
  p2 <- patchDistDir(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = 315, torus = TRUE)
  expect_identical(p1, patch(ws, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistDir(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3,
                     angle = -45)
  expect_identical(p1, patch(ws, c(-2, -1, 1), c(-2, -1, 7), duplicate = TRUE, out = TRUE))

  w1 <- createWorld(-5, 5, -5, 5)
  p1 <- patchDistDir(world = w1, agents = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)), dist = 4,
                     angle = 270, torus = TRUE)
  expect_identical(p1, patch(w1, c(-4, 5, -1), c(0, 1, 5)))
  p1 <- patchDistDir(world = w1, agents = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)),
                     dist = -4, angle = 270, torus = FALSE)
  expect_identical(p1, patch(w1, c(4, 2, 7), c(0, 1, 5), duplicate = TRUE, out = TRUE))

  # Turtles
  w1 <- createWorld(0, 9, 0, 9)
  t1 <- createTurtles(n = 3, coords = cbind(xcor = c(0.1, 0.9, 3), ycor = c(-0.4, 1, 5.2)))
  p1 <- patchDistDir(world = w1, agents = t1, dist = 3, angle = 45)
  expect_identical(p1, patch(w1, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistDir(world = w1, agents = t1, dist = 3, angle = -45, torus = TRUE)
  p2 <- patchDistDir(world = w1, agents = t1, dist = 3, angle = 315, torus = TRUE)
  expect_identical(p1, patch(w1, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistDir(world = w1, agents = t1, dist = 3, angle = -45)
  expect_identical(p1, patch(w1, c(NA, NA, 1), c(NA, NA, 7), out = TRUE, duplicate = TRUE))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- stackWorlds(w1, w2)
  p1 <- patchDistDir(world = ws, agents = t1, dist = 3, angle = 45)
  expect_identical(p1, patch(ws, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistDir(world = ws, agents = t1, dist = 3, angle = -45, torus = TRUE)
  p2 <- patchDistDir(world = ws, agents = t1, dist = 3, angle = 315, torus = TRUE)
  expect_identical(p1, patch(ws, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistDir(world = ws, agents = t1, dist = 3, angle = -45)
  expect_identical(p1, patch(ws, c(-2, -1, 1), c(-2, -1, 7), duplicate = TRUE, out = TRUE))

  w1 <- createWorld(-5, 5, -5, 5)
  t2 <- createTurtles(n = 3, coords = cbind(pxcor = c(-0.1, -2.2, 3.4), pycor = c(0.2, 0.8, 5.4)))
  p1 <- patchDistDir(world = w1, agents = t2, dist = 4, angle = 270, torus = TRUE)
  expect_identical(p1, patch(w1, c(-4, 5, -1), c(0, 1, 5)))
  p1 <- patchDistDir(world = w1, agents = t2, dist = -4, angle = 270, torus = FALSE)
  expect_identical(p1, patch(w1, c(4, 2, 7), c(0, 1, 5), duplicate = TRUE, out = TRUE))
})

test_that("patches work", {
  w1 <- createWorld(data = runif(100), minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  p1 <- patches(w1)
  p2 <- cbind(pxcor = rep(0:9, 10), pycor = rep(9:0, each = 10))
  expect_equivalent(p1, p2)
  w2 <- createWorld(data = runif(100), minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  w3 <- stackWorlds(w1, w2)
  p3 <- patches(w3)
  expect_identical(p3, p1)
})

test_that("patchSet works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  p1 <- patchAt(world = w1, agents = patch(w1, c(0, 1, 2), c(0, 0, 0)), dx = 1, dy = 1)
  p2 <- patchDistDir(world = w1, agents = patch(w1, 0, 0), dist = 1, angle = 45)
  p3 <- patch(world = w1, x = 4.3, y = 8)
  p4 <- patchSet(p1, p2, p3)
  expect_identical(p4, unique(p4))
  expect_equivalent(p4, unique(rbind(p1, p2, p3)))
})

test_that("randomPxcor and randomPycor work", {
  w1 <- createWorld(0, 9, -10, -5)
  pxcor100 <- randomPxcor(world = w1, n = 100)
  expect_equivalent(min(pxcor100), minPxcor(w1))
  expect_equivalent(max(pxcor100), maxPxcor(w1))

  w1[] <- runif(60)
  w2 <- w1
  w2[] <- runif(60)
  ws <- stackWorlds(w1, w2)
  pxcor100 <- randomPxcor(world = ws, n = 100)
  expect_equivalent(min(pxcor100), minPxcor(ws))
  expect_equivalent(max(pxcor100), maxPxcor(ws))

  pycor100 <- randomPycor(world = w1, n = 100)
  expect_equivalent(min(pycor100), minPycor(w1))
  expect_equivalent(max(pycor100), maxPycor(w1))
  pycor100 <- randomPycor(world = ws, n = 100)
  expect_equivalent(min(pycor100), minPycor(ws))
  expect_equivalent(max(pycor100), maxPycor(ws))
})
