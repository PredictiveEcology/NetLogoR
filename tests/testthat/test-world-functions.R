test_that("maxPxcor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(10, maxPxcor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(10, maxPxcor(w3))
})

test_that("maxPycor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(15, maxPycor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(15, maxPycor(w3))
})

test_that("minPxcor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(0, minPxcor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(0, minPxcor(w3))
})

test_that("minPycor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(-5, minPycor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(-5, minPycor(w3))
})

test_that("worldWidth works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(11, worldWidth(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(11, worldWidth(w3))
})

test_that("worldHeight works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(21, worldHeight(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(21, worldHeight(w3))
})

test_that("clearPatches works", {
  # NLworldMatrix
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3, data = 1:20)
  w2 <- clearPatches(w1)
  w3 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3)
  expect_equivalent(w2, w3)

  # worldArray
  w4 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3, data = 1:20)
  w5 <- w4
  w6 <- stackWorlds(w4, w5)
  w7 <- clearPatches(w6)
  expect_equivalent(w7, w3)
})

test_that("raster2world and world2raster work", {
  r1 <- raster(nrows = 10, ncols = 10, xmn = -5, xmx = 10, ymn = 2, ymx = 20)
  r1[] <- runif(100)
  r2 <- r1
  r2[] <- runif(100)
  rs <- stack(r1, r2)
  w1 <- raster2world(r1)
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  ws <- raster2world(rs)
  expect_identical(w1[], ws[][, 1])

  r1w <- world2raster(w1)
  rsw <- world2raster(ws)

  r4 <- raster(extent(c(0, 20000, 0, 20000)), nrows = 100, ncols = 100)
  res(r4) <- 200
  r4[]<-runif(10000)
  w4 <- raster2world(r4)
  expect_identical(ncol(r4), ncol(w4@.Data))
  expect_identical(nrow(r4), nrow(w4@.Data))
  expect_identical(as.numeric(w4@.Data[1,]), values(r4)[1:100])
})

test_that("spatRast2world and world2spatRast work", {
  r1 <- rast(nrows = 10, ncols = 10, xmin = -5, xmax = 10, ymin = 2, ymax = 20)
  r1[] <- runif(100)
  r2 <- r1
  r2[] <- runif(100)
  rs <- c(r1, r2)
  expect_error(spatRast2world(rs)) # duplicated names in layers
  names(rs) <- c("r1", "r2")
  w1 <- spatRast2world(r1)
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  ws <- spatRast2world(rs)
  expect_identical(w1[], ws[][, 1])
  w4 <- spatRast2world(r2)
  w5 <- stackWorlds(w1, w4)
  expect_identical(w5[[2]], ws[[2]])
  
  r1w <- world2spatRast(w1)
  rsw <- world2spatRast(ws)
  expect_identical(names(rsw), names(rs))

  r4 <- rast(xmin = 0, xmax = 20000, ymin = 0, ymax = 20000, nrows = 100, ncols = 100)
  res(r4) <- 200
  r4[]<-runif(10000)
  w4 <- spatRast2world(r4)
  expect_equal(ncol(r4), ncol(w4@.Data))
  expect_equal(nrow(r4), nrow(w4@.Data))
  expect_identical(as.numeric(w4@.Data[1,]), values(r4)[1:100])
  
})

