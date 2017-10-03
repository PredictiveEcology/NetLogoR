test_that("createWorld works", {
  w1 <- createWorld(minPxcor = -2, maxPxcor = 7, minPycor = -2, maxPycor = 5, data = 1:80)
  expect_equivalent(1:80, as.numeric(t(w1@.Data)))
  expect_equivalent(extent(c(-2.5, 7.5, -2.5, 5.5)), w1@extent)
  expect_equivalent(-2, w1@minPxcor)
  expect_equivalent(7, w1@maxPxcor)
  expect_equivalent(-2, w1@minPycor)
  expect_equivalent(5, w1@maxPycor)
  expect_equivalent(c(1, 1), w1@res)
  expect_equivalent(cbind(pxcor = rep(-2:7, 8), pycor = rep(5:-2, each = 10)), w1@pCoords)

  w2 <- createWorld()
  expect_equivalent(as.numeric(rep(NA, 33 * 33)), as.numeric(t(w2@.Data)))
  expect_equivalent(extent(c(-16.5, 16.5, -16.5, 16.5)), w2@extent)
  expect_equivalent(-16, w2@minPxcor)
  expect_equivalent(16, w2@maxPxcor)
  expect_equivalent(-16, w2@minPycor)
  expect_equivalent(16, w2@maxPycor)
  expect_equivalent(c(1, 1), w2@res)
  expect_equivalent(cbind(pxcor = rep(-16:16, 33), pycor = rep(16:-16, each = 33)), w2@pCoords)

  w3 <- createWorld(minPxcor = -2, maxPxcor = 7, minPycor = -2, maxPycor = 5)
  expect_equivalent(rep(as.numeric(NA), length(w1)), as.numeric(t(w3@.Data)))
})

test_that("stackWorlds works", {
  w1 <- createWorld(minPxcor = -2, maxPxcor = 7, minPycor = -4, maxPycor = 5, data = 1:100)
  w2 <- createWorld(minPxcor = -2, maxPxcor = 7, minPycor = -4, maxPycor = 5, data = 101:200)
  w3 <- createWorld(minPxcor = -3, maxPxcor = 6, minPycor = -4, maxPycor = 5, data = 1:100)
  w4 <- createWorld(minPxcor = -2, maxPxcor = 6, minPycor = -4, maxPycor = 5, data = 1:90)
  expect_error(stackWorlds(w2, w3))
  expect_error(stackWorlds(w2, w4))
  w5 <- stackWorlds(w1, w2)
  expect_identical(w5@extent, w2@extent)
  expect_identical(w5@pCoords, w2@pCoords)
  expect_identical(w5@res, w2@res)
  expect_identical(w5@minPxcor, w2@minPxcor)
  expect_identical(w5@maxPxcor, w2@maxPxcor)
  expect_identical(w5@minPycor, w2@minPycor)
  expect_identical(w5@maxPycor, w2@maxPycor)
  expect_equivalent(w5@.Data[, , "w1"], w1@.Data)
  expect_equivalent(w5@.Data[, , "w2"], w2@.Data)

  w3 <- createWorld(minPxcor = -2, maxPxcor = 7, minPycor = -4, maxPycor = 5, data = -1:-100)
  w6 <- stackWorlds(w1, w2, w3)
  expect_identical(w5@extent, w6@extent)
  expect_identical(w5@pCoords, w6@pCoords)
  expect_identical(w5@res, w6@res)
  expect_identical(w5@minPxcor, w6@minPxcor)
  expect_identical(w5@maxPxcor, w6@maxPxcor)
  expect_identical(w5@minPycor, w6@minPycor)
  expect_identical(w5@maxPycor, w6@maxPycor)
  expect_equivalent(w6@.Data[, , "w1"], w1@.Data)
  expect_equivalent(w6@.Data[, , "w2"], w2@.Data)
  expect_equivalent(w6@.Data[, , "w3"], w3@.Data)
})

test_that("[] works for worldMatrix", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(1, 2, 3, 4))
  expect_equivalent(w1[], c(1, 2, 3, 4))

  w1p00 <- w1[0, 0]
  expect_identical(w1p00, 3)
  w1p01p11 <- w1[c(0, 1), 1]
  expect_identical(w1p01p11, c(1, 2))
  w1p01p11 <- w1[c(1, 0), 1]
  expect_identical(w1p01p11, c(2, 1))

  w1[1, c(0, 1)] <- c(10, 20)
  expect_identical(as.numeric(t(w1@.Data))[c(2, 4)], c(20, 10))
  w1[1, c(0, 1)] <- c(NA, NA)
  expect_identical(as.numeric(t(w1@.Data))[c(2, 4)], as.numeric(c(NA, NA)))
  w1[1, c(0, 1)] <- c(NA, 20)
  expect_identical(as.numeric(t(w1@.Data))[c(2, 4)], c(20, NA))
  w1[1, c(1, 0)] <- c(100, 200)
  expect_identical(as.numeric(t(w1@.Data))[c(2, 4)], c(100, 200))
  w1[] <- c(10, 20, 30, 40)
  expect_equivalent(w1[], c(10, 20, 30, 40))
  w1[] <- -1
  expect_equivalent(w1[], c(-1, -1, -1, -1))
  w1[] <- NA
  expect_equivalent(w1[], as.numeric(c(NA, NA, NA, NA)))
})

test_that("[] works with worldArray", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(1, 2, 3, 4))
  w2 <- w1
  w2[] <- c(10, 20, 30, 40)
  ws <- stackWorlds(w1, w2)
  expect_equivalent(ws[], cbind(c(1, 2, 3, 4), c(10, 20, 30, 40)))

  wsp00 <- ws[0, 0]
  expect_identical(wsp00, cbind(w1 = 3, w2 = 30))
  wsp01p11 <- ws[c(0, 1), 1]
  expect_identical(wsp01p11, cbind(w1 = c(1, 2), w2 = c(10, 20)))
  w1p01p11 <- ws[c(1, 0), 1]
  expect_identical(w1p01p11, cbind(w1 = c(2, 1), w2 = c(20, 10)))

  ws[1, c(0, 1)] <- cbind(c(10, 20), c(100, 200))
  expect_identical(ws[1, c(0, 1)], cbind(w1 = c(10, 20), w2 = c(100, 200)))
  ws[] <- cbind(c(15, 25, 35, 45), c(-1, -2, -3, -4))
  expect_equivalent(ws[], cbind(c(15, 25, 35, 45), c(-1, -2, -3, -4)))
  expect_equivalent(as.numeric(t(ws@.Data[, , 1])), c(15, 25, 35, 45))
  ws[] <- cbind(-1, -2)
  expect_equivalent(ws[], cbind(c(-1, -1, -1, -1), c(-2, -2, -2, -2)))
  ws[] <- cbind(NA, NA)
  expect_equivalent(ws[], cbind(c(NA, NA, NA, NA), c(NA, NA, NA, NA)))
})

test_that("cellFromPxcorPycor works", {
  w3 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  cellNum <- cellFromPxcorPycor(world = w3, pxcor = c(9, 0, 1), pycor = c(0, 0, 9))
  expect_equivalent(cellNum, c(100, 91, 2))
  cellNum <- cellFromPxcorPycor(world = w3, pxcor = c(1, 0, 9), pycor = c(9, 0, 0))
  expect_equivalent(cellNum, c(2, 91, 100))
  w4 <- w3
  w5 <- stackWorlds(w3, w4)
  cellNum <- cellFromPxcorPycor(world = w5, pxcor = c(9, 0, 1), pycor = c(0, 0, 9))
  expect_equivalent(cellNum, c(100, 91, 2))
})

test_that("PxcorPycorFromCell works", {
  w3 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  pCoords1 <- PxcorPycorFromCell(world = w3, cellNum = c(100, 91, 2))
  pCoords2 <- cbind(pxcor = c(9, 0, 1), pycor = c(0, 0, 9))
  expect_equivalent(pCoords1, pCoords2)
  w4 <- w3
  w5 <- stackWorlds(w3, w4)
  pCoords1 <- PxcorPycorFromCell(world = w5, cellNum = c(100, 91, 2))
  expect_equivalent(pCoords1, pCoords2)
  pxcor <- sample(0:9, size = 5)
  pycor <- sample(0:9, size = 5)
  cellNum <- cellFromPxcorPycor(world = w5, pxcor = pxcor, pycor = pycor)
  pCoords <- PxcorPycorFromCell(world = w5, cellNum = cellNum)
  expect_equivalent(cbind(pxcor, pycor), pCoords)
})

test_that("NLworldIndex works", {
  data <- 1:16
  w1 <- createWorld(minPxcor = -1, maxPxcor = 2, minPycor = 0, maxPycor = 3, data = data)
  index <- sample(1:16, size = 1)
  expect_equivalent(data[index], w1[NLworldIndex(w1, index)])
  index <- sample(1:16, size = 3)
  expect_equivalent(data[index], w1[NLworldIndex(w1, index)])
})
