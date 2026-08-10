test_that("createWorld works", {
  w1 <- createWorld(minPxcor = -2, maxPxcor = 7, minPycor = -2, maxPycor = 5, data = 1:80)
  expect_equivalent(1:80, as.numeric(t(w1@.Data)))
  exts <- w1@extent
  expect_equivalent(c(-2.5, 7.5, -2.5, 5.5), as.vector(exts))
  expect_equivalent(-2, w1@minPxcor)
  expect_equivalent(7, w1@maxPxcor)
  expect_equivalent(-2, w1@minPycor)
  expect_equivalent(5, w1@maxPycor)
  expect_equivalent(c(1, 1), w1@res)
  expect_equivalent(cbind(pxcor = rep(-2:7, 8), pycor = rep(5:-2, each = 10)), w1@pCoords)

  w2 <- createWorld()
  expect_equivalent(as.numeric(rep(NA, 33 * 33)), as.numeric(t(w2@.Data)))
  exts <- w2@extent
  expect_equivalent(c(-16.5, 16.5, -16.5, 16.5), as.vector(exts))
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
  expect_equivalent(w5@extent, w2@extent)
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

test_that("[] works with a worldArray of any number of layers", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(1, 2, 3, 4))
  w2 <- createWorld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(10, 20, 30, 40))
  w3 <- createWorld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(100, 200, 300, 400))

  ## a single layer, and more than the two that [ used to assume
  ws1 <- stackWorlds(w1)
  expect_identical(ws1[0, 0], cbind(w1 = 3))

  ws3 <- stackWorlds(w1, w2, w3)
  expect_identical(numLayers(ws3), 3L)
  expect_identical(ws3[0, 0], cbind(w1 = 3, w2 = 30, w3 = 300))
  expect_identical(
    ws3[c(0, 1), 1],
    cbind(w1 = c(1, 2), w2 = c(10, 20), w3 = c(100, 200))
  )
  expect_equivalent(ws3[], cbind(c(1, 2, 3, 4), c(10, 20, 30, 40), c(100, 200, 300, 400)))

  ws4 <- stackWorlds(w1, w2, w3, w1)
  expect_identical(dim(ws4[0, 0]), c(1L, 4L))
})

test_that("a worldArray keeps character layers as levels", {
  num <- createWorld(0, 4, 0, 4, data = 1:25)
  hab <- createWorld(0, 4, 0, 4, data = rep(c("sea", "land", "sea", "land", "sea"), 5))
  w <- stackWorlds(num, hab)

  ## the array stays numeric, so the numeric layer is not stringified (#49)
  expect_false(is.character(w@.Data))
  expect_identical(names(w@levels), "hab")
  expect_identical(w@levels$hab, c("land", "sea"))

  ## of() reports characters for the coded layer and numbers for the other
  expect_identical(of(world = w, agents = cbind(pxcor = 1, pycor = 1), var = "hab"), "land")
  expect_identical(of(world = w, agents = cbind(pxcor = 1, pycor = 1), var = "num"), 17)

  ## a mix of the two cannot be a matrix, so it comes back as a data.frame
  both <- of(world = w, agents = cbind(pxcor = 1, pycor = 1), var = c("num", "hab"))
  expect_s3_class(both, "data.frame")
  expect_identical(both$num, 17)
  expect_identical(both$hab, "land")

  ## a single layer round-trips back to a character worldMatrix
  expect_identical(w[["hab"]]@.Data, hab@.Data)
  expect_identical(w$hab@.Data, hab@.Data)
  expect_identical(w[["num"]]@.Data, num@.Data)

  ## an all-numeric worldArray is untouched by any of this
  wn <- stackWorlds(num, createWorld(0, 4, 0, 4, data = 25:1))
  expect_length(wn@levels, 0)
  expect_true(is.matrix(of(world = wn, agents = cbind(pxcor = 1, pycor = 1),
                           var = c("num", "createWorld(0, 4, 0, 4, data = 25:1)"))))
})

test_that("NLwith and NLset work on a character worldArray layer", {
  num <- createWorld(0, 4, 0, 4, data = 1:25)
  hab <- createWorld(0, 4, 0, 4, data = rep(c("sea", "land", "sea", "land", "sea"), 5))
  w <- stackWorlds(num, hab)

  land <- NLwith(agents = patches(w), world = w, var = "hab", val = "land")
  expect_identical(NROW(land), 10L)
  expect_identical(unique(of(world = w, agents = land, var = "hab")), "land")
  expect_identical(NROW(NLwith(agents = patches(w), world = w, var = "hab", val = "nope")), 0L)

  ## assigning an existing category, and one the layer has not seen before
  w2 <- NLset(world = w, agents = cbind(pxcor = 0, pycor = 0), var = "hab", val = "land")
  expect_identical(of(world = w2, agents = cbind(pxcor = 0, pycor = 0), var = "hab"), "land")

  w3 <- NLset(world = w, agents = cbind(pxcor = 0, pycor = 0), var = "hab", val = "ice")
  expect_identical(of(world = w3, agents = cbind(pxcor = 0, pycor = 0), var = "hab"), "ice")
  expect_identical(w3@levels$hab, c("land", "sea", "ice"))
  expect_false(is.character(w3@.Data))
  ## the patches that were not assigned keep their original values
  expect_identical(of(world = w3, agents = cbind(pxcor = 1, pycor = 0), var = "hab"), "land")

  ## the numeric layer is unaffected
  w4 <- NLset(world = w, agents = cbind(pxcor = 0, pycor = 0), var = "num", val = -99)
  expect_identical(of(world = w4, agents = cbind(pxcor = 0, pycor = 0), var = "num"), -99)

  ## turning only some patches of a numeric layer into characters is rejected
  expect_error(
    NLset(world = w, agents = cbind(pxcor = 0, pycor = 0), var = "num", val = "oops"),
    "only some patches"
  )
})

test_that("[[<- re-encodes a replaced worldArray layer", {
  num <- createWorld(0, 4, 0, 4, data = 1:25)
  hab <- createWorld(0, 4, 0, 4, data = rep("sea", 25))
  w <- stackWorlds(num, hab)

  ## swapping a numeric layer in clears the stale levels
  w2 <- w
  w2[["hab"]] <- num
  expect_length(w2@levels$hab, 0)
  expect_identical(of(world = w2, agents = cbind(pxcor = 1, pycor = 1), var = "hab"), 17)

  ## and swapping a character layer in records new ones
  w3 <- w
  w3[["num"]] <- hab
  expect_identical(w3@levels$num, "sea")
  expect_identical(of(world = w3, agents = cbind(pxcor = 1, pycor = 1), var = "num"), "sea")
  expect_false(is.character(w3@.Data))
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
