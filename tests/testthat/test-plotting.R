test_that("plot and points methods work", {
  withr::local_pdf(nullfile())

  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = sample(1:25))
  w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  a1 <- stackWorlds(w1, w2)
  t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
  t1_0 <- createTurtles(n = 10, coords = cbind(rep(0, 10), rep(0, 10)))

  expect_silent(plot(w1))
  expect_silent(plot(w1, axes = TRUE))
  expect_silent(plot(a1))
  expect_silent(plot(a1[["w1"]]))
  expect_silent(plot(t1))
  expect_silent(plot(t1_0))

  ## turtles can be added on top of an already plotted world
  plot(w1)
  expect_silent(points(t1))
})

test_that("plot and points dispatch to the NetLogoR methods", {
  expect_type(getS3method("plot", "worldMatrix"), "closure")
  expect_type(getS3method("plot", "worldArray"), "closure")
  expect_type(getS3method("plot", "agentMatrix"), "closure")
  expect_type(getS3method("points", "agentMatrix"), "closure")
})

test_that("numLayers and layerNames report the worldArray layers", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 25:1)
  a1 <- stackWorlds(w1, w2)

  expect_identical(numLayers(w1), 1L)
  expect_identical(numLayers(a1), 2L)
  expect_identical(layerNames(a1), c("w1", "w2"))
})
