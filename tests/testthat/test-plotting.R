test_that("createTurtles works", {
  withr::local_package("quickPlot")
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = sample(1:25))
  w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  a1 <- stackWorlds(w1, w2)
  t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
  t1_0 <- createTurtles(n = 10, coords = cbind(rep(0, 10), rep(0, 10)))


  if (requireNamespace("sp", quietly = TRUE)) {
    sp1 <- turtles2spdf(t1)
  }
  if (requireNamespace("sf", quietly = TRUE)) {
    sp2 <- turtles2sf(t1)
  }

  if (requireNamespace("raster", quietly = TRUE)) {
    r1 <- world2raster(w1)
    s1 <- world2raster(a1)
  }

  clearPlot()

  # TODO Error in .local(x, ...) : matrix should not have more than 2 columns
  #   extent is not being found correctly
  plotNum <- 1
  Plot(t1)
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)

  if (requireNamespace("sp", quietly = TRUE)) {
    plotNum <- plotNum + 1
    mess <- capture_messages(Plot(sp1)) # "setting graphics device" message occurs the first time
    expect_silent(Plot(sp1))
    a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == plotNum)
    expect_true(length(a$curr@quickPlotGrobList) == plotNum)
  }

  plotNum <- plotNum + 1
  expect_silent(Plot(w1, axes = TRUE, new = TRUE))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList$w1) == 1)

  expect_silent(Plot(t1, addTo = "w1"))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList$w1) == 2)

  plotNum <- plotNum + 1
  expect_silent(Plot(w2))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList$w2) == 1)

  plotNum <- plotNum + 2 # an 2x array
  expect_silent(Plot(a1))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList$`a1$w1`) == 1)

  # TODO -- these points are not correctly aligned on raster
  expect_silent(Plot(t1, addTo = "a1$w1"))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList$`a1$w1`) == 2)

  if (requireNamespace("raster", quietly = TRUE)) {
    plotNum <- plotNum + 2 # an 2x array
    expect_silent(Plot(s1))
    a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == plotNum)
    expect_true(length(a$curr@quickPlotGrobList) == plotNum)
    expect_true(length(a$curr@quickPlotGrobList$`a1$w1`) == 2)

    plotNum <- plotNum + 1
    expect_silent(Plot(r1))
    a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == plotNum)
    expect_true(length(a$curr@quickPlotGrobList) == plotNum)
    expect_true(length(a$curr@quickPlotGrobList$`a1$w1`) == 2)

    s1$layer.1[3] <- 15
    s1$layer.2[3] <- 25

    expect_silent(Plot(s1$layer.1)) # doesn't change s1$layer.2
    expect_silent(Plot(s1$layer.2)) # doesn't change s1$layer.1
    expect_silent(Plot(a1$w1))
  }

  expect_silent(rePlot())
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == plotNum)
  expect_true(length(a$curr@quickPlotGrobList) == plotNum)

  # Test singularity of agents in agentMatrix
  clearPlot()
  expect_silent(Plot(t1_0, axes = TRUE, new = TRUE)) # agentMatrix
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer) == 1)
  expect_true(length(a$curr@quickPlotGrobList) == 1)
})
