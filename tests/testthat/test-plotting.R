test_that("createTurtles works",{
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = sample(1:25))
  w2 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  a1 <- stackWorlds(w1, w2)
  t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
  t1_0 <- createTurtles(n = 10, coords = cbind(rep(0,10), rep(0,10)))

  r1 <- world2raster(w1)
  s1 <- world2raster(a1)
  sp1 <- turtles2spdf(t1)

  library(quickPlot)
  clearPlot()
  expect_silent(Plot(t1)) #agentMatrix
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==1)
  expect_true(length(a$curr@quickPlotGrobList)==1)

  expect_silent(Plot(sp1))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==2)
  expect_true(length(a$curr@quickPlotGrobList)==2)

  expect_silent(Plot(w1, axes = TRUE, new=TRUE))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==3)
  expect_true(length(a$curr@quickPlotGrobList)==3)
  expect_true(length(a$curr@quickPlotGrobList$w1)==1)

  expect_silent(Plot(t1, addTo = "w1"))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==3)
  expect_true(length(a$curr@quickPlotGrobList)==3)
  expect_true(length(a$curr@quickPlotGrobList$w1)==2)

  expect_silent(Plot(w2))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==4)
  expect_true(length(a$curr@quickPlotGrobList)==4)
  expect_true(length(a$curr@quickPlotGrobList$w2)==1)

  expect_silent(Plot(a1))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==6)
  expect_true(length(a$curr@quickPlotGrobList)==6)
  expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==1)

  expect_silent(Plot(t1, addTo = "a1$w1"))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==6)
  expect_true(length(a$curr@quickPlotGrobList)==6)
  expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==2)

  expect_silent(Plot(s1))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==8)
  expect_true(length(a$curr@quickPlotGrobList)==8)
  expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==2)

  expect_silent(Plot(r1))
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==9)
  expect_true(length(a$curr@quickPlotGrobList)==9)
  expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==2)

  s1$layer.1[3] <- 15
  s1$layer.2[3] <- 25

  expect_silent(Plot(s1$layer.1)) # doesn't change s1$layer.2
  expect_silent(Plot(s1$layer.2)) # doesn't change s1$layer.1
  expect_silent(Plot(a1[["w1"]]) )

  expect_silent(rePlot())
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==9)
  expect_true(length(a$curr@quickPlotGrobList)==9)

  # Test singularity of agents in agentMatrix
  clearPlot()
  expect_silent(Plot(t1_0, axes= TRUE, new=TRUE)) #agentMatrix
  a <- getFromNamespace(".getQuickPlot", ns = "quickPlot")(paste0("quickPlot", dev.cur()))
  expect_true(length(a$isBaseLayer)==1)
  expect_true(length(a$curr@quickPlotGrobList)==1)

})
