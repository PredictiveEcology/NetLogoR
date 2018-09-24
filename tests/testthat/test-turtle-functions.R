test_that("createTurtles works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)

  t1 <- createTurtles(world = w1, n = 10)
  expect_equivalent(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), of(agents = t1,
                                                                    var = c("xcor", "ycor")))
  t1head <- of(agents = t1, var = "heading") >= 0 & of(agents = t1, var = "heading") <= 360
  expect_equivalent(t1head, rep(TRUE, 10))
  expect_equivalent(of(agents = t1, var = "breed"), rep("turtle", 10))
  expect_equivalent(length(unique(of(agents = t1, var = "color"))), 10)
  expect_equivalent(of(agents = t1, var = "who"), 0:9)
  expect_equivalent(of(agents = t1, var = "prevX"), as.numeric(rep(NA, 10)))
  expect_equivalent(of(agents = t1, var = "prevY"), as.numeric(rep(NA, 10)))
  expect_equivalent(NLcount(t1), 10)

  t2 <- createTurtles(n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)))
  expect_equivalent(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), of(agents = t2,
                                                                    var = c("xcor", "ycor")))
  t3 <- createTurtles(world = w1, n = 10, heading = 0)
  expect_equivalent(rep(0, 10), of(agents = t3, var = "heading"))
  t4 <- createTurtles(world = w1, n = 10, heading = 1:10)
  expect_equivalent(1:10, of(agents = t4, var = "heading"))
  t5 <- createTurtles(world = w1, n = 10, breed = "caribou")
  expect_equivalent(rep("caribou", 10), of(agents = t5, var = "breed"))
  t6 <- createTurtles(world = w1, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_equivalent(c(rep("caribou", 5), rep("moose", 5)), of(agents = t6, var = "breed"))
})

test_that("createOTurtles works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)

  t1 <- createOTurtles(world = w1, n = 10)
  expect_equivalent(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), of(agents = t1,
                                                                    var = c("xcor", "ycor")))
  expect_equivalent(seq(0, 360 - (360 / 10), by = 360 / 10), of(agents = t1, var = "heading"))
  expect_equivalent(of(agents = t1, var = "breed"), rep("turtle", 10))
  expect_equivalent(length(unique(of(agents = t1, var = "color"))), 10)
  expect_equivalent(of(agents = t1, var = "who"), 0:9)
  expect_equivalent(of(agents = t1, var = "prevX"), as.numeric(rep(NA, 10)))
  expect_equivalent(of(agents = t1, var = "prevY"), as.numeric(rep(NA, 10)))
  expect_equivalent(NLcount(t1), 10)

  t2 <- createOTurtles(world = w1, n = 10, breed = "caribou")
  expect_equivalent(rep("caribou", 10), of(agents = t2, var = "breed"))
  t3 <- createOTurtles(world = w1, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_equivalent(c(rep("caribou", 5), rep("moose", 5)), of(agents = t3, var = "breed"))

  t4 <- createOTurtles(world = w1, n = 1)
  expect_equivalent(of(agents = t4, var = "heading"), 0)
  expect_equivalent(cbind(xcor = 2, ycor = 2), of(agents = t4, var = c("xcor", "ycor")))

  # Create one turtle
  t7 <- createOTurtles(world = w1, n = 1)
  expect_equivalent(of(agents = t7, var = "heading"), 0)
  expect_equivalent(cbind(xcor = 2, ycor = 2), of(agents = t7, var = c("xcor", "ycor")))
})

test_that("fd works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 0, ycor = 0), heading = 90)
  t2 <- fd(world = w1, turtles = t1, dist = 1)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 1,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t2,
                                                                    var = c("xcor", "ycor")))
  t3 <- fd(world = w1, turtles = t1, dist = 5, torus = FALSE, out = TRUE)
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 5,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t3,
                                                                    var = c("xcor", "ycor")))
  t4 <- fd(world = w1, turtles = t1, dist = 5, torus = TRUE)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t4,
                                                                 var = c("xcor", "ycor")))
  t5 <- fd(world = w1, turtles = t1, dist = -1, torus = TRUE)
  expect_equivalent(cbind(xcor = rep(4, 10), ycor = of(agents = t1, var = "ycor")),
                    of(agents = t5, var = c("xcor", "ycor")))

  # Argument out
  t3out <- fd(world = w1, turtles = t1, dist = 5, torus = FALSE, out = FALSE)
  expect_equivalent(of(agents = t3out, var = c("xcor", "ycor")), of(agents = t1,
                                                                    var = c("xcor", "ycor")))

  t6 <- createTurtles(n = 2, coords = cbind(xcor = 0, ycor = 0), heading = 90)
  t71 <- fd(world = w1, turtles = t6, dist = c(5, 1), torus = FALSE, out = TRUE)
  t72 <- fd(world = w1, turtles = t6, dist = c(5, 1), torus = FALSE, out = FALSE)
  t73 <- fd(world = w1, turtles = t6, dist = c(5, 1), torus = TRUE, out = TRUE)
  t74 <- fd(world = w1, turtles = t6, dist = c(5, 1), torus = TRUE, out = FALSE)
  expect_equivalent(of(agents = t71, var = c("xcor", "ycor")), cbind(xcor = c(5, 1),
                                                                     ycor = c(0, 0)))
  expect_equivalent(of(agents = t72, var = c("xcor", "ycor")), cbind(xcor = c(0, 1),
                                                                     ycor = c(0, 0)))
  expect_equivalent(of(agents = t73, var = c("xcor", "ycor")), cbind(xcor = c(0, 1),
                                                                     ycor = c(0, 0)))
  expect_equivalent(of(agents = t74, var = c("xcor", "ycor")), cbind(xcor = c(0, 1),
                                                                     ycor = c(0, 0)))

  # Work without world provided when torus = FALSE and out = TRUE
  t2 <- fd(turtles = t1, dist = 1)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 1,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t2,
                                                                    var = c("xcor", "ycor")))
  t3 <- fd(turtles = t1, dist = 5, torus = FALSE, out = TRUE)
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 5,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t3,
                                                                    var = c("xcor", "ycor")))
  expect_error(fd(turtles = t1, dist = 5, torus = TRUE))
  expect_error(fd(turtles = t1, dist = -1, torus = TRUE))
  expect_error(fd(turtles = t1, dist = 5, torus = TRUE))
  expect_error(fd(turtles = t1, dist = 5, torus = FALSE, out = FALSE))
  t71 <- fd(turtles = t6, dist = c(5, 1), torus = FALSE, out = TRUE)
  expect_equivalent(of(agents = t71, var = c("xcor", "ycor")), cbind(xcor = c(5, 1),
                                                                     ycor = c(0, 0)))
  expect_error(fd(turtles = t6, dist = c(5, 1), torus = FALSE, out = FALSE))
  expect_error(fd(turtles = t6, dist = c(5, 1), torus = TRUE, out = TRUE))
  expect_error(fd(wturtles = t6, dist = c(5, 1), torus = TRUE, out = FALSE))
})

test_that("bk works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)), heading = 90)
  t2 <- bk(world = w1, turtles = t1, dist = -1)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 1,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t2,
                                                                    var = c("xcor", "ycor")))
  t3 <- bk(world = w1, turtles = t1, dist = -5, torus = FALSE)
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 5,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t3,
                                                                    var = c("xcor", "ycor")))
  t4 <- bk(world = w1, turtles = t1, dist = -5, torus = TRUE)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t4,
                                                                 var = c("xcor", "ycor")))
  t5 <- bk(world = w1, turtles = t1, dist = 1, torus = TRUE)
  expect_equivalent(cbind(xcor = rep(4, 10), ycor = of(agents = t1, var = "ycor")),
                    of(agents = t5, var = c("xcor", "ycor")))

  # Argument out
  t3out <- bk(world = w1, turtles = t1, dist = -5, torus = FALSE, out = FALSE)
  expect_equivalent(of(agents = t3out, var = c("xcor", "ycor")), of(agents = t1,
                                                                    var = c("xcor", "ycor")))

  t6 <- createTurtles(n = 2, coords = cbind(xcor = 4, ycor = 0), heading = 90)
  t71 <- bk(world = w1, turtles = t6, dist = c(5, 1), torus = FALSE, out = TRUE)
  t72 <- bk(world = w1, turtles = t6, dist = c(5, 1), torus = FALSE, out = FALSE)
  t73 <- bk(world = w1, turtles = t6, dist = c(5, 1), torus = TRUE, out = TRUE)
  t74 <- bk(world = w1, turtles = t6, dist = c(5, 1), torus = TRUE, out = FALSE)
  expect_identical(of(agents = t71, var = c("xcor", "ycor")), cbind(xcor = c(-1, 3),
                                                                    ycor = c(0, 0)))
  expect_identical(of(agents = t72, var = c("xcor", "ycor")), cbind(xcor = c(4, 3),
                                                                    ycor = c(0, 0)))
  expect_identical(of(agents = t73, var = c("xcor", "ycor")), cbind(xcor = c(4, 3),
                                                                    ycor = c(0, 0)))
  expect_identical(of(agents = t74, var = c("xcor", "ycor")), cbind(xcor = c(4, 3),
                                                                    ycor = c(0, 0)))

  # Works without world provided when torus = FALSE and out = TRUE
  t2 <- bk(turtles = t1, dist = -1)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 1,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t2,
                                                                    var = c("xcor", "ycor")))
  t3 <- bk(turtles = t1, dist = -5, torus = FALSE)
  expect_equivalent(cbind(xcor = of(agents = t1, var = "xcor") + 5,
                          ycor = of(agents = t1, var = "ycor")), of(agents = t3,
                                                                    var = c("xcor", "ycor")))
  expect_error(bk(turtles = t1, dist = -5, torus = TRUE))
  expect_error(bk(turtles = t1, dist = 1, torus = TRUE))
  expect_error(bk(turtles = t1, dist = -5, torus = FALSE, out = FALSE))
  t71 <- bk(turtles = t6, dist = c(5, 1), torus = FALSE, out = TRUE)
  expect_identical(of(agents = t71, var = c("xcor", "ycor")), cbind(xcor = c(-1, 3),
                                                                    ycor = c(0, 0)))
  expect_error(bk(turtles = t6, dist = c(5, 1), torus = FALSE, out = FALSE))
  expect_error(bk(turtles = t6, dist = c(5, 1), torus = TRUE, out = TRUE))
  expect_error(bk(turtles = t6, dist = c(5, 1), torus = TRUE, out = FALSE))
})

test_that("home works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 10, coords = cbind(xcor = runif(10, 0, 4), ycor = runif(10, 0, 4)))
  t2 <- home(world = w1, turtles = t1, home = "home0")
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), of(agents = t2,
                                                                   var = c("xcor", "ycor")))
  t3 <- home(world = w1, turtles = t1, home = "center")
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), of(agents = t3,
                                                                   var = c("xcor", "ycor")))
  t4 <- home(world = w1, turtles = t1, home = "pCorner")
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), of(agents = t4,
                                                                   var = c("xcor", "ycor")))
  t5 <- home(world = w1, turtles = t1, home = "corner")
  expect_identical(cbind(xcor = rep(-0.5, 10), ycor = rep(-0.5, 10)), of(agents = t5,
                                                                         var = c("xcor", "ycor")))

  w3 <- createWorld(minPxcor = -5, maxPxcor = -1, minPycor = -10, maxPycor = -5)
  expect_error(home(world = w3, turtles = t1, home = "home0"))
})

test_that("dx and dy work", {
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 90)
  expect_equivalent(dx(turtles = t1), 1)
  expect_equivalent(dx(turtles = t1, dist = 2), 2)
  expect_equivalent(dy(turtles = t1), 0)
  expect_equivalent(dy(turtles = t1, dist = 2), 0)
  t2 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 0)
  expect_equivalent(dx(turtles = t2), 0)
  expect_equivalent(dx(turtles = t2, dist = 2), 0)
  expect_equivalent(dy(turtles = t2), 1)
  expect_equivalent(dy(turtles = t2, dist = 2), 2)
  t3 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 225)
  expect_equivalent(dx(turtles = t3, dist = sqrt(2)), -1)
  expect_equivalent(dy(turtles = t3, dist = sqrt(2)), -1)
})

test_that("die works", {
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1))
  t2 <- die(turtles = t1, who = 1:9)
  expect_equivalent(NLcount(t2), 1)
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 1, ycor = 10))
  expect_equivalent(of(agents = t2, var = "who"), 0)
  t3 <- die(turtles = t1, who = 0)
  expect_equivalent(NLcount(t3), 9)
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = 2:10, ycor = 9:1))
  expect_equivalent(of(agents = t3, var = "who"), 1:9)
  t4 <- die(turtles = t1, who = numeric(0))
  expect_identical(t4, t1)
})

test_that("hatch works", {
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1))
  t2 <- hatch(turtles = t1, who = 1, n = 1)
  expect_equivalent(NLcount(t2), NLcount(t1) + 1)
  expect_equivalent(of(agents = turtle(t2, 10), var = c("xcor", "ycor")), cbind(2, 9))
  expect_equivalent(of(agents = turtle(t2, 10), var = "heading"), of(agents = turtle(t1, 1),
                                                                     var = "heading"))
  expect_identical(of(agents = turtle(t2, 10), var = "breed"), "turtle")
  t3 <- hatch(turtles = t1, who = 4, n = 2, breed = "young")
  expect_equivalent(NLcount(t3), NLcount(t1) + 2)
  expect_equivalent(of(agents = turtle(t3, c(10, 11)), var = c("xcor", "ycor")),
                    cbind(xcor = c(5, 5), ycor = c(6, 6)))
  expect_equivalent(of(agents = turtle(t3, c(10, 11)), var = "heading"),
                    rep(of(agents = turtle(t1, 4), var = "heading"), 2))
  expect_identical(of(agents = turtle(t3, c(10, 11)), var = "breed"), c("young", "young"))

  # Several turtle parents
  t4 <- hatch(turtles = t1, who = c(1, 4), n = 2)
  expect_equivalent(NLcount(t4), 14)
  expect_equivalent(of(agents = t4, var = "who"), 0:13)
  expect_equivalent(of(agents = turtle(t4, c(10:13)), var = c("xcor", "ycor")),
                    cbind(xcor = c(2, 2, 5, 5), ycor = c(9, 9, 6, 6)))
  expect_equivalent(of(agents = turtle(t4, c(10:13)), var = "breed"), rep("turtle", 4))
  t5 <- hatch(turtles = t1, who = c(1, 4), n = 2, breed = "young")
  expect_equivalent(NLcount(t5), 14)
  expect_equivalent(of(agents = t5, var = "who"), 0:13)
  expect_equivalent(of(agents = turtle(t5, c(10:13)), var = c("xcor", "ycor")),
                    cbind(xcor = c(2, 2, 5, 5), ycor = c(9, 9, 6, 6)))
  expect_equivalent(of(agents = turtle(t5, c(10:13)), var = "breed"), rep("young", 4))

  # Different numbers of young
  t6 <- hatch(turtles = t1, who = c(1, 4), n = c(2, 3))
  expect_equivalent(NLcount(t6), 15)
  expect_equivalent(of(agents = t6, var = "who"), 0:14)
  expect_equivalent(of(agents = turtle(t6, c(10:14)), var = c("xcor", "ycor")),
                    cbind(xcor = c(2, 2, 5, 5, 5), ycor = c(9, 9, 6, 6, 6)))
  expect_equivalent(of(agents = turtle(t6, c(10:14)), var = "breed"), rep("turtle", 5))
  t7 <- hatch(turtles = t1, who = c(1, 4), n = c(2, 0), breed = "young")
  expect_equivalent(NLcount(t7), 12)
  expect_equivalent(of(agents = t7, var = "who"), 0:11)
  expect_equivalent(of(agents = turtle(t7, c(10:11)), var = c("xcor", "ycor")),
                    cbind(xcor = c(2, 2), ycor = c(9, 9)))
  expect_equivalent(of(agents = turtle(t7, c(10:11)), var = "breed"), rep("young", 2))
})

test_that("canMove works", {
  w1 <- createWorld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
  t1 <- createTurtles(world = w1, n = 4, heading = c(0, 90, 180, 270))
  expect_identical(canMove(world = w1, turtles = t1, dist = 1), rep(TRUE, 4))
  expect_identical(canMove(world = w1, turtles = t1, dist = 4), rep(TRUE, 4))
  expect_identical(canMove(world = w1, turtles = t1, dist = 6), rep(FALSE, 4))
  expect_identical(canMove(world = w1, turtles = t1, dist = c(1, 4, 6, 4)),
                   c(TRUE, TRUE, FALSE, TRUE))
})

test_that("randomXcor and randomYcor work", {
  w1 <- createWorld(minPxcor = 1, maxPxcor = 100, minPycor = -100, maxPycor = -1)
  t1 <- createTurtles(n = 10000,
                      coords = cbind(xcor = randomXcor(world = w1, n = 10000),
                                     ycor = randomYcor(world = w1, n = 10000)))
  expect_identical(canMove(world = w1, turtles = t1, dist = 0), rep(TRUE, NLcount(t1)))

  w2 <- w1
  w1[] <- runif(10000)
  w2[] <- runif(10000)
  ws <- stackWorlds(w1, w2)
  t2 <- createTurtles(n = 10000,
                      coords = cbind(xcor = randomXcor(world = ws, n = 10000),
                                     ycor = randomYcor(world = ws, n = 10000)))
  expect_identical(canMove(world = ws, turtles = t2, dist = 0), rep(TRUE, NLcount(t2)))

  expect_equivalent(randomXcor(world = w1, n = 0), numeric())
  expect_equivalent(randomYcor(world = w1, n = 0), numeric())
})

test_that("towards works", {
  w1 <- createWorld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)

  # Patches to turtle
  t1 <- createTurtles(world = w1, n = 1)
  pTOt <- towards(world = w1, agents = patches(world = w1), agents2 = t1, torus = FALSE)
  expect_equivalent(pTOt[100], 315)
  t2 <- createTurtles(n = 1, coord = cbind(xcor = 5.5, ycor = 10.5))
  pTOt <- towards(world = w1, agents = patches(world = w1), agents2 = t2, torus = TRUE)
  expect_equivalent(pTOt[95], 135)

  # Turtles to patch
  t3 <- createTurtles(n = 4, coords = cbind(xcor = c(2, 5, 6, 7), ycor = c(4, 6, 2, 9)))
  tTOp <- towards(world = w1, agents = t3, agents2 = patch(world = w1, x = 5, y = 4),
                  torus = FALSE)
  expect_equivalent(tTOp[1], 90)
  tTOp <- towards(world = w1, agents = t3, agents2 = patch(world = w1, x = 7, y = 2),
                  torus = TRUE)
  expect_equivalent(tTOp[4], 0)

  # Turtles to location
  tTOl <- towards(world = w1, agents = t3, agents2 = cbind(x = 8, y = 4), torus = FALSE)
  expect_equivalent(tTOl[1], 90)
  tTOl <- towards(world = w1, agents = t3, agents2 = cbind(x = 8, y = 4), torus = FALSE)
  expect_equivalent(tTOl[3], 45)
  tTOl <- towards(world = w1, agents = t3, agents2 = cbind(x = 8, y = 4), torus = TRUE)
  expect_equivalent(tTOl[1], 270)

  # Turtles to turtle
  tTOt <- towards(world = w1, agents = t3, agents2 = t3, torus = FALSE)
  expect_equivalent(tTOt, of(agents = t3, var = "heading"))
  tTOt <- towards(world = w1, agents = t3, agents2 = t2, torus = FALSE)
  expect_equivalent(tTOt[4], 315)
  t4 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 4), heading = 25)
  tTOt <- towards(world = w1, agents = t4, agents2 = t3, torus = FALSE)
  expect_equivalent(tTOt[1], of(agents = t4, var = "heading"))
  tTOt <- towards(world = w1, agents = t4, agents2 = cbind(x = 8, y = 4), torus = FALSE)
  expect_equivalent(tTOt, 90)


  # Works without world provided when torus = FALSE
  expect_error(towards(agents = patches(world = w1), agents2 = t2, torus = TRUE))
  tTOp <- towards(agents = t3, agents2 = patch(world = w1, x = 5, y = 4), torus = FALSE)
  expect_equivalent(tTOp[1], 90)
  expect_error(towards(agents = t3, agents2 = patch(world = w1, x = 7, y = 2), torus = TRUE))
  tTOl <- towards(agents = t3, agents2 = cbind(x = 8, y = 4), torus = FALSE)
  expect_equivalent(tTOl[1], 90)
  expect_equivalent(tTOl[3], 45)
  expect_error(towards(agents = t3, agents2 = cbind(x = 8, y = 4), torus = TRUE))
  tTOt <- towards(agents = t3, agents2 = t3, torus = FALSE)
  expect_equivalent(tTOt, of(agents = t3, var = "heading"))
  tTOt <- towards(agents = t3, agents2 = t2, torus = FALSE)
  expect_equivalent(tTOt[4], 315)
})

test_that("face works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 5)
  t2 <- face(world = w1, turtles = t1, agents2 = cbind(x = 2, y = 0))
  expect_equivalent(of(agents = t2, var = "heading"), rep(180, 5))
  t3 <- face(world = w1, turtles = t1, agents2 = patch(world = w1, x = 0, y = 2))
  expect_equivalent(of(agents = t3, var = "heading"), rep(270, 5))
  t4 <- createTurtles(n = 1, coords = cbind(xcor = 1, ycor = 3))
  t5 <- face(world = w1, turtles = t1, agents2 = t4)
  expect_equivalent(of(agents = t5, var = "heading"), rep(315, 5))
  t6 <- face(world = w1, turtles = t4, agents2 = cbind(x = 1, y = 0), torus = FALSE)
  expect_equivalent(of(agents = t6, var = "heading"), 180)
  t7 <- face(world = w1, turtles = t4, agents2 = cbind(x = 1, y = 0), torus = TRUE)
  expect_equivalent(of(agents = t7, var = "heading"), 0)
  t8 <- face(world = w1, turtles = t1, agents2 = t1)
  expect_equivalent(of(agents = t8, var = "heading"), of(agents = t1, var = "heading"))

  # Works without world provided when torus = FALSE
  t2 <- face(turtles = t1, agents2 = cbind(x = 2, y = 0))
  expect_equivalent(of(agents = t2, var = "heading"), rep(180, 5))
  t3 <- face(turtles = t1, agents2 = patch(world = w1, x = 0, y = 2))
  expect_equivalent(of(agents = t3, var = "heading"), rep(270, 5))
  t5 <- face(turtles = t1, agents2 = t4)
  expect_equivalent(of(agents = t5, var = "heading"), rep(315, 5))
  t6 <- face(turtles = t4, agents2 = cbind(x = 1, y = 0), torus = FALSE)
  expect_error(face(turtles = t4, agents2 = cbind(x = 1, y = 0), torus = TRUE))
  expect_equivalent(of(agents = t6, var = "heading"), 180)
  t8 <- face(turtles = t1, agents2 = t1)
  expect_equivalent(of(agents = t8, var = "heading"), of(agents = t1, var = "heading"))
})

test_that("left and right work", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 4, heading = c(0, 90, 180, 270))
  t2 <- left(turtles = t1, angle = 45)
  expect_identical(of(agents = t2, var = "heading"), c(315, 45, 135, 225))
  t3 <- right(turtles = t2, angle = 45)
  expect_identical(of(agents = t3, var = "heading"), of(agents = t1, var = "heading"))
})

test_that("downhill works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2))
  t2 <- downhill(world = w1, turtles = t1, nNeighbors = 4)
  t3 <- downhill(world = w1, turtles = t1, nNeighbors = 8)
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 3))
  expect_equivalent(of(agents = t2, var = "heading"), 0)
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = 1, ycor = 3))
  expect_equivalent(of(agents = t3, var = "heading"), 315)
  t4 <- downhill(world = w1, turtles = t3, nNeighbors = 8)
  t5 <- downhill(world = w1, turtles = t4, nNeighbors = 8)
  expect_equivalent(of(agents = t4, var = c("xcor", "ycor")), of(agents = t5,
                                                                 var = c("xcor", "ycor")))
  t6 <- createTurtles(n = 1, coords = cbind(xcor = 1, ycor = 0))
  t7 <- downhill(world = w1, turtles = t6, nNeighbors = 8)
  t8 <- downhill(world = w1, turtles = t6, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t7, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 1))
  expect_equivalent(of(agents = t8, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 4))
  t9 <- createTurtles(n = 2, coords = cbind(xcor = c(1, 1), ycor = c(0, 1)))
  t10 <- downhill(world = w1, turtles = t9, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t10, var = "heading"), c(225, 315))
  expect_equivalent(of(agents = t10, var = c("xcor", "ycor")), cbind(xcor = c(0, 0),
                                                                     ycor = c(4, 2)))

  w2 <- w1
  w2[] <- 25:1
  ws <- stackWorlds(w1, w2)
  t2 <- downhill(world = ws, pVar = "w1", turtles = t1, nNeighbors = 4)
  t3 <- downhill(world = ws, pVar = "w1", turtles = t1, nNeighbors = 8)
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 3))
  expect_equivalent(of(agents = t2, var = "heading"), 0)
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = 1, ycor = 3))
  expect_equivalent(of(agents = t3, var = "heading"), 315)
  t4 <- downhill(world = ws, pVar = "w1", turtles = t3, nNeighbors = 8)
  t5 <- downhill(world = ws, pVar = "w1", turtles = t4, nNeighbors = 8)
  expect_equivalent(of(agents = t4, var = c("xcor", "ycor")), of(agents = t5,
                                                                 var = c("xcor", "ycor")))
  t6 <- createTurtles(n = 1, coords = cbind(xcor = 1, ycor = 0))
  t7 <- downhill(world = ws, pVar = "w1", turtles = t6, nNeighbors = 8)
  t8 <- downhill(world = ws, pVar = "w1", turtles = t6, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t7, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 1))
  expect_equivalent(of(agents = t8, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 4))
  t9 <- createTurtles(n = 2, coords = cbind(xcor = c(1, 1), ycor = c(0, 1)))
  t10 <- downhill(world = ws, pVar = "w1", turtles = t9, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t10, var = "heading"), c(225, 315))
  expect_equivalent(of(agents = t10, var = c("xcor", "ycor")), cbind(xcor = c(0, 0),
                                                                     ycor = c(4, 2)))

  # The turtle on the min patch does not move
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- 10
  w1[0, 0] <- 0
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
  t2 <- downhill(world = w1, turtles = t1, nNeighbors = 4)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t1, var = "heading"), of(agents = t2, var = "heading"))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))

  # The function handles dupliacate patch values
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2))
  t2 <- downhill(world = w1, turtles = t1, nNeighbors = 4)
  expect_equivalent(nrow(merge(of(agents = t2, var = c("xcor", "ycor")),
                               patch(w1, c(1, 2, 2, 2, 3), c(2, 1, 2, 3, 2)),
                               by.x = c("xcor", "ycor"), by.y = c("pxcor", "pycor"))), 1)
})

test_that("uphill works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4, data = 1:25)
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2))
  t2 <- uphill(world = w1, turtles = t1, nNeighbors = 4)
  t3 <- uphill(world = w1, turtles = t1, nNeighbors = 8)
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 1))
  expect_equivalent(of(agents = t2, var = "heading"), 180)
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = 3, ycor = 1))
  expect_equivalent(of(agents = t3, var = "heading"), 135)
  t4 <- uphill(world = w1, turtles = t3, nNeighbors = 8)
  t5 <- uphill(world = w1, turtles = t4, nNeighbors = 8)
  expect_equivalent(of(agents = t4, var = c("xcor", "ycor")), of(agents = t5,
                                                                 var = c("xcor", "ycor")))
  t6 <- createTurtles(n = 1, coords = cbind(xcor = 1, ycor = 4))
  t7 <- uphill(world = w1, turtles = t6, nNeighbors = 8)
  t8 <- uphill(world = w1, turtles = t6, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t7, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 3))
  expect_equivalent(of(agents = t8, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 0))
  t9 <- createTurtles(n = 2, coords = cbind(xcor = c(1, 1), ycor = c(0, 1)))
  t10 <- uphill(world = w1, turtles = t9, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t10, var = "heading"), c(90, 135))
  expect_equivalent(of(agents = t10, var = c("xcor", "ycor")), cbind(xcor = c(2, 2),
                                                                     ycor = c(0, 0)))

  w2 <- w1
  w2[] <- 25:1
  ws <- stackWorlds(w1, w2)
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2))
  t2 <- uphill(world = ws, pVar = "w1", turtles = t1, nNeighbors = 4)
  t3 <- uphill(world = ws, pVar = "w1", turtles = t1, nNeighbors = 8)
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 1))
  expect_equivalent(of(agents = t2, var = "heading"), 180)
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = 3, ycor = 1))
  expect_equivalent(of(agents = t3, var = "heading"), 135)
  t4 <- uphill(world = ws, pVar = "w1", turtles = t3, nNeighbors = 8)
  t5 <- uphill(world = ws, pVar = "w1", turtles = t4, nNeighbors = 8)
  expect_identical(of(agents = t4, var = c("xcor", "ycor")), of(agents = t5,
                                                                var = c("xcor", "ycor")))
  t6 <- createTurtles(n = 1, coords = cbind(xcor = 1, ycor = 4))
  t7 <- uphill(world = ws, pVar = "w1", turtles = t6, nNeighbors = 8)
  t8 <- uphill(world = ws, pVar = "w1", turtles = t6, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t7, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 3))
  expect_equivalent(of(agents = t8, var = c("xcor", "ycor")), cbind(xcor = 2, ycor = 0))
  t9 <- createTurtles(n = 2, coords = cbind(xcor = c(1, 1), ycor = c(0, 1)))
  t10 <- uphill(world = ws, pVar = "w1", turtles = t9, nNeighbors = 8, torus = TRUE)
  expect_equivalent(of(agents = t10, var = "heading"), c(90, 135))
  expect_equivalent(of(agents = t10, var = c("xcor", "ycor")), cbind(xcor = c(2, 2),
                                                                     ycor = c(0, 0)))

  # The turtle on the max patch does not move
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- 0
  w1[0, 0] <- 10
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
  t2 <- uphill(world = w1, turtles = t1, nNeighbors = 4)
  expect_identical(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                var = c("xcor", "ycor")))
  expect_identical(of(agents = t1, var = "heading"), of(agents = t2, var = "heading"))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))

  # The function handles dupliacate patch values
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2))
  t2 <- uphill(world = w1, turtles = t1, nNeighbors = 4)
  expect_equivalent(nrow(merge(of(agents = t2, var = c("xcor", "ycor")),
                               patch(w1, c(1, 2, 2, 2, 3), c(2, 1, 2, 3, 2)),
                               by.x = c("xcor", "ycor"), by.y = c("pxcor", "pycor"))), 1)
})

test_that("patchAhead works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4), heading = 0)
  pAhead <- patchAhead(world = w1, turtles = t1, dist = 2, torus = FALSE)
  expect_identical(pAhead, cbind(pxcor = c(0, 1, 2, NA, NA), pycor = c(2, 3, 4, NA, NA)))
  pAhead <- patchAhead(world = w1, turtles = t1, dist = 2, torus = TRUE)
  expect_identical(pAhead, cbind(pxcor = c(0, 1, 2, 3, 4), pycor = c(2, 3, 4, 0, 1)))
})

test_that("patchHere works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4))
  pTurtles <- patchHere(world = w1, turtles = t1)
  expect_identical(pTurtles, cbind(pxcor = c(0, 1, 2, 3, 4), pycor = c(0, 1, 2, 3, 4)))
  expect_identical(pTurtles, patch(world = w1, x = 0:4, y = 0:4, duplicate = TRUE, out = TRUE))
  t2 <- createTurtles(n = 3, coords = cbind(xcor = 4:6, ycor = 4:6))
  pTurtles <- patchHere(world = w1, turtles = t2)
  expect_identical(pTurtles, cbind(pxcor = c(4, NA, NA), pycor = c(4, NA, NA)))
  expect_identical(pTurtles, patch(world = w1, x = 4:6, y = 4:6, duplicate = TRUE, out = TRUE))
})

test_that("patchLeft and patchRight work", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4), heading = 0)
  pLeft <- patchLeft(world = w1, turtles = t1, dist = 1, angle = 45, torus = FALSE)
  expect_identical(pLeft, cbind(pxcor = c(NA, 0, 1, 2, NA), pycor = c(NA, 2, 3, 4, NA)))
  pLeft <- patchLeft(world = w1, turtles = t1, dist = 1, angle = 45, torus = TRUE)
  expect_identical(pLeft, cbind(pxcor = c(4, 0, 1, 2, 3), pycor = c(1, 2, 3, 4, 0)))

  pRight <- patchRight(world = w1, turtles = t1, dist = 1, angle = 45, torus = FALSE)
  expect_identical(pRight, cbind(pxcor = c(1, 2, 3, 4, NA), pycor = c(1, 2, 3, 4, NA)))
  pRight <- patchRight(world = w1, turtles = t1, dist = 1, angle = 45, torus = TRUE)
  expect_identical(pRight, cbind(pxcor = c(1, 2, 3, 4, 0), pycor = c(1, 2, 3, 4, 0)))

  pRight <- patchRight(world = w1, turtles = t1, dist = 1, angle = -45, torus = TRUE)
  expect_identical(pLeft, pRight)

  t2 <- createTurtles(n = 2, coords = cbind(xcor = 2, ycor = 2), heading = 180)
  pLeft <- patchLeft(world = w1, turtles = t2, dist = c(2, 0.8), angle = c(90, -90))
  expect_identical(pLeft, cbind(pxcor = c(4, 1), pycor = c(2, 2)))
  pRight <- patchRight(world = w1, turtles = t2, dist = c(2, 0.8), angle = c(90, -90))
  expect_identical(pRight, cbind(pxcor = c(0, 3), pycor = c(2, 2)))
})

test_that("setXY works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4))
  t2 <- setXY(turtles = t1, xcor = 1, ycor = 1, torus = FALSE)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = rep(1, 5),
                                                                    ycor = rep(1, 5)))
  t2 <- setXY(turtles = t1, xcor = 1, ycor = 1)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = rep(1, 5),
                                                                    ycor = rep(1, 5)))
  t3 <- setXY(world = w1, turtles = t1, xcor = c(1, 1, 1, 1, 5), ycor = rep(1, 5), torus = TRUE)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t3,
                                                                 var = c("prevX", "prevY")))
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = c(1, 1, 1, 1, 0),
                                                                    ycor = rep(1, 5)))
})

test_that("sprout works", {
  t1 <- sprout(patches = cbind(pxcor = 2, pycor = 2), n = 3)
  t2 <- sprout(patches = cbind(pxcor = c(1, 2, 3), pycor = c(1, 2, 3)), n = 3)
  t3 <- sprout(patches = cbind(pxcor = 3, pycor = 3), n = 3, turtles = t1)
  t4 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = 2, turtles = t1)
  t5 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = 2, turtles = t1,
               breed = "wolf", heading = c(0, 180))
  t6 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = 2, turtles = t1,
               breed = "wolf", heading = 90)
  expect_identical(of(agents = t1, var = c("xcor", "ycor")), cbind(xcor = c(2, 2, 2),
                                                                   ycor = c(2, 2, 2)))
  expect_identical(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = c(1, 2, 3),
                                                                   ycor = c(1, 2, 3)))
  expect_identical(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = c(2, 2, 2, 3, 3, 3),
                                                                   ycor = c(2, 2, 2, 3, 3, 3)))
  expect_identical(of(agents = t4, var = c("xcor", "ycor")), cbind(xcor = c(2, 2, 2, 3, 2),
                                                                   ycor = c(2, 2, 2, 0, 3)))
  expect_identical(of(agents = t4, var = c("xcor", "ycor")), of(agents = t5,
                                                                var = c("xcor", "ycor")))
  expect_equivalent(length(unique(of(agents = t2, var = "who"))), NLcount(t2))
  expect_equivalent(length(unique(of(agents = t3, var = "who"))), NLcount(t3))
  expect_identical(of(agents = t5, var = "breed"), c(of(agents = t1, var = "breed"),
                                                     "wolf", "wolf"))
  expect_equivalent(length(unique(t4@levels$breed)), 1)
  expect_equivalent(length(unique(t5@levels$breed)), 2)
  expect_equivalent(of(agents = t5, var = c("xcor", "ycor")), of(agents = t6,
                                                                var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t6, var = "heading")[c(4, 5)], c(90, 90))

  # When length(n) != 0
  t7 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = c(2, 3))
  expect_equivalent(NLcount(t7), 5)
  expect_equivalent(cbind(xcor = c(3, 3, 2, 2, 2), ycor = c(0, 0, 3, 3, 3)),
                    of(agents = t7, var = c("xcor", "ycor")))
  t8 <- sprout(patches = cbind(pxcor = c(1, 1), pycor = c(0, 1)), n = c(2, 1), breed = "wolf",
               turtles = t7)
  expect_equivalent(NLcount(t8), 8)
  expect_equivalent(cbind(xcor = c(3, 3, 2, 2, 2, 1, 1, 1), ycor = c(0, 0, 3, 3, 3, 0, 0, 1)),
                    of(agents = t8, var = c("xcor", "ycor")))
  expect_equivalent(c(rep("turtle", 5), rep("wolf", 3)), of(agents = t8, var = "breed"))
  t9 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = c(2, 3), heading = c(0, 90))
  expect_equivalent(c(0, 0, 90, 90, 90), of(agents = t9, var = "heading"))
  t10 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = c(2, 3), heading = 90,
                breed = c("wolf", "sheep"), color = c("black", "white"))
  expect_equivalent(c(90, 90, 90, 90, 90), of(agents = t10, var = "heading"))
  expect_equivalent(c(rep("wolf", 2), rep("sheep", 3)), of(agents = t10, var = "breed"))
  expect_equivalent(c(rep("black", 2), rep("white", 3)), of(agents = t10, var = "color"))
  t11 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = c(2, 3), color = "blue")
  expect_equivalent(rep("blue", 5), of(agents = t11, var = "color"))

  # When turtles have additional variables
  t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c("F", "F", "M"))
  t12 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = c(2, 3), breed = "sheep",
                turtles = t1)
  expect_equivalent(c(rep("turtle", 3), rep("sheep", 5)), of(agents = t12, var = "breed"))
  expect_equivalent(c("F", "F", "M", rep(NA, 5)), of(agents = t12, var = "sex"))
  t1 <- turtlesOwn(turtles = t1, tVar = "age", tVal = c(1, 2, 3))
  t12 <- sprout(patches = cbind(pxcor = c(3, 2), pycor = c(0, 3)), n = c(2, 3), breed = "sheep",
                turtles = t1)
  expect_equivalent(c("F", "F", "M", rep(NA, 5)), of(agents = t12, var = "sex"))
  expect_equivalent(c(1, 2, 3, rep(NA, 5)), of(agents = t12, var = "age"))
})

test_that("inspect works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 4, heading = c(0, 90, 180, 270))
  inspectT11 <- inspect(turtles = t1, who = 1)
  dfT11 <- cbind.data.frame(who = 1, heading = 90, prevX = as.numeric(NA),
                            prevY = as.numeric(NA), xcor = 2, ycor = 2)
  expect_equivalent(dfT11, inspectT11[, c("who", "heading", "prevX", "prevY", "xcor",
                                          "ycor")]) # color removed b/c unknown

  inspectT112 <- inspect(turtles = t1, who = c(1, 2))
  dfT112 <- cbind.data.frame(who = c(1, 2), heading = c(90, 180),
                             prevX = as.numeric(c(NA, NA)), prevY = as.numeric(c(NA, NA)),
                             xcor = c(2, 2), ycor = c(2, 2))
  expect_equivalent(dfT112, inspectT112[, c("who", "heading", "prevX", "prevY", "xcor",
                                            "ycor")]) # color removed b/c unknown
})

test_that("moveTo works", {
  t1 <- createTurtles(n = 4, coords = cbind(xcor = c(1, 2, 3, 4), ycor = c(1, 2, 3, 4)))
  t2 <- moveTo(turtles = t1, agents = turtle(t1, who = 3))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = c(4, 4, 4, 4),
                                                                    ycor = c(4, 4, 4, 4)))
  expect_equivalent(of(agents = t2, var = c("prevX", "prevY")), of(agents = t1,
                                                                   var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t2, var = c("who", "heading", "breed", "color")),
                    of(agents = t1, var = c("who", "heading", "breed", "color")))
  t3 <- moveTo(turtles = t1, agents = t1)
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t3,
                                                                 var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t3, var = c("prevX", "prevY")), of(agents = t1,
                                                                   var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t3, var = c("who", "heading", "breed", "color")),
                    of(agents = t1, var = c("who", "heading", "breed", "color")))

  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 4, coords = cbind(xcor = c(1, 2, 3, 4), ycor = c(1, 2, 3, 4)))
  t2 <- moveTo(turtles = t1, agents = patch(world = w1, x = 4, y = 4))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = c(4, 4, 4, 4),
                                                                    ycor = c(4, 4, 4, 4)))
  expect_equivalent(of(agents = t2, var = c("prevX", "prevY")), of(agents = t1,
                                                                   var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t2, var = c("who", "heading", "breed", "color")),
                    of(agents = t1, var = c("who", "heading", "breed", "color")))
  t3 <- moveTo(turtles = t1, agents = patch(world = w1, x = c(1, 2, 3, 4), y = c(1, 2, 3, 4)))
  expect_equivalent(of(agents = t1, var = c("xcor", "ycor")), of(agents = t3,
                                                                 var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t3, var = c("prevX", "prevY")), of(agents = t1,
                                                                   var = c("xcor", "ycor")))
  expect_equivalent(of(agents = t3, var = c("who", "heading", "breed", "color")),
                    of(agents = t1, var = c("who", "heading", "breed", "color")))
})

test_that("randomXYcor works", {
  set.seed(20180924)

  w1 <- createWorld(minPxcor = 1, maxPxcor = 100, minPycor = -100, maxPycor = -1)
  t1 <- createTurtles(n = 10000, coords = randomXYcor(world = w1, n = 10000))
  expect_identical(canMove(world = w1, turtles = t1, dist = 0), rep(TRUE, NLcount(t1)))

  w2 <- w1
  w1[] <- runif(10000)
  w2[] <- runif(10000)
  ws <- stackWorlds(w1, w2)
  t2 <- createTurtles(n = 10000, coords = randomXYcor(world = w1, n = 10000))
  expect_identical(canMove(world = ws, turtles = t2, dist = 0), rep(TRUE, NLcount(t2)))
})

test_that("tExist works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), breed = c(rep("sheep", 5),
                                                                                  rep("wolf", 5)))
  expect_identical(tExist(turtles = t1, who = 3), TRUE)
  expect_identical(tExist(turtles = t1, who = 3, breed = "sheep"), TRUE)
  expect_identical(tExist(turtles = t1, who = 9, breed = "sheep"), FALSE)
  expect_identical(tExist(turtles = t1, who = 9, breed = "moose"), FALSE)
  expect_identical(tExist(turtles = t1, who = c(3, 9)), c(TRUE, TRUE))
  expect_identical(tExist(turtles = t1, who = c(3, 9), breed = "sheep"), c(TRUE, FALSE))
  expect_identical(tExist(turtles = t1, who = c(9, 3), breed = "sheep"), c(FALSE, TRUE))
  expect_identical(tExist(turtles = t1, who = c(3, 9), breed = c("wolf", "sheep")), c(TRUE, TRUE))
  expect_identical(tExist(turtles = t1, who = c(3, 9), breed = "wolf"), c(FALSE, TRUE))
  expect_identical(tExist(turtles = t1, who = c(3, 9), breed = c("sheep", "wolf")), c(TRUE, TRUE))
  expect_identical(tExist(turtles = t1, who = c(3, 11, 9)), c(TRUE, FALSE, TRUE))
  expect_identical(tExist(turtles = t1, who = c(3, 11, 9), breed = "sheep"), c(TRUE, FALSE, FALSE))
  expect_identical(tExist(turtles = t1, who = c(3, 11, 9), breed = c("sheep", "wolf")),
                   c(TRUE, FALSE, TRUE))
})

test_that("turtle works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), breed = c(rep("sheep", 5),
                                                                                  rep("wolf", 5)))
  t2 <- turtle(t1, 3)
  expect_equivalent(of(agents = t2, var = "who"), 3)
  expect_equivalent(of(agents = t2, var = "breed"), "sheep")
  expect_equivalent(NLcount(t2), 1)
  t3 <- turtle(t1, 3, "sheep")
  expect_identical(t2, t3)
  t4 <- turtle(t1, 9, "sheep")
  expect_equivalent(NLcount(t4), 0)
  t5 <- turtle(t1, 9, "moose")
  expect_equivalent(NLcount(t5), 0)
  t6 <- turtle(t1, who = c(3, 9))
  expect_equivalent(of(agents = t6, var = "who"), c(3, 9))
  expect_equivalent(of(agents = t6, var = "breed"), c("sheep", "wolf"))
  expect_equivalent(NLcount(t6), 2)
  t7 <- turtle(t1, who = c(3, 9), breed = "sheep")
  expect_equivalent(of(agents = t7, var = "who"), 3)
  expect_equivalent(of(agents = t7, var = "breed"), "sheep")
  expect_equivalent(NLcount(t7), 1)
  t8 <- turtle(t1, who = c(9, 3), breed = "sheep")
  expect_identical(t7, t8)
  t9 <- turtle(t1, who = c(3, 9), breed = c("wolf", "sheep"))
  expect_equivalent(t9, t6)
  t10 <- turtle(t1, who = c(3, 9), breed = "wolf")
  expect_equivalent(of(agents = t10, var = "who"), 9)
  expect_equivalent(of(agents = t10, var = "breed"), "wolf")
  expect_equivalent(NLcount(t10), 1)
  t11 <- turtle(t1, who = c(3, 9), breed = c("sheep", "wolf"))
  expect_identical(t9, t11)
  t12 <- turtle(t1, who = c(3, 11, 9))
  expect_identical(t12, t6)
  t13 <- turtle(t1, who = c(3, 11, 9), breed = "sheep")
  expect_equivalent(t13, t8)
  t14 <- turtle(t1, who = c(3, 11, 9), breed = c("sheep", "wolf"))
  expect_identical(t14, t12)
  t15 <- turtle(t6, 3)
  expect_equivalent(t15, t13)
  t16 <- turtle(t1, 11)
  expect_equivalent(noTurtles(), t16)
})

test_that("turtlesOn works", {
  # Examples with Simplify = TRUE
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 12)
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 0:9, ycor = 0:9), breed = c(rep("sheep", 5),
                                                                                rep("wolf", 5)))
  t2 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, 0))
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 0))
  t3 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = 0, y = 0))
  expect_equivalent(of(agents = t3, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 0))
  t4 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, 0), breed = "sheep")
  expect_equivalent(of(agents = t4, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 0))
  t5 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = 0, y = 0),
                  breed = "sheep")
  expect_equivalent(of(agents = t5, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 0))

  t6 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)))
  expect_equivalent(of(agents = t6, var = c("xcor", "ycor")), cbind(xcor = c(0, 5, 6),
                                                                    ycor = c(0, 5, 6)))
  t7 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 5, 6),
                                                           y = c(0, 5, 6)))
  expect_equivalent(of(agents = t7, var = c("xcor", "ycor")), cbind(xcor = c(0, 5, 6),
                                                                    ycor = c(0, 5, 6)))
  t8 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)), breed = "sheep")
  expect_equivalent(of(agents = t8, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 0))
  t9 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 5, 6),
                                                           y = c(0, 5, 6)), breed = "sheep")
  expect_equivalent(of(agents = t9, var = c("xcor", "ycor")), cbind(xcor = 0, ycor = 0))

  t10 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)),
                   breed = c("sheep", "wolf"))
  expect_equivalent(of(agents = t10, var = c("xcor", "ycor")), of(agents = t6,
                                                                  var = c("xcor", "ycor")))
  t11 <- turtlesOn(world = w1, turtles = t1,
                   agents = patch(world = w1, x = c(0, 5, 6), y = c(0, 5, 6)),
                   breed = c("sheep", "wolf"))
  expect_equivalent(of(agents = t11, var = c("xcor", "ycor")), of(agents = t7,
                                                                  var = c("xcor", "ycor")))

  t12 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)),
                   breed = "moose")
  expect_equivalent(NLcount(t12), 0)

  # Examples with Simplify = FALSE
  t2 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, 0), simplify = FALSE)
  expect_equivalent(t2, cbind(0, 1))
  t3 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = 0, y = 0),
                  simplify = FALSE)
  expect_equivalent(t3, t2)
  t4 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, 0), breed = "sheep",
                  simplify = FALSE)
  expect_equivalent(t4, t2)
  t5 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = 0, y = 0),
                  breed = "sheep", simplify = FALSE)
  expect_equivalent(t5, t2)

  t6 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)),
                  simplify = FALSE)
  expect_equivalent(t6, cbind(c(0, 5, 6), 1:3))
  t7 <- turtlesOn(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 5, 6),
                                                           y = c(0, 5, 6)), simplify = FALSE)
  expect_equivalent(t7, t6)
  t8 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)),
                  breed = "sheep", simplify = FALSE)
  expect_equivalent(t8, t2)
  t9 <- turtlesOn(world = w1, turtles = t1,
                  agents = patch(world = w1, x = c(0, 5, 6), y = c(0, 5, 6)),
                  breed = "sheep", simplify = FALSE)
  expect_equivalent(t9, t8)

  t10 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)),
                   breed = c("sheep", "wolf"), simplify = FALSE)
  expect_equivalent(t10, t6)
  t11 <- turtlesOn(world = w1, turtles = t1,
                   agents = patch(world = w1, x = c(0, 5, 6), y = c(0, 5, 6)),
                   breed = c("sheep", "wolf"), simplify = FALSE)
  expect_equivalent(t11, t7)

  t12 <- turtlesOn(world = w1, turtles = t1, agents = turtle(t1, who = c(0, 5, 6)),
                   breed = "moose", simplify = FALSE)
  expect_equivalent(nrow(t12), 0)

  # New tests with bug fixed on Feb 7 2018
  t13 <- turtlesOn(world = w1, turtles = t1, agents = patches(w1))
  expect_equivalent(t13, t13)

  t14 <- turtlesOn(world = w1, turtles = t1[1], agents = patches(w1))
  expect_equivalent(t14, t1[1])

  t15 <- turtlesOn(world = w1, turtles = t1[1], agents = patch(w1, 1, 1))
  expect_equivalent(t15, noTurtles())

  w2 <- createWorld(1, 5, 1, 4, data = 0)
  t21 <- createTurtles(n = 5, coords = randomXYcor(w2, n = 5))
  t22 <- turtlesOn(world = w2, turtles = t21, agents = patches(w2))
  expect_equivalent(t22, t21)
})

test_that("noTurtles works", {
  t1 <- noTurtles()
  expect_equivalent(NLcount(t1), 0)
  expect_equivalent(nrow(t1@.Data), 0)
  expect_equivalent(ncol(t1@.Data), 8)
  expect_equivalent(t1@levels$breed, character(0))
  expect_equivalent(t1@levels$color, character(0))
})

test_that("turtlesAt works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 0:9, ycor = 0:9), breed = c(rep("sheep", 5),
                                                                                rep("wolf", 5)))
  t2 <- turtlesAt(world = w1, turtles = t1, agents = turtle(turtles = t1, who = 0), dx = 1, dy = 1)
  t3 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = 0, y = 0), dx = 1,
                  dy = 1)
  t4 <- turtlesAt(world = w1, turtles = t1, agents = turtle(turtles = t1, who = 0), dx = 1, dy = 1,
                  breed = "sheep")
  t5 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = 0, y = 0), dx = 1,
                  dy = 1, breed = "sheep")
  expect_equivalent(of(agents = t2, var = c("xcor", "ycor")), cbind(xcor = 1, ycor = 1))
  expect_identical(of(agents = t3, var = c("xcor", "ycor")), of(agents = t2,
                                                                var = c("xcor", "ycor")))
  expect_identical(of(agents = t4, var = c("xcor", "ycor")), of(agents = t2,
                                                                var = c("xcor", "ycor")))
  expect_identical(of(agents = t5, var = c("xcor", "ycor")), of(agents = t2,
                                                                var = c("xcor", "ycor")))

  t6 <- turtlesAt(world = w1, turtles = t1, agents = turtle(turtles = t1, who = c(0, 1)),
                  dx = c(1, 2), dy = c(1, 2))
  expect_equivalent(of(agents = t6, var = c("xcor", "ycor")), cbind(xcor = c(1, 3), ycor = c(1, 3)))
  t7 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 1), y = c(0, 1)),
                  dx = c(1, 2), dy = c(1, 2))
  expect_identical(of(agents = t7, var = c("xcor", "ycor")), of(agents = t6,
                                                                var = c("xcor", "ycor")))
  t8 <- turtlesAt(world = w1, turtles = t1, agents = turtle(turtles = t1, who = c(0, 1)),
                  dx = c(1, 2), dy = c(1, 2), breed = "sheep")
  expect_identical(of(agents = t8, var = c("xcor", "ycor")), of(agents = t6,
                                                                var = c("xcor", "ycor")))
  t9 <- turtlesAt(world = w1, turtles = t1, agents = turtle(turtles = t1, who = c(0, 1)),
                  dx = c(1, 2), dy = c(1, 2), breed = "wolf")
  expect_equivalent(NLcount(t9), 0)

  t10 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 4), y = c(0, 4)),
                   dx = c(1, 2), dy = c(1, 2), breed = "sheep")
  expect_identical(of(agents = t10, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("xcor", "ycor")))
  t11 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 4), y = c(0, 4)),
                   dx = c(1, 2), dy = c(1, 2), breed = c("sheep", "wolf"))
  expect_identical(of(agents = t11, var = c("xcor", "ycor")), cbind(xcor = c(1, 6), ycor = c(1, 6)))
  t12 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 8), y = c(0, 8)),
                   dx = c(1, 2), dy = c(1, 2), breed = c("sheep", "wolf"))
  expect_identical(of(agents = t12, var = c("xcor", "ycor")), of(agents = t2,
                                                                 var = c("xcor", "ycor")))
  t13 <- turtlesAt(world = w1, turtles = t1, agents = patch(world = w1, x = c(0, 8), y = c(0, 8)),
                   dx = 10, dy = 10, breed = c("sheep", "wolf"))
  expect_equivalent(NLcount(t13), 0)
})

test_that("turtleSet works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), breed = "sheep")
  t2 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2), breed = "wolf")
  t3 <- createTurtles(n = 1, coords = randomXYcor(w1, n = 1), breed = "sheperd")

  expect_warning(turtleSet(t1, t2, t3))

  t2 <- NLset(turtles = t2, agents = t2, var = "who", val = c(10, 11))
  t3 <- NLset(turtles = t3, agents = t3, var = "who", val = 12)
  tAll <- turtleSet(t1, t2, t3)
  expect_equivalent(NLcount(tAll), 13)

  expect_warning(turtleSet(t1, t1))

  t4 <- turtleSet(t1, noTurtles())
  expect_equivalent(t4, t1)

  t5 <- turtleSet(noTurtles(), noTurtles())
  expect_equivalent(noTurtles(), t5)

  t3 <- turtlesOwn(turtles = t3, tVar = "age", tVal = 10)
  tAll <- turtleSet(t1, t2, t3)
  expect_equivalent(NLcount(tAll), 13)
  expect_equivalent(length(of(agents = tAll, var = "who")), unique(length(of(agents = tAll,
                                                                             var = "who"))))
  expect_equivalent(rbind(cbind(inspect(t1, who = 0:9), age = NA),
                          cbind(inspect(t2, who = 10:11), age = NA), inspect(t3, who = 12)),
                    inspect(tAll, who = 0:12))

  t2 <- turtlesOwn(turtles = t2, tVar = "sex", tVal = c("F", "F"))
  tAll <- turtleSet(t1, t2, t3)
  expect_equivalent(NLcount(tAll), 13)
  expect_equivalent(length(of(agents = tAll, var = "who")), unique(length(of(agents = tAll,
                                                                             var = "who"))))
  expect_equivalent(of(agents = tAll, var = "sex"), c(rep(NA, 10), rep("F", 2), NA))
})

test_that("turtlesOwn works", {
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0, ycor = 0))
  t2 <- turtlesOwn(turtles = t1, tVar = "age", tVal = c(1, 2, 3, 4, 5))
  expect_identical(t2@.Data, cbind(t1@.Data, age = c(1, 2, 3, 4, 5)))
  t3 <- turtlesOwn(turtles = t2, tVar = "sex")
  expect_identical(t3@.Data, cbind(t2@.Data, sex = rep(NA, 5)))
  t4 <- turtlesOwn(turtles = t1, tVar = "breed2", tVal = c("bb", "aa", "aa", "cc", "bb"))
  expect_identical(t4@.Data, cbind(t1@.Data, breed2 = c(2, 1, 1, 3, 2)))
  expect_equivalent(t4@levels$breed2, c("aa", "bb", "cc"))
  t5 <- turtlesOwn(turtles = t1, tVar = "female", tVal = c("TRUE", "TRUE", "FALSE", "FALSE",
                                                           "FALSE"))
  expect_identical(t5@.Data, cbind(t1@.Data, female = c(2, 2, 1, 1, 1)))
  expect_equivalent(t5@levels$female, c("FALSE", "TRUE"))
})

test_that("subHeadings works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  t1 <- createTurtles(n = 4, world = w1, heading = c(0, 90, 180, 270))
  angles1 <- subHeadings(angle1 = t1, angle2 = 0)
  expect_equivalent(angles1, c(0, -90, -180, 90))
  angles2 <- subHeadings(angle1 = t1, angle2 = 0, range360 = TRUE)
  expect_equivalent(angles2, c(0, 270, 180, 90))

  angles3 <- subHeadings(angle1 = c(0, 90, 180, 270), angle2 = 0)
  expect_equivalent(angles3, c(0, -90, -180, 90))
  angles4 <- subHeadings(angle1 = c(0, 90, 180, 270), angle2 = 0, range360 = TRUE)
  expect_equivalent(angles4, c(0, 270, 180, 90))

  angles5 <- subHeadings(angle1 = c(0, 90, 180, 270), angle2 = turtle(turtles = t1, who = 0))
  expect_equivalent(angles5, c(0, -90, -180, 90))
  angles6 <- subHeadings(angle1 = c(0, 90, 180, 270), angle2 = turtle(turtles = t1, who = 0),
                         range360 = TRUE)
  expect_equivalent(angles6, c(0, 270, 180, 90))

  angles7 <- subHeadings(angle1 = t1, angle2 = turtle(turtles = t1, who = 0))
  expect_equivalent(angles7, c(0, -90, -180, 90))
  angles8 <- subHeadings(angle1 = t1, angle2 = turtle(turtles = t1, who = 0), range360 = TRUE)
  expect_equivalent(angles8, c(0, 270, 180, 90))

  angles9 <- subHeadings(angle1 = t1, angle2 = t1)
  expect_equivalent(angles9, rep(0, 4))

  # With different length for angle1 and angle2
  angles1 <- subHeadings(angle1 = t1, angle2 = 0)
  expect_equivalent(angles1, c(0, -90, -180, 90))
  angles1 <- subHeadings(angle1 = 0, angle2 = t1)
  expect_equivalent(angles1, c(0, 90, 180, -90))

  expect_error(subHeadings(angle1 = t1, angle2 = turtle(turtles = t1, who = c(0, 1))))
})

test_that("other works", {
  # Patches
  w1 <- createWorld(0, 9, 0, 9)
  w1[] <- 1:100
  p1 <- other(agents = patches(w1), except = cbind(pxcor = 0, pycor = 0))
  expect_equivalent(nrow(p1), 99)
  p2 <- other(agents = patches(w1), except = cbind(pxcor = c(0, 1, 2, 2), pycor = c(0, 1, 2, 2)))
  expect_equivalent(nrow(p2), 97)
  p3 <- other(agents = patches(w1), except = cbind(pxcor = 0, pycor = -1))
  expect_equivalent(nrow(p3), 100)

  w2 <- w1
  w2[] <- 100:1
  ws <- stackWorlds(w1, w2)
  p4 <- other(agents = patches(ws), except = cbind(pxcor = 0, pycor = 0))
  expect_equivalent(nrow(p4), 99)
  p5 <- other(agents = patches(ws), except = cbind(pxcor = c(0, 1, 2, 2), pycor = c(0, 1, 2, 2)))
  expect_equivalent(nrow(p5), 97)
  p6 <- other(agents = patches(ws), except = cbind(pxcor = 0, pycor = -1))
  expect_equivalent(nrow(p6), 100)

  # Turtles
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 0, ycor = 0))
  t2 <- other(agents = t1, except = turtle(turtles = t1, who = 0))
  expect_equivalent(NLcount(t2), 9)
  expect_identical(t2@.Data, t1@.Data[2:10, ])
  t3 <- other(agents = t1, except = turtle(turtles = t1, who = c(1, 2, 3)))
  expect_equivalent(NLcount(t3), 7)
  expect_identical(t3@.Data, t1@.Data[c(1, 5:10), ])

  t4 <- other(agents = turtle(turtles = t1, who = c(1, 2, 3)), except = turtle(turtles = t1,
                                                                               who = 0))
  expect_identical(t4, turtle(turtles = t1, who = c(1, 2, 3)))
  t5 <- other(agents = turtle(turtles = t1, who = 0), except = turtle(turtles = t1,
                                                                      who = c(1, 2, 3)))
  expect_identical(t5, turtle(turtles = t1, who = 0))

  t6 <- other(agents = t1, except = t1)
  expect_equivalent(NLcount(t6), 0)
})

test_that("layoutCircle works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t2 <- layoutCircle(world = w1, turtles = t1, radius = 3)
  t3 <- createOTurtles(n = 10, world = w1)
  expect_equivalent(of(agents = t2, var = "heading"), of(agents = t3, var = "heading"))
  t4 <- layoutCircle(world = w1, turtles = turtle(turtles = t1, who = 0), radius = 3)
  expect_equivalent(of(agents = t4, var = c("xcor", "ycor")), cbind(xcor = 4.5, ycor = 7.5))
  expect_equivalent(of(agents = t4, var = "heading"), 0)
  t5 <- layoutCircle(world = w1, turtles = turtle(turtles = t1, who = c(0, 1)), radius = 6,
                     torus = FALSE)
  expect_equivalent(of(agents = t5, var = c("xcor", "ycor")), cbind(xcor = c(4.5, 4.5),
                                                                    ycor = c(10.5, -1.5)))
  expect_equivalent(of(agents = t5, var = "heading"), c(0, 180))
  t6 <- layoutCircle(world = w1, turtles = turtle(turtles = t1, who = c(0, 1)), radius = 6,
                     torus = TRUE)
  expect_equivalent(of(agents = t6, var = c("xcor", "ycor")), cbind(xcor = c(4.5, 4.5),
                                                                    ycor = c(0.5, 8.5)))
})

test_that("of works", {
  # Patches
  w1 <- createWorld(data = 1:25, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1p1 <- of(world = w1, agents = patch(w1, 0, 4))
  expect_equivalent(w1p1, 1)
  w1p12 <- of(world = w1, agents = patch(w1, c(0, 1), c(4, 4)))
  expect_equivalent(w1p12, c(1, 2))
  w1all <- of(world = w1, agents = patches(w1))
  expect_equivalent(w1all, 1:25)
  w1p31 <- of(world = w1, agents = patch(w1, c(2, 0), c(4, 4)))
  expect_equivalent(w1p31, c(3, 1))

  w2 <- createWorld(data = 0, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w3 <- stackWorlds(w1, w2)
  w1p1 <- of(world = w3, var = "w1", agents = patch(w1, 0, 4))
  expect_equivalent(w1p1, 1)
  w1p12 <- of(world = w3, var = "w1", agents = patch(w1, c(0, 1), c(4, 4)))
  expect_equivalent(w1p12, c(1, 2))
  w1all <- of(world = w3, var = "w1", agents = patches(w1))
  expect_equivalent(w1all, 1:25)
  w1p31 <- of(world = w3, var = "w1", agents = patch(w1, c(2, 0), c(4, 4)))
  expect_equivalent(w1p31, c(3, 1))

  w2p1 <- of(world = w3, var = "w2", agents = patch(w1, 0, 4))
  expect_equivalent(w2p1, 0)
  w2p12 <- of(world = w3, var = "w2", agents = patch(w1, c(0, 1), c(4, 4)))
  expect_equivalent(w2p12, c(0, 0))
  w2all <- of(world = w3, var = "w2", agents = patches(w1))
  expect_equivalent(w2all, rep(0, 25))
  w2p31 <- of(world = w3, var = "w2", agents = patch(w1, c(2, 0), c(4, 4)))
  expect_equivalent(w2p31, c(0, 0))

  # Work with multiple var
  p1 <- of(world = w3, var = c("w2", "w1"), agents = patch(w1, 0, 4))
  expect_equivalent(p1, cbind(0, 1))
  p2 <- of(world = w3, var = c("w1", "w2"), agents = patches(w3))
  expect_equivalent(p2, cbind(1:25, rep(0, 25)))

  # Turtles
  t <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), breed = "sheep",
                     heading = 21:30, color = c(rep("blue", 5), rep("red", 5)))
  twho <- of(agents = t, var = "who")
  expect_equivalent(twho, 0:9)
  theading <- of(agents = t, var = "heading")
  expect_equivalent(theading, 21:30)
  tbreed <- of(agents = t, var = "breed")
  expect_equivalent(tbreed, rep("sheep", 10))
  tcolor <- of(agents = t, var = "color")
  expect_equivalent(tcolor, c(rep("blue", 5), rep("red", 5)))
  tprevX <- of(agents = t, var = "prevX")
  expect_equivalent(tprevX, as.numeric(rep(NA, 10)))
  tprevY <- of(agents = t, var = "prevY")
  expect_equivalent(tprevY, as.numeric(rep(NA, 10)))
  txcor <- of(agents = t, var = "xcor")
  expect_equivalent(txcor, 1:10)
  tycor <- of(agents = t, var = "ycor")
  expect_equivalent(tycor, 10:1)

  t1 <- turtlesOwn(turtles = t, tVar = "age", tVal = c(2, 3, 4, 5, 2, 4, 6, 6, 3, 5))
  t1age <- of(agents = t1, var = "age")
  expect_equivalent(t1age, c(2, 3, 4, 5, 2, 4, 6, 6, 3, 5))

  t2who <- of(agents = turtle(turtles = t1, who = 0), var = "who")
  expect_equivalent(t2who, 0)
  t2heading <- of(agents = turtle(turtles = t1, who = 0), var = "heading")
  expect_equivalent(t2heading, 21)
  t2breed <- of(agents = turtle(turtles = t1, who = 0), var = "breed")
  expect_equivalent(t2breed, "sheep")
  t2color <- of(agents = turtle(turtles = t1, who = 0), var = "color")
  expect_equivalent(t2color, "blue")
  t2prevX <- of(agents = turtle(turtles = t1, who = 0), var = "prevX")
  expect_equivalent(t2prevX, as.numeric(NA))
  t2prevY <- of(agents = turtle(turtles = t1, who = 0), var = "prevY")
  expect_equivalent(t2prevY, as.numeric(NA))
  t2xcor <- of(agents = turtle(turtles = t1, who = 0), var = "xcor")
  expect_equivalent(t2xcor, 1)
  t2ycor <- of(agents = turtle(turtles = t1, who = 0), var = "ycor")
  expect_equivalent(t2ycor, 10)
  t2age <- of(agents = turtle(turtles = t1, who = 0), var = "age")
  expect_equivalent(t2age, 2)

  # With multiple var
  t3 <- of(agents = turtle(turtles = t1, who = 0), var = c("who", "heading"))
  expect_equivalent(cbind(who = 0, heading = 21), t3)
  t4 <- of(agents = turtle(turtles = t1, who = 0), var = c("who", "xcor"))
  expect_equivalent(cbind(who = 0, xcor = 1), t4)
  t5 <- of(agents = t1, var = c("who", "xcor", "age"))
  expect_equivalent(cbind(who = 0:9, xcor = 1:10, age = c(2, 3, 4, 5, 2, 4, 6, 6, 3, 5)), t5)
  t6 <- of(agents = t1, var = c("who", "xcor", "age", "breed"))
  expect_equivalent(data.frame(who = 0:9, xcor = 1:10, age = c(2, 3, 4, 5, 2, 4, 6, 6, 3, 5),
                               breed = rep("sheep", 10), stringsAsFactors = FALSE), t6)
})

test_that("spdf2turtles and turtles2spdf work", {
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 1:10), heading = 1:10)
  t1 <- turtlesOwn(turtles = t1, tVar = "age", tVal = 1:10)
  t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c(rep("M", 5), rep("F", 5)))
  t2 <- turtles2spdf(t1)
  expect_equivalent(t2@coords, of(agents = t1, var = c("xcor", "ycor")))
  expect_equivalent(t2@data, inspect(turtles = t1, who = 0:9)[3:10])

  sp1 <- SpatialPointsDataFrame(coords = cbind(x = c(1, 2, 3), y = c(1, 2, 3)),
                                data = cbind.data.frame(age = c(0, 0, 3), sex = c("F", "F", "M")))
  sp1Turtles <- spdf2turtles(sp1)
  expect_equivalent(colnames(sp1Turtles@.Data), c("xcor", "ycor", "who", "heading", "prevX",
                                                  "prevY", "breed", "color", "age", "sex"))
  expect_equivalent(of(agents = sp1Turtles, var = "age"), c(0, 0, 3))
  expect_equivalent(of(agents = sp1Turtles, var = "sex"), c("F", "F", "M"))
  expect_equivalent(of(agents = sp1Turtles, var = "who"), c(0, 1, 2))
  expect_equivalent(of(agents = sp1Turtles, var = "xcor"), c(1, 2, 3))

  sp2 <- spdf2turtles(t2)
  expect_equivalent(colnames(sp2@.Data), c("xcor", "ycor", "who", "heading", "prevX", "prevY",
                                           "breed", "color", "age", "sex"))
  expect_equivalent(of(agents = sp2, var = "age"), 1:10)
  expect_equivalent(of(agents = sp2, var = "sex"), c(rep("M", 5), rep("F", 5)))
  expect_equivalent(of(agents = sp2, var = "who"), 0:9)
  expect_equivalent(of(agents = sp2, var = "xcor"), 1:10)
})
