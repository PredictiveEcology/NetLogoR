## ---- eval=FALSE---------------------------------------------------------
#  # Create a world according to a given extent
#  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = 0, maxPycor = 10)
#  
#  # Report the distance between the patch [pxcor = 0, pycor = 0] and the patch [pxcor = 1, pycor = 1]
#  pDist <- NLdist(agents = cbind(pxcor = 0, pycor = 0),
#                  agents2 = cbind(pxcor = 1, pycor = 1, world = w1, torus = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  # Create 10 turtles in the world w1
#  t1 <- createTurtles(n = 10, world = w1)
#  
#  # Move all the turtles by a distance of 1
#  t1 <- fd(world = w1, turtles = t1, dist = 1)

## ---- eval=FALSE---------------------------------------------------------
#  # For all patches, assign a random value between 0 and 1
#  pQuality <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, data = runif(n = 100, min = 0, max = 1))

## ---- eval=FALSE---------------------------------------------------------
#  # Now each turtle in t1 has a "sex" variable
#  t1 <- turtlesOwn (turtles = t1, tVar = "sex",
#                    tVal = c("M", "M", "M", "M", "M", "F", "F", "F", "F", "F"))

## ---- eval=FALSE---------------------------------------------------------
#  # 5 sheep and 5 wolves
#  t2 <- createTurtles(world = w1, n = 10, breed = c(rep("sheep", 5), rep("wolf", 5)))
#  
#  # Or
#  sheep <- createTurtles(world = w1, n = 5, breed = "sheep") # 5 sheep
#  wolves <- createTurtles(world = w1, n = 5, breed = "wolf") # 5 wolves

## ---- eval=FALSE---------------------------------------------------------
#  # Turtle 0 which was "sheep" becomes "wolf"
#  t2 <- NLset(turtles = t2, agents = turtle(t2, who = 0), var = "breed", val = "wolf")

## ---- eval=FALSE---------------------------------------------------------
#  # Reports the pQuality value of the patches:
#  # [pxcor = 0, pycor = 0], [pxcor = 0, pycor = 1], and [pxcor = 0, pycor = 2]
#  of(world = pQuality, agents = patch(pQuality, c(0,0,0), c(0,1,2)))

## ---- eval=FALSE---------------------------------------------------------
#  system.file("examples", package = "NetLogoR")

