################################################################################
# Ants
# by Wilensky (1997) NetLogo Ants model.
# http://ccl.northwestern.edu/netlogo/models/Ants
#
# Converted into R using the NetLogoR package
# by Sarah Bauduin
#
#

## Packages required
#library(NetLogoR)
#library(testthat) # for testing

## Global variables (model parameters)
nAnts <- 125 # varies from 0 to 200 in the NetLogo model
rDiff <- 50 # varies from 0 to 99 in the NetLogo model
rEvap <- 10 # varies from 0 to 99 in the NetLogo model
# torus = FALSE


## Model setup
# World
# One worldMatrix for each patch variable, then stack them
# As all the worldMatrix must have the same extent, you can copy past the object to a new name
# Patch values must be assigned to each world before stacking them

# Chemical
chemical <- createWorld(minPxcor = -35, maxPxcor = 35, minPycor = -35, maxPycor = 35, data = 0) # amount of chemical on the patches

# Nest
nest <- chemical # 1 on nest patches, 0 elsewhere
nestScent <- chemical # higher closer to the nest
distNest <- NLdist(agents = patches(nest), agents2 = cbind(x = 0, y = 0))
patchNest <- which(distNest < 5) # patch/cell numbers for the ones at distance less than 5 from [x = 0, y = 0]
nest <- NLset(world = nest, agents = patches(nest), val = 0) # 0 to all the patches
nest <- NLset(world = nest, agents = PxcorPycorFromCell(world = nest, cellNum = patchNest), val = 1) # and reassign 1 to the patches in the nest
nestScent <- NLset(world = nestScent, agents = patches(nestScent), val = 200 - distNest) # spread nestScent over the whole world, stronger near the nest

# Food
foodSource <- chemical # number (1, 2, or 3) to identify the food sources
food <- chemical # amount of food on the patches (0, 1, or 2)
distFood1 <- NLdist(agents = patches(foodSource), agents2 = cbind(x = 0.6 * maxPxcor(foodSource), y = 0)) # setup food source 1 on the right
patchFood1 <- which(distFood1 < 5)
foodSource <- NLset(world = foodSource, agents = patches(foodSource), val = 0) # 0 to all patches
foodSource <- NLset(world = foodSource, agents = PxcorPycorFromCell(world = foodSource, cellNum = patchFood1), val = 1) # and reassign 1 to the patches in the foodSource 1
distFood2 <- NLdist(agents = patches(foodSource), agents2 = cbind(x = -0.6 * maxPxcor(foodSource), y = -0.6 * maxPycor(foodSource))) # setup food source 2 on the lower-left
patchFood2 <- which(distFood2 < 5)
foodSource <- NLset(world = foodSource, agents = PxcorPycorFromCell(world = foodSource, cellNum = patchFood2), val = 2) # and reassign 2 to the patches in the foodSource 2
distFood3 <- NLdist(agents = patches(foodSource), agents2 = cbind(x = -0.8 * maxPxcor(foodSource), y = 0.8 * maxPycor(foodSource))) # setup food source 3 on the upper-left
patchFood3 <- which(distFood3 < 5)
foodSource <- NLset(world = foodSource, agents = PxcorPycorFromCell(world = foodSource, cellNum = patchFood3), val = 3) # and reassign 3 to the patches in the foodSource 3
food <- NLset(world = food, agents = patches(food), val = 0)
patchFood123 <- PxcorPycorFromCell(world = food, cellNum = c(patchFood1, patchFood2, patchFood3))
food <- NLset(world = food, agents = patchFood123, val = sample(c(1,2), size = NLcount(patchFood123), replace = TRUE)) # set "food" at sources to either 1 or 2, randomly
world <- stackWorlds(chemical, nest, nestScent, foodSource, food)

# Ants
ants <- createTurtles(n = nAnts, coords = cbind(xcor = 0, ycor = 0), color = "red") # red = not carrying food

# # Visualize the world
# plot(world) # all the layers
# plot(ants) # the ants only
# plot(world2raster(world)$layer.5) # only the food layer
# points(ants, pch = 16) # add the ants on the food layer

# Initialize the output objects
f_fS_world <- of(world = world, var = c("food", "foodSource"), agents = patches(world))
food1 <- sum(f_fS_world[f_fS_world[, "foodSource"] == 1, "food"]) # sum of food values for the patches with foodSource equals to 1
food2 <- sum(f_fS_world[f_fS_world[, "foodSource"] == 2, "food"]) # sum of food values for the patches with foodSource equals to 2
food3 <- sum(f_fS_world[f_fS_world[, "foodSource"] == 3, "food"]) # sum of food values for the patches with foodSource equals to 3


## Functions used in the go procedure
toNest <- function(turtles){
  #print(paste("toNest", NLcount(turtles)))

  pHere <- patchHere(world = world, turtles = turtles)
  nest_chem_pHere <- of(world = world, agents = pHere, var = c("nest", "chemical")) # nest values (1 or 0) and chemical in the order of the turtles
  whoTurtles <- of(agents = turtles, var = "who")

  # Inside the nest
  turtlesIn <- turtle(turtles, who = whoTurtles[nest_chem_pHere[, "nest"] == 1]) # whoTurtles for which their nest value equals 1
  if(NLcount(turtlesIn) != 0){
    turtlesIn <- NLset(turtles = turtlesIn, agents = turtlesIn, var = "color", val = "red")
    turtlesIn <- right(turtles = turtlesIn, angle = 180) # drop food and head out again
  }

  # Outside of the nest
  turtlesOut <- turtle(turtles, who = whoTurtles[nest_chem_pHere[, "nest"] == 0]) # whoTurtles for which their nest value equals 0
  if(NLcount(turtlesOut) != 0){
    pHereOut <- pHere[nest_chem_pHere[, "nest"] == 0, , drop = FALSE] # pHere for which their nest value equals 0 (drop = FALSE keeps a format matrix when 1 row)
    world <- NLset(world = world, agents = pHereOut, var = "chemical", val = nest_chem_pHere[nest_chem_pHere[, "nest"] == 0, "chemical"] + 60) # drop some chemical
    turtlesOut <- upPatch(turtles = turtlesOut, varPatch = "nestScent") # head toward the greatest value of nestScent
  }

  # Because turtles have been modified separately as turtlesIn and turtlesOut, we need to put them back together to execute the following procedure
  turtles <- turtleSet(turtlesIn, turtlesOut) # there should not be any problem (e.g., duplicates) as turtles are always either in or out, but never both or neither
  #print(paste("toNest", NLcount(turtles)))
  return(list(turtles, world))
}

lookFood <- function(turtles){
  #print(paste("lookFood", NLcount(turtles)))

  pHere <- patchHere(world = world, turtles = turtles)
  food_chem_pHere <- of(world = world, agents = pHere, var = c("food", "chemical")) # faster to extract both values at the same time
  whoTurtles <- of(agents = turtles, var = "who")

  # Where there is food
  turtlesFood <- turtle(turtles, who = whoTurtles[food_chem_pHere[, "food"] > 0])
  if(NLcount(turtlesFood) != 0){
    turtlesFood <- NLset(turtles = turtlesFood, agents = turtlesFood, var = "color", val = "orange") # pick up food
    turtlesFood <- right(turtles = turtlesFood, angle = 180) # and turn around
    turtles <- turtleSet(turtlesFood, other(agents = turtles, except = turtlesFood)) # reconstruct the turtles with turtlesFood modified and the others
    world <- NLset(world = world, agents = pHere[food_chem_pHere[, "food"] > 0, , drop = FALSE], var = "food", val = food_chem_pHere[food_chem_pHere[, "food"] > 0, "food"] - 1) # and reduce the food source
  }

  # Go in the direction where the chemical smell is strongest
  turtlesChem <- turtle(turtles, who = whoTurtles[food_chem_pHere[, "chemical"] >= 0.05 & food_chem_pHere[, "chemical"] < 2])
  if(NLcount(turtlesChem) != 0){
    turtlesChem <- upPatch(turtles = turtlesChem, varPatch = "chemical")
    turtles <- turtleSet(turtlesChem, other(agents = turtles, except = turtlesChem)) # reconstruct the turtles with turtlesChem modified and the others
  }

  #print(paste("lookFood", NLcount(turtles)))
  return(list(turtles, world))
}

upPatch <- function(turtles, varPatch){
  #print(paste("upPatch", NLcount(turtles)))

  # sniff left and right, and go where the strongest smell is
  pAhead <- patchAhead(world = world, turtles = turtles, dist = 1)
  scentAhead <- of(world = world, var = varPatch, agents = pAhead)
  pRight <- patchRight(world = world, turtles = turtles, dist = 1, angle = 45)
  scentRight <- of(world = world, var = varPatch, agents = pRight)
  pLeft <- patchLeft(world = world, turtles = turtles, dist = 1, angle = 45)
  scentLeft <- of(world = world, var = varPatch, agents = pLeft)

  whoTurtles <- of(agents = turtles, var = "who")
  tRight <- turtle(turtles, who = whoTurtles[scentRight > scentLeft & scentRight > scentAhead])
  tRight <- right(turtles = tRight, angle = 45)
  tLeft <- turtle(turtles, who = whoTurtles[scentLeft > scentRight & scentLeft > scentAhead])
  tLeft <- left(turtles = tLeft, angle = 45)

  turtlesMoved <- turtleSet(tRight, tLeft)
  turtles <- turtleSet(turtlesMoved, other(agents = turtles, except = turtlesMoved))
  #print(paste("upPatch", NLcount(turtles)))
  return(turtles)
}

wiggle <- function(turtles){
  #print(paste("wiggle", NLcount(turtles)))

  turtles <- right(turtles, angle = runif(n = NLcount(turtles), min = -40, max = 40)) # random angle between - 40 (40 to left) and 40 (40 to right)
  turtlesMove <- canMove(world = world, turtles = turtles, dist = 1)
  whoTurtles <- of(agents = turtles, var = "who")
  turtlesCannot <- turtle(turtles, whoTurtles[turtlesMove == FALSE])
  # turtlesCannot <- right(turtles = turtlesCannot, angle = 180)
  # If a turtle is in a corner, rotating 180° may not solve the problem
  # For an easier solution, it will face the center of the world
  turtlesCannot <- face(turtles = turtlesCannot, agents2 = cbind(x = 0, y = 0))

  turtles <- turtleSet(turtlesCannot, turtle(turtles, whoTurtles[turtlesMove == TRUE]))
  #print(paste("wiggle", NLcount(turtles)))
  return(turtles)
}


## Go
# time <- 0
while(sum(f_fS_world[, "food"]) != 0){ # as long as there is food in the world
#for(i in 1:200){ # to test function speed
  # Ants not carrying food
  aRed <- NLwith(agents = ants, var = "color", val = "red")
  if(NLcount(aRed) != 0){
    resLookFood <- lookFood(aRed) # look for it
    aRed <- resLookFood[[1]]
    world <- resLookFood[[2]]
  }

  # Ants carrying food
  aOrange <- NLwith(agents = ants, var = "color", val = "orange")
  if(NLcount(aOrange) != 0){
    resToNest <- toNest(aOrange) # take it back to nest
    aOrange <- resToNest[[1]]
    world <- resToNest[[2]]
  }

  # Ants moving
  ants <- turtleSet(aRed, aOrange)
  ants <- wiggle(ants)
  ants <- fd(ants, dist = 1)

  # World update
  world <- diffuse(world = world, pVar = "chemical", share = rDiff / 100, nNeighbors = 8)
  pWorld <- patches(world)
  pChem <- of(world = world, var = "chemical", agents = pWorld)
  world <- NLset(world = world, agents = pWorld, var = "chemical", val = pChem * (100 - rEvap) / 100) # slowly evaporate chemical

  # Output update
  f_fS_world <- of(world = world, var = c("food", "foodSource"), agents = patches(world))
  food1 <- c(food1, sum(f_fS_world[f_fS_world[, "foodSource"] == 1, "food"]))
  food2 <- c(food2, sum(f_fS_world[f_fS_world[, "foodSource"] == 2, "food"]))
  food3 <- c(food3, sum(f_fS_world[f_fS_world[, "foodSource"] == 3, "food"]))

  # # Time
  # time <- time + 1
  # print(time)
  #
  # # Plots
  # plot(world2raster(world)$layer.5) # food
  # points(ants, pch = 16)
  #
  # expect_equivalent(NLcount(ants), nAnts)
}

## Plot outputs
timeStep <- 1:length(food1)
plot(timeStep, food1, type = "l", col = "coral", lwd = 2, ylab = "Food", xlab = "Time step",
     ylim = c(min = 0, max = max(c(max(food1), max(food2), max(food3)))))
lines(timeStep, food2, col = "yellow", lwd = 2)
lines(timeStep, food3, col = "green", lwd = 2)

legend("topright", legend = c("food1", "food2", "food3"), lwd = c(2, 2, 2), col = c("coral", "yellow", "green"),
       bg = "white")
