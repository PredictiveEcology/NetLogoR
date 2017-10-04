################################################################################
# Wolf sheep predation
# by Wilensky (1997) NetLogo Wolf Sheep Predation model.
# http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation
#
# Converted into R using the NetLogoR package
# by Sarah Bauduin
#
#

## Packages required
library(NetLogoR)


## Global variables (some represent the model buttons)
# Grass settings
grassOn <- TRUE # TRUE to include grass in the model, FALSE to only include wolves and sheep
grassTGrowth <- 30 # how long it takes for grass to regrow once it is eaten
numGreen <- numeric() # keep track of how much grass there is

# Sheep settings
nSheep <- 100 # initial sheep population size
gainFoodSheep <- 4 # amount of energy sheep get for every grass patch eaten
reproSheep <- 4 # probability in % of a sheep reproducing at each time step
numSheep <- nSheep # keep track of how many sheep there are

# Wolf settings
nWolf <- 50 # initial wolf population size
gainFoodWolf <- 20 # amount of energy wolves get for every sheep eaten
reproWolf <- 5 # probability in % of a wolf reproducing at each time step
numWolves <- nWolf # keep track of how many wolves there is

# The world is wrapped,
# so torus = TRUE will be used in the movement functions (e.g., fd())


## Setup
# Create the world
grass <- createWorld(minPxcor = -25, maxPxcor = 25, minPycor = -25, maxPycor = 25)

# If grassOn is TRUE, assign grass and countdown values to patches
# Because there are multiple patches variables, a worldArray is needed
# If grassOn is TRUE, the grass grows and the sheep eat it, if FALSE, the sheep don't need to eat
if (grassOn == TRUE) {
  # Initialize patch values (grass and countdown) at random
  # 0 or 1 (i.e., green or brown in the NetLogo model)
  grassVal <- sample(c(0, 1), size = NLcount(patches(grass)), replace = TRUE)
  grass <- NLset(world = grass, agents = patches(grass), val = grassVal)
  countdown <- grass # countdown is a new NLworld with the same extent as grass
  countdownVal <- runif(n = NLcount(patches(grass)), min = 0, max = grassTGrowth) # grass grow clock
  countdown <- NLset(world = countdown, agents = patches(countdown), val = countdownVal)
  field <- stackWorlds(grass, countdown)

}
# When no patches values are used,
# using grass, countdown or field as the world argument required by a function
# does not change anything because they all have the same extent and number of patches.
# When patches values are used (e.g., when the sheep eat the grass),
# use only field as the world argument for the functions
# which update and retrieve the patches values.
# When field is updated,
# the values on the individual worldMatrix grass and countdown are not updated,
# only the layers in field are.

# Create the sheep
sheep <- createTurtles(n = nSheep, coords = randomXYcor(world = grass, n = nSheep),
                       breed = "aSheep", color = rep("red", nSheep))

# Add the energy variable
sheep <- turtlesOwn(turtles = sheep, tVar = "energy",
                    tVal = runif(n = nSheep, min = 0, max = 2 * gainFoodSheep))

# Create the wolves
wolves <- createTurtles(n = nWolf, coords = randomXYcor(world = grass, n = nWolf),
                        breed = "wolf", color = rep("black", nWolf))

# Add the energy variable
wolves <- turtlesOwn(turtles = wolves, tVar = "energy",
                     tVal = runif(n = nWolf, min = 0, max = 2 * gainFoodWolf))

# Initialize the count of grass
if (grassOn == TRUE) {
  pGreen <- NLwith(world = field, var = "grass", agents = patches(field),
                   val = 1) # patches equal to 1 (green)
  numGreen <- NLcount(pGreen)
}

# Visualize the world
if (grassOn == TRUE){
  plot(field[[1]])
  points(sheep, pch = 16, col = "red")
  points(wolves, pch = 16, col = "black")
} else {
  grass <- NLset(world = grass, agents = patches(grass), val = 0) # cannot plot an empty world
  plot(grass)
  points(sheep, pch = 16, col = "red")
  points(wolves, pch = 16, col = "black")
}


## Functions used in the go procedure
# Always return the object updated by the function
# When only one type of input is permitted (e.g., only sheep or only wolves),
# the function does not need to express arguments.
# When a function can be used by both sheep and wolves,
# the argument "turtles" must be used when building the function
# and be replaced by either sheep or wolves when calling the function.


## sheep and wolves
move <- function(turtles) {
  # In NetLogo, two functions are used to give a random heading
  # by rotating the turtles to the right and then to the left.
  # With NetLogoR, it can be replaced by only one function,
  # as a negative value to turn right will turn left:
  turtles <- right(turtles, angle = runif(n = NLcount(turtles), min = -50, max = 50))
  turtles <- fd(world = grass, turtles = turtles, dist = 1, torus = TRUE)
  return(turtles)
}


## only sheep
eatGrass <- function() {
  pGreen <- NLwith(world = field, var = "grass", agents = patches(field),
                   val = 1) # patches with grass equal to 1 (green)
  sheepOnGreen <- turtlesOn(world = field, turtles = sheep,
                            agents = pGreen) # sheep on green patches

  if (NLcount(sheepOnGreen) != 0) {
    # These sheep gain energy by eating
    energySheep <- of(agents = sheepOnGreen, var = "energy") # energy before eating
    sheep <- NLset(turtles = sheep, agents = sheepOnGreen, var = "energy",
                   val = energySheep + gainFoodSheep) # update energy

    # If a sheep is on a green patch (value equal to 1),
    # it eats the grass and turns it to brown (value to 0).
    pHere <- patchHere(world = field, turtles = sheepOnGreen)
    field <- NLset(world = field, agents = pHere, var = "grass", val = 0)
  }

  return(list(field, sheep)) # return the two objects updated in this function
}


## sheep and wolves
death <- function(turtles) {
  # When energy dips below 0, die
  whoEnergy <- of(agents = turtles, var = c("who", "energy"))
  # "who" numbers of the turtles with their energy value below 0
  who0 <- whoEnergy[which(whoEnergy[, "energy"] < 0), "who"]

  if (length(who0) != 0) {
    turtles <- die(turtles = turtles, who = who0)
  }

  return(turtles)
}


## sheep and wolves
reproduce <- function(turtles, reproTurtles) {
  # Throw dice to see if the turtles will reproduce
  repro <- runif(n = NLcount(turtles), min = 0, max = 100) < reproTurtles
  whoTurtles <- of(agents = turtles, var = "who") # "who" of the turtles before they reproduce
  reproWho <- whoTurtles[repro] # "who" of turtles which reproduce
  reproInd <- turtle(turtles, who = reproWho) # turtles which reproduce

  # if there is at least one turtle reproducing...
  if (NLcount(reproInd) != 0) {
    energyTurtles <- of(agents = reproInd, var = "energy")
    # Divide the energy between the parent and offspring
    turtles <- NLset(turtles = turtles, agents = reproInd, var = "energy", val = energyTurtles / 2)
    turtles <- hatch(turtles = turtles, who = reproWho, n = 1) # hatch one offspring per parent

    # Move the offspring by 1 step
    whoNewTurtles <- of(agents = turtles, var = "who") # "who" of the turtles after they reproduced
    whoOffspring <- which(!whoNewTurtles %in% whoTurtles) # "who" of offspring
    offspring <- turtle(turtles = turtles, who = whoOffspring)
    offspringMoved <- right(turtles = offspring,
                            angle = runif(n = NLcount(offspring), min = 0, max = 360))
    offspringMoved <- fd(world = grass, turtles = offspring, dist = 1, torus = TRUE)
    # Update the headings and coordinates of the offsprings inside the turtles
    valOffspring <- of(agents = offspringMoved, var = c("heading", "xcor", "ycor"))
    turtles <- NLset(turtles = turtles, agents = offspring, var = c("heading", "xcor", "ycor"),
                     val = valOffspring)
  }

  return(turtles)
}


## only wolves
catchSheep <- function() {
  # "who" numbers of sheep that are on the same patches as the wolves
  sheepWolves <- turtlesOn(world = grass, turtles = sheep, agents = wolves, simplify = FALSE)
  if (nrow(sheepWolves) != 0) {
    # sheepWolves[,"whoTurtles"] are the "who" numbers of sheep
    # sheepWolves[,"id"] represent the rank/order of the individual wolf in the wolves
    # (! not the "who" numbers of the wolves)
    sheepGrabbed <- oneOf(agents = sheepWolves) # grab one random sheep

    sheep <- die(turtles = sheep, who = sheepGrabbed) # kill the grabbed sheep
    whoWolves <- of(agents = wolves, var = "who")
    whoGrabbingWolves <- whoWolves[unique(sheepWolves[, "id"])]
    grabbingWolves <- turtle(turtles = wolves, who = whoGrabbingWolves)
    energyGrabbingWolves <- of(agents = grabbingWolves, var = "energy")
    # Get energy from eating for the wolves who grabbed sheep
    wolves <- NLset(turtles = wolves, agents = grabbingWolves, var = "energy",
                    val = energyGrabbingWolves + gainFoodWolf)
  }

  return(list(sheep, wolves))# return the two objects updated in this function
}


## only patches
growGrass <- function() {
  # Identify patches with grass equal to 0 (brown) and countdown less or equal to 0
  pBrown <- NLwith(world = field, var = "grass", agents = patches(field), val = 0)
  # Countdown values for the patches equal to 0 (brown)
  pBrownCountdown <- of(world = field, var = "countdown", agents = pBrown)

  pBrownCountdown0 <- which(pBrownCountdown <= 0) # patches with a countdown <= 0
  if (length(pBrownCountdown0) != 0) {
    # Patches with grass equal to 0 (brown) and countdown <= 0
    pGrow <- pBrown[pBrownCountdown0, , drop = FALSE]
    # Grow some grass on these patches and reset the countdown
    field <- NLset(world = field, var = c("grass", "countdown"), agents = pGrow,
                 val = cbind(grass = rep(1, NLcount(pGrow)),
                             countdown = rep(grassTGrowth, NLcount(pGrow))))
  }

  pBrownCountdown1 <- which(!pBrownCountdown <= 0) # patches with a countdown > 0
  if (length(pBrownCountdown1) != 0) {
    # Patches with grass equal to 0 (brown) and countdown > 0
    pWait <- pBrown[pBrownCountdown1, , drop = FALSE]
    # Decrease the countdown for the patches which wait
    field <- NLset(world = field, var = "countdown", agents = pWait,
                   val = pBrownCountdown[pBrownCountdown1] - 1)
  }

  return(field)
}


## Go
time <- 0
maxTime <- 500
while ((NLany(sheep) | NLany(wolves)) & time < maxTime) {
  ## as long as there are sheep or wolves in the world (time steps maximum at 500)

  # Ask sheep
  if (NLcount(sheep) != 0) {
    sheep <- move(sheep)
    if (grassOn == TRUE) {
      energySheep <- of(agents = sheep, var = "energy")
      sheep <- NLset(turtles = sheep, agents = sheep, var = "energy", val = energySheep - 1)
      eatGrassResults <- eatGrass() # in the results are stored both "field" and "sheep"
      field <- eatGrassResults[[1]] # reassign the object with their updated values
      sheep <- eatGrassResults[[2]]
    }
    sheep <- death(sheep)
    if (NLcount(sheep) != 0) {
      sheep <- reproduce(sheep, reproSheep)
    }
  }

  # Ask wolves
  if (NLcount(wolves) != 0) {
    wolves <- move(wolves)
    energyWolves <- of(agents = wolves, var = "energy")
    wolves <- NLset(turtles = wolves, agents = wolves, var = "energy", val = energyWolves - 1)
    catchSheepResults <- catchSheep() # in the results are stored both "sheep" and "wolves"
    sheep <- catchSheepResults[[1]] # reassign the object with their updated values
    wolves <- catchSheepResults[[2]]
    wolves <- death(wolves)
    if (NLcount(wolves) != 0) {
      wolves <- reproduce(wolves, reproWolf)
    }
  }

  # Ask grass
  if (grassOn == TRUE) {
    field <- growGrass()
    pGreen <- NLwith(world = field, var = "grass", agents = patches(field),
                     val = 1) # patches equal to 1 (green)
    npGreen <- NLcount(pGreen)
    numGreen <- c(numGreen, npGreen) # add the new number of green patches
  }

  numSheep <- c(numSheep, NLcount(sheep)) # add the new number of sheep
  numWolves <- c(numWolves, NLcount(wolves)) # add the new numbr of wolves

  time <- time + 1
  print(time) # slow the model
}

## Plot outputs
library(quickPlot)
dev()
timeStep <- 1:length(numSheep)

if (grassOn == TRUE) {
  plot(timeStep, numSheep, type = "l", col = "blue", lwd = 2, ylab = "Population size",
       xlab = "Time step", ylim = c(min = 0, max = max(c(max(numSheep), max(numWolves),
                                                         max(numGreen / 4)))))
  lines(timeStep, numWolves, col = "red", lwd = 2)
  lines(timeStep, numGreen / 4, col = "green", lwd = 2)

  legend("topleft", legend = c("Sheep", "Wolves", "Grass / 4"), lwd = c(2, 2, 2),
         col = c("blue", "red", "green"), bg = "white")
} else {
  plot(timeStep, numSheep, type = "l", col = "blue", lwd = 2, ylab = "Population size",
       xlab = "Time step", ylim = c(min = 0, max = max(c(max(numSheep), max(numWolves)))))
  lines(timeStep, numWolves, col = "red", lwd = 2)

  legend("topleft", legend = c("Sheep", "Wolves"), lwd = c(2, 2), col = c("blue", "red"),
         bg = "white")
}
