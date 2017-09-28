# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "WolfSheepPredation",
  description = paste("Translation into R using the NetLogoR and SpaDES packages",
                      "of the Wolf-Sheep-Predation NetLogo model created by Wilensky (1997)"),
  keywords = c("NetLogo", "NetLogoR", "SpaDES", "wolf", "predator", "sheep", "prey", "predation"),
  authors = c(person("Sarah", "Bauduin", email = "sarahbauduin@hotmail.fr",
                     role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.2.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day", # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "WolfSheepPredation.Rmd"),
  reqdPkgs = list("NetLogoR", "quickPlot", "SpaDES.core", "SpaDES.tools"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA,
                    "This describes the simulation time at which the first plot event
                    should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA,
                    "This describes the simulation interval time at which plot events
                    should occur"),
    defineParameter(".saveInitialTime", "numeric", 0, NA, NA,
                    "This describes the simulation time at which the first save event
                    should occur"),
    defineParameter(".saveInterval", "numeric", 1, NA, NA,
                    "This describes the simulation interval time at which save events
                    should occur"),
    defineParameter("grassOn", "logical", TRUE, NA, NA,
                    "TRUE to include grass in the model, FALSE to only include wolves and sheep"),
    defineParameter("grassTGrowth", "numeric", 30, 0, NA,
                    "How long it takes for grass to regrow once it is eaten"),
    defineParameter("nSheep", "numeric", 100, 0, NA, "Initial sheep population size"),
    defineParameter("gainFoodSheep", "numeric", 4, 0, NA,
                    "Amount of energy sheep get for every grass patch eaten"),
    defineParameter("reproSheep", "numeric", 4, 0, 100,
                    "Probability in % of a sheep reproducing at each time step"),
    defineParameter("nWolf", "numeric", 50, 0, NA, "Initial wolf population size"),
    defineParameter("gainFoodWolf", "numeric", 20, 0, NA,
                    "Amount of energy wolves get for every sheep eaten"),
    defineParameter("reproWolf", "numeric", 5, 0, 100,
                    "Probability in % of a wolf reproducing at each time step")
  ),
  inputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    sourceURL = "",
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.WolfSheepPredation = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # Create the world, the sheep and the wolves
    sim <- sim$WolfSheepPredationInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$WolfSheepPredation$.plotInitialTime,
                         "WolfSheepPredation", "plot")
    sim <- scheduleEvent(sim, params(sim)$WolfSheepPredation$.saveInitialTime,
                         "WolfSheepPredation", "save")
    sim <- scheduleEvent(sim, start(sim), "WolfSheepPredation", "event")

  } else if (eventType == "plot") {

    dev(4)
    sim <- sim$WolfSheepPredationPosition(sim)
    dev(5)
    sim <- sim$WolfSheepPredationPopSize(sim)

    sim <- scheduleEvent(sim, time(sim) + params(sim)$WolfSheepPredation$.plotInterval,
                         "WolfSheepPredation", "plot")

  } else if (eventType == "save") {

    sim <- sim$WolfSheepPredationSave(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$WolfSheepPredation$.saveInterval,
                         "WolfSheepPredation", "save")

  } else if (eventType == "event") {

    sim <- sim$WolfSheepPredationEvent(sim)
    sim <- scheduleEvent(sim, time(sim) + 1, "WolfSheepPredation", "event")

  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
WolfSheepPredationInit <- function(sim) {
  # Create the world
  grass <- createWorld(minPxcor = -25, maxPxcor = 25, minPycor = -25, maxPycor = 25)
  if (params(sim)$WolfSheepPredation$grassOn == FALSE) {
    grass <- NLset(world = grass, agents = patches(grass), val = 1) # cannot plot an empty world
  }
  # If grassOn is TRUE, assign grass and countdown values to patches
  # Because there are multiple patches variables, a worldArray is needed
  # If grassOn is TRUE, the grass grows and the sheep eat it, if FALSE, the sheep don't need to eat
  if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
    # Initialize patch values (grass and countdown) at random
    # 0 or 1 (i.e., green or brown in the NetLogo model)
    grassVal <- sample(c(0, 1), size = NLcount(patches(grass)), replace = TRUE)
    grass <- NLset(world = grass, agents = patches(grass), val = grassVal)
    countdown <- grass # countdown is a new world with the same extent as grass
    # Grass grow clock
    countdownVal <- runif(n = NLcount(patches(grass)),
                          min = 0, max = params(sim)$WolfSheepPredation$grassTGrowth)
    countdown <- NLset(world = countdown, agents = patches(countdown), val = countdownVal)
    sim$field <- stackWorlds(grass, countdown)
  }
  # When no patches values are used,
  # using grass, countdown or field as the world argument required by a function
  # does not change anything because they all have the same extent and number of patches.
  # When patches values are used (e.g., when the sheep eat the grass),
  # use only field as the world argument for the functions
  # which update and retrieve the patches values.
  # When field is updated, the values on the individual world grass and countdown are not updated,
  # only the layers in field are.

  # Assign the created world to the sim object
  sim$grass <- grass
  if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
    sim$field <- sim$field
  }

  # Create the sheep
  sheep <- createTurtles(
    n = params(sim)$WolfSheepPredation$nSheep,
    coords = randomXYcor(world = grass, n = params(sim)$WolfSheepPredation$nSheep),
    breed = "aSheep", color = rep("red", params(sim)$WolfSheepPredation$nSheep)
  )
  # Add the energy variable
  sheep <- turtlesOwn(turtles = sheep, tVar = "energy",
                      tVal = runif(n = params(sim)$WolfSheepPredation$nSheep, min = 0,
                                   max = 2 * params(sim)$WolfSheepPredation$gainFoodSheep))
  sim$sheep <- sheep # assign the created sheep to the sim object

  # Create the wolves
  wolves <- createTurtles(n = params(sim)$WolfSheepPredation$nWolf,
                          coords = randomXYcor(world = grass,
                                               n = params(sim)$WolfSheepPredation$nWolf),
                          breed = "wolf",
                          color = rep("black", params(sim)$WolfSheepPredation$nWolf))
  # Add the energy variable
  wolves <- turtlesOwn(turtles = wolves, tVar = "energy",
                       tVal = runif(n = params(sim)$WolfSheepPredation$nWolf, min = 0,
                                    max = 2 * params(sim)$WolfSheepPredation$gainFoodWolf))
  sim$wolves <- wolves # assign the created wolves to the sim object

  sim$numSheep <- numeric() # keep track of how many sheep there are
  sim$numWolves <- numeric() # keep track of how many wolves there are

    # Initialize the count of grass if grassOn == TRUE
  if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
    sim$numGreen <- numeric() # keep track of how much grass there is
  }

  return(invisible(sim))
}

### template for save events
WolfSheepPredationSave <- function(sim) {
  sim$numSheep <- c(sim$numSheep, NLcount(sim$sheep)) # add the new number of sheep
  sim$numWolves <- c(sim$numWolves, NLcount(sim$wolves)) # add the new numbr of wolves

  if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
    # patches equal to 1 (green)
    pGreen <- NLwith(world = sim$field, var = "grass", agents = patches(sim$field), val = 1)
    sim$numGreen <- c(sim$numGreen, NLcount(pGreen)) # add the new number of green patches
  }

  return(invisible(sim))
}

### template for plot events
WolfSheepPredationPosition <- function(sim) { # Plot the positions
  if (time(sim) == start(sim)) clearPlot()

  if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
    grassRas <- raster(sim$field@.Data[, , "grass"])
    extent(grassRas) <- extent(sim$field)
    Plot(grassRas, na.color = "white")
  } else {
    grassRas <- raster(sim$grass@.Data)
    extent(grassRas) <- extent(sim$grass)
    Plot(grassRas, col = "green")
  }

  if (NLcount(sim$sheep) > 0)
    sheep <- SpatialPoints(coordinates(sim$sheep))
    Plot(sheep, addTo = "grassRas", cols = "blue")
  if (NLcount(sim$wolves) > 0)
    wolves <- SpatialPoints(coordinates(sim$wolves))
    Plot(wolves, addTo = "grassRas", cols = "red")

  return(invisible(sim))
}

# Plot the population sizes
WolfSheepPredationPopSize <- function(sim) {
  if (time(sim) == params(sim)$WolfSheepPredation$.plotInitialTime) {
    clearPlot()
    plot(time(sim), NLcount(sim$wolves), xlim = c(start(sim), end(sim)),
         col = "red", pch = 19, cex = 0.5,
         ylim = c(0, params(sim)$WolfSheepPredation$nSheep * 6))
    points(time(sim), NLcount(sim$sheep),
           col = "blue", pch = 19, cex = 0.5)
  } else {
    points(time(sim), NLcount(sim$wolves),
           col = "red", pch = 19, cex = 0.5)
    points(time(sim), NLcount(sim$sheep),
           col = "blue", pch = 19, cex = 0.5)
    if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
      points(time(sim), sim$numGreen[time(sim)] / 4,
             col = "green", pch = 19, cex = 0.5)
    }
  }

  return(invisible(sim))
}

### template for the main event using the different functions defined under
WolfSheepPredationEvent <- function(sim){
  if (NLany(sim$sheep) | NLany(sim$wolves)) {
    # Ask sheep
    if (NLcount(sim$sheep) != 0) {
      moveSheep(sim)
      if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
        energySheep <- of(agents = sim$sheep, var = "energy")
        sim$sheep <- NLset(turtles = sim$sheep, agents = sim$sheep, var = "energy",
                           val = energySheep - 1)
        eatGrass(sim)
      }
      dieSheep(sim)
      if (NLcount(sim$sheep) != 0) {
        reproduceSheep(sim)
      }
    }

    # Ask wolves
    if (NLcount(sim$wolves) != 0) {
      moveWolves(sim)
      energyWolves <- of(agents = sim$wolves, var = "energy")
      sim$wolves <- NLset(turtles = sim$wolves, agents = sim$wolves,
                          var = "energy", val = energyWolves - 1)
      catchSheep(sim)
      dieWolves(sim)
      if (NLcount(sim$wolves) != 0) {
        reproduceWolves(sim)
      }
    }

    # Ask grass
    if (params(sim)$WolfSheepPredation$grassOn == TRUE) {
      growGrass(sim)
    }

  }

  return(invisible(sim))
}

### template for moveSheep
moveSheep <- function(sim) {
  sim$sheep <- move(sim$sheep)

  return(invisible(sim))
}

### template for moveWolves
moveWolves <- function(sim) {
  sim$wolves <- move(sim$wolves)

  return(invisible(sim))
}

### template for dieSheep
dieSheep <- function(sim) {

  sim$sheep <- death(sim$sheep)

  return(invisible(sim))
}

### template for dieWolves
dieWolves <- function(sim) {
  sim$wolves <- death(sim$wolves)

  return(invisible(sim))
}

### template for reproduceSheep
reproduceSheep <- function(sim) {
  sim$sheep <- reproduce(sim$sheep, params(sim)$WolfSheepPredation$reproSheep)

  return(invisible(sim))
}

### template for reproduceWolves
reproduceWolves <- function(sim) {
  sim$wolves <- reproduce(sim$wolves, params(sim)$WolfSheepPredation$reproWolf)

  return(invisible(sim))
}

#### Sheep and Wolves procedures

move <- function(turtles) {
  # turtles <- right(turtles, angle = runif(n = NLcount(turtles), min = 0, max = 50))
  # turtles <- left(turtles, angle = runif(n = NLcount(turtles), min = 0, max = 50))
  # The two above functions can be replaced by this next one,
  # as a negative value to turn right will turn left:
  turtles <- right(turtles, angle = runif(n = NLcount(turtles), min = -50, max = 50))
  turtles <- fd(world = grass, turtles = turtles, dist = 1, torus = TRUE)
  return(turtles)
}

death <- function(turtles) {
  # When energy dips below 0, die
  whoEnergy <- of(agents = turtles, var = c("who", "energy"))
  # "who" numbers of the turtles with their energy value below 0
  who0 <- whoEnergy[which(whoEnergy[,"energy"] < 0), "who"]

  if (length(who0) != 0) {
    turtles <- die(turtles = turtles, who = who0)
  }
  return(turtles)
}

reproduce <- function(turtles, reproTurtles) {
  # Throw dice to see if the turtles will reproduce
  repro <- runif(n = NLcount(turtles), min = 0, max = 100) < reproTurtles
  whoTurtles <- of(agents = turtles, var = "who") # "who" of the turtles before they reproduce
  reproWho <- whoTurtles[repro] # "who" of turtles which reproduce
  reproInd <- turtle(turtles, who = reproWho) # turtles which reproduce

  # if there is at least one turtle reproducing
  if (NLcount(reproInd) != 0) {
    energyTurtles <- of(agents = reproInd, var = "energy")
    # Divide the energy between the parent and offspring
    turtles <- NLset(turtles = turtles, agents = reproInd, var = "energy",
                     val = energyTurtles / 2)
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


### template for eatGrass
eatGrass <- function(sim) {
  pGreen <- NLwith(world = sim$field, var = "grass", agents = patches(sim$field),
                   val = 1) # patches with grass equal to 1 (green)
  sheepOnGreen <- turtlesOn(world = sim$field, turtles = sim$sheep,
                            agents = pGreen) # sheep on green patches

  if (NLcount(sheepOnGreen) != 0) {
    # These sheep gain energy by eating
    energySheep <- of(agents = sheepOnGreen, var = "energy") # energy before eating
    # Update energy
    sim$sheep <- NLset(turtles = sim$sheep, agents = sheepOnGreen, var = "energy",
                     val = energySheep + params(sim)$WolfSheepPredation$gainFoodSheep)

    # If a sheep is on a green patch (value equal to 1),
    # it eats the grass and turns it to brown (value to 0).
    pHere <- patchHere(world = sim$field, turtles = sheepOnGreen)
    sim$field <- NLset(world = sim$field, agents = pHere, var = "grass", val = 0)

  }

  return(invisible(sim))
}

### template for catchSheep
catchSheep <- function(sim) {
  # "who" numbers of sheep that are on the same patches as the wolves
  sheepWolves <- turtlesOn(world = sim$grass, turtles = sim$sheep,
                           agents = sim$wolves, simplify = FALSE)
  if (nrow(sheepWolves) != 0) {
    # sheepWolves[,"whoTurtles"] are the "who" numbers of sheep
    # sheepWolves[,"id"] represent the rank/order of the individual wolf in the wolves
    # (! not the "who" numbers of the wolves)
    sheepGrabbed <- oneOf(agents = sheepWolves) # grab one random sheep

    sim$sheep <- die(turtles = sim$sheep, who = sheepGrabbed) # kill the grabbed sheep
    whoWolves <- of(agents = sim$wolves, var = "who")
    whoGrabbingWolves <- whoWolves[unique(sheepWolves[,"id"])]
    grabbingWolves <- turtle(turtles = sim$wolves, who = whoGrabbingWolves)
    energyGrabbingWolves <- of(agents = grabbingWolves, var = "energy")
    # Get energy from eating for the wolves who grabbed sheep
    sim$wolves <- NLset(turtles = sim$wolves, agents = grabbingWolves, var = "energy",
                      val = energyGrabbingWolves + params(sim)$WolfSheepPredation$gainFoodWolf)
  }

  return(invisible(sim))
}

### template for growGrass
growGrass <- function(sim) {
  # Identify patches with grass equal to 0 (brown) and countdown less or equal to 0
  pBrown <- NLwith(world = sim$field, var = "grass", agents = patches(sim$field), val = 0)
  # Countdown values for the patches equal to 0 (brown)
  pBrownCountdown <- of(world = sim$field, var = "countdown", agents = pBrown)

  pBrownCountdown0 <- which(pBrownCountdown <= 0) # patches with a countdown <= 0
  if (length(pBrownCountdown0) != 0) {
    # Patches with grass equal to 0 (brown) and countdown <= 0
    pGrow <- pBrown[pBrownCountdown0, , drop = FALSE]
    # Grow some grass on these patches and reset the countdown
    sim$field <- NLset(
      world = sim$field, var = c("grass", "countdown"),
      agents = pGrow,
      val = cbind(
        grass = rep(1, NLcount(pGrow)),
        countdown = rep(params(sim)$WolfSheepPredation$grassTGrowth, NLcount(pGrow)))
      )
  }

  pBrownCountdown1 <- which(!pBrownCountdown <= 0) # patches with a countdown > 0
  if (length(pBrownCountdown1) != 0) {
    # patches with grass equal to 0 (brown) and countdown > 0
    pWait <- pBrown[pBrownCountdown1, , drop = FALSE]
    # Decrease the countdown for the patches which wait
    sim$field <- NLset(world = sim$field, var = "countdown", agents = pWait,
                       val = pBrownCountdown[pBrownCountdown1] - 1)
  }

  return(invisible(sim))
}
