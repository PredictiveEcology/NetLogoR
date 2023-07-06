# Run the WolfSheepPredation SpaDES module
library(NetLogoR)
library(quickPlot)
if (requireNamespace("SpaDES.core") && requireNamespace("SpaDES.tools")) {

  # Parameters values
  wolfSheepParams <- list(.plotInitialTime = 0, .plotInterval = 10,
                          .saveInitialTime = 0, .saveInterval = 1,
                          grassOn = TRUE, grassTGrowth = 30,
                          nSheep = 100, gainFoodSheep = 4, reproSheep = 4,
                          nWolf = 50, gainFoodWolf = 20, reproWolf = 5)

  # Model init
  wolfSheepSim <- SpaDES.core::simInit(
    times = list(start = 0, end = 500),
    params = list(WolfSheepPredation = wolfSheepParams),
    modules = list("WolfSheepPredation"),
    paths = list(modulePath = system.file("examples/Wolf-Sheep-Predation", package = "NetLogoR"))
  )
  # Run the model
  wolfSheepRun <- SpaDES.core::spades(wolfSheepSim) # use debug=FALSE for no messaging

  # Plot outputs
  dev(6)
  clearPlot()
  timeStep <- seq_along(wolfSheepRun$numSheep)

  maxSheep <- max(wolfSheepRun$numSheep)
  maxWolves <- max(wolfSheepRun$numWolves)
  maxGreen <- max(wolfSheepRun$numGreen / 4)

  if (wolfSheepParams$grassOn == TRUE) {
    plot(timeStep, wolfSheepRun$numSheep, type = "l", col = "blue", lwd = 2,
         ylab = "Population size", xlab = "Time step",
         ylim = c(min = 0, max = max(c(maxSheep, maxWolves, maxGreen))))
    lines(timeStep, wolfSheepRun$numWolves, col = "red", lwd = 2)
    lines(timeStep, wolfSheepRun$numGreen / 4, col = "green", lwd = 2)

    legend("topleft", legend = c("Sheep", "Wolves", "Grass / 4"), lwd = c(2, 2, 2),
           col = c("blue", "red", "green"), bg = "white")
  } else {
    plot(timeStep, wolfSheepRun$numSheep, type = "l", col = "blue", lwd = 2,
         ylab = "Population size", xlab = "Time step",
         ylim = c(min = 0, max = max(c(maxSheep, maxWolves))))
    lines(timeStep, wolfSheepRun$numWolves, col = "red", lwd = 2)

    legend("topleft", legend = c("Sheep", "Wolves"), lwd = c(2, 2),
           col = c("blue", "red"), bg = "white")
  }
}
