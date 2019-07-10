library(NetLogoR)
# install.packages("nnls")
# install.packages("lcmix", repos="http://R-Forge.R-project.org")
# install.packagse("MASS")
library(lcmix)
library(MASS)

# AGENTS
# Create a square landscape of 9 by 9 cells (81 cells total)
# Cell values are randomly chosen either 1 or 2
land <- createWorld(minPxcor = 1, maxPxcor = 9,
                    minPycor = 1, maxPycor = 9,
                    sample(c(1, 2), 81, replace = TRUE))
plot(land) # visualize the landscape
# Create three moving individuals (Three turtles)
# Place the turtles in the middle of the landscape just created
t1 <- createTurtles(n = 3, world = land)
points(t1) # visualize the turtles on the landscape
distRate <- 0.5
# MODEL
for(i in 1:10){ # run the model 10 times
  # Are the turtles on a 1 or a 2 cell
  # If it is on a 1, it will move a 1-cell distance
  # If it is on a 2, it will move a 2-cell distance
  # First, identify the cell the turtles are on
  cellTurtle <- patchHere(world = land, turtles = t1)
  # Then, what is the value of this cell
  distMove <- of(world = land, agents = cellTurtle)
  # The tutle moves with a mean of 1 or 2-cell distance 
  # at the time (distMove), drawn from a multivariate gamma 
  # distribution to show that agents move similar distances,
  # i.e., part of a social group or affected by unmeasured 
  # conditions.
  distShape <- distMove * distRate
  rho <- matrix(rep(0.8, length = nrow(t1)* nrow(t1)), ncol = nrow(t1))
  diag(rho) <- 1
  distMoveRan <- rmvgamma(2, distShape, distRate, rho)[1,] # vector 
  # The landscape is not a torus
  # And the turtles cannot move outside of the landscape
  t1 <- fd(turtles = t1, dist = distMoveRan,
           world = land, torus = FALSE, out = FALSE)
  # Then the turtles rotate with a multivariate normal turn
  #  angle, based on the mean of the group, correlated at 0.8
  meanHeading <- mean(of(agents = t1, var = "heading"))
  Sigma <- matrix(rep(0.8 * meanHeading, length = nrow(t1) * nrow(t1)), ncol = nrow(t1))
  diag(Sigma) <- meanHeading
  angleInd = mvrnorm(n = 1, mu = rep(meanHeading, nrow(t1)), Sigma = Sigma)#sd = 45)
  # The turtles rotate to the right (angleInd positive)
  # Or to the left (angleInd negative)
  t1 <- right(turtles = t1, angle = angleInd)
  points(t1) # visualize the turtles' new position
}

