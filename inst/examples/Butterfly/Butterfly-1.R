################################################################################
# Butterfly Hilltopping model (Butterfly-1.nlogo)
# by Railsback and Grimm (2012), pages 47-59
#
# Converted into R using the NetLogoR package
# by Sarah Bauduin
#
#

#library(NetLogoR)

# Create a world with the desired extent
elevation <- createWorld(minPxcor = 0, maxPxcor = 149, minPycor = 0, maxPycor = 149)

# Define the patches values
# Elevation decreases linearly with distance from the center of the hill
# Hills are at (30,30) and (120,100)
# The 1st hill is 100 units high, the 2nd hill is 50 units high
elev1 <- 100 - NLdist(agents = patches(elevation), agents2 = cbind(x = 30, y = 30))
elev2 <- 50 - NLdist(agents = patches(elevation), agents2 = cbind(x = 120, y = 100))
pElevation <- ifelse(elev1 > elev2, elev1, elev2)
# Assign the elevation values to the patches
elevation <- NLset(world = elevation, agents = patches(elevation), val = pElevation)

# Visualize the world
plot(elevation)

# Create turtles (one butterfly in this model)
t1 <- createTurtles(n = 1, coords = cbind(xcor = 85, ycor = 95)) # the butterfly's initial location is [85, 95]
# t1 <- createTurtles(n = 100, coords = cbind(xcor = 85, ycor = 95)) # can try with 100 butterflies
# Visualize the turtle
# points(turtles2spdf(t1), pch = 16)

# Define the global variable needed
q <- 0.4 # q is the probability to move directly to the highest surrounding patch

# Create a go procedure with a for loop
for(time in 1:1000){ # what is inside this loop will be iterated 1000 times

  # The "move" function can be written directly here or before in the script as a separate function and then called here
  # The output of the NetLogoR functions is the turtle t1 and it needs to be reassigned to t1
  # so that the updated turtle t1 is used at each time step

  if(runif(n = 1, min = 0, max = 1) < q){ # conditional statement

    # Move the turtle t1 uphill considering 8 neighbor patches in the elevation world
    t1 <- uphill(world = elevation, turtles = t1, nNeighbors = 8)

  } else {

    # Or move the turtle t1 to one of its neighbor patches at random in the elevation world
    # Creating local variables instead of putting everything in the same one line code helps to understand
    # It is also useful for debugging
    allNeighbors <- neighbors(world = elevation, agents = t1, nNeighbors = 8)
    oneNeighbor <- oneOf(allNeighbors)
    t1 <- moveTo(turtles = t1, agents = oneNeighbor)

  }

  # Visualize each new position for t1
  # Very slow, remove for speed
  points(turtles2spdf(t1), pch = 16)

  # Show the time step on the screen
  # Slow, remove for speed
  # print(time)
}
