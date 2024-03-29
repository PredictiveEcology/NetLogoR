% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turtle-functions.R
\name{inspect}
\alias{inspect}
\alias{inspect,agentMatrix,numeric-method}
\title{Inspect \code{turtles}}
\usage{
inspect(turtles, who)

\S4method{inspect}{agentMatrix,numeric}(turtles, who)
}
\arguments{
\item{turtles}{\code{AgentMatrix} object representing the moving \code{agents}.}

\item{who}{Integer. Vector of the \code{who} numbers for the selected \code{turtles}.}
}
\value{
\code{Dataframe} (\code{nrow} = \code{length(who)}) of the variables of the selected
individuals among the \code{turtles}.
}
\description{
Display all variables values for the selected individuals among the \code{turtles}.
}
\examples{
w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
t1 <- createOTurtles(world = w1, n = 10)
inspect(turtles = t1, who = c(2, 3))

}
\references{
Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
Center for Connected Learning and Computer-Based Modeling,
Northwestern University. Evanston, IL.
}
\seealso{
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#inspect}
}
\author{
Sarah Bauduin
}
