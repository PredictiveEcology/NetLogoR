% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patch-functions.R
\name{randomPxcor}
\alias{randomPxcor}
\alias{randomPxcor,worldNLR,numeric-method}
\title{Random \code{pxcor}}
\usage{
randomPxcor(world, n)

\S4method{randomPxcor}{worldNLR,numeric}(world, n)
}
\arguments{
\item{world}{\code{WorldMatrix} or \code{worldArray} object.}

\item{n}{Integer.}
}
\value{
Integer. Vector of length \code{n} of \code{pxcor} coordinates.
}
\description{
Report \code{n} random \code{pxcor} coordinates within the \code{world}'s extent.
}
\examples{
w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
pxcor <- randomPxcor(world = w1, n = 10)

}
\references{
Wilensky, U. 1999. NetLogo. \url{http://ccl.northwestern.edu/netlogo/}.
Center for Connected Learning and Computer-Based Modeling,
Northwestern University. Evanston, IL.
}
\seealso{
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor}
}
\author{
Sarah Bauduin
}
