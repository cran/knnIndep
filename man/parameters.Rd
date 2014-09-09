\name{parameters}
\alias{parameters}
\alias{kr}
\title{
Central probabilty
}
\description{
Probability of observing r NN distances at distance c, all previous NN distances at distance < c and all following NN distances at a distance > c
}
\usage{
parameters(r, i0, c, N)
kr(r, i0, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
    the number of points that are at the same distance c
}
  \item{i0}{
    which i0-th nearest neighbour we are considering. 
}
  \item{c}{
    the distance of the i-th nearest neighbour
}
  \item{N}{
    sample size
}
}
\value{
    for \code{kr} the number of possibilities to place r points onto the same distance when we already observed i0 points at a smaller distance

    for \code{parameters} the probability of observing r NN distances at distance c, all previous NN distances at distance < c and all following NN distances at a distance > c
}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
\examples{
knnIndep:::kr(3,5,6)
knnIndep:::parameters(3,5,6,20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
