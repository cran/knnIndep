\name{P_di}
\alias{P_di}
\title{
Probability distribution of the distance to the ith nearest neighbour
}
\description{
This function gives the distribution of the distances to the ith nearest
neighbour of a reference point.
}
\usage{
P_di(i, a, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
    which neares neighbhour to calculate the probability for
}
  \item{a}{
    the distance at which the ith nearest neighbout was observed, can be a vector
}
  \item{N}{
    how many points in a sample
}
}
\value{
    returns the probability of observing the ith nearest neighbour at distance a in a sample of size N
}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
\seealso{
\code{\link{Pc_givena},\link{Pc_givena4nn}}
}
\examples{
knnIndep:::P_di(4,3,10)
}
