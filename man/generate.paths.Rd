\name{generate.paths}
\alias{generate.paths}
\title{
Generate all nearest neighbours distances for one point in a sample
}
\description{
Help function which generates the nearest neighbhour distances for a single point in a sample, assuming rank data on a torus with the maximum distance.
}
\usage{
generate.paths(index, rx, ry, N)
}
\arguments{
  \item{index}{
    for which point to calculate the nearest neighbhour distances
}
  \item{rx}{
    ranked data (1st dimension)
}
  \item{ry}{
    ranked data (2nd dimension)
}
  \item{N}{
    Number of points in sample
}
}
\value{
    a vector of length (N-1) containing the sorted distances to the nearest neighbour of point index in the sample
}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
\examples{
    x=rank(runif(10))
    y=rank(runif(10))
    knnIndep:::generate.paths(5,x,y,10)
    #for all points in the sample
    sapply(1:10,knnIndep:::generate.paths,x,y,10)
}
