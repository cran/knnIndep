\name{P_cge_ale}
\alias{P_cge_ale}
\title{
Probability of observing the ith nearest neighbour at a distance greater or equal to c and the (i-1)th nearest neighbour was observed at distance smaller or equal a
}
\description{
This function gives the probability of observing the ith nearest neighbour at a distance greater or equal to c and the (i-1)th nearest neighbour was observed at distance smaller or equal a
$P(d_i >= c, d_(i-1) <= a)$ 
}
\usage{
P_cge_ale(i, c, a, N)
}
\arguments{
  \item{i}{
	numeric, which nearest neighbour to consider
}
  \item{c}{
	vector, the distance at which the ith NN was observed
}
  \item{a}{
	vector, the distance at which the ith NN was observed. \code{a <= c}
}
  \item{N}{
	numeric, size of the dataset
}
}
\details{
The probability is calculated by ranking the data and assuming that the data lie on a torus. For details see Dümcke et al. "A novel test for independence derived from an exact distribution of ith nearest neighbours" (manuscript in preparation)
}
\value{
Probability vector, entries with value -1 if the probability does not exist
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
%\note{
%%%  ~~further notes~~
%}
\seealso{
\code{\link{P_ceq}}, \code{\link{Pc_givena}}
}
\examples{
P_cge_ale(10,4:8,2:6,30)
}
