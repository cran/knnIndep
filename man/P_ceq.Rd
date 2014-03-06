\name{P_ceq}
\alias{P_ceq}
\title{
	Probability of observing the ith nearest neighbour at the same distance or larger as the (i-1)th nearest neighbour
}
\description{
This function gives the probability of observing the ith nearest neighbour distance larger or equal to c, and the (i-1)th nearest neighbour at distance c, $P(d_i >= c, d_(i-1) = c)$.
}
\usage{
P_ceq(i, c, N)
}
\arguments{
  \item{i}{
	numeric, which nearest neighbour to consider
}
  \item{c}{
	vector, the distance at which the ith NN was observed
}
  \item{N}{
	numeric, size of the dataset
}
}
\details{
The probability is calculated by ranking the data and assuming that the data lie on a torus. For details see Dümcke et al. "A novel test for independence derived from an exact distribution of ith nearest neighbours" (manuscript in preparation)
}
\value{
Probability vector
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
%\note{
%}

\seealso{
\code{\link{P_cge_ale}}, \code{\link{Pc_givena}}
}
\examples{
P_ceq(10,1:10,25)
}
