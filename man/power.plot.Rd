\name{power.plot}
\alias{power.plot}
\title{
Plot power of benchmarked tests of independence
}
\description{
This functions plots the results of the benchmark. Input are the estimated powers at a certin significance level from \code{\link{calculate.power}}.
}
\usage{
power.plot(powers, num.noise = seq(from = 0.1, to = 3, by = 0.1), mains = c("Linear", 
	"Quadratic", "Cubic", expression("Sine: period 4" * pi), 
	expression("Sine: period 16" * pi), "X^(1/4)", "Circle", "Step function",
	 "Torus"), col = c("black", "red", "blue", "green", "cyan", "brown", "pink"), 
	 labels = TRUE,	which = 1:nrow(powers[[1]]), show.legend = "bottomright")
}
\arguments{
  \item{powers}{
named list of matrices one for each method with dimension, with one row for each type of dependence and a column for each noise level
}
  \item{num.noise}{
matrix, noise levels at which the test were run (see \code{\link{run.tests}})
}
  \item{mains}{
character vector, title of each dependence type
}
  \item{col}{
character vector, specify the colours, one for each test
}
  \item{labels}{
labels to plot at the x axis, or TRUE (default) for standard label plotting (see \code{\link{axis}})
}
  \item{which}{
numeric vector, which type of dependence to plot
}
  \item{show.legend}{
	character, either ("bottomright", "topleft", "topright", or "bottomleft") indicates where to place the legend (see \code{\link{legend}}). \code{NULL} (default) to disable plotting a legend
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Does not return a value, used for the side-effect of plotting
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
\code{\link{calculate.power}}, \code{\link{run.tests}}
}
\examples{
mycor = function(...) cor(...)^2
noises = cbind(c(.3,.4,6),c(.3,.5,4))
colnames(noises) = c("1",".2") #mutual information of the noise levels
vals = run.tests(mycor,list(),1:2,noises,100)
power.cor = drop(calculate.power(vals))
power.plot(list(cor=power.cor),t(noises))
}
