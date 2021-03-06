% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confintplot.R
\name{confintplot}
\alias{confintplot}
\title{Plot mean and median with bootstrapped confidence intervals.}
\usage{
confintplot(x, nrep = NULL, p.level = 0.95, smooth = TRUE,
  box.width = 0.5, whisker.length = box.width * 1.4,
  col.mean = "lightblue", col.median = "orange", col.box = 1,
  lwd.box = 1, lwd.centre = 2, lty.centre = 1, ...)
}
\arguments{
\item{x}{vector, matrix, data.frame or list of variables.}

\item{nrep}{number of repetitions. By default set to
\code{max(10, 2*(10^5)/lengths(x))}.}
}
\description{
An alternative to \code{\link{boxplot}} that plots the 
mean and median for each variable and their confidence intervals,
all calculated using bootstrapping.
}
\examples{
confintplot(matrix(c(1, 3, 4, 4, 6, 5, 5, 7, 6, 5, 8, 5), ncol=2))

## works with variables of unequal length.
## returs invisibly the output of bootstats for each variable,
## plus the x-positions used when plotting.
set.seed(1)

l <- list(a=c(rnorm(5)), b=rnorm(9), ctrl=rnorm(15, 0.5))
(confintplot(l, ylab="Q", main="Pilot test results"))
 
## many variables
set.seed(1)
xx <- matrix(0.1*(1:40)*runif(160, 1, 4)^2, 20, byrow=TRUE)
confintplot(xx, ylab="Magnitude")

## more advanced graphical parameters
set.seed(1)
n <- 20
dtf <- data.frame(aa=runif(n, 0, 2)^2, bb=rnorm(n), cc=rgamma(n, 1.5))

col.mean <- "darkgray"
col.median <- "lightgray"
col.box <- rainbow(3, start=0.6, end=1)

z <- confintplot(dtf, p.level=0.99, whisker.length=0.9, box.width=0.7,
  col.mean=col.mean, col.median=col.median, col.box=col.box, 
  lwd.box=2, lwd.centre=1, lty.centre=2)

}
\keyword{hplot}
\keyword{multivariate}
