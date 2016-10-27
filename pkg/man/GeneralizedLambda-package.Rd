\name{GeneralizedLambda-package}
\alias{GeneralizedLambda-package}
\docType{package}
\title{
\packageTitle{GeneralizedLambda}
}
\description{

  This package provides functions for multiple versions (commonly
  wrongly called parameterisations) of the generalized lambda
  distribution. Functions are provided (or will be) for the density
  function, distribution function, quantile function and for random
  number generation. Code has been drawn from a number of existing
  packages and reformatted to agree with the style of the packages
  GeneralizedHyperbolic, NormalLaplace, VarianceGamma and
  SkewHyperbolic.

}
\details{
  Functions \code{dgl}, \code{pgl}, \code{qgl}, \code{rgl} provide the
  density function, distribution function, quantile function and random
  number generation for different versions of the generalized lambda
  distribution.

  \code{glCheckPars} check whether supplied parameters represent a valid
  instance of the generalized lambda distribution.
}
\author{
  Jeong Kim <jkim831@aucklanduni.ac.nz>

  Maintainer: David Scott <d.scott@auckland.ac.nz>
}
\references{

  Freimer, M., Mudholkar, G. S., Kollia, G. & Lin, C. T. (1988),
  A study of the generalized {T}ukey lambda family,
  \emph{Communications in Statistics - Theory and Methods}
  \bold{17},3547--3567.

  Gilchrist, Warren G. (2000) \emph{Modelling with Quantile Functions},
  Chapman \& Hall/CRC, London.

  Karian,  Zaven A. and Dudewicz, Edward J. (2000), \emph{Fitting
  statistical distributions: the Generalized Lambda Distribution and
  Generalized Bootstrap methods}, Chapman & Hall, London.

  Karian, Z.E., Dudewicz, E.J., and McDonald, P. (1996),
  The extended generalized lambda distribution system for fitting
  distributions to data: history, completion of theory, tables,
  applications, the ``Final Word'' on Moment fits, \emph{Communications
  in Statistics - Simulation and Computation} \bold{25}, 611--642.

  Ramberg, J. S. & Schmeiser, B. W. (1974), An approximate method for
  generating asymmetric random variables,
  \emph{Communications of the ACM} \bold{17}, 78--82.

  Van Staden, Paul J., & M.T. Loots. (2009), Method of L-moment
  Estimation for the Generalized Lambda Distribution. In
  \emph{Proceedings of the Third Annual ASEARC Conference}. Callaghan,
  NSW 2308 Australia: School of Mathematical and Physical Sciences,
  University of Newcastle.

}

\keyword{ distribution }
\seealso{
  \code{\link[gld]{GeneralizedLambdaDistribution}},
  \code{\link[GLDEX]{GLDEX.package}}, \code{\link[gldist]{gldist-package}},
  and \code{\link[fBasics]{gld}} for other implementations of the
  generalized lambda distribution.

  \code{\link{GeneralizedLambda}} for a description of the different
  generalized lambda distribution versions.
}
\examples{
qgl(seq(0, 1, 0.02), 0, 1, 0.123, -4.3,
    lower.tail = FALSE, version = "FMKL")
rgl(10, 0, 1.24, 1, 0.7, version = "VSK")
\dontrun{
  pgl(0.6, 1, 0.2, 0.3, -2, version = "RS") # Error
}
}
