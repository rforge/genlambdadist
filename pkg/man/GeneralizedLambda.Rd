\name{GeneralizedLambda}
\alias{dgl}
\alias{pgl}
\alias{qgl}
\alias{rgl}
\alias{GeneralizedLambda}
\alias{GeneralizedLambdaDistribution}

\title{The Generalized Lambda Distribution}
\description{

  Density, density quantile, distribution function, quantile function
  and random generation for the generalized lambda distribution (also
  known as the asymmetric lambda, or Tukey lambda).  Provides for four
  different parameterisations, the \code{FMKL} (recommended), the
  \code{RS}, the \code{VSK} , the \code{AS} and a five parameter version
  of the FMKL, the \code{FM5}.

}
\usage{
dgl(x, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
    param = c(lambda1, lambda2, lambda3, lambda4),
    version = "FMKL", lambda5 = NULL,
    inverse.eps = .Machine$double.eps, max.iterations = 500)

pgl(q, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
    param = c(lambda1, lambda2, lambda3, lambda4),
    version = "FMKL", lambda5 = NULL,
    inverse.eps = .Machine$double.eps, max.iterations = 500)

qgl(p, lambda1, lambda2, lambda3, lambda4,
    param = c(lambda1, lambda2, lambda3, lambda4),
    version = "FMKL", lambda5 = NULL, lower.tail = TRUE)

rgl(n, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
    param = c(lambda1, lambda2, lambda3, lambda4),
    version = "FMKL", lambda5 = NULL)
}
\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{param}{vector of length 4 specifying the parameters of the
    generalized lambda distribution (see below for details). \emph{Note
    that in the \code{VSK} parameterisation, the four parameters are
    labelled alpha, beta, delta,lambda.}
  }

  \item{lambda1}{\eqn{\lambda_1}{lambda 1}, the location parameter of
    the distribution alpha for the \code{VSK} parameterisation.}

  \item{lambda2}{\eqn{\lambda_2}{lambda 2} - scale parameter beta for
    \code{VSK}}

  \item{lambda3}{\eqn{\lambda_3}{lambda 3} - first shape parameter
    delta, a skewness parameter for \code{VSK}}

  \item{lambda4}{\eqn{\lambda_4}{lambda 4} - second shape parameter
    lambda, a tail-shape parameter for \code{VSK}}

  \item{lambda5}{\eqn{\lambda_5}{lambda 5} - a skewing parameter, in the
    fm5 parameterisation}

  \item{version}{Choose parameterisation (see below for details)}

  \item{lower.tail}{ Logical. If TRUE, probabilities are
    \eqn{P(X <= x)}, otherwise as \eqn{P(X > x)}. \code{FMKL} uses
    Freimer, Mudholkar, Kollia and Lin (1988) (default).  \code{RS} uses
    Ramberg and Schmeiser (1974). \code{VSK} uses VSK parameterisation,
    see van Staden and Loots (2009). \code{AS} uses asymmetry steepness
    \code{fm5} uses the 5 parameter version of the FMKL parameterisation
    see Gilchrist (2000)
    }
  \item{inverse.eps}{Accuracy of calculation for the numerical determination of
	\eqn{F(x)}, defaults to \code{.Machine$double.eps}}
  \item{max.iterations}{Maximum number of iterations in the numerical
	determination of \eqn{F(x)}, defaults to 500}
  }

\details{

  The generalized lambda distribution, also known as the asymmetric
  lambda, or Tukey lambda distribution, is a distribution with a wide
  range of shapes.  The distribution is defined by its quantile function
  Q(u), the inverse of the distribution function.

  The \code{GeneralizedLambda} package implements three
  parameterisations of the distribution.  The default parameterisation
  (the FMKL) is that due to \emph{Freimer Mudholkar, Kollia and Lin
  (1988)} (see references below), with a quantile function:

\deqn{Q(u) = \lambda_1 + { { \frac{u^{\lambda_3}-1}{\lambda_3} - 	%
\frac{(1 - u)^{\lambda_4}-1}{\lambda_4} } \over \lambda_2 } 		%
}{Q(u) = lambda1 + ( (u^lambda3 - 1)/lambda3 - ((1 - u)^lambda4-1)
/lambda4 ) / lambda 2}
for \eqn{\lambda_2 > 0}{lambda2 > 0}.

  A second parameterisation, the RS, chosen by setting \code{version="RS"} is
  that due to \emph{Ramberg and Schmeiser (1974)}, with the quantile function:
  \deqn{Q(u) = \lambda_1 + \frac{u^{\lambda_3} - (1-u)^{\lambda_4}} 	%
  {\lambda_2} }{Q(u) = lambda1 + ( u^lambda3 - (1-u)^lambda4 ) / %
  lambda 2}

  This parameterisation has a complex series of rules determining which
  values of the parameters produce valid statistical distributions.  See
  \link{glCheckPars} for details.

  Another parameterisation, the VSK, chosen by setting \code{version =
  "VSK"} is due to van Staden and Loots (2009), with a quantile function:

  \deqn{Q(u) = \alpha + \beta ((1 - \delta)\frac{(u^\lambda - 1)}{\lambda} - \delta\frac{((1 - u)^\lambda -1)}{\lambda} )}{Q(u) = alpha + beta ((1 - delta)(u^lambda - 1)/(lambda) - delta((1 - u)^lambda - 1)/lambda }
for \eqn{\beta > 0}{beta > 0}
and \eqn{-1 \leq \delta \leq 1}{-1 <= delta <= 1}.
  (where the parameters appear in the \code{par} argument to the
  function in the order \eqn{\alpha,\beta,\delta,\lambda}).  This
  parameterisation has simpler L-moments than other parameterisations
  and \eqn{\delta} is a skewness parameter and \eqn{\lambda} is a
  tailweight parameter.

  Another parameterisation, the FM5, chosen by setting \code{version = "FM5"}
  adds an additional skewness parameter to the FMKL parameterisation.
  This uses the same approach as that used by Gilchrist (2000)
  for the RS parameterisation.  The quantile function is
\deqn{F^{-1}(u) = \lambda_1 + { { \frac{(1 - \lambda_5)(u^{\lambda_3} - 1)}%
{\lambda_3} - \frac{(1+\lambda_5)((1-u)^{\lambda_4}-1)}{\lambda_4} }  %
\over \lambda_2 }}{F inverse (u) = lambda1 + ( (1 - lambda5)(u^lambda3 - 1)/lambda3
- (1 + lambda5)((1 - u)^lambda4 - 1)/lambda4 )/lambda 2}
for \eqn{\lambda_2 > 0}{lambda2 >0}
and \eqn{-1 \leq \lambda_5 \leq 1}{-1 <= lambda5 <= 1}.

  The distribution is defined by its quantile function and its distribution and
  density functions do not exist in closed form.  Accordingly, the results
  from \code{pgl} and \code{dgl} are the result of numerical solutions to the
  quantile function, using the Newton-Raphson method.
}
\value{

  \code{pgl} gives the distribution function (based on a numerical
	solution to \eqn{F^{-1}(u) = x}{F inv (u) = x}),

  \code{qgl} gives the quantile function.

  \code{rgl} generates random deviates.
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
\author{
  Robert King, \email{robert.king@newcastle.edu.au},
  \url{http://tolstoy.newcastle.edu.au/~rking/}

  Jeong Min Kim, \email{jkim831@aucklanduni.ac.nz}

  David Scott, \email{d.scott@auckland.ac.nz}
}

\examples{

qgl(seq(0, 1, 0.02), 0, 1, 0.123, -4.3,
    lower.tail = FALSE, version = "FMKL")
rgl(10, 0, 1.24, 1, 0.7, version = "VSK")
\dontrun{
  pgl(0.6, 1, 0.2, 0.3, -2, version = "RS") # Error
}

}
\keyword{distribution}
