\name{glCheckPars}
\alias{glCheckPars}
\alias{glCheckParsFMKL}
\alias{glCheckParsRS}
\alias{glCheckParsVSK}
\alias{glCheckParsAS}
\alias{glCheckParsFMKL5}

\title{
  Function to check the validity of parameters of the generalized lambda
  distribution
}
\description{
  Checks the validity of parameters of the generalized lambda. The tests
  are simple for the FMKL, VSK and FM5 parameterisations, and much more
  complex for the RS parameterisation.
}
\usage{
glCheckPars(param, version = "FMKL", lambda5 = NULL)
glCheckParsFMKL(param)
glCheckParsRS(param)
glCheckParsVSK(param)
glCheckParsAS(param)
glCheckParsFMKL5(param, lambda5)
}
\arguments{
  \item{param}{Vector of length 4.
    Each element of the vector defines lambda1 to lambda4, where lambdas
    from 1 to 4 define the location, scale and first and second shape
    parameters of the distribution.

    \emph{Note that the numbering of the \eqn{\lambda}{lambda} parameters
    for the \emph{fmkl} parameterisation is different to that used by
    Freimer, Mudholkar, Kollia and Lin.}
  }
  \item{lambda5}{a skewness parameter, in the fm5 parameterisation.}
  \item{version}{choose parameterisation:
    \code{FMKL} uses Freimer, Mudholkar, Kollia and Lin (1988) (default).
    \code{RS} uses Ramberg and Schmeiser (1974)
    \code{FM5} uses the 5 parameter version of the FMKL parameterisation}

}
\details{
  See \code{\link{GeneralizedLambda}} for details on the
  generalised lambda distribution.  This function determines the validity of
  parameters of the distribution.

  The FMKL parameterisation gives a valid
  statistical distribution for any real values of \eqn{\lambda_1}{lambda 1},
  \eqn{\lambda_3}{lambda 3},\eqn{\lambda_4}{lambda 4} and any positive real
  values of \eqn{\lambda_2}{lambda 2}.

  The FM5 parameterisation gives a statistical distribution for any real
  values of \eqn{\lambda_1}{lambda 1}, \eqn{\lambda_3}{lambda 3},
  \eqn{\lambda_4}{lambda 4}, any positive real values of
  \eqn{\lambda_2}{lambda 2} and values of \eqn{\lambda_5}{lambda 5} that
  satisfy \eqn{-1 \leq \lambda_5 \leq 1}{-1 <= lambda5 <= 1}.

  For the RS parameterisation, the combinations of parameters value that give
  valid distributions are the following (the region numbers in the table
  correspond to the labelling of the regions in Ramberg and
  Schmeiser (1974) and Karian, Dudewicz and McDonald (1996)):

\tabular{cccccc}{
region \tab \eqn{\lambda_1}{lambda 1} \tab \eqn{\lambda_2}{lambda 2}
\tab \eqn{\lambda_3}{lambda 3} \tab \eqn{\lambda_4}{lambda 4} \tab note \cr
1 \tab	all 	\tab \eqn{<0}	\tab \eqn{< -1}		\tab \eqn{> 1} \tab \cr
2 \tab	all 	\tab \eqn{<0}	\tab \eqn{> 1}		\tab \eqn{< -1} \tab \cr
3 \tab	all 	\tab \eqn{>0}	\tab \eqn{\ge 0}	\tab \eqn{\ge 0} \tab
one of \eqn{\lambda_3}{lambda 3} and \eqn{\lambda_4}{lambda 4} must be non-zero \cr
4 \tab  all     \tab \eqn{<0} 	\tab \eqn{\le 0}	\tab \eqn{\le 0} \tab
one of \eqn{\lambda_3}{lambda 3} and \eqn{\lambda_4}{lambda 4} must be non-zero \cr
5 \tab 	all	\tab \eqn{<0}	\tab \eqn{> -1} and \eqn{< 0} 	\tab \eqn{>1}
\tab equation 1 below must also be satisfied \cr
6 \tab 	all	\tab \eqn{<0}	\tab \eqn{>1} 	\tab \eqn{> -1} and \eqn{< 0}
\tab equation 2 below must also be satisfied \cr
}

Equation 1

\deqn{  \frac{(1-\lambda_3) ^{1-\lambda_3}(\lambda_4-1)^{\lambda_4-1}}
{(\lambda_4-\lambda_3)^{\lambda_4-\lambda_3}} <
- \frac{\lambda_3}{\lambda_4} }{% non TeX version
( (1-lambda3) ^ ( 1 - lambda3) * (lambda4 -1) ^ (lambda4 -1) ) /
( (lambda4 - lambda3) ^ (lambda4 - lambda3) ) <
- lambda3 / lambda 4 }

Equation 2

\deqn{  \frac{(1-\lambda_4) ^{1-\lambda_4}(\lambda_3-1)^{\lambda_3 - 1}}
{(\lambda_3 - \lambda_4)^{\lambda_3 - \lambda_4}} <
- \frac{\lambda_4}{\lambda_3} }{%
( (1-lambda4)^( 1 - lambda4)*(lambda3 -1)^(lambda3 -1) ) /
( (lambda3 - lambda4) ^ (lambda3 - lambda4) ) <
- lambda4 / lambda 3
}

}

\value{

  \code{glCheckPars} returns \code{TRUE} if the
  parameter values given produce a valid statistical distribution and
  "error" if they don't according to its specified
  parameterisation. The default parameterisation is FMKL and without
  specifying parameterisation, the following functions can be used:

\code{glCheckParsFMKL} is used for FMKL parameterisation
\code{glCheckParsRS} is used for RS parameterisation
\code{glCheckParsVSK} is used for VSK parameterisation
\code{glCheckParsAS} is used for AS parameterisation
\code{glCheckParsFMKL5} is used for F5 parameterisation}

\references{

  Freimer, M., Mudholkar, G. S., Kollia, G. & Lin, C. T. (1988),
  A study of the generalized {T}ukey lambda family,
  \emph{Communications in Statistics - Theory and Methods}
  \bold{17},3547--3567.

  Gilchrist, Warren G. (2000) \emph{Modelling with Quantile Functions},
  Chapman \& Hall/CRC, London.

  Karian, Z.E., Dudewicz, E.J., and McDonald, P. (1996),
  The extended generalized lambda distribution system for fitting
  distributions to data: history, completion of theory, tables,
  applications, the ``Final Word'' on Moment fits, \emph{Communications
  in Statistics - Simulation and Computation} \bold{25}, 611--642.

  Ramberg, J. S. & Schmeiser, B. W. (1974), An approximate method for
  generating asymmetric random variables,
  \emph{Communications of the ACM} \bold{17}, 78--82.

}

\author{
  Robert King, \email{robert.king@newcastle.edu.au},
  \url{http://tolstoy.newcastle.edu.au/~rking/}

  Jeong Min Kim, \email{jkim831@aucklanduni.ac.nz}

  David Scott, \email{d.scott@auckland.ac.nz}
}
\note{
  The complex nature of the rules in this function for the RS
parameterisation are the reason for the invention of the FMKL
parameterisation and its status as the default parameterisation in the
other generalized lambda functions.
}

\seealso{
  The generalized lambda functions
  \code{\link{GeneralizedLambdaDistribution}}
}

\examples{

glCheckPars(param = c(0,1,.23,4.5)) ## TRUE
\dontrun{
  glCheckPars(c(0,-1,.23,4.5)) ## error
  glCheckPars(c(0,1,0.5,-0.5), param = "RS") ## error
  glCheckPars(c(0,2,1,3.4,1.2), param = "fm5") ## error
}
glCheckParsFMKL(param = c(1,2,0.5,-2)) ## TRUE
\dontrun{
  glCheckParsRS(param = c(-1,0.23,0.5,-2)) ## error
  glCheckParsVSK(param = c(0.1,-2,0,-2)) ## error

  glCheckParsAS(param = c(1,2,0,0)) ## Not yet implemented
  glCheckParsFMKL5(param = c(1,2,0.5,-2)) ## Not yet implemented
}
}
\keyword{distribution}
