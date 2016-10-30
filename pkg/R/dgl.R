dgl <- function(x, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
                param = c(lambda1, lambda2, lambda3, lambda4),
                version = "FMKL", lambda5 = NULL,
                inverse.eps = .Machine$double.eps,
                max.iterations = 500){

  ## Check the parameters
  parResult <- glCheckPars(param, version, lambda5)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  ## calculate u=F(x) numerically, then use qdgl
  ## Unless x is outside the range, then density should be zero
  extreme <- qgl(c(inverse.eps, 1 - inverse.eps),
                 param = param, version = version)
  ## It may be better to change this to simply
  ## (x <= extreme[2])*(x >= extreme[1])
  outside.range <- !as.logical((x <= extreme[2])*(x >= extreme[1]))
  u <- pgl(x, param = param, version = version, lambda5 = lambda5,
           inverse.eps = inverse.eps, max.iterations = max.iterations)
  dens <- dqgl(u, param = param, version = version)
  dens[outside.range] <- 0
  dens
}

dqgl <- function(p, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
                 param = c(lambda1, lambda2, lambda3, lambda4),
                 version = "FMKL", lambda5 = NULL)
{
  ## Don't allow characters in lambda5
  ## common error with parameterisation stuff
  if(is.character(lambda5)) {
    stop(paste("lambda5 =", lambda5, "It should be a number between -1 and 1"))
  }

  ## Check the parameters
  parResult <- glCheckPars(param, version, lambda5)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  result <- switch(param,
                   ## Different tests apply for each parameterisation
                   freimer =,  # allows for alternate expressions
                   frm =,  # allows for alternate expressions
                   FMKL =,
                   FKML =,
                   fkm =,
                   fmkl =.dqglFMKL(p, param),
                   ramberg =, # Ramberg & Schmeiser
                   ram =,
                   RS =,
                   rs =.dqglRS(p, param),
                   fm5 = .dqglFM5(p, param, lambda5),
                   stop("Error: Parameterisation must be fkml, fm5 or rs")
	) # closes "switch"
  result
}


.dqglRS <- function(p, param)
{
  ## Check the parameters
  parResult <- glCheckPars(param, version = "RS")
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)
  outside.range <- !as.logical((p <= 1)*(p >= 0))
  ## u gets only the probabilities in [0,1]
  u <- p[!outside.range]
  dens <-  param[2]/(param[3]*(p^(param[3] -1)) +
                     param[4]*((1 - p)^(param[4] - 1)))
  dens
}


.dqglFMKL <- function(p, param)
{
  ## Check the parameters
  parResult <- glCheckPars(param, version = "FMKL")
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)
  outside.range <- !as.logical((p <= 1)*(p >= 0))
  ## u gets only the probabilities in [0,1]
  u <- p[!outside.range]
  ## The density is given by 1/Q'(u)
  dens <- param[2]/(p^(param[3] - 1) + (1 - p)^(param[4] - 1))
  dens
}

.dqglFM5 <- function(p, param, lambda5)
{
  ## Check the parameters
  parResult <- glCheckPars(param, version = "FM5", lambda5)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)
  outside.range <- !as.logical((p <= 1)*(p >= 0))
  ## u gets only the probabilities in [0,1]
  u <- p[!outside.range]
  ## The density is given by 1/Q'(u)
  dens <- param[2]/((1 - lambda5)*(u^(param[3] - 1)) +
                    (1 + lambda5)*((1 - u)^(param[4] - 1)) )
  dens
}

