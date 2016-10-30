rgl <- function(n, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
                param = c(lambda1, lambda2, lambda3, lambda4),
                version = "FMKL", lambda5 = NULL)
{
  ## Check the parameters
  parResult <- glCheckPars(param, version, lambda5)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  param <- as.numeric(param)
  ## Produce uniform observations
  p <- runif(n)
  ## convert to gl
  res <- qgl(p, param = param, version = version, lambda5 = lambda5)
  res
}


