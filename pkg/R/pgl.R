pgl <- function(q, lambda1 = 0, lambda2 = 1, lambda3 = 1, lambda4 = 1,
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

  jr <- q
  jr[sort.list(q)] <- seq(along = q)
  order.x <- order(q)
  xx <- sort(q)

  ## xx has the sorted data, and jr & order.x the information to get back to the
  ## original order.

  extreme <- qgl(c(inverse.eps, 1 - inverse.eps),
                 param = param, version = version)
  max.e <- extreme[2]
  min.e <- extreme[1]
  ind.min <- xx <= min.e
  ind.max <- xx >= max.e

  ## This simpler comparison works here because we are using inverse.eps as our
  ## tolerance

  q <-xx[as.logical((xx < max.e)*(xx > min.e))]

  ## We only want to calculate the probabilities for q values
  ## inside the support

  length.of.vector <- length(q)

  ## Need a blank u to send to C

  u <- 0*q

  result <-
    switch(version,
           freimer = ,
           frm = ,
           fmkl = ,
           FKML = ,
           fkml = ,
           ## Notes on .C call - the "numerics", param and inverse.eps
           ## don't need the as.??? call as they are implicitly double
           FMKL = .C("gl_fmkl_distfunc",
                     param[1], param[2], param[3], param[4],
                     as.double(0), as.double(1), inverse.eps,
                     as.integer(max.iterations), as.double(q), as.double(u),
                     as.integer(length.of.vector),
                     PACKAGE = "GeneralizedLambda"),

           ramberg = ,
           ram = ,
           rs = ,
           RS = .C("gl_rs_distfunc",
                   param[1], param[2], param[3], param[4],
                   as.double(0), as.double(1), inverse.eps,
                   as.integer(max.iterations), as.double(q), as.double(u),
                   as.integer(length.of.vector),
                   PACKAGE = "GeneralizedLambda"),

           FM5 = .C("gl_fm5_distfunc", param[1], param[2], param[3],
                    param[4], param[5],
                    as.double(0), as.double(1), inverse.eps,
                    as.integer(max.iterations), as.double(q), as.double(u),
                    as.integer(length.of.vector),
                    PACKAGE = "GeneralizedLambda"),

           vsk = ,
           gpd = ,
           GPD = ,
           VSK = .C("gl_vsk_distfunc",
                    param[1], param[2], param[3], param[4],
                    as.double(0), as.double(1), inverse.eps,
                    as.integer(max.iterations), as.double(q), as.double(u),
                    as.integer(length.of.vector),
                    PACKAGE = "GeneralizedLambda"),
           stop("Error: Parameterisation must be one of fmkl, rs, fm5 or vsk")
         ) # closes "switch"

  if (!(is.numeric(result[[1]]))){
    stop("Values for quantiles outside range. This shouldn't happen")
  }
  if (version =="fm5") {
    u <- result[[11]]
  }
  else 	{
    u <- result[[10]]
  }
  xx[as.logical((xx < max.e)*(xx > min.e))] <- u
  xx[ind.min] <- 0
  xx[ind.max] <- 1
  ## Revert to the original order of the dataset:
  xx[jr]
}
