qgl <- function(p, lambda1, lambda2, lambda3, lambda4,
                param = c(lambda1, lambda2, lambda3, lambda4),
                version = "FMKL", lambda5 = NULL,
                lower.tail = TRUE)
{
  if (lower.tail == FALSE) {
    p <- 1-p
  }

  ## Check the parameters
  parResult <- glCheckPars(param, version, lambda5)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  result <-
    switch(version,
           ## allows for alternate expressions
           freimer = ,
           frm = ,
           fmkl = ,
           FMKL = qglFMKL(p, param),
           ## Ramberg & Schmeiser
           ramberg = ,
           ram = ,
           RS = ,
           rs = qglRS(p, param),
           ##gpd uses GPD parameterisation
           gpd = ,
           GPD = ,
           vsk = ,
           VSK = qglVSK(p, param),
           ## Asymmetry-steepness
           asymmetry = ,
           as = ,
           AS = qglAS(p, param),
           ## Five parameter version
           fm5 = ,
           FM5 = ,
           fmkl5 = ,
           FMKL5 = qglFMKL5(param, lambda5),
           stop("Error: version not recognised, check documentation")
           )
  result
}

qglFMKL <- function(p, param)
{

  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]
  p <- as.double(p)

  ## u gets only the probabilities in [0,1]
  range <- !as.logical((p <= 1)*(p >= 0))
  u <- p[!range]
  ## If OK, determine special cases

  if (lambda3 == 0) {
    if (lambda4 == 0) {
      quants <- lambda1 + (log(u) - log(1-u))/lambda2
    } else {
      quants <- lambda1 +
          (log(u) - ((1 - u)^lambda4 - 1)/lambda4)/lambda2
    }
  } else {
    if (lambda4 == 0) {
      quants <- lambda1 +
          ((u^lambda3 - 1)/lambda3 - log(1 - u))/lambda2
    } else {
      quants <- lambda1 + ((u^lambda3 - 1)/lambda3
                           - ((1-u)^lambda4 - 1)/lambda4)/lambda2
    }
  }
  ## Now we have the quantiles for p values inside [0,1], put them in the right
  ## place in the result vector

  result <- p*NaN
  result[!range] <- quants

  ## The remaining "quantiles" are NaN
  result
}
## end of FMKL

qglRS <- function(p, param)
{
  u <- p


  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]
  quants <- lambda1 + (u^lambda3 - (1 - u)^lambda4)/lambda2
  quants
}

qglVSK <- function(p, param)
{

  ## param is a parameter containining (alpha,beta,lambda,delta)
  alpha <- param[1]
  pbeta <- param[2]
  delta <- param[3]
  lambda <- param[4]

  outside.range <- !as.logical((p <= 1)*(p >= 0))
  u <- p[!outside.range]
  if (lambda == 0){
    quants <- alpha + pbeta * ((1 - delta)*log(u) - delta*log(1 - u))
  } else {
    ## These special cases are here in case u=1 when delta is 0 and lambda is
    ## negative see delta zero question in Robert Kings gld package notes
    if (delta == 0){
      quants <- alpha + pbeta*((1 - delta)*(u^lambda - 1)/lambda)
    } else {
      if (delta ==1) {
        quants <- alpha + pbeta*( -delta*((1 - u)^lambda - 1)/lambda)
      } else {
        quants <- alpha + pbeta*((1 - delta)*(u^lambda - 1)/lambda -
                                     delta*((1 - u)^lambda -1)/lambda)
      }
    }
  }

  result <- p*NaN
  result[!outside.range] <- quants
  result

}

qglAS <- function(p, param)
{
  med <- param[1]
  iqr <- param[2]
  chi <- param[3]
  xi <- param[4]

  .Call("gldist_qgl", p, med, iqr, chi, xi)
}


qglFMKL5 <- function(p, param, lambda5)
{
  stop("qglFMKL5 not yet implemented")
}






