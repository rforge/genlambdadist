glCheckPars <- function(param, version = "FMKL", lambda5 = NULL)
{
  switch(version,
         ## allows for alternate expressions
         freimer = ,
         frm = ,
         fmkl = ,
         FMKL = glCheckParsFMKL(param),
         ## Ramberg & Schmeiser
         ramberg = ,
         ram = ,
         RS = ,
         rs = glCheckParsRS(param),
         ## gpd uses GPD GLD version
         gpd = ,
         GPD = ,
         vsk = ,
         VSK = glCheckParsVSK(param),
         ## Asymmetry-steepness
         asymmetry = ,
         as = ,
         AS = glCheckParsAS(param),
         ## Five parameter version
         fm5 = ,
         FM5 = ,
         fmkl5 = ,
         FMKL5 = glCheckParsFMKL5(param, lambda5),
         stop("Error: version not recognised, check documentation")
         )
}




glCheckParsErrorMessage  <- function(param, version){
  paste("The parameter values", paste(param, collapse = " "),
        "\ndo not produce a proper distribution for the", version,
        "GLD version \n - see documentation for glCheckPars")
}

glCheckParsFMKL <- function(param)
{
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]
  if (lambda2 <= 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}


glCheckParsRS <- function(param)
{
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  if (lambda3 * lambda4 > 0) {
    if ((lambda3 > 0) & (lambda4 > 0)) {
      if (lambda2 <= 0) {
        ret <- FALSE
      } else {
        ret <- TRUE
      }
    }
    if ((lambda3 < 0) & (lambda4 < 0)) {
      if (lambda2 >= 0) {
        ret <- FALSE
      } else {
        ret <- TRUE
      }
    }
  } else {
    if (lambda2 >= 0) {
      return(FALSE)
    }
    if ((lambda3 > 0) & (lambda3 < 1) & (lambda4 < 0)) {
      return(FALSE)
    }
    if ((lambda4 > 0) & (lambda4 < 1) & (lambda3 < 0)) {
      return(FALSE)
    }
    lc <- lambda3
    ld <- lambda4
    if ((lambda3 > -1) & (lambda3 < 0) & (lambda4 > 1)) {
      if (((1 - lc)^(1 - lc)*(ld - 1)^(ld - 1))/
          ((ld - lc)^(ld - lc)) > -lc/ld) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    if ((lambda4 > -1) & (lambda4 < 0) & (lambda3 > 1)) {
      if (((1 - ld)^(1 - ld)*(lc - 1)^(lc - 1))/
          ((lc - ld)^(lc - ld)) > -ld/lc) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    if (lambda3 == 0) {
      if (lambda4 > 0) {
        if (lambda2 < 0) {
          return(FALSE)
        }
        ret <- TRUE
      }
      if (lambda4 == 0) {
        warning("RS GLD version: lambda3 and lambda4
                zero gives a point mass at lambda1")
      }
      if (lambda4 < 0) {
        if (lambda2 > 0) {
          return(FALSE)
        }
        ret <- TRUE
      }
    }
    if (lambda4 == 0) {
      if (lambda3 > 0) {
        if (lambda2 < 0) {
          return(FALSE)
        }
        ret <- TRUE
      }
      if (lambda3 == 0) {
        warning("RS GLD version: lambda3 and lambda4
                zero gives a point mass at lambda1")
      }
      if (lambda3 < 0) {
        if (lambda2 > 0) {
          return(FALSE)
        }
        ret <- TRUE
      }
    }
    if (is.null(ret)) {
        warning("RS param return not set:
                please email maintainer with example")
        ret <- TRUE}
  }
  ret
}


glCheckParsVSK <- function(param)
{
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  if (lambda2 <= 0) {
    ret <- FALSE
    warning("Negative or zero beta")
  } else {  # delta check
    if ((lambda3 < 0) | (lambda3 > 1)) {
      ret <- FALSE
    } else {
      ret <- TRUE
    }
  }
  ret
}

glCheckParsAS <- function(param)
{
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  stop("glCheckParsAS not yet implemented")
}

glCheckParsFMKL5 <- function(param, lambda5)
{
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  stop("glCheckParsFMKL5 not yet implemented")
}




