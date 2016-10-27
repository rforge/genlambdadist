
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


glCheckParsFMKL <- function(param)
{
  errMessage <- ""
  case <- TRUE
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]
  if (lambda2 <= 0) {
    case <- "error"
    errMessage <- "lambda2 must be greater than or equal to zero"
  }
  result <- list(case = case, errMessage = errMessage)
  return(result)
}


glCheckParsRS <- function(param)
{
  errMessage <- ""
  case <- TRUE
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  if (lambda3 * lambda4 > 0) {
    if ((lambda3 > 0) & (lambda4 > 0)) {
      if (lambda2 <= 0) {
        case <- "error"
        errMessage <- paste("lambda2 must be postive when lambda3",
                            "and lambda4 > 0")
      } else {
        case <- TRUE
      }
    }
    if ((lambda3 < 0) & (lambda4 < 0)) {
      if (lambda2 >= 0) {
        case <- "error"
        errMessage <- paste("lambda2 must be negative when lambda3",
                            "and lambda4 < 0")
      } else {
        case <- TRUE
      }
    }
  } else {
    if (lambda2 >= 0) {
      case <- "error"
      errMessage <- paste("lambda2 must be greater than or equal",
                          "to zero when lambda3*lambda4 >= 0")
    }
    if ((lambda3 > 0) & (lambda3 < 1) & (lambda4 < 0)) {
      case <- "error"
      errMessage <- paste("when lambda3 is between 0 and 1",
                          "lambda4 must not be negative")
    }
    if ((lambda4 > 0) & (lambda4 < 1) & (lambda3 < 0)) {
      case <- "error"
      errMessage <- paste("when lambda4 is between 0 and 1",
                          "and lambda3 must not be a negative")
    }
    lc <- lambda3
    ld <- lambda4
    if ((lambda3 > -1) & (lambda3 < 0) & (lambda4 > 1)) {
      if (((1 - lc)^(1 - lc)*(ld - 1)^(ld - 1))/
          ((ld - lc)^(ld - lc)) > -lc/ld) {
        case <- "error"
      } else {
        case <- TRUE
      }
    }
    if ((lambda4 > -1) & (lambda4 < 0) & (lambda3 > 1)) {
      if (((1 - ld)^(1 - ld)*(lc - 1)^(lc - 1))/
          ((lc - ld)^(lc - ld)) > -ld/lc) {
        case <- "error"
      } else {
        case <- TRUE
      }
    }
    if (isTRUE(all.equal(lambda3,0))) {
      if (lambda4 > 0) {
        if (lambda2 < 0) {
          case <- "error"
          errMessage <- paste("when lambda3 is equal to zero",
          "and lambda4 is postive,lambda2 must be negative")
        }
        case <- TRUE
      }
      if (isTRUE(all.equal(lambda4,0))) {
        case <- "error"
        errMessage <- paste("lambda3 and lambda4 zero gives a",
        "point mass at lambda1")

      }
      if (lambda4 < 0) {
        if (lambda2 > 0) {
          case <- "error"
          errMessage <- paste("when lambda3 is equal to zero and",
          "lambda4 is negative, lambda2 must be postive")
        }
        case <- TRUE
      }
    }
    if (isTRUE(all.equal(lambda4,0))) {
      if (lambda3 > 0) {
        if (lambda2 < 0) {
          case <- "error"
          errMessage <- paste("when lambda3 is postive and lambda4",
          "is zero, lambda2 must be negative")
        }
        case <- TRUE
      }
      if (isTRUE(all.equal(lambda3,0))) {
        case <- "error"
        errMessage <- paste("lambda3 and lambda4 zero gives a point",
        "mass at lambda1")
      }
      if (lambda3 < 0) {
        if (lambda2 > 0) {
          case <- "error"
          errMessage <- paste("when lambda3 is negative and lambda4",
          "is zero,lambda2 must be postive")
        }
        case <- TRUE
      }
    }
    if (is.null(case)) {
        warning("RS param return not set:
                please email maintainer with example")
        case <- TRUE}
  }
  result <- list(case = case, errMessage = errMessage)
  return(result)
}


glCheckParsVSK <- function(param)
{
  errMessage <- ""
  case <- TRUE
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  if (lambda2 <= 0) {
    case <- "error"
    errMessage <- "lambda2 must be greater or equal to zero"

  } else {  # delta check
    if ((lambda3 < 0) | (lambda3 > 1)) {
      case <- "error"
      errMessage <- "lambda3 must between 0 and 1"
    }
  }
  result <- list(case = case, errMessage = errMessage)
  return(result)
}

glCheckParsAS <- function(param)
{
  errMessage <- ""
  case <- TRUE
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  stop("glCheckParsAS not yet implemented")
}

glCheckParsFMKL5 <- function(param, lambda5)
{
  errMessage <- ""
  case <- TRUE
  lambda4 <- param[4]
  lambda3 <- param[3]
  lambda2 <- param[2]
  lambda1 <- param[1]

  stop("glCheckParsFMKL5 not yet implemented")
}





