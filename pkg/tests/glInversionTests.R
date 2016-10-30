require(GeneralizedLambda)
glInversionTestpq <- function(ps, param, version = "FMKL", lambda5 = NULL,
                              n = 10)
{
  if (is.null(ps)){
    ps <- rgl(n, param = param, version = version, lambda5 = lambda5)
  }
  qpps <- qgl(pgl(ps, param = param, version = version, lambda5 = lambda5),
              param = param, version = version, lambda5 = lambda5)
  diffs <- qpps - ps
  result <- data.frame(ps = ps, qpps = qpps, diffs = diffs)
}

glInversionTestqp <- function(qs = c(0.001, 0.01, 0.025, 0.05, 0.1, 0.2, 0.4,
                                     0.5, 0.6, 0.8, 0.9, 0.95, 0.975, 0.99,
                                     0.999),
                              param, version = "FMKL", lambda5 = NULL)
{
  pqqs <- pgl(qgl(qs, param = param, version = version, lambda5 = lambda5),
              param = param, version = version, lambda5 = lambda5)
  diffs <- pqqs - qs
  result <- data.frame(qs = qs, pqqs = pqqs, diffs = diffs)
}

paramDF <- data.frame(replicate(4,runif(100,-3,3)))
paramDF[, 2] <- abs(paramDF[, 2])
## paramDF[, 2] <- ifelse((paramDF[, 3] < 0) & (paramDF[, 4] < 0),
##                        -paramDF[, 2], paramDF[, 2])
versions <- c("FMKL")


nRows <- 10*NROW(paramDF)*length(versions)
pqResults <-
  data.frame(matrix(numeric(7*nRows), nrow = nRows,
                    dimnames = list(1:nRows,
                                    c("ps","qpps","diffs",
                                      paste0("lambda",1:4)))),
             version = character(nRows),
             stringsAsFactors = FALSE)

for (i in 1:length(versions)){
  for (j in 1:NROW(paramDF)){
    startRow <- (i-1)*NROW(paramDF)*10 + (j-1)*10 + 1
    endRow <- (i-1)*NROW(paramDF)*10 + j*10
    pqResults[startRow:endRow, 1:3] <-
      glInversionTestpq(NULL, paramDF[j, ], version = versions[i] )
    pqResults[startRow:endRow, 4:7] <- paramDF[j, ]
    pqResults[startRow:endRow, 8] <- as.character(versions[i])
  }
}

max(abs(pqResults["diffs"]))


nRows <- 15*NROW(paramDF)*length(versions)
qpResults <-
  data.frame(matrix(numeric(7*nRows), nrow = nRows,
                    dimnames = list(1:nRows,
                                    c("qs","pqqs","diffs",
                                      paste0("lambda",1:4)))),
             version = character(nRows),
             stringsAsFactors = FALSE)

for (i in 1:length(versions)){
  for (j in 1:NROW(paramDF)){
    startRow <- (i-1)*NROW(paramDF)*15 + (j-1)*15 + 1
    endRow <- (i-1)*NROW(paramDF)*15 + j*15
    qpResults[startRow:endRow, 1:3] <-
      glInversionTestqp(param =  paramDF[j, ], version = versions[i] )
    qpResults[startRow:endRow, 4:7] <- paramDF[j, ]
    qpResults[startRow:endRow, 8] <- as.character(versions[i])
  }
}

max(abs(qpResults["diffs"]))


