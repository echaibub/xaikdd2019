
library(pROC)
library(randomForest)
library(dplyr)
library(plyr)


RestrictedResponseShuffling <- function(dat, respName, protName) {
  sdat <- dat
  protLevels <- unique(dat[, protName])
  nlevels <- length(protLevels)
  for (i in seq(nlevels)) {
    idx <- which(dat[, protName] == protLevels[i])
    shuffledLabels <- dat[idx, respName][sample(length(idx))]
    sdat[idx, respName] <- shuffledLabels
  }
  
  sdat
}


StandardResponseShuffling <- function(dat, respName) {
  n <- nrow(dat)
  sdat <- dat
  sdat[, respName] <- dat[sample(n), respName]
  
  sdat
}


GetAUC <- function(dat,
                   idxTrain, 
                   idxTest, 
                   labelName, 
                   featNames,
                   negClassName, 
                   posClassName) {
  dat <- dat[, c(labelName, featNames)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  myFormula <- as.formula(paste(labelName, " ~ ", paste(featNames, collapse = " + ")))
  fit <- randomForest(myFormula, data = dat[idxTrain,], ntree = 1000)
  predProbs <- predict(fit, dat[idxTest, -1, drop = FALSE], type = "prob")
  rocObj <- roc(dat[idxTest, 1], predProbs[, posClassName], direction = "<", 
                levels = c(negClassName, posClassName))    
  aucObs <- pROC::auc(rocObj)[1]
  approxVar <- GetNormApproxVarAUC(dat[idxTest, 1], negClassName, posClassName)
  imp <- fit$importance
  
  list(aucObs = aucObs, predProbs = predProbs, rocObj = rocObj, approxVar = approxVar, imp = imp)
}


GetNormApproxVarAUC <- function(ytest, negClassName, posClassName) {
  ytest <- factor(ytest)
  ylevels <- levels(ytest)
  n1 <- sum(ytest == negClassName)
  n2 <- sum(ytest == posClassName)
  n <- n1 + n2
  v <- (n + 1)/(12 * n1 * n2)
  
  c(v = v, n = n, nNeg = n1, nPos = n2)
}


RestrictedPermNullAUC <- function(dat, 
                                  idxTrain,
                                  idxTest,
                                  nperm,
                                  labelName, 
                                  protName,
                                  featNames,
                                  negClassName, 
                                  posClassName,
                                  verbose = FALSE,
                                  parallel = TRUE) {
  dat <- dat[, c(labelName, protName, featNames)]
  dat[, labelName] <- factor(as.character(dat[, labelName]), 
                             levels = c(negClassName, posClassName)) 
  
  ## get observed AUC
  obs <- GetAUC(dat, 
                idxTrain,
                idxTest,
                labelName,
                featNames,
                negClassName, 
                posClassName)
  oAUC <- obs$aucObs
  approxVar <- obs$approxVar
  
  trainData <- dat[idxTrain,]
  testData <- dat[idxTest,]
  
  ## generate label recognition permutation null distribution
  myFormula <- as.formula(paste(labelName, " ~ ", paste(featNames, collapse = " + ")))
  if (verbose) {
    res_auc <- plyr::llply(1:nperm, .parallel = parallel,  function(num){
      trainDataS <- RestrictedResponseShuffling(trainData, labelName, protName)
      testDataS <- RestrictedResponseShuffling(testData, labelName, protName)
      fitS <- randomForest(myFormula, data = trainDataS)
      predProbsS <- predict(fitS, testDataS[, -1, drop = FALSE], type = "prob")
      rocObjS <- roc(testDataS[, 1], predProbsS[, posClassName], direction = "<", 
                     levels = c(negClassName, posClassName)) 
      pROC::auc(rocObjS)[1]
    }, .progress = progress_text(char = "."))
  }
  else {
    res_auc <- plyr::llply(1:nperm, .parallel = parallel,  function(num){
      trainDataS <- RestrictedResponseShuffling(trainData, labelName, protName)
      testDataS <- RestrictedResponseShuffling(testData, labelName, protName)
      fitS <- randomForest(myFormula, data = trainDataS)
      predProbsS <- predict(fitS, testDataS[, -1, drop = FALSE], type = "prob")
      rocObjS <- roc(testDataS[, 1], predProbsS[, posClassName], direction = "<", 
                     levels = c(negClassName, posClassName)) 
      pROC::auc(rocObjS)[1]
    })
  }
  restrictedPermNull <- unlist(res_auc)
  meanRestrictedPermNull <- mean(restrictedPermNull)
  varRestrictedPermNull <- var(restrictedPermNull)
  avarStandNull <- as.numeric(approxVar["v"])
  cAUC <- (oAUC - meanRestrictedPermNull) * sqrt(avarStandNull/varRestrictedPermNull) + 0.5
  
  list(oAUC = oAUC,
       cAUC = cAUC,
       meanRestrictedPermNull = meanRestrictedPermNull,
       varRestrictedPermNull = varRestrictedPermNull,
       approxVar = approxVar,
       restrictedPermNull = restrictedPermNull)
}


TrainTestRandomSplit <- function(n) {
  nseq <- seq(n)
  ntrain <- ceiling(n/2)
  idxTrain <- sample(nseq, ntrain, replace = FALSE)
  idxTest <- setdiff(nseq, idxTrain)
  
  list(idxTrain = idxTrain, idxTest = idxTest)
}


StratifiedTrainTestRandomSplit <- function(dat, varNames) {
  combinedVar <- apply(dat[, varNames], 1, paste, collapse = "_")
  cvLevels <- unique(combinedVar)
  nLevels <- length(cvLevels)
  idxTrain <- vector(mode = "list", length = nLevels)
  idxTest <- vector(mode = "list", length = nLevels)
  for (i in seq(nLevels)) {
    idx <- which(combinedVar == cvLevels[i])
    idxTrain[[i]] <- sample(idx, ceiling(length(idx)/2), replace = FALSE)
    idxTest[[i]] <- setdiff(idx, idxTrain[[i]])
  }
  
  list(idxTrain = unlist(idxTrain), idxTest = unlist(idxTest))
}


PartialCorrelation <- function(response, feature, protected) {
  resResp <- lm(response ~ protected)$resid
  resFeat <- lm(feature ~ protected)$resid
  
  cor(resResp, resFeat)
}


PermNullsCorrelation <- function(nperm, dat, respName, featName, protName) {
  restrPerm <- rep(NA, nperm)
  standPerm <- rep(NA, nperm)
  ocor <- cor(dat[, featName], dat[, respName])
  for (i in seq(nperm)) {
    sdat1 <- RestrictedResponseShuffling(dat, respName, protName)
    sdat2 <- StandardResponseShuffling(dat, respName)
    restrPerm[i] <- cor(sdat1[, featName], sdat1[, respName])
    standPerm[i] <- cor(sdat2[, featName], sdat2[, respName])
  }
  restrMean <- mean(restrPerm)
  restrSd <- sd(restrPerm)
  standMean <- mean(standPerm)
  standSd <- sd(standPerm)
  ## adjusted correlation
  cCor <- (ocor - restrMean) * (standSd/restrSd) + standMean
  
  list(oCor = ocor,
       cCor = cCor, 
       restrPerm = restrPerm, 
       standPerm = standPerm)
}


SimulateDataSyntheticExample <- function(n,
                                         b.X.A1, 
                                         b.X.A2, 
                                         b.X.A4,
                                         b.X.A5,
                                         b.A2.A1,
                                         b.Y.A2, 
                                         b.A3.Y, 
                                         b.A4.A3) {
  A1 <- sample(c(-1, 1), n, replace = TRUE)
  A5 <- sample(c(-1, 1), n, replace = TRUE)
  
  linpredA2 <- as.vector(b.A2.A1 * A1)
  probsA2 <- 1/(1 + exp(-linpredA2))
  aux <- runif(n, 0, 1)
  A2 <- ifelse(aux < probsA2, 1, -1)
  
  linpredY <- as.vector(b.Y.A2 * A2)
  probsY <- 1/(1 + exp(-linpredY))
  aux <- runif(n, 0, 1)
  Y <- ifelse(aux < probsY, 1, -1)
  
  linpredA3 <- as.vector(b.A3.Y * Y)
  probsA3 <- 1/(1 + exp(-linpredA3))
  aux <- runif(n, 0, 1)
  A3 <- ifelse(aux < probsA3, 1, -1)
  
  linpredA4 <- as.vector(b.A4.A3 * A3)
  probsA4 <- 1/(1 + exp(-linpredA4))
  aux <- runif(n, 0, 1)
  A4 <- ifelse(aux < probsA4, 1, -1)
  
  X <- b.X.A1 * A1 + b.X.A2 * A2 + b.X.A4 * A4 + b.X.A5 * A5 + rnorm(n)
  
  data.frame(A1, A2, A3, A4, A5, X, Y)
}


SimulateColliderBiasData <- function(n, bAY, bAX, bYX) {
  X <- rnorm(n)
  linpredY <- as.vector(bYX * X)
  probsY <- 1/(1 + exp(-linpredY))
  aux <- runif(n, 0, 1)
  Y <- ifelse(aux < probsY, 1, -1)
  linpredA <- as.vector(bAY * Y + bAX * X)
  probsA <- 1/(1 + exp(-linpredA))
  aux <- runif(n, 0, 1)
  A <- ifelse(aux < probsA, 1, -1)
  
  data.frame(X, A, Y)
}


SimulateMBiasData <- function(n, b.A.U1, b.A.U2, b.X.U1, b.Y.U2) {
  U1 <- rnorm(n)
  U2 <- rnorm(n)
  X <- b.X.U1 * U1 + rnorm(n)
  
  linpredY <- as.vector(b.Y.U2 * U2)
  probsY <- 1/(1 + exp(-linpredY))
  aux <- runif(n, 0, 1)
  Y <- ifelse(aux < probsY, 1, -1)
  
  linpredA <- as.vector(b.A.U1 * U1 + b.A.U2 * U2)
  probsA <- 1/(1 + exp(-linpredA))
  A <- rbinom(n, 1, probsA)
  
  data.frame(X, A, Y)
}


SimulateConfoundedMediationBiasData <- function(n, bAX, bYA, bAU, bYU, bYX) {
  U <- rnorm(n)
  X <- rnorm(n)
  linpredA <- as.vector(bAX * X + bAU * U)
  probsA <- 1/(1 + exp(-linpredA))
  aux <- runif(n, 0, 1)
  A <- ifelse(aux < probsA, 1, -1)
  linpredY <- as.vector(bYA * A + bYU * U + bYX * X)
  probsY <- 1/(1 + exp(-linpredY))
  aux <- runif(n, 0, 1)
  Y <- ifelse(aux < probsY, 1, -1)
  
  data.frame(X, A, Y)
}




