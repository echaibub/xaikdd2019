
source("utility_functions_xai.R")

####################################################
## correlation illustrations
####################################################

myseed <- 1234567890

set.seed(myseed)
dat <- SimulateDataSyntheticExample(n = 1e+3,
                                    b.X.A1 = 1, 
                                    b.X.A2 = 1, 
                                    b.X.A4 = 1,
                                    b.X.A5 = 1,
                                    b.A2.A1 = 1,
                                    b.Y.A2 = 1, 
                                    b.A3.Y = 1, 
                                    b.A4.A3 = 1)

dat$A1 <- as.factor(dat$A1)
dat$A2 <- as.factor(dat$A2)
dat$A3 <- as.factor(dat$A3)
dat$A4 <- as.factor(dat$A4)
dat$A5 <- as.factor(dat$A5)

dat$combined.A1.A3 <- paste(dat$A1, dat$A3, sep = "")
dat$combined.A2.A3 <- paste(dat$A2, dat$A3, sep = "")
dat$combined.A2.A4 <- paste(dat$A2, dat$A4, sep = "")



pcor1 <- PartialCorrelation(dat$Y, dat$X, dat$A1)
pcor2 <- PartialCorrelation(dat$Y, dat$X, dat$A2)
pcor3 <- PartialCorrelation(dat$Y, dat$X, dat$A3)
pcor4 <- PartialCorrelation(dat$Y, dat$X, dat$A4)
pcor5 <- PartialCorrelation(dat$Y, dat$X, dat$A5)
pcor13 <- PartialCorrelation(dat$Y, dat$X, dat$combined.A1.A3)
pcor23 <- PartialCorrelation(dat$Y, dat$X, dat$combined.A2.A3)
pcor24 <- PartialCorrelation(dat$Y, dat$X, dat$combined.A2.A4)


nperm <- 1e+4

set.seed(myseed)
a1 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "A1")

set.seed(myseed)
a2 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "A2")

set.seed(myseed)
a3 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "A3")

set.seed(myseed)
a4 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "A4")

set.seed(myseed)
a5 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "A5")

set.seed(myseed)
a13 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "combined.A1.A3")

set.seed(myseed)
a23 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "combined.A2.A3")

set.seed(myseed)
a24 <- PermNullsCorrelation(nperm, dat, respName = "Y", featName = "X", protName = "combined.A2.A4")


cl <- 1.5
mylim <- c(0, 22)
y <- 18
nc1 <- 30
nc2 <- 30
mylwd <- 2
myxlim <- c(-0.2, 0.5)

par(mfrow = c(4, 2), mar = c(3, 3.5, 0.5, 0.5), mgp = c(2, 0.75, 0))
############################
out <- a1
pcor <- pcor1
main <- expression(A[1])
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(a)", bty = "n", cex = cl)
############################
out <- a2
pcor <- pcor2
main <- expression(A[2])
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(b)", bty = "n", cex = cl)
############################
out <- a3
pcor <- pcor3
main <- expression(A[3])
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(c)", bty = "n", cex = cl)
############################
out <- a4
pcor <- pcor4
main <- expression(A[4])
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(d)", bty = "n", cex = cl)
############################
out <- a5
pcor <- pcor5
main <- expression(A[5])
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(e)", bty = "n", cex = cl)
############################
out <- a13
pcor <- pcor13
main <- expression(paste(A[1], A[3]))
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(f)", bty = "n", cex = cl)
############################
out <- a23
pcor <- pcor23
main <- expression(paste(A[2], A[3]))
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(g)", bty = "n", cex = cl)
############################
out <- a24
pcor <- pcor24
main <- expression(paste(A[2], A[4]))
hist(out$restrPerm, probability = TRUE, xlim = myxlim, 
     nclass = nc1, col = rgb(0,0, 1, 0.5), xlab = "correlation", 
     main = NULL, ylim = mylim)
hist(out$standPerm, probability = TRUE, nclass = nc2, 
     col = rgb(1, 0, 0, 0.5), add = TRUE)
abline(v = out$oCor, col = "cyan", lwd = mylwd)
abline(v = out$cCor, col = "darkorange", lwd = mylwd)
points(x = pcor, y = y, pch = 25, cex = 2.5, col = "darkgreen", bg = "darkgreen")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(h)", bty = "n", cex = cl)



###########################################
## AUC illustrations
###########################################

set.seed(myseed)
dat <- SimulateDataSyntheticExample(n = 1e+3,
                                    b.X.A1 = 1.5, 
                                    b.X.A2 = 1.5, 
                                    b.X.A4 = 1.5,
                                    b.X.A5 = 1.5,
                                    b.A2.A1 = 1.5,
                                    b.Y.A2 = 1.5, 
                                    b.A3.Y = 1.5, 
                                    b.A4.A3 = 1.5)

dat$A1 <- as.factor(dat$A1)
dat$A2 <- as.factor(dat$A2)
dat$A3 <- as.factor(dat$A3)
dat$A4 <- as.factor(dat$A4)
dat$A5 <- as.factor(dat$A5)

dat$combined.A1.A3 <- paste(dat$A1, dat$A3, sep = "")
dat$combined.A2.A3 <- paste(dat$A2, dat$A3, sep = "")
dat$combined.A2.A4 <- paste(dat$A2, dat$A4, sep = "")

dat$Y <- as.factor(dat$Y)

nperm <- 1e+4

set.seed(myseed)
auxSplit <- TrainTestRandomSplit(n = nrow(dat))
idxTrain <- auxSplit$idxTrain
idxTest <- auxSplit$idxTest

set.seed(myseed)
obs <- GetAUC(dat,
              idxTrain, 
              idxTest, 
              labelName = "Y", 
              featNames = "X",
              negClassName = "-1", 
              posClassName = "1")
obs$aucObs
obs$approxVar["v"]

set.seed(myseed)
r1 <- RestrictedPermNullAUC(dat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A1",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)

set.seed(myseed)
r2 <- RestrictedPermNullAUC(dat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A2",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)

set.seed(myseed)
r3 <- RestrictedPermNullAUC(dat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A3",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)

set.seed(myseed)
r4 <- RestrictedPermNullAUC(dat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A4",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)

set.seed(myseed)
r5 <- RestrictedPermNullAUC(dat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A5",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)


set.seed(myseed)
r13 <- RestrictedPermNullAUC(dat, 
                             idxTrain,
                             idxTest,
                             nperm,
                             labelName = "Y", 
                             protName = "combined.A1.A3",
                             featNames = "X",
                             negClassName = "-1", 
                             posClassName = "1",
                             verbose = TRUE,
                             parallel = FALSE)


set.seed(myseed)
r23 <- RestrictedPermNullAUC(dat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "combined.A2.A3",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)


set.seed(myseed)
r24 <- RestrictedPermNullAUC(dat, 
                             idxTrain,
                             idxTest,
                             nperm,
                             labelName = "Y", 
                             protName = "combined.A2.A4",
                             featNames = "X",
                             negClassName = "-1", 
                             posClassName = "1",
                             verbose = TRUE,
                             parallel = FALSE)



myxlim <- c(0.35, 0.85)
cl <- 1.5
mylwd <- 2
mylim <- c(0, 23)
nc <- 30
xaxis <- seq(0.35, 0.65, length.out = 1000)
adensi <- dnorm(xaxis, 0.5, sqrt(obs$approxVar["v"]))

par(mfrow = c(4, 2), mar = c(3, 3.5, 0.5, 0.5), mgp = c(2, 0.75, 0))
######################
out <- r1
main <- expression(A[1])
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(a)", bty = "n", cex = cl)
######################
out <- r2
main <- expression(A[2])
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(b)", bty = "n", cex = cl)
######################
out <- r3
main <- expression(A[3])
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = 40, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(c)", bty = "n", cex = cl)
######################
out <- r4
main <- expression(A[4])
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(d)", bty = "n", cex = cl)
######################
out <- r5
main <- expression(A[5])
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(e)", bty = "n", cex = cl)
######################
out <- r13
main <- expression(paste(A[1], A[3]))
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(f)", bty = "n", cex = cl)
######################
out <- r23
main <- expression(paste(A[2], A[3]))
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(g)", bty = "n", cex = cl)
######################
out <- r24
main <- expression(paste(A[2], A[4]))
hist(out$restrictedPermNull, xlim = myxlim, xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), nclass = nc, 
     main = NULL, ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topleft", legend = main, bty = "n", cex = cl)
legend("topright", legend = "(h)", bty = "n", cex = cl)



##########################################
## collider bias
##########################################

nperm <- 1e+4

set.seed(myseed)
cdat <- SimulateColliderBiasData(n = 5e+3, bAY = 5, bAX = 5, bYX = 0)

cdat$A <- as.factor(cdat$A)
cdat$Y <- as.factor(cdat$Y)

set.seed(myseed)
auxSplit <- TrainTestRandomSplit(n = nrow(cdat))
idxTrain <- auxSplit$idxTrain
idxTest <- auxSplit$idxTest

obs <- GetAUC(cdat,
              idxTrain, 
              idxTest, 
              labelName = "Y", 
              featNames = "X",
              negClassName = "-1", 
              posClassName = "1")
obs$aucObs

set.seed(myseed)
rcb <- RestrictedPermNullAUC(cdat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)



##########################################
## M bias
##########################################

set.seed(myseed)
mdat <- SimulateMBiasData(n = 5e+3, b.A.U1 = 5, b.A.U2 = 5, b.X.U1 = 10, b.Y.U2 = 10)

mdat$A <- as.factor(mdat$A)
mdat$Y <- as.factor(mdat$Y)

set.seed(myseed)
auxSplit <- TrainTestRandomSplit(n = nrow(mdat))
idxTrain <- auxSplit$idxTrain
idxTest <- auxSplit$idxTest

obs <- GetAUC(mdat,
              idxTrain, 
              idxTest, 
              labelName = "Y", 
              featNames = "X",
              negClassName = "-1", 
              posClassName = "1")

set.seed(myseed)
rmb <- RestrictedPermNullAUC(mdat, 
                            idxTrain,
                            idxTest,
                            nperm,
                            labelName = "Y", 
                            protName = "A",
                            featNames = "X",
                            negClassName = "-1", 
                            posClassName = "1",
                            verbose = TRUE,
                            parallel = FALSE)


###########################################
## confounded mediation bias
###########################################

set.seed(myseed)
mdat <- SimulateConfoundedMediationBiasData(n = 5e+3, bAX = 3, bYA = 3, bAU = 5, bYU = 5, bYX = 0)

mdat$A <- as.factor(mdat$A)
mdat$Y <- as.factor(mdat$Y)

set.seed(myseed)
auxSplit <- TrainTestRandomSplit(n = nrow(mdat))
idxTrain <- auxSplit$idxTrain
idxTest <- auxSplit$idxTest

obs <- GetAUC(mdat,
              idxTrain, 
              idxTest, 
              labelName = "Y", 
              featNames = "X",
              negClassName = "-1", 
              posClassName = "1")

set.seed(myseed)
rcmb <- RestrictedPermNullAUC(mdat, 
                             idxTrain,
                             idxTest,
                             nperm,
                             labelName = "Y", 
                             protName = "A",
                             featNames = "X",
                             negClassName = "-1", 
                             posClassName = "1",
                             verbose = TRUE,
                             parallel = FALSE)


cl <- 1.5
mylwd <- 2
mylim <- c(0, 55)

par(mfrow = c(1, 3), mar = c(3, 3.5, 1.5, 0.5), mgp = c(2, 0.75, 0))
#################
out <- rcb
xaxis <- seq(0.35, 0.65, length.out = 1000)
adensi <- dnorm(xaxis, 0.5, sqrt(out$approxVar["v"]))
hist(out$restrictedPermNull, xlim = c(0.35, 0.65), xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), 
     main = "collider bias", ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topright", legend = "(a)", bty = "n", cex = cl)
#################
out <- rmb
xaxis <- seq(0.35, 0.65, length.out = 1000)
adensi <- dnorm(xaxis, 0.5, sqrt(out$approxVar["v"]))
hist(out$restrictedPermNull, xlim = c(0.35, 0.65), xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5),
     main = "M-bias", ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topright", legend = "(b)", bty = "n", cex = cl)
#################
out <- rcm
xaxis <- seq(0.35, 0.65, length.out = 1000)
adensi <- dnorm(xaxis, 0.5, sqrt(out$approxVar["v"]))
hist(out$restrictedPermNull, xlim = c(0.35, 0.65), xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5),
     main = "confounded mediation", ylim = mylim)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")
legend("topright", legend = "(c)", bty = "n", cex = cl)

