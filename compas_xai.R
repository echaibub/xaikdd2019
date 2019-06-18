
source("utility_functions_xai.R")

## url from downloaded data
raw_data <- read.csv("https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv")
dim(raw_data)


## filtering used by ProPublica
dat0 <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                     days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
nrow(dat0)
dim(dat0)

## select variables of interest
dat <- dplyr::select(dat0, race, priors_count, two_year_recid, decile_score)

## update number of factor levels
dat$race <- factor(dat$race)


priors_count_cat <- dat$priors_count
priors_count_cat[dat$priors_count > 7] <- ">7"
dat$priors_count_cat <- as.factor(priors_count_cat)

table(dat$priors_count_cat)
table(dat$two_year_recid, dat$priors_count_cat)

dat <- dat[dat$race  %in% c("African-American", "Caucasian"),]
dat$race <- factor(dat$race)
table(dat$two_year_recid, dat$race)

nperm <- 1e+4

##########################################
## generate train/test random split
##########################################

varNames <- c("two_year_recid", "priors_count_cat", "race")

set.seed(1234567)
datSplit <- StratifiedTrainTestRandomSplit(dat, varNames)


##########################################
## confounding example
##########################################

set.seed(1234567)
obs1 <- GetAUC(dat, 
               idxTrain = datSplit$idxTrain,
               idxTest = datSplit$idxTest,
               labelName = "two_year_recid", 
               featNames = "decile_score",
               negClassName = "0", 
               posClassName = "1")


rnull1 <- RestrictedPermNullAUC(dat, 
                                idxTrain = datSplit$idxTrain,
                                idxTest = datSplit$idxTest,
                                nperm = nperm,
                                labelName = "two_year_recid", 
                                protName = "race",
                                featNames = "decile_score",
                                negClassName = "0", 
                                posClassName = "1",
                                verbose = TRUE,
                                parallel = FALSE)



##########################################
## mediation example
##########################################

set.seed(1234567)
obs2 <- GetAUC(dat, 
               idxTrain = datSplit$idxTrain,
               idxTest = datSplit$idxTest,
               labelName = "two_year_recid", 
               featNames = "race",
               negClassName = "0", 
               posClassName = "1")


set.seed(1234567)
rnull2 <- RestrictedPermNullAUC(dat, 
                                  idxTrain = datSplit$idxTrain,
                                  idxTest = datSplit$idxTest,
                                  nperm = nperm,
                                  labelName = "two_year_recid", 
                                  protName = "priors_count_cat",
                                  featNames = "race",
                                  negClassName = "0", 
                                  posClassName = "1",
                                  verbose = TRUE,
                                  parallel = FALSE)


#################################################
#################################################
#################################################


cl <- 1.5
mylwd <- 2
mylim <- c(0, 45)
nc <- 30

par(mfrow = c(1, 1), mar = c(3, 3.0, 1.5, 0.5), mgp = c(2, 0.75, 0))
#################
out <- rnull1
xaxis <- seq(0.45, 0.55, length.out = 1000)
adensi <- dnorm(xaxis, 0.5, sqrt(out$approxVar["v"]))
hist(out$restrictedPermNull, xlim = c(0.45, 0.75), xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5), 
     main = "confounding example", ylim = mylim, nclass = nc)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")


nc <- 30
par(mfrow = c(1, 1), mar = c(3, 3.0, 1.5, 0.5), mgp = c(2, 0.75, 0))
out <- rnull2
xaxis <- seq(0.45, 0.55, length.out = 1000)
adensi <- dnorm(xaxis, 0.5, sqrt(out$approxVar["v"]))
hist(out$restrictedPermNull, xlim = c(0.45, 0.75), xlab = "AUROC", 
     probability = TRUE, col = rgb(0, 0, 1, 0.5),
     main = "mediation example", ylim = mylim, nclass = nc)
abline(v = out$oAUC, col = "cyan", lwd = mylwd)
abline(v = out$cAUC, col = "darkorange", lwd = mylwd)
lines(xaxis, adensi, col = "red")


