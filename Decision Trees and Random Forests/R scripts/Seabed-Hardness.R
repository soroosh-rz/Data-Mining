###############################################
#######   Predicting Seabed Hardness     ######
#######   Using Random Forest in R       ######
###############################################

# We want to predict seabed hardness. It is an
# important environmental property for predicting
# marine biodiversity that can be used to support
# marine zone management in Australia.

# But it is difficult to measure. Inferring seabed
# hardness over large areas has become possible with
# the widespread use and development of multibeam
# sonar systems for seabed mapping purposes.

# In all, 140 samples of seabed hardness were considered
# in this study. The hardness of the seabed substrate at
# at these sampling locations was determined based on the
# underwater video transects taken.

# PREDICTORS

# 15 variables were were selected and used in this study:

# 1) Bathymetry (bathY): water depth 
# 2) Seabed slope (slope)
# 3) Topographic relief (relief)
# 4) Surface area (surface)
# 5) Topographic position index (tpi)
# 6) Planar curvature (planar.curv) of the surface
# 7) Profile curvature (profile.curv) of the surface
#    in the direction of the slope
# 8) Local Moran I (bathy.moran): a measure of local
#    spatial autocorrelation in bathymetry
# 9) Backscatter (bs): a diffused reflection of acoustic
#    energy due to scattering process back to the
#    direction from which is was generated.
# 10) Homogeneity of backscatter (homegeneity)
# 11) Variance of backscatter (variance)
# 12) Local Moran I of backscatter (bs.moran)
# 13) Prock: The probability of substrate
# 14) Easting, and
# 15) Northing

# Features of the Data

# if you put the data in a temp directory under C:
# setwd("C:/temp/")

# if you want to select the file interactively:
hard <- read.table(file.choose(), header = TRUE, sep = ",")

# We made it a data frame
class(hard)

# Since some missing data were recorded as "9999",
# we replace them with "NA":
hard[hard==9999] <- NA

# Take a look at the structure of the dataset:
str(hard)

# In total, the data of 15 predictors are available
# at 140 sample locations for developing models to
# predict seabed hardness. The study areas and hardness
# are factors and all the predictors are numerical

# Exploratory Data Analyses

# Given that the seabed hardness was recorded in 
# terms of "hard" and "soft", the data needs to be
# transformed into a numerical variable in order to
# explore the relationships of hardness and the
# predictive variables. So the hardness variables
# were converted into a numeric variable of either
# 0 ("soft") or 1 ("hard") by this code:
hard$hardness2 <- 0
hard$hardness2[hard$hardness == "hard"] <- 1

# Now we perform exploratory analyses:
hcor <- round(cor(hard[,-c(1, 17)], use = "na.or.complete", method = "spearman"), 2)
install.packages("gridExtra")
library(gridExtra)
grid.draw(tableGrob(hcor[, -16], show.csep = TRUE, 
                    show.rsep = TRUE, show.box = TRUE, 
                    separator = "grey"))

# The results of correlation analysis show that
# seabed hardness is strongly correlated with prock,
# bathy, and bs; and it is weakly correlated with
# bathy.moran, relief, slope, surface, homogeneity,
# and bs.moran.

# There are strong correlations among some predictors,
# such as easting and northing, prock and bathy, prock
# and bs, homegeneity and variance.

# We look at the relationships:
pairs(hard[, -c(1, 17, 18)])
h2 <- hard[,-c(1, 17)]
par(mfrow = c(3,5))
for (i in 1:15){
    h3 <- subset(h2, abs(h2[,i]) >= 0)
    plot(h3[, i], h3[,16], ylab = "Hardness", xlab = names(h3)[i])
    lines(lowess(h3[,16]~h3[,i]), col = "blue")
}

# High prock values are typically associated with the
# "hard" substrate. Low prock values are associated
# with the "soft" substrate. A similar pattern was
# observed for backscatter, whereas bathymetry showed
# ab opposite pattern. These relationships are
# typically non-linear. These variables could potentially
# be good predictors of seabed hardness.

### APPLICATION OF RANDOM FOREST FOR
### PREDICTING SEABED HARDNESS

# RF is an ensemble method that combines many
# individual regressions or classification trees
# in this way: from the original sample many
# bootstrap samples and portions of predictors
# are drawn, and an unpruned regression or classification
# tree is fitted to each bootstrap sample using the
# sampled predictors. For the complete forest the
# status of the response variable is usually
# predicted as the average of the predictions of
# all trees for regression and as the classes with
# majority vote for classification.

# In the randomForest() function, one parameter that
# needs to be specified is mtry or "the number of
# variables randomly sampled as candidates at each
# split". It is often selected based on the result
# of tuneRF(), whereby the tuneRF() function searches
# for the optimal value of mtry:

hard2 <- na.omit(hard)
dim(hard2)

# This indicates the 3 of 140 samples contained NAs
# and were subsequently excluded. Consequently, only
# 137 samples are left to develop a predictive model:

library(randomForest)
tuneRF(hard2[,-c(1,17,18)], hard2[,17], ntreeTry = 100)
set.seed(123)
rf.1 <- randomForest(hard2[,-c(1,17,18)], hard2[,17], data = hard2,
importance = TRUE, ntree = 500, proximity = TRUE)
varImpPlot(rf.1)
names(rf.1)

dev.rf1 <- predict(rf.1, hard2)
grid.table(table(hard2[,17], dev.rf1))

### MODEL VALIDATION USING rfcv

result1 <- replicate(100, rfcv(hard2[,-c(1,17,18)], hard2[,17], scale =
"non.log", cv.fold = 10, step = -1), simplify = FALSE)
error.cv <- sapply(result1, "[[", "error.cv")
matplot(result1[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type = "l",
lwd = c(2, rep(1, ncol(error.cv))), col = c(2, rep(1, ncol(error.cv))), lty = 1, xlab = "Number of variables", ylab = "CV Error")

plot(result1[[1]]$n.var, (1-rowMeans(error.cv))*100, type = "l", lwd = 2, col = 2, lty = 1, xlab = "Number of variables", ylab = "Correct classification
rate (%)")

### OPTIMAL PREDICTIVE MODEL

ccr.cv.1 <- NULL; kappa.cv.1 <- NULL; mccr.cv.1 <- NULL; mkappa.cv.1 <- NULL
for (i in 1:100){
rfcv.1 <- rf.cv(hard2[,-c(1,17,18)], hard2[,17], cv.fold = 10, ntree = 500)
ccr.cv.1[i] <- rfcv.1$ccr.cv; kappa.cv.1[i] <- rfcv.1$kappa.cv
mccr.cv.1[i] <- mean(ccr.cv.1); mkappa.cv.1[i] <- mean(kappa.cv.1)
}
x <- c(1:100)
par(mfrow=c(2,1), font.axis = 2, font.lab = 2)
plot(ccr.cv.1 ~ x, xlab = "Iteration", ylab = "Correct classification rate")
points(mccr.cv.1 ~ x, col = 2)
plot(kappa.cv.1 ~ x, xlab = "Iteration", ylab = "Kappa")
points(kappa.cv.1 ~ x, col = 2)
par(mfrow = c(3,1), font.axis = 2, font.lab = 2)
partialPlot(rf.1, hard2, prock)
partialPlot(rf.1, hard2, bs)
partialPlot(rf.1, hard2, bathy)

### APPLICATION OF OPTIMAL PREDICTIVE MODEL

ra <- read.csv("area_a1.csv", sep = ",", header = FALSE)
dim(ra);
[1] 4653653      15
pred.a1 <- ra[, c(1,2)]
pred.a1$hardness <- predict(rf.1, ra)
write.table(pred.a1, "hardness.pred.a1_3vars.csv", sep = ",", row.names =
FALSE)

### APPENDIX BA: R FUNCTION, rf.cv, Shows the Cross-
# Validated Prediction Performance of a Predictive
# Model

rf.cv <- function (trainx, trainy, cv.fold = 10,
    mtry = function(p) max(1, floor(sqrt(p))), ntree=500, ...) {
    classRF <- is.factor(trainy)
    n <- nrow(trainx)
    p <- ncol(trainx)
    cv.pred <- NULL
    if (classRF) {
        f <- trainy
    }     else {
        stop ("This function is only for categorical response variable")
    }
    nlvl <- table(f)
    idx <- numeric(n)
    for (i in 1:length(nlvl)) {
        idx[which(f == levels(f)[i])] <- sample(rep(1:cv.fold,
            length = nlvl[i]))
    }
    for (i in 1:cv.fold) {
        all.rf <- randomForest(trainx[idx != i, , drop = FALSE],
            trainy[idx != i], trainx[idx == i, , drop = FALSE],
            trainy[idx == i], mtry = mtry(p), ntree=ntree)
        cv.pred[idx == i] <- all.rf$test$predicted
    }

    require(psy)
        data1<-as.data.frame(cbind(cv.pred, trainy))
        kappa.cv<-ckappa(data1)$kappa
        ccr.cv<-sum(diag(table(data1)))/sum(table(data1))*100

    list(kappa.cv = kappa.cv, ccr.cv = ccr.cv, predicted = cv.pred)
}

