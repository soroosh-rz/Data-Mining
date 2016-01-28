#############################################
#####   Introduction to Decision Trees  #####
#####         and Random Forest         #####
#############################################

# We show how to build predictive models with
# packages party, rpart and randomForest. Other
# related packages are partykit and ipred.

# This section shows how to build a decision tree 
# for the iris data with function ctree() in package
# party [Hothorn et al., 2010]. Sepal.Length, 
# Sepal.Width, Petal.Length and Petal.Width are used
# to predict the Species of Iris flowers.

# We first use function ctree() to build  a decision 
# tree, and predict() makes prediction for new data.

data(iris)
str(iris)

# Before modeling, the iris data is split below into 
# two subsets: training (70%) and test (30%).

# for replication
set.seed(1234)
# samples a 1 or a 2 as many times as there are rows 
# in iris with 70% 1's and 30% 2's
ind <- sample(2, nrow(iris), 
              replace=TRUE, prob=c(0.7, 0.3))

ind

rownames(iris)

# we use the 70% for training
trainData <- iris[ind==1,]

# and the 30% for testing
testData <- iris[ind==2,]

# We then load package party, build a decision tree,
# and check the prediction result. Function ctree()
# provides some parameters, such as MinSplit, MinBucket,
# MaxSurrogate and MaxDepth (all part of the controls =
# ctree_control() argument), to control the training 
# of decision trees. Below we use default settings to 
# build a decision tree.

# In this code, myFormula specifies that Species is the
# target variable and all other variables are independent
# variables.

library(party)

?ctree

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(iris_ctree), trainData$Species)

# Now we have a look at the tree by printing the rules
print(iris_ctree)

# and plotting the tree
plot(iris_ctree)

# in the above plot, the barplot for each leaf node shows
# the probabilities of an instance falling into the three
# species. In the simple plot below, they are shown as "y"
# in leaf nodes. For example, node 2 is labeled with 
# "n=40, y=(1, 0, 0)", which means that it contains 40 
# training instances and all of them belong to the first
# class "setosa".

# here is a simple plot:
plot(iris_ctree, type="simple")

# so now we test the classification ability of the test
# tree with the test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

# However, ctree() does not handle missing values well, and
# another problem is that, when a variable exists in training 
# data and is fed into ctree() but does not appear in the
# built decision tree, the test data must also have that 
# variable to make prediction.

# Otherwise, a call to predict() would fail. Also, if the value 
# levels of a categorical variable in test data are different
# from that in training data, it would also fail to make prediction
# on the test data. 

##### Decision Trees with Package rpart

# So here we use package rpart to build a decision tree on the
# bodyfat data

# Recursive partitioning is an algorithm to 
# "grow" a regression tree. It first parti-
# tions the observations by univariate splits 
# in a recursive way, and secondly fits a 
# constant model in each cell of the resulting
# partition.

# Function rpart() is used to build a decision tree,
# and the tree with the minimum prediction error 
# is selected. After that, it is applied to new data
# to make prediction with function predict().

# Please see included session documentation:
# "Recursive-Partitioning-HSAUR2.pdf"

# The available algorithms differ on 3 points:
# 1) How to select the splitting covariate;
# 2) How the split point is estimated;
# 3) Which stopping criterion is applied.

# We look at an R demo with one of the more
# popular algorithms for classification and
# regression trees using R package 'rpart'.

# The algorithm first examines all possible
# splits for all covariates and chooses the
# split which leads to two groups that are
# 'purer' than the current group with respect
# to the values of the response variable y.

# 'Gini' stop criterion is default in rpart.

set.seed(290875)

# body fat data is in mboost
install.packages("mboost")

library("stabs")
library("parallel")
library("mboost")

library("rpart")

data("bodyfat", package="TH.data")
# data("bodyfat", package = "mboost")

# look at help for bodyfat data
help("bodyfat", package = "mboost")

class(bodyfat$DEXfat)

# look at dimensions (71 rows, 10 variables)
dim(bodyfat)

# look at attributes
attributes (bodyfat)

# Next, the data is split into training and test subsets, 
# and a decision tree is built on the training data:
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]

# train a decision tree
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train,
                       # use control arg to restrict # of obs
                       # for potential binary split to 10:
                       control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)

print(bodyfat_rpart$cptable)

print(bodyfat_rpart)

# The build tree can be plotted:
plot(bodyfat_rpart)
# adds the text
text(bodyfat_rpart, use.n=T)

# We prune the tree...
# Select the tree with the minimum prediction error:
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)

# Then plot it:
plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

# Then, the selected tree is used to make prediction 
# and the predicted values are compared with actual
# labels. In the code below, function abline() draws
# a diagonal line. The predictions of a good model
# are expected to be equal or very close to their 
# actual values, that is, most points should be on
# or close to the diagonal line.

DEXfat_pred <- predict(bodyfat_prune, 
                       newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed",
       ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)

###############################################
#######        Random Forest             ######
###############################################

### Now we model a random forest

# again split iris into new, different
# training (70%) and test (30%) subsets

ind <- sample(2, nrow(iris), 
              replace=TRUE, 
              prob=c(0.7, 0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# we load randomForest
library(randomForest)

# we train randomForest
# predict species using all variables

# two limitations of randomForest function:
# 1) cannot handle data with missing values so
#    users must impute values for those missing
# 2) 32 maximum number of levels for each
#    categorical variable; must transform them

# build same model as before; are predicting
# Species based on all other variables:

rf <- randomForest(Species ~ ., 
                   data=trainData, 
                   ntree=100,
                   proximity=TRUE)

# show conditional table cells' frequencies
table(predict(rf), trainData$Species)

# we take a look at the results
print(rf)

# show the components of the output 'rf' object
attributes(rf)

# plot error rates with various
# number of trees
plot(rf)

# find importance of variables
importance(rf)

# plots importance
varImpPlot(rf)

# test random forest using test data
irisPred <- predict(rf, newdata=testData)

# check the results
table(irisPred, testData$Species)

# check margins of data which is
# proportion of votes for the correct
# class minus maximum proportion of
# votes for other classes. Positive
# margin indicates correct classification

plot(margin(rf, testData$Species))
