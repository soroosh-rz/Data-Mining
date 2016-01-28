######################################
###     Decision Trees, Bagging    ###  
###   and Random Forest Exercise   ###
######################################

# We use the prediction tool Random Forest, which 
# is very powerful and yet easy to use. To help 
# understanding, we set it in a context of other 
# tools:

#  1. Decision Tree: A building block.
#  2. Bagging: Improvement by tree ensembles.
#  3. Random Forest: Injecting more randomness 
#     into tree ensembles.

# WE use the Forensic Glass data in the MASS package. 
# The goal is predict type (of glass fragments) with 
# a set of predictors (of chemical properties). 

# Sample using 90% of data points as the training sample 
# and use the other 10% as test sample. Take a look at the
# data first:

library(MASS)
data(fgl)
str(fgl)
# set.seed for replication
set.seed(1234)
# establish the training and test data sets:
ind <- sample(2, nrow(fgl), 
            replace=TRUE, prob=c(0.9, 0.1))

# we use the 90% for training
trainData <- fgl[ind==1,]

# we use the 10% for testing
testData <- fgl[ind==2,]

# Decision Tree

# A tree-based prediction method (e.g. CART) partitions 
# the feature (variables) space into a set of rectangles, 
# on which fixed constants (predictions) are assigned.

# We can use the rpart function in the rpart package, 
# which implements CART. Load the rpart library and then
# create a tree modeling: type ~ (as a function of) all
# of the rest of the variables using the training data
# only
library(rpart)
p1 <- rpart(type ~ ., data = trainData)
# look at the rules
p1
str(p1)
class(p1)
# plot the tree
plot(p1)
# add text to the tree
text(p1)

# Bagging

# Bagging (Boostrap Aggregation) simply grows multiple trees, 
# each tree growing on a different bootstrap sample. It then
# reports the majority vote or mean response (across all trees)
# as the prediction. We can use the bagging() function in
# the ipred package.
install.packages("ipred")
library(ipred)

# look at the bagging function with help
?bagging

# Perform bagging where the formula is type ~ (as a function
# of) all of the other variables. Use the nbagg= argument
# to set the number of bootstrap samples to 100 and use
# the (compute out of bag) coob= argument to the function 
# to compute an out-of-bag estimate of the error rate. 
# The coob option requests the out-of-bag estimate of 
# the misclassification error.

p2 <- bagging(type ~ ., data = fgl, nbagg=100, coob = T)
p2

# Random Forest

# Random Forest injects additional randomness into the
# bagging procedure on trees: each node is split using
# the best among a subset of predictors randomly chosen
# at that node, instead of the full set. It has the
# following merits:

# . Superior performance.
# . Robust against overfitting.
# . Easy to use, little tuning.

# Thus it is a highly recommended prediction tool. 

library(randomForest)
p3 <- randomForest(type ~ ., data = trainData, importance = T)
p3

plot(p3)

# The plot method traces the error rates (out-of-bag, 
# and by each response category) as the number of 
# trees increases. The importance option in the
# randomForest function requests the assessment of 
# predictor importances. 

# There are two global measures: one is the mean decrease
# in accuracy over all classes, the other is the mean 
# decrease in Gini index. Here is a plot of the two measures:

par(mfrow = c(2, 1))
barplot(p3$importance[, 7], main = "Importance (Dec.Accuracy)")
barplot(p3$importance[, 8], main = "Importance (Gini Index)")
par(mfrow = c(1, 1))

# Comparison of Approaches

# The prediction accuracy on the test sample for Tree, 
# Bagging, and Random Forest can be computed as follows
# (note that testData$type is variable with the actual values):

# First, make a data frame with the correct types, the tree-predicted
# types, the bagging-predicted types and the forest-predicted types:
data.frame(Truth = testData$type, Tree = predict(p1, testData, type = "class"),
               Bagging = predict(p2, testData), Forest = predict(p3, testData))

# Incidentally, Bagging may perform better than Forest 
# on this test sample. Note Bagging is a special 
# case of Forest. For a more rigorous check, we shall 
# estimate the test error rate.

# Error Rate Estimation To compare the performances of 
# different prediction tools, we can do a 10-fold cross
# validation to estimate the test error, using the
# errorest() function in the ipred package. 

# This function requires a predict function that specifies
# only two arguments (object and newdata) and returns
# a predicted class (or scalar). So we first need to 
# write a wrapper function for predict.rpart:

mypredict.rpart <- function(object, newdata) {
  predict(object, newdata = newdata, type = "class")
  }

# We should see a significant improvement by Random Forest
# in the following error rate comparison:

c(Tree = errorest(type ~ ., data = trainData, model = rpart, predict = mypredict.rpart)$error,
    Bagging = errorest(type ~ ., data = trainData, model = bagging)$error,
    Forest = errorest(type ~ ., data = trainData, model = randomForest)$error)

# Here is error rate comparison for the Iris data:

c(Tree = errorest(Species ~ ., data = iris, model = rpart, 
                  predict = mypredict.rpart)$error,
    Bagging = errorest(Species ~ ., data = iris, 
                       model = bagging)$error,
    Forest = errorest(Species ~ ., data = iris, 
                      model = randomForest)$error)

# Conclusion

# We introduced a set of prediction tools (Tree, Bagging, Forest). 
# Tree is a nonlinear method and serves as the building block 
# for the other three tools.

# The drawback of Tree is that it is Unstable (sensitive to
# data noise) and has high variance in the prediction. 
# Bagging reduces such variance by bootstrapping the
# samples. In contrast, Boosting can be thought of as a 
# bias reduction tool.

# Random Forest seems to work the best for routine
# prediction tasks.