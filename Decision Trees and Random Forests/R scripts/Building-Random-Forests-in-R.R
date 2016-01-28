##########################################
#####  BUILDING AN RF MODEL USING R  #####
##########################################

# Create container to hold modeling info

library(rattle)
data(weather)
weatherDS <- new.env()
evalq({
  data <- na.omit(weather)
  nobs <- nrow(data)
  # variable RainTomorrow is target:
  form <- formula(RainTomorrow ~ .)
  target <- all.vars(form)[1]
  # exclude these variables:
  vars <- -grep('^(Date|Location|RISK_)', names(data))
  set.seed(42)
  # training set has 70% of observations:
  train <- sample(nobs, 0.7*nobs)
}, weatherDS)

# Build the random forest and store
# output in 'model'.

library(randomForest)
weatherRF <- new.env(parent=weatherDS)
evalq({
  model <- randomForest(formula=form, 
                        data=data[train, vars], 
                        ntree=500, mtry=4, 
                        importance=TRUE, 
                        localImp=TRUE,
                        na.action=na.roughfix,
                        replace=FALSE)
}, weatherRF)

### Exploring the Model

# let's look at the outputted model object:
str(weatherRF$model)

# and then take a look at randomForest function
?randomForest

# 'predicted' component contains values predicted
# for each observation in training dataset based
# on out-of-bag (OOB) samples. Here it is for
# first ten samples only:
head(weatherRF$model$predicted, 10)

# importance component records information about
# variable importance. Reports on 4 measures:

head(weatherRF$model$importance)

# Importance of each variable in predicting
# outcome for each observation also available
# in model object, localImp component:
head(weatherRF$model$localImp)[,1:4]

# error rate data stored as err.rate component:
weatherRF$model$err.rate

# corresponds to error plot in Rattle. Can see
# OOB estimate decreases quickly and then
# begins to flatten out
round(head(weatherRF$model$err.rate, 15), 4)

# Here we find the minimum error rate, together
# with a list of the indexes where each minimum
# occurs:
evalq({
  min.err <- min(data.frame(model$err.rate)["OOB"])
  min.err.idx <- which(data.frame(model$err.rate)["OOB"] 
                       == min.err)
}, weatherRF)

# We list the actual minimum value and 
# the indexes:
weatherRF$min.err
weatherRF$min.err.idx

# list the actual models where
# the minimum occurs
weatherRF$model$err.rate[weatherRF$min.err.idx,]

# the 'votes' component records the number of
# trees that vote 'No' and 'Yes" in ensemble
# for a particular observation
head(weatherRF$model$votes)

# numbers are reported as proportions and
# so add up to 1 for each observation:
head(apply(weatherRF$model$votes, 1, sum))


### Tuning Parameters
# User interface (Rattle) allows number of trees,
# number of variables, and sample size

# Here is a call to randomForest() with
# arguments explained
evalq({
  model <- randomForest(formula=form,
                        data=data[train, vars],
                        # number trees in ensemble
                        # 100 is recommended min:
                        ntree=500,
                        # number of variables
                        # considered for splitting
                        # at each node:
                        mtry=4,
                        replace=FALSE,
                        # can be used to force
                        # a smaller sample size:
                        sampsize=.632*nobs,
                        # allows us to review
                        # importance of each
                        # variable in determining
                        # outcome:
                        importance=TRUE,
                        # sets imputation for
                        # missing data:
                        localImp=FALSE,
                        # replace missing with
                        # median
                        na.action=na.roughfix)
}, weatherRF)

# Are problems with importance measures
# used by randomForest() function

# Alternative Random Forest function
# cforest() is in party package:
library(party)

# create a new data structure to store our
# forest object and attach weather dataset
# to the object
weatherCFOREST <- new.env(parent=weatherDS)

# cforest() improves on performance measures
# by subsampling without replacement

# we build the model itself
# with a call to cforest()
evalq({
  model <- cforest(form,
                   data=data[vars],
                   controls=cforest_unbiased(ntree=50, 
                                             mtry=4))
}, weatherCFOREST)


# instead of looking at all results, here we
# just list the top few most important variables:
evalq({
  varimp <- as.data.frame(sort(varimp(model), 
                               decreasing=TRUE))
  names(varimp) <- "Importance"
  head(round(varimp, 4), 3)
}, weatherCFOREST)
