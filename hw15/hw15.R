#Data Mining hw 15

# Compare the accuracies of bagging, boosting, and random forests on the wdbc 
# data set, using 70% of the data as training data and 30% as testing data.
library(adabag)
library(randomForest)

source('~/Dropbox/Tarleton/data_mining/class_notes/extras.R')
source('~/Dropbox/Tarleton/data_mining/generic_functions/dataset_ops.R')
wdbc <- read.csv('~/Dropbox/Tarleton/data_mining/dfiles/wdbc.data', 
                  header=F, sep=',')
wdbc <- wdbc[,-1]
splitset <- splitdata(wdbc,0.7,F)
train <- splitset$train

bag1 <- proc.time()
baggingmodel <- bagging(V2~., wdbc[train,])
bagpred <- predict(baggingmodel,wdbc[-train,],type='class')
bagacc <- confmatrix(bagpred$class,wdbc$V2[-train])$accuracy
bag2 <- proc.time()

forest1 <- proc.time()
forestmodel <- randomForest(V2~.,wdbc[train,])
forestpred <- predict(forestmodel,wdbc[-train,])
forestacc <- confmatrix(forestpred,wdbc$V2[-train])$accuracy
forest2 <- proc.time()

boost1 <- proc.time()
boostingmodel <- boosting(V2~., wdbc[train,])
boostpred <- predict(boostingmodel,wdbc[-train,],type='class')
boostacc <- 1 - boostpred$error
boost2 <- proc.time()
