#Data Mining hw 9
library(e1071)

# load and split the gernamcredit.csv into 70% and 30% training and test sets
gcred <- read.table("~/Dropbox/Tarleton/data_mining/dfiles/germancredit.csv", 
                     header = T, sep=',')

#since we're trying to predict when there IS a default, 
# set this to be positive value
gcred$Default <- as.factor(gcred$Default)

splitset <-splitdata(gcred, 0.7, FALSE)
train_i <- splitset$train

# Fit a naive Bayes classifier for predicting default, and calculate accuracy, 
# sensitivity, specificity, precision, and F_1 measure on test data

modelD <- naiveBayes(Default~., gcred[train_i,])
predD <- predict(modelD, gcred[-train_i,])
pphat <- predict(modelD, gcred[-train_i,],type='raw')[,2]
Dtable <- matrix(rep(0, 4), ncol=2, nrow=2)
rownames(Dtable) <- c('1','0')
colnames(Dtable) <- c('1','0')
Dtable <- as.table(Dtable)
testpreddef <- (pphat >= 0.5) * 1

Dtable[1, 1] <- sum((testpreddef == 1) & (gcred$Default[-train_i] == '1')) #TP
Dtable[2, 1] <- sum((testpreddef == 1) & (gcred$Default[-train_i] != '1')) #FP
Dtable[1, 2] <- sum((testpreddef != 1) & (gcred$Default[-train_i] == '1')) #FN
Dtable[2, 2] <- sum((testpreddef != 1) & (gcred$Default[-train_i] != '1')) #TN

w1 <- c(accuracy(Dtable), sensitivity(Dtable), 
        specificity(Dtable), precision(Dtable), F1(Dtable))

# find the probability threshold p0 that optimizes the F1 measure 
# on the training data

predD1 <- predict(modelD, gcred[train_i,])
phat <- predict(modelD, gcred[train_i,], type='raw')[,2]

idx = seq(from=0.1, to = 0.9, by = 0.01)
F1acc = rep(-1, length(idx))
c = 1

mytable <- matrix(rep(0, 4),ncol=2,nrow=2)
colnames(mytable) <- c('1','0')
rownames(mytable) <- c('1','0')
mytable <- as.table(mytable)

for(p0 in idx){

  trainpreddef <- (phat >= p0) * 1
  mytable[1,1] <- sum( (trainpreddef == 1) & (gcred$Default[train_i] == '1') )
  mytable[2,1] <- sum( (trainpreddef == 1) & (gcred$Default[train_i] != '1') )
  mytable[1,2] <- sum( (trainpreddef != 1) & (gcred$Default[train_i] == '1') )

  F1acc[c] <- F1( mytable )
  c = c + 1
}
p0 <- idx[which.max(F1acc)]


# recalculate the accuracy, sensitivity, specificity, precision, 
# and F1 measure on the test data using the new probability threshold.

nphat <- predict(modelD, gcred[-train_i,], type='raw')[,2]

testpreddef <- (nphat >= p0) * 1
mytable[1,1] <- sum( (testpreddef == 1) & (gcred$Default[-train_i] == '1') )
mytable[2,1] <- sum( (testpreddef == 1) & (gcred$Default[-train_i] != '1') )
mytable[1,2] <- sum( (testpreddef != 1) & (gcred$Default[-train_i] == '1') )

w2 <- c(accuracy(mytable), sensitivity(mytable), 
        specificity(mytable), precision(mytable), F1(mytable))
