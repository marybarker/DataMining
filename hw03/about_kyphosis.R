# Data Mining hw 3

# problem 1
##kyphosis {rpart}
#it is a data frame with 81 rows, 4 columns representing data on children 
#who have had corrective spinal surgery.
#
#columns are as follows:
#+ Kyphosis
#+ + + A factor with levels 'absent' 'present' indicating if a kyphosis was 
#+ + + present after the operation
#
#
#+ Age
#+ + + in months
#
#+ Number 
#+ + + the number of vertebrae involved
#
#+ Start
#+ + + The number of the first (topmost) vertebra operated on.
#
library(rpart)
library(rpart.plot)
library(party)
library(treemap)
attach(kyphosis)

# problem number 2
#a)
ktree=rpart(Kyphosis~Age+Number+Start,data=kyphosis)
plot(ktree,main="Kyphosis~Age+Number+Start")
prp(ktree,type=2,extra=104)
#b)
predkyphos = predict(ktree,newdata=kyphosis,type="class")
confusionmatrix=table(Kyphosis,predkyphos)
#c)
accuracy = sum(diag(confusionmatrix))/sum(confusionmatrix)
error = 1 - accuracy

# problem number 4
#a)
ktree2=ctree(Kyphosis~.,data=kyphosis)
plot(ktree2,type='simple')
#b)
predkyphos2 = predict(ktree2,newdata=kyphosis)
confusionmatrix=table(Kyphosis,predkyphos2)
#c)
accuracy = sum(diag(confusionmatrix))/sum(confusionmatrix)
error = 1 - accuracy

