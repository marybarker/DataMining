# This line replaces using the 'import dataset' option
# because I prefer writing a script over writing a 
# series of commands in console however saveable

# for this project, the dataset in FTIC.csv is an array 9128 x 6
# of stats about freshmen admitted to TSU.
# The 6 columns are TERM, QUARTER, PERCENTILE, SAT, X1st_Spring and X2nd_Fall
FTIC <- read.table("~/Dropbox/Tarleton/data_mining/dfiles/FTIC.csv", header=TRUE, sep=",")

# pick out the 'X2nd_Fall' column from the dataset. 
# this gives the retention rate for this set of students
retention=FTIC$X2nd_Fall
# The two factors which we are going to consider as 
# predictors of retention are precentile rank and sat scores
rank=FTIC$PERCENTILE
sat=FTIC$SAT

# create histograms for rank and sat and compute
# values related to distribution for later use.
hist(rank)
summary(rank)
mean(rank)
hist(sat)
summary(sat)
mean(sat)

#make retention boolean
retention=(retention=="Y")*1

#linear regression of sat and rank
model=lm(sat~rank)
summary(model)

plot(as.factor(retention), rank)

rankmodel=glm(retention~rank, family='binomial')
summary(rankmodel)

satmodel=glm(retention~sat, family='binomial')
summary(satmodel)

model=glm(retention~rank+sat, family='binomial')
summary(model)

index = ( (rank < 25)  & (sat >= 1030) )| 
        ( (rank >= 25) & (rank < 50) & (sat >= 950) ) | 
        ( (rank >= 50) & (rank < 90) & (sat >= 400) ) |
        (rank >= 90)
table(index)
retention[index]
mean(retention[index])


# fun example of a function with a list
mysumanddiff=function(x, y){
 L = list(sum = x + y, diff = x - y)
 return(L)
}

mylist=mysumanddiff(8, 3)

mylist$sum
mylist$diff

# this function evaluates retention based on threshold requirements
# for sat and rank that are passed in. Return value is a list that
# contains enrollment loss and retention rate based on sat and rank
evalthresholds=function(rank, sat, retention, rankthresholds, 
                        satthresholds){
#index[i] = 1 if student [i] fulfills admission requirements
  index=( (rank<rankthresholds[1]) & 
          (sat>=satthresholds[1]) ) | 
        ( (rank>=rankthresholds[1]) & 
          (rank<rankthresholds[2]) & 
          (sat>=satthresholds[2]) ) |
        ( (rank>=rankthresholds[2]) & 
          (rank<rankthresholds[3]) & 
          (sat>=satthresholds[3]) ) |
        ( (rank>=rankthresholds[3]) )

  x1 = sum( (index==FALSE)*1)
  x2 = mean(retention[index])
  L = list(enrollmentloss = x1, newretention = x2)
  return(L)
}
# test this function with solution in homework to see if it works.
rankthresholds=c(25,50, 90)
satthresholds=c(1030,950,400)
mylist=evalthresholds(rank,sat,retention,rankthresholds,satthresholds)
mylist$enrollmentloss
mylist$newretention

# now compute enrollmentloss and retention 
# with a different set of threshold criteria
rankthresholds=c(33,50, 90)
satthresholds=c(1610,950,400)

mylist=evalthresholds(rank,sat,retention,rankthresholds,satthresholds)
mylist$enrollmentloss
mylist$newretention

finalrankthreshold=rankthresholds
finalsatthreshold=satthresholds

# brute force method to compute better possible threshold requirements
# for admissions based on sat and rank
for(i in 10:78){
  for(j in 20:89){
    if(i < j){
      print(paste(i,j))
      rankthresholds=c(i,j,90)
      for(k in 95:160){
        print(k)
        kk = 10*k
        for(m in 50:95){
          mm = 10*m
          for(n in 40:50){
            nn = 10*n
            satthresholds=c(nn, mm, kk)
            newlist = evalthresholds(rank,sat,retention,rankthresholds,satthresholds)
            if( (newlist$enrollmentloss<=mylist$enrollmentloss) & 
                (newlist$newretention>mylist$newretention)){
                mylist=newlist
                finalrankthreshold=rankthresholds
                finalsatthreshold=satthresholds
                print(paste("changed values", i,j,k,m,n))
            }
          }
        }
      }
    }
  }
}
print("==============================================")
print("=========Final Enrollment Statistics==========")
print("==============================================")
print(paste("Enrollment loss:",mylist$enrollmentloss))
print(paste("Enrollment loss:",mylist$newretention))

print("The final rank thresholds are: ")
print(finalrankthreshold)
print(finalsatthreshold)

#expand.grid(1:5,10:13)
