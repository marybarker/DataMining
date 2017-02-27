#Data Mining hw 19
library(stats)
library(cluster)
library(fields)
library(fpc)
library(dbscan)

source('~/Dropbox/Tarleton/data_mining/generic_functions/dataset_ops.R')
source('~/Dropbox/Tarleton/data_mining/generic_functions/cluster_funs.R')
wdbc <- read.csv('~/Dropbox/Tarleton/data_mining/dfiles/wdbc.data',
                 header=F,sep=',')
wdbc <- wdbc[,-1]
nr <- nrow(wdbc) 
nc <- ncol(wdbc) 

# 1 Test whether the k-means clusters for the wdbc data set are 
#   statisticaly independent of Diagnosis using a chi-square test with 
#   a simulated p-value.

wdbc_kmeans <- kmeans_reps(wdbc[2:nc], 2, 1000)
predicted <- rep('',nr)
predicted[1 * (wdbc_kmeans$cluster == 2) == 1] <- 'B'
predicted[1 * (wdbc_kmeans$cluster == 1) == 1] <- 'M'
wdbc_tab <- table(wdbc$V2, predicted)

chisq.test(wdbc_tab, simulate.p.value = T)
chisq.test(wdbc_tab, simulate.p.value = F)

# 2 In this problem, you will apply the DBSCAN clustering method on the 
#   wdbc data. 

#  a. Create a plot of sorted k-dist values, where k = 5, and determine 
#     the optimal vlaue of Eps. 

k = 5
z <- standardize(wdbc, 2:nc)
dmat <- rdist(z[,2:nc])

#myEps = 4.35
#myEps = 3.75
myEps = 2.

sort_dmat <- apply(dmat, 2, sort)
kdist <- sort_dmat[k,]
plot(sort(kdist), type='l', 
     xlab = 'Points Sorted by k-dist', 
     ylab = 'k-dist')
lines(1:length(kdist), rep(myEps, length(kdist)), col='blue')

#  b. Perform DBSCAN using this value of Eps and k = 5

wdbc_dbscan <- dbscan(z[,-1], eps = myEps, minPts = k)
plot(z[,2], z[,8], col = (wdbc_dbscan$cluster + 1))

predicted <- rep('',nr)
predicted[1 * (wdbc_dbscan$cluster == 1) == 1] <- 'B'
predicted[1 * (wdbc_dbscan$cluster == 0) == 1] <- 'M'
wdbc_tab <- table(wdbc$V2, predicted)

#  c. How many clusters were identified? 


#  d. What percentage of the points in the data were classified as noise? 
sum( (wdbc_dbscan$cluster == 0) * 1) / length(wdbc_dbscan$cluster)

#  e. Test whether the dbscan clusters are statistically independent of 
#     Diagnosis using a chi-square test with a simulated p-value. 

chisq.test(wdbc_tab, simulate.p.value=T)
