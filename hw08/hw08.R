# Data Mining hw 8
library(e1071)
data(houseVotes84, package='pmml')

#1. For the following problems, consult the HouseVotes84 
#   data set. 

##a. What is the probability that a randomly selected 
##   representative is a Democrat? 
  print(sum(houseVotes84$Class == 'democrat')/nrow(houseVotes84))
  # probability is 61% it wil be a Democrat

  # alt way: 
  print(table(houseVotes84$Class) / sum(table(houseVotes84$Class)))

##b. Given that a representative is a Republican, what 
##   is the probability that he or she voted for water 
##   project cost sharing? 

  repwater <- naiveBayes(V2~Class, houseVotes84)

  ##Verify by hand computation of Bayes formula: 
  rep <- (houseVotes84$Class == 'republican') * 1
  dem <- (houseVotes84$Class == 'democrat') * 1

  idx <- 1 - (is.na(houseVotes84$V2) * 1) 
  v2 <- houseVotes84$V2[complete.cases(houseVotes84$V2)]

  prob_water = sum( (v2 == 'y') * 1) / length(v2)
  prob_nwater = sum((v2 == 'n') * 1) / length(v2)
  prob_rep = sum(rep) / nrow(houseVotes84)
  prob_dem = sum(dem) / nrow(houseVotes84)
  p_w_rep = sum( (v2 == 'y') & (rep[idx == 1] > 0) ) / (sum(rep))
  p_w_dem = sum( (v2 == 'y') & (dem[idx == 1] > 0) ) / (sum(dem))

  # probability is 0.3846154 by both computations.

##c. Given that a representative voted for adoption of the 
##   budget resolution, against the physician fee freeze and 
##   for duty free exports, find the probability that he or she 
##   is a Democrat (Hint: if x is a vector, you can use 
##   rbind(x) to force R to treat it as a row vector. You can 
##   set missing values of x to NA, and naive Bayes will 
##   ignore them.)
##
##      i.e. We're checking probability that
##        Class == 'democrat' given 
##        V3 = 'y'
##        V4 = 'n'
##        V15 = 'y'

  dem3415 <- naiveBayes(Class~V3 + V4 + V15, houseVotes84)
  dvec <- rbind(rep('NA', 17))
  dvec[4] <- 'y'
  dvec[5] <- 'n'
  dvec[16] <- 'y'
  predict(dem3415, newdata = dvec, type='raw')

#2. Investigate normality of the quantitative variables in the 
#   wdbc.data data set, using Shapiro-Wilk tests, histograms, 
#   and qq-plots. 
  wdbc <- read.table('~/Dropbox/Tarleton/data_mining/dfiles/wdbc.data',
                     header=F,sep=',')
  wdbc <- wdbc[,-1]
  names(wdbc)[1] <- c('diagnosis')

  for(i in 2:ncol(wdbc)){
  	name <- toString(names(wdbc)[i])
  	hist(wdbc[,i], main=paste('histogram of ',name))
  	print(paste(name, shapiro.test(wdbc[,i])$p))
  	qqnorm(wdbc[,i], main=paste('Q-Q test for ',name))
  }

#3. After splitting wdbc.data into 70% training and 30% test 
#   data, compare the test accuracies of naive Bayes, treating 
#   the quantitative variables as 

  splitset <- splitdata(wdbc, 0.7, FALSE)
  train_d <- splitset$traindata
  test_d  <- splitset$testdata
  train_i <- splitset$train

##a. normally distributed variables
  pred_diag_n <- naiveBayes(diagnosis ~., wdbc[train_i,])
  pred <- predict(pred_diag_n, wdbc[-train_i,])
  table(pred, wdbc$diagnosis[-train_i])
  err_normal = confmatrix(wdbc$diagnosis[-train_i], pred)$error

##b. categorical variables with 4 levels. 

  disc_wdbc <- wdbc
  for (j in 2:ncol(wdbc)){
    disc_wdbc[,j] <- cut(wdbc[,j], 4)
  }
  pred_diag_n <- naiveBayes(diagnosis ~., disc_wdbc[train_i,])
  pred <- predict(pred_diag_n, disc_wdbc[-train_i,])
  table(pred, disc_wdbc$diagnosis[-train_i])
  err_cat = confmatrix(disc_wdbc$diagnosis[-train_i], pred)$error

#4. Use 10-fold cross validation to estimate the accuracy of 
#   naive Bayes on wdbc.data, treating the quantitative 
#   variables as 

##a. normally distributed variables
err_k10_normal1 <- kfold_val(10, 2, wdbc, 1)$error

# 0.6616758

##b. categorical variables with L levels, L = 2, 3, ... , 10.
  nlevels = 10
  err_k10_cat <- 1:nlevels

  for(L in 2:nlevels){
    disc <- wdbc
    for(j in 2:ncol(wdbc)){
      disc[,j] <- cut(wdbc[,j], L)
    }
    err_k10_cat[L] <- kfold_val(10, 2, disc, 1)
  }
  v1 = 2:L
  plot(v1, err_k10_cat[2:L], 
      xlab='# levels', ylab='kfold error', type='p', pch=16,
      col=c('black','blue',
            'green','yellow',
            'orange','red',
            'purple','brown',
            'gray','pink')[v1])

# highest accuracy occurs when the number of levels is: 
which.min(err_k10_cat)

#5. Investigate the effect of the Laplace smoothing argument in 
#   the naiveBayes function. What appears to be the optimal value
#   for this argument? 
  splitset1 <- splitdata(houseVotes84, 0.7, FALSE)
  train_d1 <- splitset1$traindata
  test_d1  <- splitset1$testdata
  train_i1 <- splitset1$train

myl = seq(from=0,to=3,by=0.15)
len=length(myl)
k = 10

err = rep(-1, len)
k_err = rep(-1, len)
k_errs_for_plot <- matrix(, nrow=len, ncol=k)

for(i in 1:len){

   pred <- naiveBayes(Class~., houseVotes84[train_i1,], laplace=myl[i])
   err[i] <- confmatrix(
           predict(pred, houseVotes84[-train_i1,]), 
           houseVotes84$Class[-train_i1])$error

  l <- kfold_val(k, 3, houseVotes84, 1, myl[i])
  k_err[i] <- l$error
  k_errs_for_plot[i,] <- l$accuracy

}

for(i in 1:k){
plot(myl, k_errs_for_plot[,i], type='p',col='red', ylab='error', main=paste('fold # ', toString(i)))
  for(j in 1:k){
  	if(j != i){
      lines(myl, k_errs_for_plot[,j], type='p',col='black')
    }
  }
}

plot(myl, as.numeric(k_err),xlab='Value for laplace smoothing',ylab='error',
      main='naiveBayes with k-fold cv and varying laplace')

