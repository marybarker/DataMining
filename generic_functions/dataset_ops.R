#dataset functions: 

#splitdata - splits a dataframe into test and train sets
#  inputs: 
#    1. dataset to be split
#    2. (decimal) percent to be used for training set
#    3. optional boolean to decide whether to take a sample with replacement
#       (default no replacement)
#  output: 
#    list of length 3, 
#    list[[1]] = traindata
#    list[[2]] = testdata
#    list[[3]] = train (vector of indices from original set for train)

#kfold_val - performs cross-validation on a dataset
#  inputs:
#    1. integer (k) for how many folds to use.
#    2. type for what type of tree to use 
#       0, 1, 2, 3, 4, 5 for ctree, rpart, naiveBayes (2), neural network (2)
#    3. data set 
#    4. column index for which variable is being predicted
#    5. optional argument if naiveBayes is used for laplace value or size for nnet
#  output: 
#    avg. error for model used

#error_leave_one_out - performs leave-one-out cross-validation.
#  inputs: 
#    1. type (0 or 1) for what type of tree to use 
#       (ctree or rpart resp)
#    2. data set 
#    3. column index for which variable is being predicted
#    4. column index for which variable is being predicted
#    5. optional argument if naiveBayes is used for laplace value or size for nnet
#  outputs: 
#   see kfold_val

#delete_d_cv - performs delte-d cross validation
#  inputs: 
#    1. integer: number of times to iterate (m)
#    2. integer: number of entries to leave out (d)
#    3. type (0 or 1) for ctree or rpart respectively
#    4. data set
#    5. column index for which variable is being predicted
#  outputs: 
#   see kfold_val

#bootstrap - performs bootstrap validation
#  inputs:
#    1. integer: number of times to iterate (b)
#    2. type(0 or 1) for (ctree, rpart) resp.
#    3. data set
#    4. column index for which variable is being predicted
#  outputs: 
#   see kfold_val

#confmatrix - compute the confusion matrix for a model w.r.t. 
#             predicted variable V
#  inputs: 
#    1. V the predicted variable
#    2. actual variable 
#  outputs: 
#    1. list with entries: 
#       1.1: table of predicted vs. actual V
#       1.2: accuracy
#       1.3: error

#standardize - scale data in set to have normal 
#  inputs: 
#    1. dataset to be scaled
#    2. vector of the columns in dataset to be 
#       scaled (will ignore all not included)
#  outputs: 
#    1. new dataset with scaled values

splitdata = function(data, trainfrac, rep = FALSE){
	if((trainfrac > 1) | (trainfrac < 0)){
		print("error in function splitdata: trainfrac not in [0 1]")
	}

	tot_size = nrow(data)
	train_list = sample(tot_size, round(trainfrac * tot_size, digits=0), 
	                    replace = rep)
	traindata <- data[train_list, ]
	testdata <- data[-train_list, ]
	mylist <- list(traindata = traindata, testdata = testdata, train = train_list)
    return(mylist)
}

kfold_val = function(k, treetype, wdbc, idx = 1, val = 0){
  test_acc <- rep(-1, k)

  n1 <- names(wdbc)[idx]
  names(wdbc)[idx] <- 'mynewtestidx'
  size = ceiling(nrow(wdbc)/k)
  folds = sample(rep(1:k,size))
  myvec = folds[1:nrow(wdbc)]

  for(i in 1:k){
    testdata <- wdbc[myvec==i,]
    traindata <- wdbc[myvec!=i,]
    if(treetype < 1){       # ctree
      mynewtree <- ctree(mynewtestidx~., traindata)
      p_tree_train <- predict(mynewtree, testdata)
    }else if(treetype < 2){ # rpart
      mynewtree <- rpart(mynewtestidx~., traindata)
      p_tree_train <- predict(mynewtree, testdata, type = 'class')
    }else if(treetype < 3){ # naiveBayes, no laplace
      mynewtree <- naiveBayes(mynewtestidx~., traindata)
      p_tree_train <- predict(mynewtree, testdata)
    }else if(treetype < 4){ # naiveBayes, laplace
      mynewtree <- naiveBayes(mynewtestidx~., traindata, laplace = val)    	
      p_tree_train <- predict(mynewtree, testdata)
    }else if(treetype < 5){ # neural network (1 hidden layer), quantitative pred
      mynewtree <- nnet(mynewtestidx~., traindata, size = val, linout=FALSE,trace=FALSE)
      p_tree_train <- predict(mynewtree, testdata, type='class')
    }else if(treetype < 6){ # neural network (1 hidden layer), categorical pred
      mynewtree <- nnet(mynewtestidx~., traindata, size = val, linout=TRUE,trace=FALSE)
      p_tree_train <- predict(mynewtree, testdata)
    }
    m_tree = table(testdata$mynewtestidx, p_tree_train)

    test_acc[i] = (sum(diag(m_tree)) / sum(m_tree))
  }
  total_acc = sum(test_acc) / k
  names(wdbc)[idx] <- n1

  mynewlist <- list(error = 1 - total_acc, accuracy = 1 - test_acc)
  return(mynewlist)
}

error_leave_one_out = function(treetype, wdbc, idx, val = 0){
	myerror = kfold_val(nrow(wdbc), treetype, wdbc, idx, val)
	return(myerror)
}

delete_d_cv = function(m, d, mytree, wdbc, idx){
  total_acc = 0
  n1 <- names(wdbc)[idx]
  names(wdbc)[idx] <- 'mynewtestidx'

  for(i in 1:m){
	splitlist <- splitdata(wdbc, 1 - d / nrow(wdbc), FALSE)
    traindata <- splitlist$traindata
    testdata <- splitlist$testdata
    if(mytree < 1){
      mynewtree = ctree(mynewtestidx~., traindata)
    }else{
      mynewtree = rpart(mynewtestidx~., traindata, type='class')
    }

    p_tree_train <- predict(mynewtree, testdata)

    m_tree = table(testdata$mynewtestidx, p_tree_train)
    test_acc = (sum(diag(m_tree)) / sum(m_tree))
    total_acc = total_acc + test_acc
  }
  total_acc = total_acc / m
  names(wdbc)[idx] <- n1
  return(1 - total_acc)
}

bootstrap = function(b, mytree, wdbc, idx){
	n1 <- names(wdbc)[idx]
	names(wdbc)[idx] <- 'mynewidx'
    total_acc = 0
    n = nrow(wdbc)
    for(i in 1:b){
      	 splitlist <- splitdata(wdbc, 1, TRUE)
      	 traindata <- splitlist$traindata
      	 testdata <- splitlist$testdata
      	 if(mytree < 1){
      	 	mynewtree = ctree(mynewidx~., traindata)
      	 }else{
      	    mynewtree = rpart(mynewidx~., traindata)	
      	 }
      	 p_tree_train <- predict(mynewtree, testdata)
   	     m_tree = table(testdata$mynewidx, p_tree_train)
   	     test_acc = sum(diag(m_tree)) / sum(m_tree)
   	     total_acc = total_acc + test_acc
    }

    names(wdbc)[idx] <- n1
    total_acc = total_acc / b
    return(1 - total_acc)
}

euclidean = function(x1, x2){
  return(sqrt((x1 - x2) %*% (x1 - x2)))
}

confmatrix = function(predy, y){
  matrix = table(predy, y)
  accuracy = sum(diag(matrix))/sum(matrix)
  return(list(matrix = matrix, accuracy = accuracy, error = 1 - accuracy))
}


standardize = function(myset, cols){
 mynames <- names(myset)[-cols]
 idx <- rep(0, ncol(myset))
 idx[-cols] = 1

 xbar <- apply(myset[,cols], 2, mean) # mean for each column
 s <- apply(myset[,cols], 2, sd)      # standard dev. for each col.

 xbarMatrix <- cbind(rep(1,nrow(myset))) %*% xbar # matrix with each row xbar
 sMatrix <- cbind(rep(1,nrow(myset))) %*% s       # matrix with each row s

 sdized <- (myset[,cols] - xbarMatrix)
 sdized <- sdized / sMatrix

 j = 1
 if(idx[1] > 0){
   new_sdized <- data.frame(myset[,1])
 }else{
   new_sdized <- data.frame(sdized[,1])
   j = 2
 }
 for(i in 2:ncol(myset)){
   if(idx[i] > 0){
     new_sdized <- cbind(new_sdized, myset[,i])
   }else{
   	 new_sdized <- cbind(new_sdized, sdized[,j])
   	 j = j + 1
   }
 }
 names(new_sdized) <- names(myset)
 return(new_sdized)
}

w_acc = function(mytable, w){
  tp = as.numeric(mytable[1, 1])
  tn = as.numeric(mytable[2, 2])
  fp = as.numeric(mytable[2, 1])
  fn = as.numeric(mytable[1, 2])

  return( (w[1] * tp + w[4] * tn) / 
          (w[1] * tp + w[2] * fp + w[3] * fn + w[4] * tn) )
}
accuracy = function(mytable){
  w = rep(1, 4)
  return(w_acc(mytable, w))
}
sensitivity = function(mytable){
  w = c(1, 0, 1, 0)
  return(w_acc(mytable, w))
}
specificity = function(mytable){
  w = c(0, 1, 0, 1)
  return(w_acc(mytable, w))
}
precision = function(mytable){
  w = c(1, 1, 0, 0)
  return(w_acc(mytable, w))
}
F1 = function(mytable){
  w = c(2, 1, 1, 0)
  return(w_acc(mytable, w))
}
