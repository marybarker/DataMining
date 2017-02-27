# this program has a function that accepts a vector of arbitrary length and computes
# the Gini impurity, entropy and the classification error measures. 

# checks that the set of values does add up to 1.
# otherwise we're missing one or more sets.  
check_add_to_1 = function(v1){
  value = sum(v1)
  v2 = v1
  if(value < 1){
  	v2 = rep(0, length(v1) + 1)
    v2 = c(v1, 1 - value)
    value = 0
  }
  if(value == 1){
  	v2 = v1
  }
  return(v2)
}

gini_eval = function(distribution_v){
  v = check_add_to_1(distribution_v)
  gini = 1 - sum(v * v)
  return(gini)
}

entropy_eval = function(distribution_v){
  # create a temporary array that has same entries as
  # distribution_v, except where distribution_v = 0. 
  # there tmp is 1 so I won't get nan computing 
  # log(tmp) (want the result to be 0 anyway)
  v = check_add_to_1(distribution_v)
  tmp = (v == 0) * 1
  tmp = tmp + v

  entropy = -1 * sum(tmp * (log(tmp) / log(2)))
  return(entropy)
}

class_error_eval = function(distribution_v){
  v = check_add_to_1(distribution_v)
  class_error = 1 - max(v)
  return(class_error)
}

x = seq(from=0,to=1,by=0.01)
y1 <- rep(0, 101)
y2 <- rep(0, 101)
y3 <- rep(0, 101)
for(i in 1:100){
  y1[i] = gini_eval(x[i])
}
for(i in 1:100){
  y2[i] = entropy_eval(x[i])
}
for(i in 1:100){
  y3[i] = class_error_eval(x[i])
}
plot(x, y1, type='l',col='black', ylim=c(0,1))
lines(x, y2, type='l',col='green')
lines(x, y3, type='l',col='blue')

###################################################
######   TEST FUNCTIONS WITH SAMPLE VECTOR   ######
###################################################

#class_distribution_vector = c(0.0, 0.3, 0.3, 0.4)
#mygini =  gini_eval(class_distribution_vector)
#print(' ')
#print("============================")
#print(paste("Gini measure: ",mygini))
#print("============================")
#print(' ')

#myentropy =  entropy_eval(class_distribution_vector)
#print(' ')
#print("============================")
#print(paste("entropy measure: ",myentropy))
#print("============================")
#print(' ')

#myentropy1 =  entropy_eval1(class_distribution_vector)
#print(' ')
#print("============================")
#print(paste("entropy1 measure: ",myentropy1))
#print("============================")
#print(' ')

#myclass_error =  class_error_eval(class_distribution_vector)
#print(' ')
#print("============================")
#print(paste("Gini measure: ",myclass_error))
#print("============================")
#print(' ')

