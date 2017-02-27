
people <- read.table("~/Dropbox/Tarleton/data_mining/dfiles/Hw2.csv", header=TRUE, sep=",")

gender = people$Gender
cartype = people$CarType
shirtsize = people$ShirtSize
class = people$Class

N = length(class)
#typing 'N' into console now returns 1000

source("~/Dropbox/Tarleton/data_mining/hw02/measures.R")

#################################################
#             Test class entropy
#################################################
table(class)
#result of calling table(class) is the following: 
# C0   C1
#480  520
class1 = (class == 'C0')*1
class2 = (class == 'C1')*1
class_n1 = sum(class1)/N
class_n2 = sum(class2)/N
entropy = entropy_eval(c(class_n1, class_n2))
#typing 'entropy' into console results in 0.9988455 

#################################################
#         Test gender weighted entropy
#################################################
genderm = (gender == 'M')*1
genderf = (gender == 'F')*1

gender_n1 = sum(genderm)/N
gender_n2 = sum(genderf)/N

class11 = sum( ((genderm > 0) & (class1 > 0)) * 1)
class12 = sum( ((genderm > 0) & (class2 > 0)) * 1)
class21 = sum( ((genderf > 0) & (class1 > 0)) * 1)
class22 = sum( ((genderf > 0) & (class2 > 0)) * 1)

class11 = class11 / sum(genderm)
class12 = class12 / sum(genderm)
class21 = class21 / sum(genderf)
class22 = class22 / sum(genderf)

e1 = entropy_eval(c(class11, class12))
e2 = entropy_eval(c(class21, class22))
weighted_entropy = (gender_n1 * e1 + gender_n2 * e2) 

#################################################
#         Test cartype weighted entropy
#################################################
fam = (cartype == 'Family')*1
lux = (cartype == 'Luxury')*1
spt = (cartype == 'Sports')*1

car_n1 = sum(fam) / N
car_n2 = sum(lux) / N
car_n3 = sum(spt) / N

class11 = sum( (fam > 0) & (class1 > 0) )
class12 = sum( (fam > 0) & (class2 > 0) )
class21 = sum( (lux > 0) & (class1 > 0) )
class22 = sum( (lux > 0) & (class2 > 0) )
class31 = sum( (spt > 0) & (class1 > 0) )
class32 = sum( (spt > 0) & (class2 > 0) )

class11 = class11 / sum(fam)
class12 = class12 / sum(fam)
class21 = class21 / sum(lux)
class22 = class22 / sum(lux)
class31 = class31 / sum(spt)
class32 = class32 / sum(spt)

e1 = entropy_eval(c(class11, class12))
e2 = entropy_eval(c(class21, class22))
e3 = entropy_eval(c(class31, class32))

weighted_entropy = car_n1 * e1 + car_n2 * e2 + car_n3 * e3

#################################################
#         Test shirtsize weighted entropy
#################################################
table(shirtsize)
x = (shirtsize == 'ExtraLarge')*1 # 201
l = (shirtsize == 'Large')*1      # 200
m = (shirtsize == 'Medium')*1     # 359
s = (shirtsize == 'Small')*1      # 240
shirt_n1 = sum(x) / N
shirt_n2 = sum(l) / N
shirt_n3 = sum(m) / N
shirt_n4 = sum(s) / N

class11 = sum( (x > 0) & (class1 > 0) )
class12 = sum( (x > 0) & (class2 > 0) )
class21 = sum( (l > 0) & (class1 > 0) )
class22 = sum( (l > 0) & (class2 > 0) )
class31 = sum( (m > 0) & (class1 > 0) )
class32 = sum( (m > 0) & (class2 > 0) )
class41 = sum( (s > 0) & (class1 > 0) )
class42 = sum( (s > 0) & (class2 > 0) )

class11 = class11 / sum(x)
class12 = class12 / sum(x)
class21 = class21 / sum(l)
class22 = class22 / sum(l)
class31 = class31 / sum(m)
class32 = class32 / sum(m)
class41 = class41 / sum(s)
class42 = class42 / sum(s)

e1 = entropy_eval(c(class11, class12))
e2 = entropy_eval(c(class21, class22))
e3 = entropy_eval(c(class31, class32))
e4 = entropy_eval(c(class41, class42))
weighted_entropy = shirt_n1 * e1 + shirt_n2 * e2 + shirt_n3 * e3 + shirt_n4 * e4

