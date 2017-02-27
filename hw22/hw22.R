#Data Mining hw 22
library(bestglm)
library(pROC)
library(MKmisc)

source('~/Dropbox/Tarleton/data_mining/generic_functions/dataset_ops.R')
source('~/Dropbox/Tarleton/data_mining/class_notes/outliers.R')
source('~/Dropbox/Tarleton/data_mining/class_notes/useful_logistic_ftns.R')
bio <- read.csv("~/Dropbox/Tarleton/data_mining/dfiles/BIOL120Data.csv", header=T,sep=',')

path = '~/Dropbox/Tarleton/data_mining/hw22/'

# 1. Import the data set BIO120.txt, which contains data for 3146 Biol 120 students, 
#    including the following variables: 
#    * Grade: 1 = A, B, or C, 0 = all other grades.
#    * Rank: Percentile rank represented as a decimal between 0 and 1, w 
#      values close to 1 corresponding to higher ranked students. 
#    * Math and Verbal: Math and Verbal SAT scores
#    * Prev: 1 = student has taken Bio120 before, and 0 = student has not
#    * Rdg: Status of student regarding the remedial course Reading 100. 
#      possible levels are Never Taken, Concurrently Enrolled, Passed, and Failed. 
#    * Father's and Mother's education levels
#    * Gender

# 2. Build the best possible logistic regression model for predicting grade based on 
#     the other variables.
#
#    (a). Divide the data set into two parts for the purpose of cross-validation. 

          splitset <- splitdata(bio, 0.7)
          train = splitset$train
          bio.train <- bio[train,]
          bio.test <- bio[-train,]

#    (b). Fit a univariate model regressing grade onto each of the other variables. 
#         For the quantitative variables, attempt to determine if higher order terms 
#         are needed using the groupplot function (see LogisticRegressionFunctions.txt 
#         for some helpeful functions). As with linear regression, you can use a 
#         likelihood ratio test to formally test whether these terms are needed 
#         (LRtest function)
#         For the categorical variables, a univariate model can help to determine if 
#         some of the levels can be grouped together to create a variable with fewer 
#         levels. This is essential for the father and mother variables which have 8 
#         levels. It is likely that a stepwise regression will eliminate one of the 
#         parent's education variables, since they are highly correlated and have a 
#         large number of parameters. 

          basicmodel <- glm(grade~., data = bio.train, family=binomial)
          summary(basicmodel)

          #rank
          rank.model1 = glm(grade~rank, data=bio.train, family=binomial)
          summary(rank.model1)
          LRtest(rank.model1, basicmodel)
          #1.110223e-16
          # Conclusion: keep rank

          # rank higher order terms? 
          quantlogitplot(bio.train$grade, bio.train$rank, 1, 'math', 'logit', 10, c(-3,3))
          dev.copy(png, paste0(path,'grade_rank_logit.png'))
          dev.off()
          quantlogitplot(bio.train$grade, bio.train$rank, 2, 'math', 'logit', 10, c(-3,3))
          dev.copy(png, paste0(path,'grade_rank_logit_deg2.png'))
          dev.off()

          rank.model2 = glm(grade~rank+I(rank^2), data=bio.train, family=binomial)
          summary(rank.model2)
          LRtest(rank.model2, basicmodel)
          # 6.023138e-09
          rank.model3 = glm(grade~rank+I(rank^2)+I(rank^3), data=bio.train, family=binomial)
          summary(rank.model3)
          LRtest(rank.model3, basicmodel)
          # 5.600436e-08
          rank.model4 = glm(grade~rank+I(rank^2)+I(rank^3)+I(rank^4), data=bio.train, family=binomial)
          summary(rank.model4)
          LRtest(rank.model4, basicmodel)
          # 4.13323e-08

          #math
          math.model1 = glm(grade~math, data=bio.train, family=binomial)
          summary(math.model1)
          LRtest(math.model1, basicmodel)
          # 0
          # Conclusion: keep math

          quantlogitplot(bio.train$grade, bio.train$math, 1, 'math', 'logit', 20, c(-3,3))
          dev.copy(png, paste0(path,'grade_math_logit.png'))
          dev.off()
          # doesn't look like HOT will help. 

          #verbal
          verbal.model1 = glm(grade~verbal, data=bio.train, family=binomial)
          summary(verbal.model1)
          LRtest(verbal.model1, basicmodel)
          # 0 
          # Conclusion: keep verbal

          quantlogitplot(bio.train$grade, bio.train$verbal, 1, 'verbal', 'logit', 20, c(-3,3))
          dev.copy(png, paste0(path,'grade_verbal_logit.png'))
          dev.off()
          # doesn't look like HOT will help. 


          #PREV
          table(bio$prev)
          prevmodel = glm(grade~rank+math+verbal+rdg+father+mother+gender, data=bio.train, family=binomial)
          LRtest(prevmodel, basicmodel)
          #3.690684e-05
          # Conclusion: keep prev

          #gender
          table(bio$gender)
          gendermodel = glm(grade~rank+math+verbal+prev+rdg+father+mother, data=bio.train, family=binomial)
          LRtest(gendermodel, basicmodel)
          #0.346872
          # Conclusion: drop gender

          #RDG
          table(bio$rdg)
          rdgmodel = glm(grade~rank+math+verbal+prev+father+mother+gender, data=bio.train, family=binomial)
          LRtest(rdgmodel, basicmodel)
          #0.5859034
          # Conclusion: drop rdg

          #FATHER
          table(bio$father)
          fathermodel = glm(grade~rank+math+verbal+prev+rdg+mother+gender, data=bio.train, family=binomial)
          LRtest(fathermodel, basicmodel)
          #0.8947546
          # Conclusion: drop father
          # re - leveling father
          father.recode = c('HighSchool/SomeCollege', 
                            'Bachelor/Grad', 
                            'Bachelor/Grad', 
                            'HighSchool/SomeCollege', 
                            'NoHighSchool/SomeHighSchool', 
                            'NA', 
                            'HighSchool/SomeCollege', 
                            'NoHighSchool/SomeHighSchool')

          new.father = father.recode[bio$father]
          new.father.model = glm(grade~rank+math+verbal+prev+rdg+mother+gender+new.father[train], 
                                 data=bio.train, family=binomial)
          summary(new.father.model)
          LRtest(new.father.model, basicmodel)
          #0.8278164 
          # Conclusion--still drop father

          #MOTHER
          table(bio$mother)
          mothermodel = glm(grade~rank+math+verbal+prev+rdg+father+gender, data=bio.train, family=binomial)
          LRtest(mothermodel, basicmodel)
          #0.02942975
          # Conclusion: keep mother

          # re - leveling mother
          mother.recode = c('HighSchool/SomeCollege', 
                            'Bachelor/Grad', 
                            'Bachelor/Grad', 
                            'HighSchool/SomeCollege', 
                            'NoHighSchool/SomeHighSchool', 
                            'NA', 
                            'HighSchool/SomeCollege', 
                            'NoHighSchool/SomeHighSchool')

          new.mother = mother.recode[bio$mother]
          new.mother.model = glm(grade~rank+math+verbal+prev+rdg+father+gender+new.mother[train], 
                                 data=bio.train, family=binomial)
          summary(new.mother.model)
          LRtest(new.mother.model, basicmodel)
          # 0.1521139
          # Conclusion: Bad recoding

          mother.recode = c('NA', 
                            'Bachelor/Grad', 
                            'Bachelor/Grad', 
                            'HighSchool/SomeCollege', 
                            'NoHighSchool', 
                            'NA', 
                            'HighSchool/SomeCollege', 
                            'SomeHighSchool')
          new.mother = mother.recode[bio$mother]
          new.mother.model = glm(grade~rank+math+verbal+prev+rdg+father+gender+new.mother[train], 
                                 data=bio.train, family=binomial)
          summary(new.mother.model)
          LRtest(new.mother.model, basicmodel)
          # 0.05273772
          # Conclusion: better recoding

#    (c). Use stepwise and best subsets methods to narrow down the list of predictor 
#         variables. Given the small number of predictor variables, you can also adopt 
#         a manual selection approach to select the variables or to modify the results 
#         of the stepwise/best subsets procedures.

          model = glm(grade~rank+math+verbal+prev+rdg+father+new.mother[train]+gender, 
                       data=bio.train, family=binomial)
          step.model=step(model)

          X = model.matrix(model)
          X = X[,2:ncol(X)]
          y = bio.train$grade
          Xy = data.frame(X,y)

#          best.model = bestglm(Xy, family=binomial)
#          summary(best.model$BestModel)
          # Conclusion: keep rank, math, verbal, prev, new.mother


#    (d). Fit a tentative final model. The quantitative variables should be checked again
#         for functional form and categorical variables should be checked for groupings. 
#         You can also consider adding interaction terms. 
          new.bio <- bio
          new.bio$mother <- new.mother

          tentative.final = glm(grade~rank+I(rank^2)+I(rank^3)+I(rank^4)+math+verbal+prev+mother, 
                                data=new.bio[train,], family=binomial)
          summary(tentative.final)

#    (e). Assess the performance of the model by determining its classification accuracy 
#         using a cutoff probability of 0.5 and finding the area under the ROC curve. 
#         Each of these metrics can be calculated from the training sample using 
#         leave-one-out or delete-d cross-validation, and they can be calculated using 
#         the validation sample. 

          results = predict(tentative.final, new.bio[-train,], type='response')
          predicted.grade = (results >= 0.5) * 1
          table(predicted.grade)
          myacc <- confmatrix(bio$grade[-train], predicted.grade)
          #0.7256356

          rc <- roc(bio$grade[-train], results)
          plot(rc, main=paste0('area under curve = ', rc$auc))

#    (f). Finally, assess the fit of the final model using the Hosmer-Lemeshow goodness-
#         of-fit test.

          Pihat = predict(tentative.final, type='response')
          HLgof.test(fit=Pihat, obs=bio.train$grade)
