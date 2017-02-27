#data mining hw 23
path = '~/Dropbox/Tarleton/data_mining/hw23/'
library(MASS)
library(lawstat)

# 1 Import the file math5305Lab6Data.txt, whose columns are the variables 
#   Y, X1, X2, and X3. The goal of these first three problems is to 
#   perform diagnostics to assess the assumptions of normality and constancy 
#   of variabce for a model predicting Y from the Xj's, to transform Y if 
#   necessary, to assess the transformed model using diagnostics, adn to 
#   compare the original and transformed models via residual sums of squares. 

this.data <- read.csv('~/Dropbox/Tarleton/data_mining/dfiles/5305Lab6', 
						header=F, sep=',', col.names=c('Y', 'X1', 'X2', 'X3'))

#   We begin by fitting a model and assessing it with diagnostics. 

#   (a) Fit model = lm(Y~X1+X2+X3), compute the fitted values Yhat, and the 
#       residuals e. You can use the command Yhat = predict(model) to obtain 
#       Yhat. 

		model = lm(Y~., data=this.data)
		Yhat = predict(model)
		e = model$residuals

#   (b) Plot Y vs Yhat. If the model were valid, what would you expect this 
#       plot to look like? Does the plot suggest the existence of curvature 
#       in the model? 

		plot(Yhat, this.data$Y)
		dev.copy(png, paste0(path, 'Yhat_vs_Y.png'))
		dev.off()

#   (c) Plot e vs Yhat. If the model were valid, what would you expect this 
#       plot to look like? Does the plot suggest the existence of curvature 
#       in the model? 

		plot(Yhat, e)
		dev.copy(png, paste0(path, 'Yhat_vs_e.png'))
		dev.off()

#   (d) Now that we know curvature is present, there are two courses of 
#       action we can take: transform Y or transform the Xj's, or both. 
#       Generally, if there are problems with the errors, we should transform 
#       Y, and if the errors are ok, we should transform the Xj's. Let's 
#       investigate the errors. 

#   (e) Plot a qq-plot to check normality of the error terms using the qqnorm 
#       command. 

		qqnorm(e)
		dev.copy(png, paste0(path, 'residuals_qq_plot.png'))
		dev.off()
	
#   (f) Perform the Shapiro-Wilks test to check normality of the error terms 
#       using the shapiro.test command.

		shapiro.test(e)

#   (g) Based on the results in parts (e) and (f), do the error terms for this 
#       model appear to be normal? 

#   (h) Check constancy of error variance by plotting |e| vs. Yhat. 

		plot(Yhat, abs(e))
		dev.copy(png, paste0(path,'Yhat_vs_abs_e.png'))
		dev.off()

#   (i) Check constancy of error variance by performing the Brown-Forsythe test. 

		levene.test(e, as.factor(Yhat <= median(Yhat)))

#   (j) Based on the results in parts (h) and (i), do the error terms for this 
#       model appear to have constant variance? 

#   (k) Does a transformation of Y appear to be necessary? 


#   (l) Finally, calculate the residual sum of squares ||e||2. Note that this 
#       value is 
#                 || e ||2 = sum(Yi - Yhati)2, i = 1, ... , n
#       so it is similar to a prediciton sum of squares. It measures the sum of 
#       square errors between the predictions Yhati and the actual observations Yi. 
#       This number is very large, so to put it in perspective, calculate 
#                          ||e||2
#                       -------------
#                       || Y - Ybar||2
#       Assessing the model by this criterion is equivalent to using 
#                                 ||e||2
#                     R2 = 1 - ------------.
#                              ||Y - Ybar||2
		sum(e*e)

		R2 <- 1.0 - sum(e*e) / 
				(sum( (this.data$Y - mean(this.data$Y)) * 
					  (this.data$Y - mean(this.data$Y)) ))

		R2


# 2 Let lambda be the optimal value produced by the Box-Cox transformation. 
#   Transform Y by defining Y_tilde_i = (Y_i)^lambda for i = 1, ... , 100.

		boxcox.results = boxcox(model)
		lambda = boxcox.results$x[which.max(boxcox.results$y)]
		Y.tilde <- this.data$Y^lambda

#   (a) Fit a model tmodel by regressing y_tilde on X1, X2, and X3, and find the 
#       corresponding fitted values Y_tilde_hat and e_tilde

		model.t <- lm(Y.tilde~this.data$X1+this.data$X2+this.data$X3)
		Y.tilde.hat <- predict(model.t)
		e.tilde <- model.t$residuals

#   (b) Plot Y_tilde vs Y_tilde_hat and e_tilde vs. Y_tilde_hat. How do these plots 
#       compare to those from problem 2? Does curvature appear to exist in the 
#       transformed model? 

		plot(Y.tilde.hat, Y.tilde)
		dev.copy(png, paste0(path,'Ytilde_vs_Ytildehat.png'))
		dev.off()
		plot(Y.tilde.hat, e.tilde)
		dev.copy(png, paste0(path,'etilde_vs_Ytildehat.png'))
		dev.off()

#   (c) Investigate normality of the errors for the transformed model. 

		qqnorm(e.tilde)
		dev.copy(png, paste0(path, 'residuals_qq_plot_t.png'))
		dev.off()

		shapiro.test(e.tilde)


#   (d) Investigate constancy of error variance for the transformed mode. 

		plot(Y.tilde.hat, abs(e.tilde))
		dev.copy(png, paste0(path,'Yhat_t_vs_abs_e_t.png'))
		dev.off()

		levene.test(e.tilde,as.factor(Y.tilde.hat <= median(Y.tilde.hat)))

#   (e) Do the errors for the transformed model appear to satisfy the assumptions 
#       of normality and constant error variance? How do your results compare to 
#       Those from problem 2? 

# 3 Now let's apply the results from the transformed model to the original variable Y. 

#   (a) First, create a vector of fitted values for Y by defining 
#       Yhat_i = (Y_tilde_hat_i)^(1 / lambda), for i = 1, ... , 100, and create a 
#       vector of residuals by defining e_i = Yi - Yhati, for i = 1, ..., 100. These 
#       are predicted values and residuals for the original model, but they take 
#       advantage of the information from the transformed model. 

		Y.tilde.hat.t = (Y.tilde.hat)^(1/lambda)

#   (b) Plot Y vs. Yhat and e vs. Yhat. Did the transformation appear to correct 
#       problems with the functional form? 

		plot(Y.tilde.hat.t, this.data$Y)
		dev.copy(png, paste0(path,'Y_vs_Ytildehat_t.png'))
		dev.off()
		plot(Y.tilde.hat.t, e.tilde)
		dev.copy(png, paste0(path,'e_vs_Ytildehat_t.png'))
		dev.off()

#   (c) Finally, calculate ||e||2, ||e||2 / (||Y - Ybar||2), and 
#       R2 = 1 - ||e||2 / (||Y - Ybar||2) as in question 2. Which model fits the data
#       better/has a lower residual sum of squares? 
		sum(e.tilde*e.tilde)

		R2 <- 1.0 - sum(e.tilde*e.tilde) / 
					(sum( (Y.tilde.hat.t - mean(Y.tilde.hat.t)) * 
						  (Y.tilde.hat.t - mean(Y.tilde.hat.t)) ))

		1 - R2

		R2



# 4 Import the UCI Machine Learning Repository's Auto-MPG data set and create the best 
#   possible linear regression model for predicting mpg from the other variables. Use 
#   diagnostics and remedial measures to investigate curvature and assumptions related 
#   to the design matrix and error terms.

		auto <- read.csv('~/Dropbox/Tarleton/data_mining/dfiles/auto_data.csv', 
						header=T, na.strings='?', dec='.', strip.white=T)

		names(auto) <- c('mpg', 'cyl', 'displ', 'hp', 'wt', 'accel', 'year', 'orig', 'name')
		model = lm(mpg~., data=auto)
		Yhat = predict(model)
		e = model$residuals

		plot(Yhat, auto$mpg)
		dev.copy(png, paste0(path, 'auto_Yhat_vs_Y.png'))
		dev.off()

		plot(Yhat, e)
		dev.copy(png, paste0(path, 'auto_Yhat_vs_e.png'))
		dev.off()

		qqnorm(e)
		dev.copy(png, paste0(path, 'auto_residuals_qq_plot.png'))
		dev.off()

		shapiro.test(e)

		plot(Yhat, abs(e))
		dev.copy(png, paste0(path,'auto_Yhat_vs_abs_e.png'))
		dev.off()

		levene.test(e,as.factor(Yhat <= median(Yhat)))

		sum(e*e)

		R2 <- 1.0 - sum(e*e) / 
				(sum( (this.data$Y - mean(this.data$Y)) * 
					  (this.data$Y - mean(this.data$Y)) ))

		R2
