# Linear Regression - 1 predictor cars data

# Source: http://r-statistics.co/Linear-Regression.html
# Data: cars

set.seed(71) # reproducibility setting
data(cars)
head(cars)

#
# Part 1. Building lm for the description of data 
#

# Graphical Analysis

# 1. Scatter plot
# Check the linear relationship between the independent and dependent variables

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

# 2. Box plot
# Check for outliers.
# May consider deleting them. 

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", 
        sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  

# box plot for 'speed'
boxplot(cars$dist, main="Distance", 
        sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  
# box plot for 'distance'
boxplot(cars$dist, main="Distance", 
        sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  

# 3. Density plot 
#  Check if the response variable is close to normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

# Correlation

cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 
# A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the response variable (Y) is unexplained by the predictor (X), 
# in which case, we should probably look for better explanatory variables.

# Build Linear Model

linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)

# result
# => dist = ???17.579 + 3.932???speed

# Linear Regression Diagnostics

# Before using a regression model, 
# you have to ensure that it is statistically significant.

summary(linearMod)  # model summary

# The p Value: Checking for statistical significance

# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12 = 0
# model p-value: less than significance level 0.05
# the model is significant. 

# t value Pr(>|t|)-p value for speed variable 
# = 9.464 , 1.49-12: less than significance level: 1.49e-12 = 0
# speed variable's p-value: less than significance level 0.05
# the coefficient is significant 

# The model passed F statistic test and
# speed variable also passed t-statistic test
# We conclude we have a statistically significant model.

# Check R-Squared and Adj R-Squared
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 

# check residuals
linearMod$residuals
# plot the residuals
# check the normality of residuals
# windows()
par(mfrow=c(1, 1))
plot(density(linearMod$residuals), 
     main="Density Plot: Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", 
               round(e1071::skewness(linearMod$residuals), 2)))
polygon(density(linearMod$residuals), col="red")

#
# Part 2. Predicting Linear Models
#

# By calculating accuracy measures (like min_max accuracy) 
# and error rates (MAPE or MSE), 
# we can find out the prediction accuracy of the model.

# Step 1: Create the training (development) and 
# test (validation) data samples from original data.

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Step 2: Develop the model on the training data and 
# use it to predict the distance on test data

# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

# Step 3: Review diagnostic measures.
summary (lmMod)  # model summary

# Step 4: Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
# correlation between the actuals and predicted values can be used as 
# a form of accuracy measure

# MinMaxAccuracy and MeanAbsolutePercentageError (MAPE)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 48.38%, mean absolute percentage deviation
install.packages("DAAG")
# k- Fold Cross validation
library(DAAG)
windows()
cvResults <- suppressWarnings(
  CVlm(data = cars, 
       form.lm=dist ~ speed, 
       m=5, 
       dots=FALSE, 
       seed=29, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Small symbols are predicted values while bigger ones are actuals."));  
# performs the CV

attr(cvResults, 'ms')  # => 251.2783 mean squared error

