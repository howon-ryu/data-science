### Intro. to olsrr package 
# https://olsrr.rsquaredacademy.com/articles/intro.html

# The olsrr package provides following tools for teaching 
# and learning OLS regression(linear regression) using R:
  
# comprehensive regression output
# residual diagnostics
# measures of influence
# heteroskedasticity tests
# collinearity diagnostics
# model fit assessment
# variable contribution assessment
# variable selection procedures
install.packages("olsrr")
library(olsrr)

# Regression
ols_regress(mpg ~ disp + hp + wt + qsec, data = mtcars)
head(mtcars)

# Residual vs Fitted Values Plot
# Plot to detect non-linearity, unequal error variances, and outliers.
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
model
summary(model)
# intercept=> 기울기, 나머지는 각각의 y절편
# residuals: 잔차(직선을 구하기 위한 에러값)
ols_plot_resid_fit(model)


# Residual Fit Spread Plot
# Plot to detect non-linearity, influential observations and outliers.
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_plot_resid_fit_spread(model)

# Stepwise Regression
# Build regression model from a set of candidate predictor variables 
# by entering and removing predictors based on p values, 
# in a stepwise manner until there is no variable left to enter or remove any more.

### Variable Selection
# stepwise regression
head(surgical)
model <- lm(y ~ ., data = surgical)
model
summary(model)
ols_step_both_p(model) # hybrid (or stepwise) method
#변수를 추가해가면서 결정계수를 높이고 aic를 낯춤
# Plot
plot(k) 
#
# Stepwise AIC Backward Regression
# Build regression model from a set of candidate predictor variables 
# by removing predictors based on Akaike Information Criteria, 
# in a stepwise manner until there is no variable left to remove any more.

# Variable Selection
# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
k

# Plot
plot(k)
### summary를 해석할때 R-squared = 결정계수 , Adjussted R-squared = 수정결정계수
# Variable Selection Methods
# https://olsrr.rsquaredacademy.com/articles/variable_selection.html

# All Possible Regression (test all the subsets)
# 모든 가능한 독립변수 조합에서의 다중 공선성 수치를 나타낸다
# 결정계수(r-squared)가 높을수록 독립변수가 종속변수를 많이 설명한다는 뜻
# cp : 표준화된 단차 제곱합의 추정량

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_all_possible(model)
k
plot(k)

# Best Subset Regression
# Select the subset of predictors that do the best at meeting 
# some well-defined objective criterion, such as having 
# the largest R2 value or the smallest MSE, Mallow??s Cp or AIC.
head(mtcars)
# 독립변수의 개수별로 가장 적합한 모델을 나타낸다
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_best_subset(model)
k
plot(k)

# Stepwise Forward Regression
# Build regression model from a set of candidate predictor variables 
# by entering predictors based on p values, 
# in a stepwise manner until there is no variable left to enter any more. 
# The model should include all the candidate predictor variables. 
# If details is set to TRUE, each step is displayed.

# Variable Selection
# stepwise forward regression by p value - (F-test)
model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_p(model) # for backward,  ols_step_backward_p()
k
plot(k)

# Stepwise Backward Regression by AIC
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model) # for backward, ols_step_forward_aic()
k
plot(k)

# Stepwise Regression by p vlaue 
model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model) # for aic, ols_step_both_p()
k
plot(k)
