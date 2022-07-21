library(olsrr)
library(dplyr)
library(DAAG)
library(ggplot2)
library(mlbench)
library(tidyverse)
library(dplyr)
library(patchwork)
data <- read.csv('possum.csv')

# 데이터 분석
head(data)
summary(data)
str(data)
#데이터 전처리


# 1~3열 제거 (case: 순서, site : 덧에 잡힌 장소 번호 , pop: 인구..? 장소...?)
d<-data[,-c(1,2,3)]
head(d)
df <- na.omit(d)  # 모든 변수에 결측치 없는 데이터 추출
df  
table(is.na(df))
summary(df)
head(df)
df$sex[df$sex == 'm'] <- 1
df$sex[df$sex == 'f'] <- 2
head(df)
str(df)
dtype(df$sex)
#회귀 모델 만들기
ols_regress(sex ~ age + hdlngth + skullw + totlngth+taill+footlgth+earconch+eye+chest+belly, data = df)
head(df)

model <- lm(sex ~ age + hdlngth + skullw + totlngth+taill+footlgth+earconch+eye+chest+belly, data = df)
model
summary(model)
plot(model)
# 예측모델 트레인 테스트 나누기
# library(caret)

set.seed(1000) 
idx<- caret::createDataPartition(df$sex, p=0.7)
df_train<-df[idx$Resample1,]
df_test<-df[-idx$Resample1,]
df_train
lmMod <- lm(sex ~ age + hdlngth + skullw + totlngth+taill+footlgth+earconch+eye+chest+belly, data=df_train)  # build the model
distPred <- predict(lmMod, df_test)
distPred
summary (lmMod)

actuals_preds <- data.frame(cbind(actuals=as.numeric(df_test$sex), predicteds=distPred))  # make actuals_predicteds dataframe.
actuals_preds
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy #0.74 => 실제값과 예측값이 얼마나 근접한가
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # 절대 백분율 편차 0.35 => 오차와 실제의 비율

rmse<-function(y1,y2){
  sqrt(mean((y1-y2)^2))
}

rmse_test_lm<-rmse(distPred,as.numeric(df_test$sex))
rmse_test_lm
cvResults <- suppressWarnings(
  CVlm(data = df, 
       form.lm=as.numeric(sex) ~ age + hdlngth + skullw + totlngth+taill+footlgth+earconch+eye+chest+belly, 
       m=10, 
       dots=FALSE, 
       seed=29, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Small symbols are predicted values while bigger ones are actuals."));  
# performs the CV

cv_ms<-attr(cvResults, 'ms')
cv_rms<-sqrt(cv_ms)
cv_rms
# ① Residual
# 예측하고자 하는 변수의 실제 값과 회귀분석으로 얻어진 값 사이의 차이에는 표준오차로 인하여 차이가 발생하며 이를 잔차(residual)이라 한다
# 
# ② Significance stars
# 계산된 p값에 따라 표시되는 별표의 수는 중요도의 수준을 나타내며, ***는 높은 중요도, *는 낮은 중요도를 의미한다

# 
# ③ Estimated coefficient
# 회귀분석에 의하여 산출된 기울기 값이다.
# 
# ④ Standard error of the coefficient estimate
# coefficient 추정치의 변동성을 측정한다
# 
# ⑤ t-value of the coefficient estimate
# 변수에 대한 coefficient가 해당 모델에 대하여 의미가 있는지 여부를 측정하며 이 값 자체는 사용하지 않으나 p값과 유의 수준을
# 계산하는데 사용한다
# 
# ⑥ Variable p-value
# 유의수준으로 p값이 작을수록 신뢰구간에 들어간다
# 
# ⑦  Significance legend
# 변수 옆에 구둣점이 많을수록 바람직 하다.
# 공백은 나쁨, 점은 꽤 좋음, 별은 좋음, 복수의 별은 아주 좋음을 의미한다
# 
# ⑧ Residual Std error / Degrees of freedom
# Residual Std error는 잔차의 표준편차이며, 자유도 (degree of freedom)은 샘플에 포함된 관측치의 개수와 모델에 사용된
# 변수갯수와의 차이이다
# 
# ⑨ R – squared
# 모델에 의하여 해석되는 예측의 변동량으로, 모델의 적합성을 평가하는 척도로도 사용된다. 1이 최고치이므로 1에 가까운 값이
# 바람직하다.
# 
# ⑩ F-statistics & resulting p-value
# 모델에서 F-test를 수행한다. 해당 모델의 변수를 취하고, 더 적은 수의 매개변수를 가진 모델과 비교한다.
# 이론적으로 매개변수가 많은 쪽이 더 잘 맞아야 하며, 더 많은 매개변수를 가진 모델이 더 적은 수의 매개변수를 가진 모델보다
# 잘 수행하지 않으면 F-test는 더 높은 p-value를 갖게 된다.
# 반면에 매개변수가 더 많은 모델이 매개변수가 적은 모델보다 낮다면 더 낮은 p-value를 갖게 된다.

# 잔차
# intercept=> 기울기, 나머지는 각각의 y절편
# residuals: 잔차(직선을 구하기 위한 에러값)
ols_plot_resid_fit(model)
ols_plot_resid_fit_spread(model)
#
kk <- ols_step_all_possible(model)
kk
plot(kk)
model
# 독립변수의 개수별로 가장 적합한 모델을 나타낸다
k <- ols_step_best_subset(model)
k
plot(k) # 현제 플롯과  summary로 볼때 aic값이 가장 작고, adj.r-square값이 가장 큰 4번 모델이 적합해 보임
#stepwise forward regression by p value - (F-test)
k <- ols_step_forward_p(model) # for backward,  ols_step_backward_p()
k #eye>totlngth>hdlngth>chest 전방 선택시 순서로 중요한 변수임
plot(k)
#Stepwise Backward Regression by AIC
# aic는 낮을 수록 좋은것! 따라서 aic가 높은 예측변수들을 하나씩 빼나가는 과정!
k <- ols_step_backward_aic(model) # for backward, ols_step_forward_aic()
k
plot(k)# 후방제거 시 earconch>footlgth>age>skullw>shest>taill 순서로 빠져야 함
#Stepwise Regression by p vlaue 
k <- ols_step_both_p(model) # for aic, ols_step_both_p()
k
plot(k)


# t-test(평균에 대한 평가가),f-test(분산에 대한 평가)의 pvalue가 0에 가까울수록