# colinearity test

# Source: 
# https://mindscale.kr/course/basic-stat-r/collinearity/
# Reference:
# https://jaaamj.tistory.com/84

# data: crab.csv - 게의 크기와 무게

df <- read.csv('crab.csv')
head(df)

# 회귀모델
model = lm(y ~ sat + weight + width, df)
summary(model)

# 모델의 설명력은 0.51, 
# F-statistic p vlaue 가 낮으므로 모델은 유의미
# weight 의 p value 로 0.64 로 높아서 유의미 하지 않음

# (다중)공선성의 진단
# 분산팽창계수(VIF, Variance Inflation Factor)를 구하여 판단
# 엄밀한 기준은 없으나 보통 10보다 크면 다중공선성이 있다고 판단
# (5를 기준으로 하기도 함)

# car package
library(car)

#test colinearity
vif(model)

# weight와 width의 VIF가 각각 4.8과 4.6이다. 
# 게의 무게(weight)와 너비(width)는 서로 상관이 높기 때문에 
# VIF가 약간 높게 나타나는 것이다.


# 상관관계가 큰 변수에 대한 대처
# 회귀계수가 통계적으로 유의미하지 않다면 대처
# 회귀계수가 통계적으로 유의미하다면 VIF가 크더라도 특별히 대처할 필요없음

# 변수들을 더하거나 빼서 새로운 변수를 만든다
# (개념적으로나 이론적으로) 두 예측변수를 더하거나 빼더라도 문제가 없는 경우
# 예) 남편의 수입과 아내의 수입이 서로 상관이 높다면, 
# 두 개를 더해 가족 수입이라는 하나의 변수로 투입한다

# 더하거나 빼기 어려운 경우는 변수를 모형에서 제거한다
# 단, 변수를 제거하는 것은 자료의 다양성을 해치고, 
# 분석하려던 가설이나 이론에 영향을 미칠 수 있기 때문에 가급적 자제

# 두 변수 중 width 를 제거한다. 
# 다시 lm
summary(lm(y ~ sat + weight, df))

# 결과로써
# weight 의 p value 가 낮아져 유의미함
# 설명력은 약간 낮아지만 OK.

# 이전의 분석에서는 weight가 유의미하지 않게 나왔지만, 
# width를 제거한 후에는 유의미하게 나왔다. 
# weight와 width가 공선성이 있기 때문에 width를 제거하자 
# weight가 유의미해진 것으로 볼 수 있다.

# 상관관계 분석
library(PerformanceAnalytics)
chart.Correlation(df)

# plot에서  weight 와 width 의 상관관계 가 높게 나온다. 
# 두 변수의 상관 계수는 0.89 로 역시 높다. 
