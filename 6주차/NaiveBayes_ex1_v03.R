# Naive Bayes example for nb 

# source: 
# https://www.edureka.co/blog/naive-bayes-in-r/

# Data:
# diabetes.csv

# note:
# use nb function in klaR package NOT the one in e1017 
# it is written from naive bayesian from e1017 package.

# reference
# https://uc-r.github.io/naive_bayes
install.packages("Amelia")
install.packages("mice")
# Step 1: Install and load the requires packages
#Loading required packages
library(Amelia)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(klaR) # for NaiveBayes (=nb) function

library(mice)
# 
# library(e1071)
# Reproducibility setting 고정 난수 100개 생성
set.seed(100)

# Step 2: Import the data set
#Reading data into R
data<- read.csv("diabetes.csv")

#Setting outcome variables as categorical
data$Outcome # 1,0으로 되어 있는 outcome
data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))
data$Outcome # true, false로 변경
# Step 3: Studying the Data Set
#Studying the structure of the data
str(data)
head(data)

# Step 4: Data Cleaning
#Convert '0' values into NA
data
data[, 2:7][data[, 2:7] == 0]# 데이터중 0인 애들
data[, 2:7][data[, 2:7] == 0] <- NA
data[, 2:7][data[, 2:7] == 0]# 모두 na로 변경
#visualize the missing data
missmap(data) # 결측치 확인
#Use mice package to predict missing values 결측치 채우기
k<-data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")]
k
mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
summary(mice_mod)
mice_complete <- complete(mice_mod) # 결측치가 채워진 자료 넣기
mice_complete
#Transfer the predicted missing values into the main data set
# 결측치가 완료된 값으로 대체
data$Glucose <- mice_complete$Glucose
data$BloodPressure <- mice_complete$BloodPressure
data$SkinThickness <- mice_complete$SkinThickness
data$Insulin<- mice_complete$Insulin
data$BMI <- mice_complete$BMI

# check again
# 결측치가 없는것을 확인
missmap(data)

# Step 5: Exploratory Data Analysis

#Data Visualization
#Visual 1 나이
data
ggplot(data, aes(Age, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

#visual 2 
c <- ggplot(data, aes(x=Pregnancies, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Pregnancy Distribution by Outcome")
c
c + theme_bw()

#visual 3
P <- ggplot(data, aes(x=BMI, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="BMI Distribution by Outcome")
P + theme_bw()

#visual 4
ggplot(data, aes(Glucose, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Glucose Distribution by Outcome")


#visual 5 전체 상관계수 구하기
ggpairs(data)

# Step 6: Data Modelling
#Building a model
#split data into training and test data sets 인덱스 뽑기
indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)
indxTrain
training <- data[indxTrain,] # 트레이닝
training
testing <- data[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100
testing# 테스트
# check the class ratioes
prop.table(table(training$Outcome)) * 100 #트레이닝의 true / false 비율
prop.table(table(testing$Outcome)) * 100 #테스트의 true / false 비율

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-9] #예측변수
x
y = training$Outcome # 목표 변수
y
# optimize model 
#nb라는 모델을 만들때는 fl, usekernel, adjust 값이 필요하다
modelLookup("nb")

# set up tune grid values

fL <- seq(0, 1, 0.2) #0.0 0.2 0.4 0.6 0.8 1.0
fL
usekernel <- c(TRUE, FALSE)
usekernel #TRUE, FALSE
adjust <- seq(0, 2, 0.5)# 0.0 0.5 1.0 1.5 2.0
adjust
grid <- expand.grid(fL=fL, usekernel=usekernel, adjust=adjust)
#그리드로 합쳐서 조합하기
grid

# optimization
#조율모수의 후보집합 생성
#tunegrid는 특정 값이 필요할때 사용, 행이 조율모수 설정이고 열이 조율모수인경우 사용
model = train(x,
              y,
              'nb',
              trControl=trainControl(method='cv',number=10), # 훈련할 파라미터
              tuneGrid=grid
              )
model

# Step 7: Model Evaluation

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing ) 
Predict

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(Predict, testing$Outcome)

#Plot Variable performance
X <- varImp(model) # Variable importance - RF
x
plot(X)

