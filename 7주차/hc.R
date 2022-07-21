library(tidyverse)
library(rpart)
library(e1071)
library(caret)
# library(xlsx)
library(nnet)
library(Amelia)
library(kernlab) # svmLinear2
library(rattle)
hc<- read.csv("hcvdat.csv") 

str(hc)
data <- hc
summary(data)
head(data)
data<-data[,-1]
mode<- factor(data$Category, levels = c("0=Blood Donor", "0s=suspect Blood Donor", "1=Hepatitis", "2=Fibrosis","3=Cirrhosis" ))
mode
head(data)
missmap(data)

sum(is.na(data))
data<-na.omit(data)
summary(data)
missmap(data)
Sex <- as.numeric(data$Sex) 
Category<- as.numeric(data$Category)
preProcValues = preProcess(data)
preProcValues
missmap(preProcValues)
ds <- predict(preProcValues, data)
ds
summary(ds)
set.seed(4) # random seed
indexes = createDataPartition(ds$Category, p = .6, list = F)
train = ds[indexes, ]
test = ds[-indexes, ]
train
test


fit = rpart(Category~., 
            data = train
)

printcp(fit)

fancyRpartPlot(fit)

pred = predict(fit, test, type = "class" )
pred = as.factor(fit)
print(data.frame(test, pred))
confusionMatrix(pred, test$Category)


tmp <- data.frame(test, pred)
tmp
#tmp
#tmp$pred
acc <- sum(tmp$Category == tmp$pred) / nrow(tmp)
acc

## nnet
tuneGrid = expand.grid(size = 1, decay = 5e-04)
tuneGrid
# 훈련에 지정할 훈련 파라미터 생성
# cv : 교차검증
# repeatedcv(교차 검증의 반복) , number : 교차검증 몇겹, repeats:반복 횟수 
trControl = trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
#maxit = 200 : 200반복해서 가장 좋은 모델 고르기 
train
missmap(train)
missmap(test)
sum(is.na(train$Category))

#nnet
model_nnet = train(Category ~.,
              data = train,
              method = 'nnet',
              maxit = 200,
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid=tuneGrid
)
model_nnet





#svm
model_svm1 <- train(Category ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                #tuneLength = 10,
                trControl = trControl,
                tuneGrid = tuneGrid
)
model_svm1






