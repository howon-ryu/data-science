

#1. amazon mechanical Turk 란 무엇인가?
#아마존 메커니컬 터크(Amazon Mechanical Turk) 란 아마존의 웹 서비스 사업모델 중 하나로 
#업무 요구자(Requester) 가 소액의 보상을 내걸고 간단한 업무를 올려놓으면 
#불특정 다수의 노동자(Worker) 들이 이 업무를 수행하고 해당 보상을 받는 서비스이다. 
#여기서의 업무는 특정 웹사이트 검색하기, 간단한 설문조사 하기 부터 외국어 번역 등 다양한 업무들이 주어지며 
#1센트에서 몇십 달러까지의 보상이 걸린다. 
#아마존은 플랫폼을 구축해 놓고 Requester 와 Worker 를 연결시켜 주면서 일정액의 수수료를 받고 있다.

#라이브러리
library(ggplot2)
library(caret)
library(rpart)
library(dplyr)
library(rattle)
library(rpart.plot)
library(pROC)

# 데이터 불러오기
ob <- read.csv("ObesityDataSet.csv", header = T)
ob$NObeyesdad[ob$NObeyesdad == 'Insufficient_Weight'] <- 1
ob$NObeyesdad[ob$NObeyesdad == 'Normal_Weight'] <- 2
ob$NObeyesdad[ob$NObeyesdad == 'Overweight_Level_I' | ob$NObeyesdad == 'Overweight_Level_II'] <- 3
ob$NObeyesdad[ob$NObeyesdad == 'Obesity_Type_I' | ob$NObeyesdad == 'Obesity_Type_II' | ob$NObeyesdad == 'Obesity_Type_III'] <- 4
ob$NObeyesdad <- as.numeric(ob$NObeyesdad)
ob1 <- data.frame(Gender = as.factor(ob$Gender),
                  Age = ob$Age,
                  Height = ob$Height,
                  Weight = ob$Weight,
                  family_history = as.factor(ob$family_history_with_overweight),
                  FAVC = as.factor(ob$FAVC),
                  FCVC = ob$FCVC,
                  NCP = ob$NCP,
                  CAEC = factor(ob$CAEC, levels = c('no', 'Sometimes', 'Frequently', 'Always')),
                  SMOKE = as.factor(ob$SMOKE),
                  CH2O = ob$CH2O,
                  SCC = as.factor(ob$SCC),
                  FAF = ob$FAF,
                  TUE = ob$TUE,
                  CALC = factor(ob$CALC, levels = c('no', 'Sometimes', 'Frequently', 'Always')),
                  MTRANS = factor(ob$MTRANS, levels = c('Walking', 'Bike', 'Public_Transportation', 'Motorbike', 'Automobile')),
                  NObeyesdad = factor(ob$NObeyesdad, levels = c(1, 2, 3, 4)))


make_tree <- function(data, cp_ = -1, ...){
  indexes = createDataPartition(data$NObeyesdad, p = .6, list = F)
  train = data[indexes, ]
  test = data[-indexes, ]
  fit <- rpart(NObeyesdad~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  acc <- sum(tmp$NObeyesdad == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  #return(fit)
}
#return 푼 버전
make_tree_r <- function(data, cp_ = -1, ...){
  indexes = createDataPartition(data$NObeyesdad, p = .6, list = F)
  train = data[indexes, ]
  test = data[-indexes, ]
 
  fit <- rpart(NObeyesdad~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  acc <- sum(tmp$NObeyesdad == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  return(fit)
}
# 2. 전체 데이터 eda
summary(ob1)

fit <- rpart(NObeyesdad~.,
             data = train,
             cp = -1, method = "class")
dev.off()
fancyRpartPlot(fit)
pdf("fit.pdf")
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off()

# 3. 결측지 x
sum(is.na(ob1))
# 4. rpart 파라미터 최적화
# rpart의 인자들을 다르게 해보자
for(i in 1:20){ #최대 깊이를 1~20
  make_tree(ob1, maxdepth = i)
}

for(i in seq(1, 100, 5)){ #1, 6, 11, ..., 96
  make_tree(ob1, minsplit = i)
}

for(i in 1:10){ # 1~10
  make_tree(ob1, minbucket = i)
}

for(i in seq(0.01, 0.5, 0.05)){ #0.01, 0.015, 0.02, ..., 0.5
  make_tree(ob1, cp_ = i)
}
ff<-make_tree_r(ob1, minsplit = 2, minbucket = 1, maxdepth = 30, cp_ = -1)
fancyRpartPlot(ff)

error<-printcp(ff)
pred = predict(ff, test, type = "class",)
confusionMatrix(as.factor(test$NObeyesdad), pred)
error[,1]
min = 1.1
index = 0
cp_ = 0
for(i in 1:length(result[,1])){
  if( result[i,4] < min ){
    min = result[i,4]
    index = i
  }
}
minsplit_ = error[index,2]
cp_ = error[index,1]
minsplit_
cp_
#=>>minsplit = 10, minbucket = 3, maxdepth = 10

#pre-pruning

make_tree_pre <- function(data, ...){
  
  indexes = createDataPartition(data$NObeyesdad, p = .6, list = F)
  train = data[indexes, ]
  test = data[-indexes, ]
  parms = list(split = 'gini')
  fit <- rpart(NObeyesdad~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  acc <- sum(tmp$NObeyesdad == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  
  return(fit)
  
}
make_tree_pre_acc <- function(data, ...){
  
  indexes = createDataPartition(data$NObeyesdad, p = .6, list = F)
  train = data[indexes, ]
  test = data[-indexes, ]
  parms = list(split = 'gini')
  fit <- rpart(NObeyesdad~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  tmp
  acc <- sum(tmp$NObeyesdad == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  return(acc*100)
  
  
}

final_pre<-make_tree_pre(ob1, minsplit=minsplit_, minbucket = 3, maxdepth = 10)
dev.off()
fancyRpartPlot(final_pre)
dev.off()
pred_final_pre = predict(final_pre, test, type = "class",)
pred_final_pre
confusionMatrix(as.factor(test$NObeyesdad), pred_final_pre)
pdf("final_pre.pdf")
fancyRpartPlot(final_pre)
dev.off()
# full-tree 만들기
ff<-make_tree_r(ob1, minsplit = 2, minbucket = 1, maxdepth = 30, cp_ = -1)
fancyRpartPlot(ff)

#post-pruning


make_tree_post <- function(data, cp_ = cp_, ...){
  indexes = createDataPartition(data$NObeyesdad, p = .6, list = F)
  train = data[indexes, ]
  test = data[-indexes, ]
  
  fit <- rpart(NObeyesdad~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  acc <- sum(tmp$NObeyesdad == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  
  return(fit)
}
make_tree_post_acc <- function(data, cp_ = cp_, ...){
  indexes = createDataPartition(data$NObeyesdad, p = .6, list = F)
  train = data[indexes, ]
  test = data[-indexes, ]
  
  fit <- rpart(NObeyesdad~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  acc <- sum(tmp$NObeyesdad == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  return(acc*100)
  
}

final_post<-make_tree_post(ob1, minsplit=2, minbucket = 1, maxdepth = 30, cp=cp_)
dev.off()
fancyRpartPlot(final_post)
dev.off()
pred_final_post = predict(final_post, test, type = "class",)
confusionMatrix(as.factor(test$NObeyesdad), pred_final_post)
pdf("final_post.pdf")

fancyRpartPlot(final_post)
dev.off()


# pre, post 트리 정확도 , 민감도, 특이도
# 정확도
ten<-c(1,2,3,4,5,6,7,8,9,10)
final_pre_line <-NULL
for(i in 1:10){ # 1~10
  final_pre_line<-c(final_pre_line,make_tree_pre_acc(ob1, minsplit=minsplit_, minbucket = 3, maxdepth = 10))
}
final_pre_line
final_post_line<-NULL
for(i in 1:10){ # 1~10
  final_post_line<-c(final_post_line,make_tree_post_acc(ob1, minsplit=2, minbucket = 1, maxdepth = 30, cp=cp_))
}
final_post_line
fi<-data.frame(final_pre_line,final_post_line)
fi
pp<-plot(ten,final_pre_line,xlab = "num",ylab="acc" )
plo_acc <- ggplot(data = fi, aes(x = ten)) +
  geom_line(aes(y = final_pre_line, colour = "final_pre_line")) + 
  geom_line(aes(y = final_post_line, colour = "final_post_line")) 
plo_acc
# 민감도(실제 true 중 얼마나 true라고 예측했나), 특이도(false중 실제로 false인 선택지)

confusionMatrix(as.factor(test$NObeyesdad), pred_final_pre)
confusionMatrix(as.factor(test$NObeyesdad), pred_final_post)

#러닝커브
tr_rate <- function(data, cp_ = cp_,...){ #데이터 외에 추가 입력은 rpart함수 조정
  ts_acc <- vector()
  tr_acc <- vector()
  train_rate <- seq(.1, .95, .05)
  for(i in train_rate){
    indexes = createDataPartition(data$NObeyesdad, p = i, list = F)
    train = data[indexes, ]
    test = data[-indexes, ]
    fit <- rpart(NObeyesdad~.,
                 data = train,
                 cp = cp_, ...)
    tr_pred = predict(fit, train, type = "class", )
    ts_pred = predict(fit, test, type = "class", )
    #cat("\ni = ", i, "일때 \n")
    #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
    tmp1 <- data.frame(train, tr_pred)
    tmp2 <- data.frame(test, ts_pred)
    tr <- sum(tmp1$NObeyesdad == tmp1$tr_pred) / nrow(tmp1)
    ts <- sum(tmp2$NObeyesdad == tmp2$ts_pred) / nrow(tmp2)
    tr_acc <- c(tr_acc, tr)
    ts_acc <- c(ts_acc, ts)
  }
  cat("tr_acc : ", tr_acc, "\n", "ts_acc : ", ts_acc, "\n")
  mat <- data.frame(train_rate, tr_acc, ts_acc)

  pl <- ggplot(data = mat, aes(x = train_rate)) +
    geom_line(aes(y = tr_acc, colour = "tr_acc")) + 
    geom_line(aes(y = ts_acc, colour = "ts_acc")) + 
    scale_colour_manual("",
                        breaks = c("tr_acc", "ts_acc"),
                        values = c("red", "blue")) +
    xlab("Train_rate") + scale_y_continuous("accuracy", limits = c(0.8, 1)) + 
    labs(title = "train_rate_accuracy") +
    geom_point(aes(y = tr_acc, colour = "tr_acc")) + geom_point(aes(y = ts_acc, colour = "ts_acc"))
  pl
  #pl <- ggplot(data = data.frame()) +
  #  geom_line(aes(x = train_rate, y = tr_acc), color = "red") + geom_point(aes(x = train_rate, y = tr_acc), color = "red") + 
  #  geom_line(aes(x = train_rate, y = ts_acc), color = "blue")+ geom_point(aes(x = train_rate, y = ts_acc), color = "blue") + 
  #  ylab("accuracy, red_train, blue_test")
  #pl
}
tr_rate(ob1, minsplit = minsplit_, minbucket = 3, maxdepth = 10, cp_ = cp_)
# rule set 추출
rpart.rules(final_pre)
# " when Weight >=  102 " -> 32%//  
#"when Weight is 73 to  90 & Height >=1.7 & CAEC is Sometimes or Always"->15%
rpart.rules(final_post)
# "when Weight >=  100"->34% //
# "when Weight is 74 to 90 & Height >= 1.7 & CAEC is Sometimes"->14%//
#"when Weight <  60  & Height >=  1.7 -> 9%//
#"when Weight is 60 to 75 & Height <  1.6 & CAEC is no or Sometimes " -> 8%




library(rpart.plot)
final_pre<-rpart(ob1, minsplit=minsplit_, minbucket = 3, maxdepth = 10)# pre 트리 만들기
final_post<-rpart(ob1, minsplit=2, minbucket = 1, maxdepth = 30, cp=cp_)# post 트리 만들기
rpart.rules(final_pre)
rpart.rules(final_post)

