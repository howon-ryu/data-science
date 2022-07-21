# 데이터 불러오기
ob <- read.csv("ObesityDataSet.csv", header = T)

# 목표변수 4가지로 변환 후 상관계수 계산을 위해 수치형으로 변경경
ob$NObeyesdad[ob$NObeyesdad == 'Insufficient_Weight'] <- 1
ob$NObeyesdad[ob$NObeyesdad == 'Normal_Weight'] <- 2
ob$NObeyesdad[ob$NObeyesdad == 'Overweight_Level_I' | ob$NObeyesdad == 'Overweight_Level_II'] <- 3
ob$NObeyesdad[ob$NObeyesdad == 'Obesity_Type_I' | ob$NObeyesdad == 'Obesity_Type_II' | ob$NObeyesdad == 'Obesity_Type_III'] <- 4
ob$NObeyesdad <- as.numeric(ob$NObeyesdad)

# 데이터 전처리 : factor형으로 변경할거 변경
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

# 목표변수 비율 그래프 그리는 것들
library(ggplot2)
freqob <- xtabs(~NObeyesdad, data = ob1)
proportions(freqob) *100

ggplot(ob1, aes(x = NObeyesdad)) + geom_bar(fill = c(1, 2, 3, 4))
freqob
pie(freqob)

# 수치형 변수들과 상관계수 계산
cor(ob1$Age, ob$NObeyesdad)
cor(ob1$Height, ob$NObeyesdad)
cor(ob1$Weight, ob$NObeyesdad)
cor(ob1$FCVC, ob$NObeyesdad)
cor(ob1$NCP, ob$NObeyesdad)
cor(ob1$CH2O, ob$NObeyesdad)
cor(ob1$FAF, ob$NObeyesdad)
cor(ob1$TUE, ob$NObeyesdad)

# 명목형 변수들의 상태표 출력
# ~표시 우측에 원하는 변수들 이용해 비율 확인 가능
# Gender, family_history, FAVC, CAEC, SMOKE, SCC, CALC, MTRANS
# ~ 과 + 사이에 원하는 명목형 변수 넣어서 확인
feq <- xtabs(~ Gender + NObeyesdad, data = ob1) #절대값 출력
feq
addmargins(feq)
proportions(feq, 1) *100 #가로, 비율들 출력
proportions(feq, 2) *100 #세로


# 데이터를 기반으로 트리를 만들어서 정확도를 확인하는 함수
# data = 데이터프레임, cp나 뒤에 다른 인자들은 rpart에 들어감
# 만약 만들어서 밖에서도 쓰고 싶으면 return # 지우고 사용
# 혼동행렬 보고 싶으면 print 앞에 # 지우면 나올거임
library(caret)
library(rpart)
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

# 열 골라서 추출하기
# dplyr의 select함수 이용
# 첫번째에 원본 데이터프레임, 그 다음부터 추출할 열 이름들
# 무조건 넣기로한 3가지 속성과 목표변수 추출
library(dplyr)
sdf <- select(ob1, Weight, family_history, FAVC, NObeyesdad)
sdf
# 나는 최적을 minsplit = 10, minbucket = 3, maxdepth = 10 으로 정함
#method는 각각 목적이 다름, 우리는 분류기 때문에 class 사용
#cp 최적값 찾기
check <- make_tree_r(sdf, minsplit = 10, minbucket = 3, maxdepth = 10)
plotcp(check)
#xerror(교차 에러?)가 최소일때의 CP값
check$cptable[which.min(check$cptable[,"xerror"]),"CP"]
# cp값이 0~0.01 사이 값들 나온다.
# 나온 cp값으로 가지치기 진행 (여기부터는 안써도 될듯, 그냥 나온 값으로 cp적당히 정하기)
ptree<-prune(check, cp= check$cptable[which.min(check$cptable[,"xerror"]),"CP"])
ptree
rpartpred<-predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$NObeyesdad)


# 뽑은 데이터와 인자들로 정확도 구하기
# 정확도가 많이 낮으므로 보류중인 열 추가
# 보류 : 키, 나이, CAEC, CH2O, SCC, FAF, TUE, MTRANS
sdf <- select(ob1, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)

# 기존 + 키
sdf <- select(ob1, Height, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + 나이
sdf <- select(ob1, Age, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + CAEC
sdf <- select(ob1, CAEC, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + CH2O
sdf <- select(ob1, CH2O, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + SCC
sdf <- select(ob1, SCC, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + FAF
sdf <- select(ob1, FAF, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + TUE
sdf <- select(ob1, TUE, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 기존 + MATRANS
sdf <- select(ob1, MTRANS, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 키만 추가해도 거의 95%가 되니 마무리
#최종 결정 열 : 몸무게, 키, 가족력, FAVC

# 선택한 열과 비교
sdf <- select(ob1, Height, Weight, family_history, FAVC, NObeyesdad)
make_tree(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
#모든 변수를 이용해 생성성
make_tree(ob1, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
#더 적은 속성으로 비슷한 성능을 보인다. 


# GINI VS INFORMATION
make_tree(sdf, parms = list(split = 'information'),
          minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
make_tree(sdf, parms = list(split = 'gini'),
          minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
# 둘 이 별 차이 없다


# 학습 데이터 비율에 따른 그래프 그리기
# 트레인 셋 비율 다르게 해서 정확도 비교
library(caret)
library(rattle)
library(rpart)
tr_rate <- function(data, cp_ = 0.01, ...){ #데이터 외에 추가 입력은 rpart함수 조정
  ts_acc <- vector()
  tr_acc <- vector()
  train_rate <- seq(.1, .95, .05)
  train_rate

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
    ts_acc
    tr_acc
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
#데이터와 인자들 넣어서 진행
tr_rate(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)

final <- make_tree_r(sdf, minsplit = 10, minbucket = 3, maxdepth = 10, cp_ = 0.005)
fancyRpartPlot(final)

# 트리 그래프를 pdf로 뽑아주는 함수
pdf("tree.pdf")
fancyRpartPlot(final)
dev.off()
