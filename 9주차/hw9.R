#install.packages("adabag")
#install.packages("randomForest")
install.packages("ROCR")
library(adabag)
library(randomForest)
library(mlbench)
library(rpart)
library(pROC)
library(ROCR)
library(scales)
library(reshape)
heart<- read.csv("hcvdat.csv", header = T)
summary(heart)
head(heart)

heart$Sex[heart$Sex %in% "m"]<-as.numeric(1)
heart$Sex[heart$Sex %in% "f"]<-as.numeric(2)
heart$Sex
heart$Sex<-as.numeric(heart$Sex)
summary(heart)
heart$CategoryF <- factor(615, levels = c("Donor", "NO"))
heart$CategoryF[heart$Category %in% "0=Blood Donor" | heart$Category %in% "0s=suspect Blood Donor"] <- "Donor"
heart$CategoryF[heart$Category %in% "1=Hepatitis" | heart$Category %in% "2=Fibrosis" | heart$Category %in% "3=Cirrhosis"] <- "NO"
heart$CategoryF
# 순번 1번열, 전처리 전 카테고리 열 제거
heart<-heart[0:-2]
head(heart)
prop.table(table(heart$CategoryF))
set.seed(615)
heart<-heart[sample(nrow(heart)),]
split<-floor(nrow(heart)/3)
split
ensembledata<-heart[0:split,]
blenderdata<-heart[(split+1):(split*2),]
testingdata<-heart[(split*2+1):nrow(heart),]
labelname<-'CategoryF'
labelname
predictors <- names(ensembledata)[names(ensembledata) != labelname]
predictors

myControl <- trainControl(method='cv', number=3, returnResamp='none')
myControl
test_model <- train(blenderdata[,predictors], blenderdata[,labelname], method='gbm', trControl=myControl)
blenderdata[,predictors]
testingdata[,predictors]
#preds_before <- ROCR::prediction(as.numeric(predict(object=test_model, testingdata[,predictors],type="raw")),as.numeric(testingdata[,labelname]))
#roc <-performance(preds_before,"tpr","fpr")
#auc(roc)
preds_bef<-predict(object=test_model,testingdata[,predictors])


tmp_before <- data.frame(testingdata, preds_bef)
tmp_before
tmp_before$preds_before
tmp_before$CategoryF 
acc <- sum(tmp_before$CategoryF == tmp_before$preds_bef) / nrow(tmp_before)
acc



auc <- roc(as.numeric(testingdata[,labelname]), as.numeric(preds_bef))
print(auc$auc) 


model_gbm <- train(ensembledata[,predictors], ensembledata[,labelname], method='gbm', trControl=myControl)

model_rpart <- train(ensembledata[,predictors], ensembledata[,labelname], method='rpart', trControl=myControl)

model_treebag <- train(ensembledata[,predictors], ensembledata[,labelname], method='treebag', trControl=myControl)


blenderdata$gbm_PROB <- predict(object=model_gbm, blenderdata[,predictors])
blenderdata$rf_PROB <- predict(object=model_rpart, blenderdata[,predictors])
blenderdata$treebag_PROB <- predict(object=model_treebag, blenderdata[,predictors])

testingdata$gbm_PROB <- predict(object=model_gbm, testingdata[,predictors])
testingdata$rf_PROB <- predict(object=model_rpart, testingdata[,predictors])
testingdata$treebag_PROB <- predict(object=model_treebag, testingdata[,predictors])

predictors <- names(blenderdata)[names(blenderdata) != labelname]
final_blender_model <- train(blenderdata[,predictors], blenderdata[,labelname], method='rpart', trControl=myControl)
blenderdata[,predictors]
blenderdata[,labelname]
preds_after <- predict(object=final_blender_model, testingdata[,predictors])
preds_after
tmp_after <- data.frame(testingdata, preds_after)
tmp_after
acc <- sum(tmp_after$CategoryF == tmp_after$preds_after) / nrow(tmp)
acc
auc <- roc(testingdata[,labelname], preds)

print(auc$auc)


preds_after <- ROCR::prediction(as.numeric(predict(object=final_blender_model, testingdata[,predictors],type="raw")),as.numeric(testingdata[,labelname]))
roc <-performance(preds_before,"tpr","fpr")
preds_bef<-predict(object=test_model,testingdata[,predictors])
plot(roc,colorize = TRUE)


# bagging model
indexes = createDataPartition(heart$CategoryF, p = .66, list=F)

train = heart[indexes, ]
is.na(train)
train<-na.omit(train)
test =  heart[-indexes, ]
is.na(test)
train<-na.omit(test)
heart.bagging<-bagging(CategoryF~., data=heart, mfinal=10)
heart.bagging$importance
heart.bagging
heart.bagging$votes[,2]
# 도식솨
plot(heart.bagging$trees[[10]])
text(heart.bagging$trees[[10]])

#예측값
pred_bagging<-predict(heart.bagging, newdata = heart)
pred_bagging
#정오분류표
table(pred_bagging$class, heart[,13])
pred_bagging$class
heart
tmp <- data.frame(heart, pred_bagging$class)
tmp
confusionMatrix(heart[,13],pred_bagging$class)
#정확도
acc <- sum(tmp$CategoryF == tmp$pred_bagging.class) / nrow(tmp)
acc
is.na(pred_bagging$class)
summary(pred_bagging$class)
summary(tmp$CategoryF)
as.factor(pred_bagging$class[1])
is.factor(pred_bagging$class[1])
is.factor(heart$CategoryF)
bagging.roc<-roc(pred_bagging$class,as.numeric(heart$CategoryF))
plot(bagging.roc)
auc(bagging.roc)


#boosting
heart.adabag<-boosting(CategoryF~., data=heart,boos = TRUE, mfinal=10)
heart.adabag$importance
#도식화
plot(heart.adabag$trees[[10]])
text(heart.adabag$trees[[10]])
#예측값
pred_boosting<-predict(heart.adabag, newdata = heart)
#정오 분류표
tb<- table(pred_boosting$class,heart[,13])
tb
#오분류율
error.rpart <- 1-(sum(diag(tb)/sum(tb)))
error.rpart
#정확도
tmp_bag <- data.frame(heart, pred_boosting$class)
tmp_bag
acc_bag <- sum(tmp_bag$CategoryF == tmp_bag$pred_boosting.class) / nrow(tmp_bag)
acc_bag
boosting.roc<-roc(pred_boosting$class,as.numeric(heart$CategoryF))
plot(boosting.roc)
auc(boosting.roc)


