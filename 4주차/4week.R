library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(rattle)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
data <- read.csv(file = "C:/Users/qorgh2akfl/Desktop/데사기/4주차/health.csv", header = TRUE)
str(data)
class(data)
head(data)
summary(data)
indexes = createDataPartition(data$NObeyesdad, p = .8, list = F)
str(indexes)
summary(indexes)
train = data[indexes, ]
test = data[-indexes, ]
train
summary(train)
summary(test)

fit <- rpart(NObeyesdad~., 
             data = train,
             cp = -1, 
             minsplit = 2,
             minbucket = 75 ) 
printcp(fit)
str(fit)
summary(fit)
windows()
plot(fit)
text(fit, cex = 1, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

pred = predict(fit, test, type = "class" )
print(data.frame(test, pred))
obey<-as.factor(test$NObeyesdad)
c1<-confusionMatrix(obey, pred, positive = 'yes')
c1
readline('Enter to resume ')

fitp <- rpart(NObeyesdad~., 
             data = train,
             # cp = -1, 
             minsplit = 4,
             minbucket = 75 ) 
printcp(fitp)
str(fitp)

windows()
plot(fitp)
text(fitp, cex = 1, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

windows()
fancyRpartPlot(fitp)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fitp)
readline('Enter to resume ')
dev.off() # close window

predp = predict(fitp, test, type = "class" )
print(data.frame(test, predp))
obey<-as.factor(test$NObeyesdad)
c2<-confusionMatrix(obey, predp, positive = 'yes')
c2
readline('Enter to resume ')


