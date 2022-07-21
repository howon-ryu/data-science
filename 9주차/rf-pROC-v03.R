# RF pROC

# ROC: https://stats.stackexchange.com/questions/188616/how-can-we-calculate-roc-auc-for-classification-algorithm-such-as-random-forest
# ROC: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
  
# RF: https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/

require(randomForest)
require(mlbench)
data(Sonar)
levels(Sonar$Class)

set.seed(71)

Sonar.rf<-randomForest(Class ~ ., data=Sonar, ntree=100, importance = T)
Sonar.rf
Sonar.rf$importance
windows()
varImpPlot(Sonar.rf)

# ROC and AUC
require(pROC)
# Sonar.rf: randomForest model object 

rf.roc<-roc(Sonar$Class, Sonar.rf$votes[,2])
# calculate sensitivity and specificity of models 
# roc(response:actual class, predictor)

levels(Sonar$Class)
# first and second level represents 
# control(negative class: -) and cases(positive class: +)  

plot(rf.roc)
auc(rf.roc)
