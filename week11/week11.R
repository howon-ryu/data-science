library(arules)
library(ggplot2)
library(arulesViz)
library(shinythemes)
library(pmml)
library(caret)
library(rpart)
library(dplyr)

library(rattle)

social = read.csv("social.csv")
head(social)
summary(social)
social <- na.omit(social)
summary(social)
head(social)
dim(social)

#상류층의 경우 인간관계 만족감이 높으나 빈곤층일지라도 불만족 보다는 보통이 더 많음
#인간관계의 경우 선형관계가 성립하는데 인간관계의 만족도가 높을수록 삶의 만족도도 비례적으로 높음
table(social$계층의식,social$만족감)
table(social$인간관계,social$만족감)

# 독립성 테스트 결과 
# h0(귀무 가설): 게층의식, 인간관계는 만족감과 연관이 없다
# h1(연구 가설): 게층의식, 인간관계는 만족감과 연관이 있다
# 둘의 p-value가 2.2e-16 임으로 귀무가설을 기각 즉, 계층의식과 인간관계는 만족감과 연관이 있으며, 종속적이다.
chisq.test(social$계층의식,social$만족감)
chisq.test(social$인간관계,social$만족감)

table(social$인간관계,social$계층의식)
chisq.test(social$인간관계,social$계층의식)

# Now, the data can be automatically recoded as a binary incidence matrix 
# by coercing the data set to transactions.
social_transaction <- as(social, "transactions")
social_transaction

summary(social_transaction)
inspect(social_transaction)



class(social_transaction) 



frequentItems <- eclat (social_transaction, 
                        parameter = list(supp = 0.021, 
                                         maxlen = 10)) 

inspect(head(frequentItems, 3))
# calculates support for frequent items

itemFrequencyPlot(social_transaction, 
                  topN=10, 
                  type="absolute", 
                  main="Item Frequency") 

# 아래 파라미터로 194903개의 rules가 생성
rules <- apriori (social_transaction, 
                  parameter = list(supp = 0.005, 
                                   conf = 0.1)) 


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
# 'high-confidence' rules.

inspect(head(rules_conf)) 
# show the support, lift and confidence for all rules

rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
# 'high-lift' rules.

inspect(head(rules_lift)) 




rules <-
  apriori(social_transaction,
          parameter = list (
            supp = 0.1, # min sup
            conf = 0.9  # min cof
               # max num of elements
          )) 

inspect(head(rules, 3, by = "confidence"))



length(rules)

rules

subsetRules <- which(rowSums(is.subset(rules, rules, proper = T)) > 1) 
# get subset rules in vector
length(subsetRules)  
#  
rules <- rules[-subsetRules] 
# remove subset rules. 
length(rules)

# lhs가 만족감일때

rules_r1 <- apriori (data=social_transaction, 
                    parameter=list (supp=0.05,
                                    conf = 0.3
                    ),
                    appearance = list (default="rhs",
                                       lhs="만족감=만족")) 
#control = list (verbose=F)) 

rules_conf_r1 <- sort (rules_r1, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf_r1,5))


rules_conf_r1 <- sort (rules_r1, by="lift", decreasing=TRUE) 
inspect(head(rules_conf_r1,5))

rules_r2 <- apriori (data=social_transaction, 
                    parameter=list (supp=0.05,
                                    conf = 0.3
                    ),
                    appearance = list (default="rhs",
                                       lhs="만족감=보통")) 
#control = list (verbose=F)) 

rules_conf_r2 <- sort (rules_r2, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf_r2,5))


rules_conf_r2 <- sort (rules_r2, by="lift", decreasing=TRUE) 
inspect(head(rules_conf_r2,5))

rules_r3 <- apriori (data=social_transaction, 
                    parameter=list (supp=0.05,
                                    conf = 0.3
                    ),
                    appearance = list (default="rhs",
                                       lhs="만족감=불만족")) 
#control = list (verbose=F)) 

rules_conf_r3 <- sort (rules_r3, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf_r3,5))


rules_conf_r3 <- sort (rules_r3, by="lift", decreasing=TRUE) 
inspect(head(rules_conf_r3,5))




# rhs 가 만족감일때

rules_1 <- apriori (data=social_transaction, 
                  parameter=list (supp=0.05,
                                  conf = 0.3
                                  ),
                  appearance = list (default="lhs",
                                     rhs="만족감=만족")) 
                  #control = list (verbose=F)) 

rules_conf_1 <- sort (rules_1, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf_1,5))
write(rules_1, file = "data1.csv", sep = ",", col.names = NA)

rules_conf_1 <- sort (rules_1, by="lift", decreasing=TRUE) 
inspect(head(rules_conf_1,5))
write(rules_1, file = "data2.csv", sep = ",", col.names = NA)
# lift순서와 confidence 순서가 같음

rules_2 <- apriori (data=social_transaction, 
                  parameter=list (supp=0.05,
                                  conf = 0.3
                  ),
                  appearance = list (default="lhs",
                                     rhs="만족감=보통")) 
#control = list (verbose=F)) 

rules_conf_2 <- sort (rules_2, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf_2,10))
write(rules, file = "data3.csv", sep = ",", col.names = NA)

rules_conf_2 <- sort (rules_2, by="lift", decreasing=TRUE) 
inspect(head(rules_conf_2,10))
write(rules_2, file = "data4.csv", sep = ",", col.names = NA)
# 만족감이 보통인 경우는 lift를 기준으로 하였을 때 교육정도가 중요함을 알수 있음

rules_3 <- apriori (data=social_transaction, 
                  parameter=list (supp=0.05,
                                  conf = 0.3
                  ),
                  appearance = list (default="lhs",
                                     rhs="만족감=불만족")) 
#control = list (verbose=F)) 

rules_conf_3 <- sort (rules_3, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf_3,5))
write(rules_3, file = "data5.csv", sep = ",", col.names = NA)

rules_conf_3 <- sort (rules_3, by="lift", decreasing=TRUE) 
inspect(head(rules_conf_3,5))
write(rules_3, file = "data6.csv", sep = ",", col.names = NA)
#만족감 불만족의 경우 confidence와  lift가 차이가 없음



tree_data <- data.frame(만족감 = social$만족감,
                           기억.집중.가능.여부
                           = social$기억.집중.가능.여부,
                           자기개발.가능여부
                           = social$자기개발.가능여부,
                           생활수준.변화
                           = social$생활수준.변화,
                           인간관계
                           = social$인간관계,
                           기부경험
                           = social$기부경험,
                           계층의식
                           = social$계층의식,
                           소득.만족도
                           = social$소득.만족도,
                           교육정도
                           = social$교육정도,
                           혼인상태
                           = social$혼인상태
                      )
summary(tree_data)
head(tree_data)
make_tree <- function(data, cp_ = -1, ...){
  indexes = createDataPartition(tree_data$만족감, p = 0.6, list = FALSE)
  train = data[indexes, ]
  test = data[-indexes, ]
  fit <- rpart(만족감~.,
               data = train,
               cp = cp_, method = "class", ...)
  pred = predict(fit, test, type = "class", )
  tmp <- data.frame(test, pred)
  acc <- sum(tmp$만족감 == tmp$pred) / nrow(tmp)
  #print(confusionMatrix(test$NObeyesdad, pred, positive = '4'))
  print(acc * 100)
  return(fit)
}
tree<-make_tree(tree_data, minsplit = 5, minbucket = 3, maxdepth = 15, cp_ = 0.002)
pdf("tree.pdf")
fancyRpartPlot(tree)
dev.off()
windows()

fancyRpartPlot(tree)
dev.off()





