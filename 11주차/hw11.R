library(arules)
library(arulesViz)
library(shinythemes)
library(pmml)
library(rpart)

# 데이터 포함
book <- read.csv("book.csv", header = T)
summary(book)
head(book)
book<-book[,-c(3,4,5,6)]
book
head(book)
book <- na.omit(book)
summary(book)
book.list<-split(book$서명,book$대출일)
book.list
summary(book)

book.trans<-as(book.list,"transactions")
book.trans

summary(book.trans)


frequentItems <- eclat (book.trans, 
                        parameter = list(supp = 0.1, 
                                         maxlen = 10)) 
#지지도 확인

inspect(head(frequentItems, 5))

itemFrequencyPlot(book.trans, 
                  topN=10, 
                  type="absolute", 
                  main="Item Frequency") 

#image(book.trans)

# 기본 rules생성
book.rules<-apriori(book.trans)

summary(book.rules)
#매개변수	  의미	                        디폴트
#support	  규칙의 최소 지지도(발생 비율)	  0.1
#confidence	규칙의 최소 신뢰도	            0.8
#minlen	    규칙에 포함되는 최소 물품 수	   1
#maxlen	    규칙에 포함되는 최대 물품 수	   10
#smax	      규칙의 최대 지지도	             1
inspect(book.rules)

rules_conf <- sort (book.rules, by="confidence", decreasing=TRUE) 
# 'high-confidence' rules.

inspect(head(rules_conf)) 
# show the support, lift and confidence for all rules

rules_lift <- sort (book.rules, by="lift", decreasing=TRUE) 
# 'high-lift' rules.

inspect(head(rules_lift)) 


adj_rules <-
  apriori(book.trans,
          parameter = list (
            supp = 0.1, # min sup 한번이상 대여에 나타남
            conf = 0.9 # 규칙의 선행이 발생하였을때 결과가 발생하는 비율 , 신뢰도
            # 즉 lsh의 책을 대출 받는 사람은 conf의 확률의%로 rhs의 책도 대출 받음을 의미함 
          )) 

inspect(head(adj_rules, length(adj_rules), by = "confidence"))
summary(adj_rules)

# adj_rules <-
#   apriori(book.trans,
#           parameter = list (
#             supp = 0.21, # min sup 두번이상 대여에 나타남
#             conf = 0.2 # 규칙의 선행이 발생하였을때 결과가 발생하는 비율 , 신뢰도
#             # 즉 lsh의 책을 대출 받는 사람은 conf의 확률의%로 rhs의 책도 대출 받음을 의미함 
#           )) 
# 
# inspect(head(adj_rules, length(adj_rules), by = "confidence"))

subsetRules <- which(rowSums(is.subset(adj_rules, adj_rules, proper = T)) > 1) 
subsetRules
length(subsetRules)  


rules <- rules[-subsetRules] 
# remove subset rules. 
length(rules)

rules <- apriori(book.trans, 
                 parameter = list (supp = 0.1, 
                                   conf = 0.9 
                                   )) 
inspect(head(rules, length(rules), by = "confidence"))
rules <- apriori (data=book.trans, 
                  parameter=list (supp=0.1,
                                  conf = 0.9
                                  ), 
                  appearance = list (default="lhs",
                                     rhs="데미안"), 
                  control = list (verbose=F)) 

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))
rules_conf <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_conf))


# c <- rpart(서명~., data=book.trans)
# c

# 'high-confidence' rules.
# inspect(head(rules_conf))
# 
# 
# rules <- apriori (data=book.trans, 
#                   parameter=list (supp=0.1,
#                                   conf = 0.9
#                                   ), 
#                   appearance = list(default="rhs",
#                                     lhs="데미안"), 
#                   control = list (verbose=F)) 
# # those who bought 'milk' also bought..
# 
# rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
# 'high-confidence' rules.



