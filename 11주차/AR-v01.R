# Association Rule mining

# Source: http://r-statistics.co/Association-Mining-With-R.html
# Reference: https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# Reference: http://r-statistics.co/Association-Mining-With-R.html
# Reference: https://blog.exploratory.io/introduction-to-association-rules-market-basket-analysis-in-r-7a0dd900a3e0

# Data: Groceries

# Example
install.packages("arulesViz")
install.packages("shinythemes")
install.packages("pmml")
library(arules)
library(arulesViz)
library(shinythemes)
library(pmml)
data("Groceries")
Groceries
class(Groceries) 
# transaction data
head(Groceries)
inspect(head(Groceries, 3))
# show first 3 transactions

# convert txt file into transacton data 
# Read transation data
# tdata <- read.transactions("transactions_data.txt", sep="\t")
# Convert df to class transations
# tData <- as(myDataFrame, "transactions") # convert to 'transactions' class

# Some utility functions

size(head(Groceries, 3)) 
# number of items in each observation

LIST(head(Groceries, 3)) 
# convert 'transactions' 
# to a list, note the LIST in CAPS

# most frequent items

# The eclat() takes in a transactions object 
# and gives the most frequent items in the data 
# based the support you provide to the supp argument. 
# The maxlen defines the maximum number of items 
# in each itemset of frequent items.
#supp = 미니멈 서포트
frequentItems <- eclat (Groceries, 
                        parameter = list(supp = 0.021, 
                                         maxlen = 10)) 
#지지도 확인
inspect(sort(frequentItems)[1:50])
inspect(head(frequentItems, 3))
# whole milk와 beef가 같이 선택될 확률 0.021
# calculates support for frequent items
Groceries
itemFrequencyPlot(Groceries, 
                  topN=10, 
                  type="absolute", 
                  main="Item Frequency") 
# plot frequent items

# How to get the product recommendation rules?

rules <- apriori (Groceries, 
                  parameter = list(supp = 0.005, 
                                   conf = 0.1)) 
rules
rr <-sort(rules,by='lift')
inspect(rr[1:20])
# 조건에 1582개 만족
# support=최소지지도, confidence=최소신뢰도, minlen=최소물품수(lhs+rhs), maxlen=최대물품수(lhs+rhs), smax=최대지지도
# Min Support as 0.001, confidence as 0.8.
# lsh = 궁금한것 즉 어떤것을 사는 사람이 whole milk를 살까?
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
# 'high-confidence' rules.

inspect(head(rules_conf)) 
# show the support, lift and confidence for all rules

rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
# 'high-lift' rules.

inspect(head(rules_lift)) 
# show the support, lift and confidence for all rules

# How To Control The Number Of Rules in Output ?

rules <-
  apriori(Groceries,
          parameter = list (
            supp = 0.01, # min sup
            conf = 0.3,  # min cof
            maxlen = 6    # max num of elements
          )) 

inspect(head(rules, 3, by = "confidence"))

# maxlen = 6 limits the elements in a rule to 6

# 1 To get ??strong?? rules, increase the value of ??conf?? parameter.
# 2 To get ??longer?? rules, increase ??maxlen??.
# 3 To get 'important' rules, increase 'supp'.

# How To Remove Redundant Rules ?
# Sometimes it is desirable to remove the rules 
# that are subset of larger rules. 
# To do so, use the below code to filter the redundant rules.

length(rules)

rules

subsetRules <- which(rowSums(is.subset(rules, rules, proper = T)) > 1) 
# get subset rules in vector
length(subsetRules)  
#  
rules <- rules[-subsetRules] 
# remove subset rules. 
length(rules)

rules <- apriori(Groceries, 
                 parameter = list (supp = 0.001, 
                                   conf = 0.5, 
                                   maxlen = 3)) 
# maxlen = 3 limits the elements in a rule to 3

# How to Find Rules Related To Given Item/s ?
# To find what factors influenced purchase of product X
rules <- apriori (data=Groceries, 
                  parameter=list (supp=0.001,
                                  conf = 0.08,
                                  maxlen = 4), 
                  appearance = list (default="lhs",
                                     rhs="whole milk"), 
                  control = list (verbose=F)) 
# get rules that lead to buying 'whole milk'

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
# 'high-confidence' rules.
inspect(head(rules_conf))

inspect(head(rules, n = 5, by="confidence"))

# To find out what products were purchased after/along with product X

rules <- apriori (data=Groceries, 
                  parameter=list (supp=0.001,
                                  conf = 0.15,
                                  minlen=2), 
                  appearance = list(default="rhs",
                                    lhs="whole milk"), 
                  control = list (verbose=F)) 
# those who bought 'milk' also bought..

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
# 'high-confidence' rules.

inspect(head(rules_conf))

write(rules, file = "data.csv", sep = ",", col.names = NA)
write.PMML(rules, file = "data.xml")

# Excercise
# example itemset 
basket1 <- c("a", "b",  "c")
basket2 <- c("a", "b")
basket3 <- c("c")
items <- as(list(basket1, basket2, basket3), "itemMatrix")

inspect(items)
is.subset(items)

items[1:2] -> items1
is.subset(items, items1)

rules <- apriori(items)
inspect(rules)
is.subset(rules)
is.subset(rules, proper = T)

subsetRules <- which(rowSums(is.subset(rules, rules, proper = T)) > 1) 
# get subset rules in vector
subsetRules
length(subsetRules)  

rules1 <- rules[-subsetRules]
inspect(rules1)

