library(arules)
library(arulesViz)
library(shinythemes)
library(pmml)
library(rpart)


health <- read.csv("health.csv", header = T)
summary(health)
head(health)
health_trans <- as(health, "transactions")
frequentItems <- eclat (health_trans, 
                        parameter = list(supp = 0.021, 
                                         maxlen = 10)) 
inspect(sort(frequentItems)[1:50])
inspect(head(frequentItems, 3))
itemFrequencyPlot(health_trans, 
                  topN=10, 
                  type="absolute", 
                  main="Item Frequency") 
head(health)
rules <- apriori (data=health, 
                  parameter=list (supp=0.001,
                                  conf = 0.08,
                                  maxlen = 4), 
                  appearance = list (default="lhs",
                                     rhs="Drink=high_risk"), 
                  control = list (verbose=F)) 
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 

inspect(head(rules_conf))
write(rules, file = "data.csv", sep = ",", col.names = NA)
write.PMML(rules, file = "data.xml")
