library(arules)
library(ggplot2)
library(arulesViz)
library(shinythemes)
library(pmml)
library(caret)
library(rpart)
library(dplyr)
library(rattle)
library(philentropy)
library(NbClust)
library(fpc)
#install.packages("fpc")
air = read.csv("air2.csv")
sum(is.na(air))
head(air)
summary(air)
air <- na.omit(air)
summary(air)
head(air)
dim(air)
sum(is.na(air))

x=air[,-1]
y=air[,1]
# air_dist<- dist(x, method = "euclidean")
# air_scaled <- scale(x, center = FALSE, apply(x, MARGIN = 2, FUN = max))
# air_scaled
# #air_scaled_dist <- dist(air_scaled, method = "euclidean")
# #air_scaled_dist
# 
# hc <- hclust(dist(x, method = "euclidean", diag = TRUE),
#              method = "centroid")
# 
# hc
# plot(hc)
# 
# set.seed(123)
# 
# nc <- NbClust(x, min.nc=2, max.nc=15, method="kmeans")
# 
# 
# 
# # nstart: 초기 파티션의 갯수
# air.kmeans <- kmeans(x, centers = 3, nstart = 5)
# air.kmeans
# plot(x, pch = air.kmeans$cluster, col = air.kmeans$cluster)
# table(air.kmeans$cluster,y)
# 
# 
# 
# air.kmeans <- kmeans(x, centers = 4, nstart = 5)
# air.kmeans
# plot(x, pch = air.kmeans$cluster, col = air.kmeans$cluster)
# table(air.kmeans$cluster,y)
# 
# 
# air.kmeans <- kmeans(x, centers = 6, nstart = 25)
# air.kmeans
# plot(x, pch = air.kmeans$cluster, col = air.kmeans$cluster)
# table(air.kmeans$cluster,y)
# 
# air.kmeans <- kmeans(x, centers = 12, nstart = 25)
# air.kmeans
# plot(x, pch = air.kmeans$cluster, col = air.kmeans$cluster)
# table(air.kmeans$cluster,y)
# 
# air.kmeans <- kmeans(x, centers = 20, nstart = 25)
# air.kmeans
# plot(x, pch = air.kmeans$cluster, col = air.kmeans$cluster)
# table(air.kmeans$cluster,y)
#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#
x2 <-x[1:100,]
x2
y2 <-y[1:100]
y2


air_dist<- dist(x2, method = "euclidean")
air_scaled <- scale(x2, center = FALSE, apply(x2, MARGIN = 2, FUN = max))
air_scaled
#air_scaled_dist <- dist(air_scaled, method = "euclidean") 
#air_scaled_dist



nc <- NbClust(x2, min.nc=2, max.nc=15, method="kmeans")#kmeans
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), # 군집이 3인경우 criteria가 최대!
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")





wssplot <- function(data, nc=20, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) # K = 1, Total SS 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(x2)
#적절한 군집의 수는 2개 withinss와 nbclust모두 2개에서 가장 큰 변화가 있었음
air.kmeans <- kmeans(x2, centers = 2, nstart = 25)
air.kmeans
plot(x2, pch = air.kmeans$cluster, col = air.kmeans$cluster)
table(air.kmeans$cluster,y2)


hc <- hclust(dist(x2, method = "euclidean", diag = TRUE), 
             method = "centroid")

hc
plot(hc)

head(hc)
clus<-cutree(hc, k = 2)  # k = no of clusters
table(clus)


plot.new() # or frame()

plot(hc)
#군집간 네모박스
rect.hclust(hc, k = 2, border = 3:5)

abline(h = 13, col="red")
# 댄드로그램 라인이 13이상부터 2개의 배타적 군집을 형성하며 군집간의 거리가 가장큼
#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

x3 <-x[1:1000,]
x3
y3 <-y[1:1000]
y3

air_dist<- dist(x3, method = "euclidean")
air_dist
air_scaled <- scale(x3, center = FALSE, apply(x3, MARGIN = 2, FUN = max))
air_scaled
#air_scaled_dist <- dist(air_scaled, method = "euclidean") 
#air_scaled_dist



nc <- NbClust(x3, min.nc=2, max.nc=15, method="kmeans")#kmeans
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), # 군집이 3인경우 criteria가 최대!
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")





wssplot <- function(data, nc=20, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) # K = 1, Total SS 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(x3)
#적절한 군집의 수는 2개 withinss와 nbclust모두 2개에서 가장 큰 변화가 있었음
air.kmeans <- kmeans(air_scaled, centers = 3, nstart = 25)
air.kmeans
plot(air_scaled, pch = air.kmeans$cluster, col = air.kmeans$cluster0)
table(air.kmeans$cluster,y3)


hc <- hclust(dist(x3, method = "euclidean", diag = TRUE), 
             method = "centroid")

hc
plot(hc)

head(hc)
clus<-cutree(hc, k = 3)  # k = no of clusters
table(clus)


plot.new() # or frame()

plot(hc)
#군집간 네모박스
rect.hclust(hc, k = 3, border = 3:5)

abline(h = 24, col="red")
abline(h = 14, col='blue')
# 24가 가장 거리가 긴 분할/12~13 이 5개 분할


# plot데이터로 봤을때 미세먼지와 초미세먼지가 가장 군집화가 잘 되었음
# 두가지로 변수 선택
head(x3)
x0<-x3[,-c(2,3)]
head(x0)
y0<-y3
x0

air_dist<- dist(x0, method = "euclidean")
air_dist
air_scaled <- scale(x0, center = FALSE, apply(x0, MARGIN = 2, FUN = max))
head(air_scaled)
air_scaled_dist <- dist(air_scaled, method = "euclidean")
air_scaled_dist
head(x0)
nc <- NbClust(air_scaled, min.nc=2, max.nc=15, method="kmeans")#kmeans
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), # 군집이 3인경우 criteria가 최대!
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")


air1.kmeans <- kmeans(air_scaled, centers = 3, nstart = 25)
air1.kmeans
plot(air_scaled, pch = air1.kmeans$cluster, col = air1.kmeans$cluster)
table(air1.kmeans$cluster,y0)
aa<-air1.kmeans$centers
air1.kmeans$cluster
plot(air_scaled, col=air1.kmeans$cluster)
points(air1.kmeans$centers,col=1:3, pch=8,cex=1.5)


air2.kmeans <- kmeans(air_scaled, centers = 3, nstart = 25)
air2.kmeans
plot(air_scaled, pch = air2.kmeans$cluster, col = air2.kmeans$cluster)
table(air2.kmeans$cluster,y0)
bb<-air2.kmeans$centers

air3.kmeans <- kmeans(air_scaled, centers = 4, nstart = 25)
air3.kmeans
plot(air_scaled, pch = air3.kmeans$cluster, col = air3.kmeans$cluster)
table(air3.kmeans$cluster,y0)
cc<-air3.kmeans$centers

air4.kmeans <- kmeans(air_scaled, centers = 2, nstart = 25)
air4.kmeans
plot(air_scaled, pch = air4.kmeans$cluster, col = air3.kmeans$cluster)
table(air4.kmeans$cluster,y0)
dd<-air4.kmeans$centers

aa
bb
cc
dd


wssplot <- function(data, nc=20, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) # K = 1, Total SS 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(air_scaled)

distance(x0, method = "euclidean") 
distance(x0, method = "manhattan") 
getDistMethods()

hc <- hclust(dist(air_scaled, method = "euclidean", diag = TRUE), 
             method = "centroid")
plot(hc)

hc <- hclust(dist(air_scaled, method = "euclidean", diag = TRUE), 
             method = "average")
plot(hc)

hc <- hclust(dist(air_scaled, method = "manhattan", diag = TRUE), 
             method = "centroid")
plot(hc)

hc <- hclust(dist(air_scaled, method = "manhattan", diag = TRUE), 
             method = "average")
plot(hc)

air_dist_m<- dist(x0, method = "manhattan")
air_scaled_m <- scale(x0, center = FALSE, apply(x0, MARGIN = 2, FUN = max))
head(air_scaled2)
air_scaled_dist_m <- dist(air_dist_m, method = "manhattan")
air_scaled_dist_m

nc <- NbClust(air_scaled_m, min.nc=2, max.nc=15, method="kmeans")#kmeans
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), # 군집이 3인경우 criteria가 최대!
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#응집도: 클러스터 안에 얼마나 데이터가 뭉쳐져 있는가
air1.kmeans$withinss # 작아야 좋음
# 분리도: 다른 클러스터간 얼마나 떨어져 있는가 
air1.kmeans$betweenss # 커야 좋음
plot(air_scaled_m, col=air1.kmeans$cluster)
points(air1.kmeans$centers,col=1:3, pch=8,cex=1.5)


#eps : 도달 가능한 거리. 이것은 이웃들의 크기를 정의한다.
#MinPts: 거리내에 있어야만 하는 최소 점들의 개수.







ds<- dbscan(x3, eps=0.42, MinPts=3)
head(ds)
table(ds$cluster, y3)
plot(ds, x3)  
x[c(1,2,3,4)]
plot(ds, x[,-c(2,3)])
plot(ds, x[c(1,2,3,4)])

