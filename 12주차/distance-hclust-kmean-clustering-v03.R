# Distance
# https://cran.r-project.org/web/packages/philentropy/vignettes/Distances.html
# https://rpubs.com/mrkm_a/ClusteringMethodsWithR

# Example data
# 3 vectors
Kor <- c(area  = 1, export = 8)
Kor
Usa <- c(area = 30,  export = 17)
Chn <- c(area = 20, export = 25)
x <- rbind(Kor, Usa, Chn)
x # data matrix

# Distance matrix
# compute the Euclidean Distance 
# with default parameters
summary(x)
x_dist <- dist(x, method = "euclidean") 
x_dist # distance matrix

# scale before distance
# % to max value: value / column_max
# 최댓값이 1이 되도록 스케일링!(변동성이 클수록 결과에 큰 영향을 미치기에 표준화 작업을 거치는것!)
x_scaled <- scale(x, center = FALSE, apply(x, MARGIN = 2, FUN = max))
x_scaled
x_scaled_dist <- dist(x_scaled, method = "euclidean") 
x_scaled_dist
#install.packages("philentropy")
library(philentropy)
# philentropy::distance
distance(x, method = "euclidean") 

# names of implemented distance/similarity functions
getDistMethods()

# distance in stats package 
hc <- hclust(dist(x, method = "euclidean", diag = TRUE), 
             method = "centroid")
hc
plot(hc)

# cutting tree
# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cutree.html
hc <- hclust(dist(USArrests))

head(hc)
cutree(hc, k = 1:5)  # k = no of clusters
cutree(hc, h = 125)  # h = height

frame()
plot(hc)
# 절단선 위아래가 커지면 좋은 절단임
# 빨간선 그어보기
abline(h = 125, col="red")

plot.new() # or frame()

plot(hc)
#군집간 네모박스
rect.hclust(hc, k = 2, border = 3:5)

# Compare the 2 and 4 grouping:
# 각사례에 대하여 소속 군집이 백터의 형태로 나옴옴
hc
g24 <- cutree(hc, k = c(2,4))
g24
table(grp2 = g24[,"2"], grp4 = g24[,"4"])

#  K-means clustering

k<-data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
print(k)
# View the firt 3 rows of the data
head(df, n = 3)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)

# Determine K

# NbClust
#install.packages("NbClust")
#군집에대한 여러가지 성능 평가가
library(NbClust)
nc <- NbClust(USArrests, min.nc=2, max.nc=15, method="kmeans")#kmeans
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), # 군집이 2인경우 criteria가 최대!
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

# WithinSS - within sum of squares
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) # K = 1, Total SS 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }

wssplot(USArrests)
