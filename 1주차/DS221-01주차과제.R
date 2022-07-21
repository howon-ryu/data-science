
academic_ability_preference  <-  c("o","x","o","x","o","x","o","x","o","x")
native  <-  c("대구","광주","충주","서울","강릉","호남","원주","인천","청주","경북")
age <- c(41,60,20,50,30,30,70,20,45,26)
income <- c("low","low","high","high","high","low","high","low","low","high")
voter <- data.frame(academic_ability_preference,native,age,income)
voter


class(voter)
result<-c(0,0,0,0,0,0,0,0,0,0)
is.vector(result)
class(result)
result
vote_result<-data.frame(result)
vote_result
class(vote_result)
pattern1 <- function(){
  
  a<-voter[intersect(which(voter$age>40),which(voter$income=="high")),]
  a
  x_a<-rownames(a)
  x_a
  vote_result[x_a,] = "보수"
  vote_result
}
pattern1()
vote_result<-pattern1()
vote_result
pattern2 <- function(){
  
  b<-voter[intersect(which(voter$age<40),union(which(voter$native=="광주"),which(voter$native=="호남"))),]
  b
  
  x_b<-rownames(b)
  x_b
  vote_result
  vote_result[x_b,] = "민주"
  vote_result
}
vote_result<-pattern2()

vote_result
pattern3 <- function(){
  
  c<-voter[intersect(which(voter$income=="high"),union(which(voter$native=="경북"),which(voter$native=="대구"))),]
  c
  
  x_c<-rownames(c)
  x_c
  vote_result
  vote_result[x_c,] = "보수"
  vote_result
}
vote_result<-pattern3()
vote_result
pattern4 <- function(){
  
  d<-voter[intersect(which(voter$academic_ability_preference=="x"),which(voter$age<26)),]
  d
  
  x_d<-rownames(d)
  x_d
  vote_result
  vote_result[x_d,] = "민주"
  vote_result
}
vote_result<-pattern4()
vote_result
pattern5 <- function(){
  
  e<-voter[intersect(which(voter$academic_ability_preference=="x"),intersect(which(voter$income=="low"),which(voter$age>50))),]
  e
  
  x_e<-rownames(e)
  x_e
  vote_result
  vote_result[x_e,] = "민주"
  vote_result
}
vote_result<-pattern5()
vote_result
ran<-c("민주","보수")
k<-which(vote_result$result==0)

vote_result[k,]=sample(ran,length(k),replace=TRUE)
vote_result
vote_result<-c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
vote_result
#vote_result[k,]
#getwd()
write.csv(
  vote_result,                 
  file="투표결과.csv",          
  
)

y <- read.csv( # data frame 을 돌려 줌
  file="투표결과.csv",          
  header=FALSE,  
  na.strings="NA", # NA char. string을 NA 로 저장, 기본값
  stringsAsFactors=default.stringsAsFactors() 
  # char.string 을 factor 로 저장, 기본값 
)
y
getwd()
data<-cbind(voter,vote_result)
voter
data
write.csv(
  data,                 
  file="최종데이터.csv",          
  
)
result
model<-lm(vote_result~academic_ability_preference+native+age+income,data)

model
