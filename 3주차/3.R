#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
data<-read.csv(file="weight-height.csv",header=T)
data<-data[,c(1,3,2)]
colnames(data) = c("Gender", "Height", "Weight")
data_m<-filter(data, Gender=="Male")
data_f<-filter(data, Gender=="Female")
data$Gender<-factor(data$Gender)
#DEA 요악 표
data_dea<-data.frame(avg_Height = c(mean(data[,2]), mean(data_m[,2]),
                                    mean(data_f[,2])),
                 avg_Weight = c(mean(data[,3]), mean(data_m[,3]),
                                mean(data_f[,3])),
                 sd_Height = c(sd(data[,2]), sd(data_m[,2]),
                               sd(data_f[,2])),
                 sd_Weight = c(sd(data[,3]), sd(data_m[,3]),
                               sd(data_f[,3])),
                 max_Height = c(max(data[,2]), max(data_m[,2]),
                                max(data_f[,2])),
                 max_Weight = c(max(data[,3]), max(data_m[,3]),
                                max(data_f[,3])),
                 min_Height = c(min(data[,2]), min(data_m[,2]),
                                min(data_f[,2])),
                 min_Weight = c(min(data[,3]), min(data_m[,3]),
                                min(data_f[,3])))
#키와 몸무게의 상관계수
cor_all=cor(data[,2:3], use="all.obs", method="pearson")
cor_m=cor(data_m[,2:3], use="all.obs", method="pearson")
cor_f=cor(data_f[,2:3], use="all.obs", method="pearson")
                 
rownames(data_dea)=c("All", "Male", "Female")
#DEA boxplot
boxplot(data[,2], data_m[,2], data_f[,2], col=c("yellow","blue","red"),
        main="DEA",xlab="Gender",ylab="Height",
        names=c("All","Male","Female"))
boxplot(data[,3], data_m[,3], data_f[,3], col=c("yellow","blue","red"),
        main="DEA",xlab="Gender",ylab="Weight",
        names=c("All","Male","Female"))

#키 히스토그램
p_h<-ggplot(data, aes(x=Height,fill=Gender, color=Gender))+
  theme(legend.position="top")
p_h+geom_histogram(bins=100,alpha=0.5, position = "identity")
#몸무게 히스토그램
p_w<-ggplot(data, aes(x=Weight,fill=Gender, color=Gender))+
  theme(legend.position="top")
p_w+geom_histogram(bins=100,alpha=0.5, position = "identity")
#키 기반 모델링
model_h<-function(df){
  df_m=filter(df, Gender=="Male")
  df_f=filter(df, Gender=="Female")
  count_all=nrow(df)
  count_x=sum(df_m[,2]<=163)+sum(df_f[,2]>=163)
  return (count_x/count_all)
}

model_h(data) #error rate = 0.0921

#몸무게 기반 모델링
model_w<-function(df){
  df_m=filter(df, Gender=="Male")
  df_f=filter(df, Gender=="Female")
  count_all=nrow(df)
  count_x=sum(df_m[,3]<=67)+sum(df_f[,3]>=67)
  return (count_x/count_all)
}

model_w(data) #error rate = 0.1708

#boundary
line_b<-function(x){
  return (x*0.5+133)
}
#Scatter plot
ggplot(data, aes(x=Weight, y=Height,color=Gender))+
  geom_point(alpha=0.5)+ 
  geom_segment(aes(x=55, y=line_b(55), xend=80, yend=line_b(80)),
                   linetype=1, color="yellow",size=2)+
  labs(title='Scatter plot')

#결정경계 기준 모델링
model_s<-function(df){
  df_m=filter(df, Gender=="Male")
  df_f=filter(df, Gender=="Female")
  count_all=nrow(df)
  count_x=sum(df_m[,2]<=line_b(df_m[,3]))+sum(df_f[,2]>=line_b(df_f[,3]))
  return (count_x/count_all)
}

model_s(data) #error rate = 0.0974

nrow(data_m)#5000명
nrow(data_f)#5000명
#항공대 학생들의 키와 몸무게, 남녀비율에 맞게 데이터를 적절히 scaling한다.
#항공대는 남녀비율이 4대1이므로 데이터에서 남자의 분포를 4배해주어야한다.
#스케일링 후 모델을 수정한뒤 데이터를 적용시킨다

