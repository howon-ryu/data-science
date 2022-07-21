y<-matrix(c(1,-1,-2,-1,1,1,1,1,1),nrow = 3, ncol = 3,byrow=T)
y
y[1,]

ncol(y)
# 1-a
sum_posneg<-function(mat1,dir){
  
  if (dir==1)
  {
    
    plus_newcol<-NULL
    minus_newcol<-NULL
    for (i in 1:nrow(mat1)){
      
      a<-0
      for(j in 1:ncol(mat1)){
        
        if(mat1[i,j]<0){
          
          #print(mat1[i,j])
          #print(a)
          a = a+mat1[i,j]
          #print(a)
          
        }
        
        
      }
      a
      minus_newcol<-c(minus_newcol,a)
      minus_newcol
      
    }
    for (i in 1:nrow(mat1)){
      
      a<-0
      for(j in 1:ncol(mat1)){
        
        if(mat1[i,j]>0){
          
          #print(mat1[i,j])
          #print(a)
          a= a+mat1[i,j]
          #print(a)
          
        }
        
        
      }
      plus_newcol<-c(plus_newcol,a)
      
    }
    plus_newcol
    minus_newcol
    newcol<-NULL
    newcol
    newcol<-cbind(newcol,plus_newcol)
    newcol
    newcol<-cbind(newcol,minus_newcol)
    print(newcol)
    
  }
  if (dir==2)
  {
    plus_newrow<-NULL
    minus_newrow<-NULL
    for (i in 1:nrow(mat1)){
      
      a<-0
      for(j in 1:ncol(mat1)){
        
        if(mat1[j,i]<0){
          
          #print(mat1[i,j])
          #print(a)
          a= a+mat1[j,i]
          #print(a)
          
        }
        
        
      }
      minus_newrow<-c(minus_newrow,a)
      
    }
    for (i in 1:nrow(mat1)){
      
      a<-0
      for(j in 1:ncol(mat1)){
        
        if(mat1[j,i]>0){
          
          #print(mat1[i,j])
          #print(a)
          a= a+mat1[j,i]
          #print(a)
          
        }
        
        
      }
      plus_newrow<-c(plus_newrow,a)
      
      
    }
    newrow<-NULL
    newrow<-rbind(newrow,plus_newrow)
    newrow<-rbind(newrow,minus_newrow)
    print(newrow)
    
  }


}
sum_posneg(y,1)
sum_posneg(y,2)
#1-b
y<-matrix(c(1,-1,-2,-1,1,1,1,1,1),nrow = 3, ncol = 3,byrow=T)
y

plus_y<-y
minus_y<-y
plus_y[plus_y<0]=0
plus_y
minus_y[minus_y>0]=0
minus_y
plus_row<-apply(plus_y,1,sum)
minus_row<-apply(minus_y,1,sum)
plus_col<-apply(plus_y,2,sum)
minus_col<-apply(minus_y,2,sum)
newrow<-NULL
newcol<-NULL
newrow<-cbind(newrow,plus_row)
newrow<-cbind(newrow,minus_row)
newcol<-rbind(newcol,plus_col)
newcol<-rbind(newcol,minus_col)
newrow
newcol
#2-a
z<-matrix(c("1","-1","-2","-1","1","1","1","1","1"),nrow = 3, ncol = 3,byrow=T)
z
for (i in 1:nrow(z)){
  
  for(j in 1:ncol(z)){
    
    k<-as.numeric(z[i,j])
    is.vector(k)
    print(z[i,j])
    print(k)
    ve<-c(ve,k)
  }
  
  print(ve)
  
}
z
k
ve
typeof(ve)
is.vector(ve)

#2-b
z<-matrix(c("1","-1","-2","-1","1","1","1","1","1"),nrow = 3, ncol = 3,byrow=T)
z
z<-as.vector(z)
zzz<-sapply(zz<-as.numeric(z),function(x)x)
zzz
is.vector(zzz)
