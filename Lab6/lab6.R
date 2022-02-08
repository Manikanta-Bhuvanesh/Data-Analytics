df<-read.csv('Student_Data_cleaned.csv')
df
v<-colnames(df)
v<-v[c(6:12)]

for (i in v) {
  print(paste("mean of ",i,mean(df[,i])))
  print(paste("sd of ",i,sd(df[,i])))
}

df<-read.csv('COVID_country_wise_latest.csv')
head(df)
df1<-read.csv('weatherHistory.csv')
head(df1)

v1<-colnames(df)
v2<-colnames(df1)
for(i in v1){
  if(typeof(df[,i])=="integer"){
    print(paste("mean of ",i,mean(df[,i],na.rm = TRUE)))
    print(paste("sd of ",i,sd(df[,i],na.rm = TRUE)))
    print(paste("min of ",i,min(df[,i],na.rm = TRUE)))
    print(paste("max of ",i,max(df[,i],na.rm = TRUE)))
    print(paste("range of ",i,range(df[,i],na.rm = TRUE)[1],range(df[,i],na.rm = TRUE)[2]))
    print(paste("var of ",i,var(df[,i],na.rm = TRUE)))
  }
}

for(i in v2){
  if(typeof(df1[,i])=="double"|typeof(df1[,i])=="integer"){
    print(paste("mean of ",i,mean(df1[,i],na.rm = TRUE)))
    print(paste("sd of ",i,sd(df1[,i],na.rm = TRUE)))
    print(paste("min of ",i,min(df1[,i],na.rm = TRUE)))
    print(paste("max of ",i,max(df1[,i],na.rm = TRUE)))
    print(paste("range of ",i,range(df1[,i],na.rm = TRUE)[1],range(df1[,i],na.rm = TRUE)[2]))
    print(paste("var of ",i,var(df1[,i],na.rm = TRUE)))
  }
}


merger<-function(a,b,c){
  df$dummy1=a
  df$dummy2=b
  df$dummy3=c
  head(df)
}
a=runif(dim(df)[1], min=1, max=50) 
b=runif(dim(df)[1], min=6, max=40)
c=runif(dim(df)[1], min=9, max=30)
merger(a,b,c)

v3<-colnames(df)
for(i in v3){
  if(typeof(df[,i])=="integer"){
    print(paste("avg of ",i,mean(df[,i],na.rm = TRUE)))
    print(paste("min of ",i,min(df[,i],na.rm = TRUE)))
    print(paste("max of ",i,max(df[,i],na.rm = TRUE)))
  }
}
j=0
summary<-data.frame()
for(i in v3){
  if(typeof(df[,i])=="integer"){
    j=j+1
    summary[j,1]=i
    summary[j,2]=mean(df[,i],na.rm = TRUE)
    summary[j,3]=sd(df[,i],na.rm = TRUE)
    summary[j,4]=min(df[,i],na.rm = TRUE)
    summary[j,5]=max(df[,i],na.rm = TRUE)
  }
}
colnames(summary)<-c("name","mean","standard deviation","min","max")
summary

writer<-function(df){
  write.csv(df,'summary.csv')
}
writer(summary)
df<-read.csv("summary.csv")
df