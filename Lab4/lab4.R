df<-read.csv("Student_Data_Uncleaned.csv")
class(df)
str(df)
summary(df)
names(df)
dim(df)
head(df)
tail(df)

i=1
repeat{
  if(!is.na(df$cat1[i])){
    df$cat1[i]=df$cat1[i]+2
  }
  if(!is.na(df$cat2[i])){
    df$cat2[i]=df$cat2[i]+2
  }
  if(!is.na(df$fat[i])){
    df$fat[i]=df$fat[i]+2
  }
  if(i==dim(df)[1]){
    break
  }
  i=i+1
}
head(df)
df1=df
v=c(1:dim(df)[1])
for(i in v){
  if(is.na(df$cat1[i])){
    temp=readline(prompt = "Enter cat1 mark : ");
    temp=as.integer(temp)
    df$cat1[i]=temp
  }
  if(is.na(df$cat2[i])){
    temp=readline(prompt = "Enter cat2 mark : ");
    temp=as.integer(temp)
    df$cat2[i]=temp
  }
  if(is.na(df$fat[i])){
    temp=readline(prompt = "Enter fat mark : ");
    temp=as.integer(temp)
    df$fat[i]=temp
  }
}


df$da01[is.na(df$da01)]<-mean(df$da01,na.rm = TRUE)
df$lab[is.na(df$lab)]<-mean(df$lab,na.rm = TRUE)
df$quiz1[is.na(df$quiz1)]<-mean(df$quiz1,na.rm = TRUE)
is.na(df$da01)
is.na(df$lab)
is.na(df$quiz1)
      
for (i in v){
  df$gt[i]=(((df$cat1[i]+df$cat2[i]+df$fat[i])/150*40)+((df$da01[i]/20)*15)+((df$quiz1[i]/20)*15)+((df$lab[i]/100)*30))
}
head(df)

for(i in v){
  n=df$gt[i]
  if(n>=90){
    df$grade[i]='S'
  }
  else if(n>=80 & n<90){
    df$grade[i]="A"
  }
  else if(n>=70 & n<80){
    df$grade[i]='C'
  }
  else if(n>=60 & n<70){
    df$grade[i]='D'
  }
  else if(n>=50 & n<60){
    df$grade[i]='E'
  }
  else{
    df$grade[i]='F'
  }
}
head(df)

for(j in v){
  if(df$gt[j]<50){
    df$result[j]="FAIL"
  }
  else{
    df$result[j]="PASS"
  }
}
head(df)

write.csv(df,'Student_Data_cleaned.csv')
df<-read.csv("Student_Data_cleaned.csv")
head(df)


class(df)
dim(df)
summary(df)



hist(df$gt)
boxplot(df$gt)

df$grandtotal<-df$gt
colnames(df)
df$grade<-as.character(df$grade)
typeof(df$grade)

head(df)

df$school<-toupper(df$school)
head(df)
df$school<-tolower(df$school)
head(df)

install.packages("stringr")
library(stringr)

df$regno<-str_trim(df$regno)
head(df)


any(is.na(df))
sum(is.na(df))
sum(is.na(df$cat1))
na.omit(df)
na.omit(df$cat1)
df[is.na(df)]<- 0
df$cat2[is.na(df$cat2)]<-0
df$fat[is.na(df$fat)]<- median(df$fat)

install.packages("tidyr")
library(tidyr)

df3<-unite(df,"reg and school",regno,school)
df3

df3<-separate(df3,"reg and school",c("regno","school"),sep="_")
head(df3)


df<-iris
summary(iris)
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)
md.pattern(iris.mis)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(iris.mis), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))
imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
imputed_Data$imp$Sepal.Width
completeData <- complete(imputed_Data,2)
fit <- with(data = iris.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))
combine <- pool(fit)
summary(combine)
