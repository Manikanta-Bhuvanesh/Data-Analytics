df<-read.csv("Diabetes_Updated.csv")
print(df)
dim(df)
colnames(df)
df$age
dim(subset(df,age>40))[1]
subset(df,gender=="female" & age>30)
subset(df,location !="Louisa")
df$diabetic <- df$glyhb >= 7.0
print(df)
subset(df,gender=="female" & age<25 & location == "Buckingham")
mean(df$glyhb,na.rm = TRUE)
subset(df,diabetic==TRUE)
str(df)
summary(df)
