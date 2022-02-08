df<-read.delim("sample.txt",header = T, sep = " ")
df
nchar(df,allowNA = FALSE, keepNA = NA)
counti=0 
countc=0
sapply(df, class)
for (i in names(df)) { 
  for (j in df[,i]) { 
    if(is.integer(j)==TRUE){ 
      counti=counti+1 } 
    if(is.character(j)==TRUE){ 
      countc=countc+1 
    } 
  } 
  }
counti
countc

my_data<-"ramesh 22"
write.table(my_data, file = "sample.txt", sep= " ",append = T, quote = F, col.names= T, row.names= F)

df<-read.delim("sample.txt",header = F,na.strings = c('', 'NA', '<NA>'))
df

class(iris)
write.xlsx(iris,file="iris.xlsx",col.names=T,row.names=T,sheetName="Sheet1")

df2<-read_csv("COVID_country_wise_latest.csv")
write.xlsx(df2,file="iris.xlsx",col.names=T,row.names=T,sheetName="Sheet2",append=TRUE)

write.table(iris,file = "data.txt",row.names = FALSE) 
scan("data.txt",what = "character")
read_table( "data.txt")

x1 <- iris$Sepal.Length 
x2 <- iris$Sepal.Width 
x3 <- iris$Petal.Length 
x4 <- iris$Petal.Width

v1 <- dnorm(x1, mean = mean(x1), sd = sd(x1)) 
plot(x1, v1)
v2 <- dnorm(x2, mean = mean(x2), sd = sd(x2)) 
plot(x2, v2)
v3 <- dnorm(x3, mean = mean(x3), sd = sd(x3)) 
plot(x3, v3)
v4 <- dnorm(x4, mean = mean(x4), sd = sd(x4)) 
plot(x4, v4)

v1 <- pnorm(x1, mean = mean(x1), sd = sd(x1),lower.tail = FALSE) 
plot(x1, v1)
v2 <- pnorm(x2, mean = mean(x2), sd = sd(x2),lower.tail = FALSE)
plot(x2, v2)
v3 <- pnorm(x3, mean = mean(x3), sd = sd(x3),lower.tail = FALSE) 
plot(x3, v3)
v4 <- pnorm(x4, mean = mean(x4), sd = sd(x4),lower.tail = FALSE) 
plot(x4, v4)

x <- seq(0, 1, by = 0.01)
v1 <- qnorm(x, mean = mean(x1), sd = sd(x1)) 
plot(v1,type = "l")
v2 <- qnorm(x, mean = mean(x2), sd = sd(x2)) 
plot(v2,type = "l")
v3 <- qnorm(x, mean = mean(x3), sd = sd(x3)) 
plot(v3,type = "l")
v4 <- qnorm(x, mean = mean(x4), sd = sd(x4)) 
plot(v4,type = "l")

v1 <- rnorm(x1, mean = mean(x1), sd = sd(x1)) 
hist(v1)
v2 <- rnorm(x2, mean = mean(x2), sd = sd(x2)) 
hist(v2)
v3 <- rnorm(x3, mean = mean(x3), sd = sd(x3)) 
hist(v3)
v4 <- rnorm(x4, mean = mean(x4), sd = sd(x4)) 
hist(v4)