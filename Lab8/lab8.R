prime<-function(n){
  s=1
  for (j in 2:n) {
    f = 1
    i = 2
    n = j
    while (i <= n / 2) {
      if (n %% i == 0) {
        f = 0
        break
      }
      i = i + 1
    }
    
    if (f == 1) {
      s=s+n
    }
  }
  print(paste("The sum of primes :",s))
}
prime(8)

occur<-function(v,n){
  count=0
  for(i in v){
    if(i==n){
      count=count+1
    }
  }
  print(paste("Number of ocuurences of ", n , " is ",count))
}

v<-sample(1:1000, 100, replace=TRUE)
v1<-c(3,5,6,6,8,7,6,3,2,45)
n=readline()
n=as.integer(n)
occur(v1,n)

fact<-function(n){
  factorial = 1
  if(n == 0) {
    return(1)
  } else {
    for(i in 1:n) {
      factorial = factorial * i
    }
    return(factorial)
  }
}
extract<-function(n){
  while (n !=0) {
    rem=n%%10
    print(fact(rem))
    n=n%/%10
  }
}
n=readline("Enter the number ")
n=as.integer(n)
extract(n)

win<-function(v,n = 0){
  if(length(v)==1){
    print(paste(v," and ",n-1))
  }
  else{
    v=v[c(TRUE, FALSE)]
    n=n+1
    return(win(v,n))
  }
}

v<-c(23, 12, 34, 56, 43, 54, 65, 27, 45, 21)
win(v)

df<-read.csv('complete.csv')
dfap<-subset(df,state=="Andhra Pradesh")
dfap<-dfap[1:10,]
plot(dfap$TotalConfirmedcases,type = "o", col = "red", xlab = "Date", ylab = "Positive cases",main = "Andhran pradesh")
dftn<-subset(df,state=="Telangana")
dftn<-dftn[1:10,]
plot(dftn$TotalConfirmedcases,type = "o", col = "red", xlab = "Date", ylab = "Positive cases",main="Telangana")
dfk<-subset(df,state=="Kerala")
dfk<-dfk[1:10,]
plot(dfk$TotalConfirmedcases,type = "o", col = "red", xlab = "Date", ylab = "Positive cases",main = "Kerla")

dfap<-subset(df,state=="Andhra Pradesh")
dfap<-dfap[1:10,]
plot(dfap$Cured,type = "o", col = "red", xlab = "Date", ylab = "Negative cases",main = "Andhran pradesh")
dftn<-subset(df,state=="Telangana")
dftn<-dftn[1:10,]
plot(dftn$Cured,type = "o", col = "red", xlab = "Date", ylab = "Negative cases",main="Telangana")
dfk<-subset(df,state=="Kerala")
dfk<-dfk[1:10,]
plot(dfk$Cured,type = "o", col = "red", xlab = "Date", ylab = "Negative cases",main = "Kerla")

df1<-df[order(df$TotalConfirmedcases,decreasing = TRUE),]
df1<-df1[1:3,]
plot(df1$Cured,type = "o", col = "red", xlab = "Date", ylab = "Positive cases",main="Top 3 highest positive cases")

df2<-df[order(df$Cured,decreasing = TRUE),]
df2<-df2[1:3,]
plot(df2$Cured,type = "o", col = "red", xlab = "Date", ylab = "Negative cases",main="Top 3 negative cases")
