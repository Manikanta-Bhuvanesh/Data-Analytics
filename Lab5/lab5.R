m = matrix(NA, nrow = 5, ncol = 5)
m
for (i in 1:5) {
  for (j in 1:5) {
    m[i, j] <- abs(i - j)
  }
}
m

x=100
y=50
i=1
repeat
{
  x=x-i
  y=y+i
  if (x<y)
    break
}
i
x
y


df<-read.csv("River_Dataset.csv")
df<-data.frame(df)
v=c(1:dim(df)[1])
x=0
y=0
for (i in v) {
    if(df$length[i]>800){
      y=y+1
    }
    else if(df$length[i]<400){
      x=x+1
    }
    else{
    }
}
print(paste("Number of short rivers ",y))
print(paste("Number of long rivers ",x))

df<-read.csv("bmi_data.csv")
dim(subset(df,Sex=="Male" & BMI<18.5))[1]
dim(subset(df,Sex=="Male" & BMI<29.9 & BMI>25))[1]


df<-read.csv("mycsv.csv")
df
mean1<-function(a){
  sum=0
  count=0
  for (i in a){
    sum=sum+i
    count=count+1
  }
  return (sum/count)
}
p=mean1(df$numbers1)
p

sd<-function(a){
  sum=0
  count=-1
  p=mean1(a)
  for (i in a){
    sum=sum+((i-p)*(i-p))
    count=count+1
  }
  return(sqrt(sum/count))
}

l=sd(df$numbers2)
l
