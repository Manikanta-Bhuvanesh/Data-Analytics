df<-read.csv("bmi_data.csv")
df

sum =0
count=0
for (i in df$Age) {
  sum = sum+i
  count=count+1
}
sum/count

sum =0
count=0
for (i in na.omit(df$Height)) {
    sum = sum+i
    count=count+1
}
sum/count

sum =0
count=0
for (i in na.omit(df$Weight)) {
  sum = sum+i
  count=count+1
}
sum/count

sum =0
count=0
for (i in na.omit(df$BMI)) {
  sum = sum+i
  count=count+1
}
sum/count




height = c(23,34,54,34,67,98,56,76,23,56,87,56,45,34,32,54,76,98,89,90)
weight = c(23,54,65,76,87,78,56,45,34,23,24,35,36,67,56,87,56,47,76,73)
m=cbind(height,weight)
m

df1= as.data.frame(m)
df1
df1$weight[12]


dim(df)
v=c(1:dim(df)[1])
v
for (i in v) {
  if(!is.na(df$BMI[i])){
    if(df$BMI[i]>=19){
      df$bmifactor[i]="High"
    }
    else if(df$BMI[i]< 19){
      df$bmifactor[i]="Low"
    }
    else{
      df$bmifactor[i]="NA"
    }
  }
  
}
print(df)

flag=0
for (i in v) {
  n=df$BMI[i]
  if(!is.na(n)){
    if((n<16)){
      flag=1
    }
    else if(n>=16 & n<17){
      flag=2
    }
    else if(n>=17 & n<18.5){
      flag=3
    }
    else if(n>=18.5 & n<25){
      flag=4
    }
    else if(n>=25 & n<30){
      flag=5
    }
    else if(n>=30 & n<35){
      flag=6
    }
    else if(n>=35 & n<40){
      flag=7
    }
    else{
      flag=8
    }
    x<- switch (flag,
            "Severe Thinness",
            "Moderate Thinness",
            "Mild Thinness",
            "Normal",
            "Overweight",
            "Obese Class I",
            "Obese Class II",
            "Obese Class III"
    )
  }
  df$bmifactor1[i]=x
}
print(df)