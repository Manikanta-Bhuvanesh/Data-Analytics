#Datascience
#importingData
df=read.csv("creditcard.csv")
#DataExploration
head(df)
tail(df)
dim(df)
colnames(df)
table(df$Class)
summary(df)
var(df$Amount)
sd(df$Amount)
names(df)
str(df)
class(df)
ggplot(test,aes(x=V1,y=Amount))+geom_point(aes(color=factor(Class)))
ggplot(train,aes(x=V1,y=Amount))+geom_point(aes(color=factor(Class)))
ggplot(test,aes(x=Amount,y=V1))+geom_boxplot(aes(color=factor(Class)))
ggplot(train,aes(x=Amount,y=V1))+geom_boxplot(aes(color=factor(Class)))
#DataManupulation
df$Amount=scale(df$Amount)
data=df[,-c(1)]
head(data)
#DataModelling
install.packages('caTools')
library(caTools)
sample = sample.split(data$Class,SplitRatio=0.80)
train= subset(data,sample==TRUE)
test= subset(data,sample==FALSE)
dim(train)
dim(test)

#DataAnalyst
Logistic_regression=glm(Class~.,test,family=binomial())
summary(Logistic_regression)
plot(Logistic_regression)
library(pROC)
lr.predict <- predict(Logistic_regression,train, probability = TRUE)







#Decision tree
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
dtm <- rpart(Class ~ . , data, method = 'class')
predictval <- predict(dtm, data, type = 'class')
probability <- predict(dtm, data, type = 'prob')
rpart.plot(dtm)
#Artificial Neural Networks
install.packages('neuralnet')
library(neuralnet)
ANN =neuralnet (Class~.,train,linear.output=FALSE)
plot(ANN)
predictANN=compute(ANN,test)
resultANN=predictANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)
#Gradient Boosting
install.packages('gbm')
library(gbm, quietly=TRUE)
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train, test)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train) / (nrow(train) + nrow(test))
  )
)
gbm.iter = gbm.perf(model_gbm, method = "test")
model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
#Plot the gbm model
plot(model_gbm)
# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test, n.trees = gbm.iter)
gbm_auc = roc(test$Class, gbm_test, plot = TRUE, col = "red")
print(gbm_auc)




