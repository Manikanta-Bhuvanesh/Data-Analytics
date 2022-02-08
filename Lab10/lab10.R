df <- iris[, -5]
set.seed(240)
kmeans.re <- kmeans(df, centers = 3, nstart = 20)
kmeans.re
kmeans.re$cluster
plot(df[c("Sepal.Length", "Sepal.Width")])
plot(df[c("Sepal.Length", "Sepal.Width")], col = kmeans.re$cluster)
plot(df[c("Sepal.Length", "Sepal.Width")], col = kmeans.re$cluster, main = "K-means with 3 clusters")
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 3) 
y_kmeans <- kmeans.re$cluster
clusplot(df[, c("Sepal.Length", "Sepal.Width")],y_kmeans,lines = 0,shade = TRUE,color = TRUE,labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')


df<-readingSkills[c(1:105), ]
split <- sample.split(df, SplitRatio = 0.8)
split
train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")
logistic_model <- glm(nativeSpeaker ~ age + shoeSize + score, data = train_reg, family = "binomial")
summary(logistic_model)
predict_reg <- predict(logistic_model,test_reg, type = "response")
predict_reg
predict_reg <- ifelse(predict_reg >0.75, 1, 0)
table(test_reg$nativeSpeaker, predict_reg)
missing_classerr <- mean(predict_reg != test_reg$nativeSpeaker)
print(paste('Accuracy =', 1 - missing_classerr))



png(file = "decision_tree.png")
output.tree <- ctree(nativeSpeaker ~ age + shoeSize + score,data = df)
plot(output.tree)
dev.off()

df = iris[,c(1,2,5)]
model <- svm(Species ~ ., data=df)
summary(model)
final_svm <- svm(Species ~ ., data=df, kernel="radial", cost=1,gamma=1)
plot(final_svm , df)


df<-hsb
set.seed(7267166)
trainIndex=createDataPartition(df$prog, p=0.7)$Resample1
train=df[trainIndex, ]
test=df[-trainIndex, ]
print(table(df$prog))
NBclassfier=naiveBayes(prog~science+socst, data=train)
print(NBclassfier)
Print=function(model){
  trainPred=predict(model, newdata = train, type = "class")
  trainTable=table(train$prog, trainPred)
  testPred=predict(NBclassfier, newdata=test, type="class")
  testTable=table(test$prog, testPred)
  trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
Print(NBclassfier)
print(table(train$prog))
newNBclassifier=naive_bayes(prog~ses+science+socst,usekernel=T,data=train)
Print(newNBclassifier)

df = iris
df = df[-c(1,8)]
iris_tr_feat <- df[,1:4]
set.seed(1)
train_pred <- knn(iris_tr_feat, iris_tr_feat, df$Species, k=3)
train_pred[1:10]
accuracy <- mean(train_pred == df$Species)
cat("Training Accuracy: ", accuracy, sep='')

df <- read.csv('winequality.csv')
df <- df[,c(1,9,11,12)]
split <- sample.split(df, SplitRatio = 0.8)
split
train_reg <- subset(df, split == "TRUE")
test_reg <- subset(df, split == "FALSE")
logistic_model <- glm( quality ~ fixed.acidity+pH + alcohol,data = df)
logistic_model
summary(logistic_model)
predict_reg <- predict(logistic_model,test_reg, type = "response")
predict_reg
predict_reg <- ifelse(predict_reg >0.5, 1, 0)
table(test_reg$quality, predict_reg)
missing_classerr <- mean(predict_reg != test_reg$quality)
print(paste('Accuracy =', 1 - missing_classerr))


png(file = "decision_tree1.png")
output.tree <- ctree(quality ~ fixed.acidity + pH + alcohol,data = df)
plot(output.tree)
dev.off()

model <- svm(quality ~., data=df)
summary(model)
final_svm <- svm(quality ~., data=df, kernel="radial", cost=1,gamma=1)
plot(final_svm , df)

split <- sample.split(df, SplitRatio = 0.7)
trainl <- subset(df, split == "TRUE")
testl <- subset(df, split == "FALSE")
train_scale <- scale(trainl[, 1:4])
test_scale <- scale(testl[, 1:4])
set.seed(120)
classifier_cl <- naiveBayes(quality ~ ., data = trainl)
classifier_cl
y_pred <- predict(classifier_cl, newdata = testl)
cm <- table(testl$quality, y_pred)
cm

df1<- df[,1:4]
set.seed(1)
train_pred <- knn(df1, df1, df$quality, k=3)
train_pred[1:10]
accuracy <- mean(train_pred == df$quality)
cat("Training Accuracy: ", accuracy, sep='')