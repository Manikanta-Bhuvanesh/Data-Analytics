df<-iris
ggplot(df, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()

ggplot(df, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = factor(Species)))


mygraph<-ggplot(df, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = factor(Species))) + stat_smooth(method = "lm",col = "#0000FF",se = FALSE,size = 2)
mygraph
mygraph + labs(title = "Iris visualization on Petal",x="Petal Length in Cm",y="Petal Width in Cm")

mybox<-ggplot(df, aes(x = Sepal.Length, y = Sepal.Width))
mybox + geom_boxplot()

mybox + geom_boxplot()+ coord_flip()

mybox + geom_boxplot(outlier.colour = "red",outlier.shape = 4,outlier.size = 3)  + theme_classic()

mybox + geom_boxplot() + stat_summary(fun.y = mean,geom = "point", size = 3,color = "red")  + theme_classic()

ggplot(df, aes(x = Sepal.Length, y = Sepal.Width,color=Species))+ geom_boxplot()

mybox + geom_boxplot() + geom_jitter(shape = 15,color = "steelblue",position = position_jitter(width = 0.21)) +theme_classic()

mybox + geom_boxplot(notch = TRUE)  + theme_classic()


mybar<-ggplot(df,aes(x=factor(Sepal.Length)))
mybar+geom_bar()
ggplot(df,aes(x=factor(Sepal.Width)))+geom_bar()

mybar + geom_bar(fill = "coral")+theme_classic()

mutate(Species = factor(Species, labels = c("setosa", "versicolor","virginica")),Sepal.Width = factor(Sepal.Width))
ggplot(df, aes(x = Sepal.Width, fill = Species)) +geom_bar() +theme_classic()



mutate(Sepal.Length = factor(Sepal.Length)) 
group_by(Species) 
myhist<-ggplot(df, aes(x = Sepal.Length, y = Sepal.Width)) +geom_bar(stat = "identity")

ggplot(df, aes(x = Sepal.Length, y = Sepal.Width,fill=Species)) +geom_bar(stat = "identity")+geom_text(aes(label =Sepal.Width ))+theme_classic() 
