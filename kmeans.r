library(ISLR)
library(ggplot2)
library(cluster)
print(ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+ geom_point(size=2))

set.seed(101)
iris.cluster <- kmeans(iris[,1:4],centers = 3,nstart = 20)
print(iris.cluster)
print(table(iris.cluster$cluster,iris$Species))
print(clusplot(iris,iris.cluster$cluster,color=T,shade=T,labels=0,lines=0))