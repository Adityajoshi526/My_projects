library(ISLR)
library(ggplot2)
library(class)
library(caTools)
stand.feat <- scale(iris[1:4])

final.data <- cbind(stand.feat,iris[5])

set.seed(101)
samp <- sample.split(final.data$Species,SplitRatio = 0.7)
train <- subset(final.data,samp==T)
test <- subset(final.data,samp==F)

predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)

missed <- mean(test$Species != predicted.species)

predicted.species <- NULL
error_rate <- NULL

for (i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error_rate[i] <- mean(test$Species != predicted.species)
}
k.value <- 1:10
error_df <- data.frame(error_rate,k.value)

print(ggplot(error_df,aes(k.value,error_rate)) + geom_point() + geom_line(lty='dotted',color='red',size=1)  + theme_bw())
#for k = 5
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=5)
missed <- mean(test$Species != predicted.species)
print(missed)

