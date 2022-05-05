library(ISLR)

mod <- svm(Species ~ .,data=iris)
#print(summary(mod))

pred.values <- predict(mod,iris[1:4])
#print(table(pred.values,iris[,5]))

tune.results <- tune(svm,train.x=iris[1:4],train.y=iris[,5],kernel='radial',ranges = list(cost=c(0.1:10),gamma=c(0.1:5)))
#print(summary(tune.results))

tuned.svm <- svm(Species ~.,data=iris,kernel='radial',cost=1.1,gamma=0.1)
print(summary(tuned.svm))