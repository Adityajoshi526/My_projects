library(caTools)
library(neuralnet)
library(ggplot2)

maxs <- apply(Boston,MARGIN = 2,max)
mins <- apply(Boston,2,min)

scaled.data <- scale(Boston,center=mins,scale=maxs-mins)
scaled.data <- as.data.frame(scaled.data)

split <- sample.split(scaled.data$medv,SplitRatio = 0.7)
train.data <- subset(scaled.data,split==T)
test.data <- subset(scaled.data,split==F)

n <- names(train.data)
f <- as.formula(paste('medv ~', paste(n[!n %in% 'medv'], collapse = ' + ')))

nn <- neuralnet(f,data=train.data,hidden = c(5,3),linear.output = T)

#print(plot(nn))

predicted.nn.values <- compute(nn,test.data[1:13])
#print(str(predicted.nn.values))

true.predictions <- predicted.nn.values$net.result * (max(Boston$medv)+min(Boston$medv))+min(Boston$medv)

test.data.r <- (test.data$medv)*(max(Boston$medv)+min(Boston$medv))+min(Boston$medv)
mse.nn <- sum((test.data.r - true.predictions)^2)/nrow(test.data)
print(mse.nn)

error.df <- data.frame(test.data.r,true.predictions)
print(head(error.df))
print(ggplot(error.df,aes(test.data.r,true.predictions))+geom_point()+stat_smooth())