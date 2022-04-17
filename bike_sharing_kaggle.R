library(dplyr)
library(ggplot2)
library(ggthemes)
library(caTools)
library(plotly)

bike <- read.csv('bikeshare.csv')
pl <- ggplot(bike,aes(x=temp,y=count))+geom_point(aes(color=temp),alpha=0.3)+theme_bw()

bike$datetime <- as.POSIXct(bike$datetime)
pl2 <- ggplot(bike,aes(x=datetime,y=count))+geom_point(aes(color=temp),alpha=0.5) + scale_color_continuous(low='#55D8CE',high='#FF6E2E') + theme_bw()

#cor(bike,[,c(temp,count)])

pl3 <- ggplot(bike,aes(x=factor(season),y=count))+geom_boxplot(aes(color=factor(season))) + theme_bw()

bike$hour <- sapply(bike$datetime,function(x){format(x,'%H')})
bike$hour <- sapply(bike$hour,as.numeric)

bike2 <- subset(bike,workingday==1)
pl4 <- ggplot(bike2,aes(x=hour,y=count))+geom_point(aes(color=temp),position=position_jitter(w=1,h=0),alpha=0.5)+scale_color_gradientn(colors=c('green','red','blue'))

bike3 <- subset(bike,workingday==0)
pl5 <- ggplot(bike3,aes(x=hour,y=count))+geom_point(aes(color=temp),position=position_jitter(w=1,h=0),alpha=0.5)+scale_color_gradientn(colors=c('green','red','blue'))

#set.seed(101)

#sample <- sample.split(bike$count,SplitRatio = 0.7)

#train <- subset(bike,sample==T)

#test <- subset(bike,sample==F)

#model.reg <- lm(count ~. ,train)
#res1 <- as.data.frame(residuals(model.reg))
#plot(model.reg)
#count.predictions <- predict(model.reg,test)
#result <- cbind(count.predictions,bike$count)
#colnames(result) <- c('Predicted','Actual')
#result <- as.data.frame(result)

#mse1 <- mean((result$actual - result$predicted)^2)
#mseroot <- mse^0.5

#sse1 <- sum((result$actual - result$predicted)^2)
#sst1 <- sum(((mean(bike$count)-result$actual)^2))

#r2 <- 1 - sse1/sst1
#print(r2)

#temp.model <- lm(count ~ temp,bike)
#print(summary(temp.model))

#temp.test <- data.frame(temp=c(25))
#test <- predict(temp.model,temp.test)
#print(test)

model.count <- lm(count ~ . - casual - registered - datetime - atemp ,bike)
print(summary(model.count))
