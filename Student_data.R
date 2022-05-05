library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
df <- read.csv('student-mat.csv',sep = ';')
num.col <- sapply(df,is.numeric)  
cor.data <- cor(df[,num.col])
pl <- ggplot(df,aes(x=G3))+geom_histogram(bins=20,alpha=0.5,fill='blue')

set.seed(101)

sample <- sample.split(df$G3, SplitRatio = 0.7)

train <- subset(df,sample == T)

test <- subset(df,sample == F)

model <- lm(G3 ~. ,train)
res <- residuals(model)
res <- as.data.frame(res)
#plot(model)
G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)

to_zero <- function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
results$predicted <- sapply(results$predicted,to_zero)

mse <- mean((results$actual - results$predicted)^2)
print(mse)
print(mse^0.5)

SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$G3)-results$actual)^2)

R2 <- 1 - SSE/SST
print(R2)
print(summary(model))