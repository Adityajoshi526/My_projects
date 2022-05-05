library(ISLR)
library(caTools)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)

college.df <- College
#print(ggplot(college.df,aes(Room.Board,Grad.Rate))+geom_point(aes(color = Private),alpha=0.8))
#print(ggplot(college.df,aes(F.Undergrad))+geom_histogram(bins=50,aes(fill=Private),color='black'))
#print(ggplot(college.df,aes(Grad.Rate))+geom_histogram(aes(fill=Private),color='black',bins=50))

subset(college.df,Grad.Rate>100)
college.df['Cazenovia College','Grad.Rate'] <- 100

set.seed(101)
sample <- sample.split(college.df$Private,SplitRatio = 0.7)
train.col <- subset(college.df,sample==T)
test.col <- subset(college.df,sample==F)

trree <- rpart(Private ~ .,method='class',data=train.col)

trree.pred <- predict(trree,test.col)

trree.pred <- as.data.frame(trree.pred)

joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}


trree.pred$Private <- sapply(trree.pred$Yes,joiner)

#print(head(trree.pred))

print(table(trree.pred$Private,test.col$Private))
miss.col <- mean(trree.pred$Private!=test.col$Private)
print(miss.col)

prp(trree)

rf <- randomForest(Private ~.,data=train.col,importance=T)

#print(rf$confusion)
#print(rf$importance)

randFor.pred <- predict(rf,test.col)
print(table(randFor.pred,test.col$Private))

randmiss <- mean(randFor.pred != test.col$Private)
print(randmiss)

