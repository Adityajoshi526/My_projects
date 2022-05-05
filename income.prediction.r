library(Amelia)
library(ggplot2)
library(dplyr)
library(caTools)

adult.sal <- as.data.frame(read.csv('adult_sal.csv'))

adult.sal <- select(adult.sal,-X)

#print(table(adult.sal$type_employer))
#DATA CLEANING
#1.Combining employer type:

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }
  return(job)
}

adult.sal$type_employer <- sapply(adult.sal$type_employer,unemp)
#print(table(adult.sal$type_employer))

gov <- function(job){
  job <- as.character(job)
  if (job == 'Local-gov' | job == 'State-gov'){
    return('SL-gov')
  }
  return(job)
}

adult.sal$type_employer <- sapply(adult.sal$type_employer,gov)
#print(table(adult.sal$type_employer))

self.emp <- function(job){
  job <- as.character(job)
  if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('Self-emp')
  }
  return(job)
}
adult.sal$type_employer <- sapply(adult.sal$type_employer,self.emp)
#print(table(adult.sal$type_employer))

#2.Combining marital status:
#print(table(adult.sal$marital))
group.marital <- function(mar){
  mar <- as.character(mar)
  if (mar == 'Seperated' | mar == 'Divorced' | mar == 'Widowed'){
    return('Not-Married')
  }else if (mar == 'Never-married'){
    return(mar)
  }else{
    return('Married')
  }
}

adult.sal$marital <- sapply(adult.sal$marital,group.marital)
#print(table(adult.sal$marital))

#3.Country coloumn

Asia <- c('China','Hong','India','Cambodia','Japan','Laos','Philippines','Vietnam','Taiwan','Thailand')
North.America <- c('Canada','United-States','Puerto-Rico')
Europe <- c('England','France','Germany','Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinidad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
      return('North-America')
  }else if (ctry %in% Latin.and.South.America){
      return('Latin/South-America')
  }else if (ctry %in% Europe){
      return('Europe')
  }else{
      return('Other')
    }
  }

adult.sal$country <- sapply(adult.sal$country,group_country)
#print(table(adult.sal$country))

#Missing data:
adult.sal[adult.sal=='?'] <- NA

adult.sal$type_employer <- factor(adult.sal$type_employer)
adult.sal$marital <- factor(adult.sal$marital)
adult.sal$country <- factor(adult.sal$country)
adult.sal$income <- factor(adult.sal$income)

#print(missmap(adult.sal,y.at=c(1),y.labels=c(''),col=c('yellow','black')))

#Drop missing data:
adult.sal <- na.omit(adult.sal)
adult.sal <- rename(adult.sal,region=country)
#print(missmap(adult.sal,y.at=c(1),y.labels=c(''),col=c('yellow','black')))

pl1 <- ggplot(adult.sal,aes(age))+geom_histogram(aes(fill=income),binwidth = 1,color='black')+theme_bw()
pl2 <- ggplot(adult.sal,aes(hr_per_week))+geom_histogram()+theme_bw()
pl3 <- ggplot(adult.sal,aes(region))+geom_bar(aes(fill=income),color='black')+theme_bw()

#LOGISTIC REGRESSION MODEL:
set.seed(101)
sample_set <- sample.split(adult.sal$income,SplitRatio = 0.7)
train.set <- subset(adult.sal,sample_set==T)
test.set <- subset(adult.sal,sample_set==F)

model <- glm(income ~ .,family=binomial(link='logit'),data=train.set)
#print(summary(model))

#new.step.model <- step(model)
#print(summary(new.step.model))

test.set$predicted.income <- predict(model,newdata=test.set,type = 'response')
tab <- table(test.set$income,test.set$predicted.income>0.5)
print(tab)
acc <- (6374+1420)/(6374+546+875+1420)
print(acc)
recall <- (6374)/(6374+546)
precision <- (6374)/(6374+875)
print(recall)
print(precision)

