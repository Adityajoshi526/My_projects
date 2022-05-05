library(neuralnet)
library(caTools)

bank.note <- read.csv('bank_note_data.csv')

set.seed(101)
split <- sample.split(bank.note$Class,SplitRatio = 0.7)
train <- subset(bank.note,split==T)
test <- subset(bank.note,split==F)

nn.bank <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data = train,hidden=c(5,3),linear.output = F)

predicted.nn.bank <- compute(nn.bank,test[1:4])
#print(head(predicted.nn.bank$net.result))

predictions <- sapply(predicted.nn.bank$net.result,round)
#print(head(predictions))

print(table(predictions,test$Class))

library(randomForest)

bank.note$Class <- factor(bank.note$Class)

set.seed(101)
split.for <- sample.split(bank.note$Class,SplitRatio = 0.7)
train.for <- subset(bank.note,split.for==T)
test.for <- subset(bank.note,split.for==F)

rf.mod <- randomForest(Class ~ .,data=train.for)
pred.val <- predict(rf.mod,test.for)
print(table(pred.val,test$Class))
