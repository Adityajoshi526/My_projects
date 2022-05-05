library(rpart)

tree <- rpart(Kyphosis ~.,method='class',data=kyphosis)
printcp(tree)
plot(tree,uniform = T,main='Kyphosis Tree')
text(tree,use.n=T,all=T)
library(rpart.plot)
prp(tree)

library(randomForest)
rf.model <- randomForest(Kyphosis ~.,data=kyphosis)
print(rf.model)