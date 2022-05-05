library(ggplot2)
df1 <- read.csv('winequality-red.csv',sep=';')
df2 <- read.csv('winequality-white.csv',sep=';')

df1$label <- sapply(df1$pH,function(x)('red'))
df2$label <- sapply(df2$pH,function(x)('white'))

wine <- rbind(df1,df2)

print(ggplot(wine,aes(residual.sugar))+geom_histogram(aes(fill=label),color='black',bins=50,alpha=0.5)+scale_fill_manual(values = c('red','green'))+theme_bw())
print(ggplot(wine,aes(citric.acid))+geom_histogram(aes(fill=label),color='black',bins=50,alpha=0.5)+scale_fill_manual(values = c('red','green'))+theme_bw())
print(ggplot(wine,aes(alcohol))+geom_histogram(aes(fill=label),color='black',bins=50,alpha=0.5)+scale_fill_manual(values = c('red','green'))+theme_bw())
print(ggplot(wine,aes(citric.acid,residual.sugar))+geom_point(aes(color=label),alpha=0.2)+scale_fill_manual(c('red','green'))+theme_bw())
print(ggplot(wine,aes(volatile.acidity,residual.sugar))+geom_point(aes(color=label),alpha=0.2)+scale_fill_manual(c('red','green'))+theme_bw())

clus.data <- wine[,1:12]

mod.wine <- kmeans(clus.data,2)
print(mod.wine$centers)

print(table(wine$label,mod.wine$cluster))
