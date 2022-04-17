library(ggplot2)
library(data.table)
getwd()=='C:/Users/Aditya/OneDrive/Documents'
df <- fread('Economist_Assignment_Data.csv',drop=1)
pl <- ggplot(df,aes(x=CPI,y=HDI)) + geom_point(shape=1,size=3,aes(color=Region)) + geom_smooth(aes(group=1),method='lm',formula=y~log(x),se=F,color='red')
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan","Afghanistan", "Congo", "Greece", "Argentina", "Brazil","India", "Italy", "China", "South Africa", "Spane","Botswana", "Cape Verde", "Bhutan", "Rwanda", "France","United States", "Germany", "Britain", "Barbados", "Norway", "Japan","New Zealand", "Singapore")

pl2 <- pl + geom_text(aes(label = Country), color='gray20',data=subset(df,Country %in% pointsToLabel),check_overlap=T)
pl3 <- pl2 + theme_bw() + scale_x_continuous(name='Corruption Perceptions Index,2011 (10=least corrupt)') + scale_y_continuous("Human Development Index,2011 (1=Best)") + ggtitle("Corruption and Human Development") 
print(pl3)


