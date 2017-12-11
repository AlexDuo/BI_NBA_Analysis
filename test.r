newdata <- read.csv("C:/Users/wan_yifei/Desktop/newdata.csv", stringsAsFactors = F, header = 1)
rawdata <- read.csv("C:/Users/wan_yifei/Desktop/NBA_stats.csv", stringsAsFactors = F, header = 1)
newdata2 <- newdata[newdata$salary>2400000 & newdata$salary<14000000, ]
head(newdata2)

linearcor <- cor(newdata[,3:17], method = 'pearson')
nonlinearcor <- cor(newdata[,3:17,], method = 'spearman')

newdata3 <- scale(newdata[,3:17,]) #noramlization
linearcor_nor <- cor(newdata3[,3:17], method = 'pearson')
nonlinearcor_nor <- cor(newdata3[,3:17,], method = 'spearman')

kappa(linearcor_nor, exact=T)

sapply(newdata2[,3:16],function(x)plot(newdata2$salary~x))
