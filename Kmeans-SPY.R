require(quantmod)
require(ggplot2)
Sys.setenv(TZ="GMT")
getSymbols('SPY',from='2000-01-01')
x=data.frame(d=index(Cl(SPY)),return=as.numeric(Delt(Cl(SPY))))
ggplot(x,aes(return))+stat_density(colour="steelblue", size=2, fill=NA)+xlab(label='Daily returns')
head(Delt(Cl(SPY)))

#how many clusters should we have? 
nasa=tail(cbind(Delt(Op(SPY),Hi(SPY)),Delt(Op(SPY),Lo(SPY)),Delt(Op(SPY),Cl(SPY))),-1)
colnames(nasa) = c("del-high","del-low","del-close")

#optimal number of clusters
#Within groups sum of squares
wss = (nrow(nasa)-1)*sum(apply(nasa,2,var))
for (i in 2:15) wss[i] = sum(kmeans(nasa, centers=i)$withinss)
wss=(data.frame(number=1:15,value=as.numeric(wss)))

ggplot(wss,aes(number,value))+geom_point()+
  xlab("Number of Clusters")+ylab("Within groups sum of squares")+geom_smooth()
#The figure above implies, that we should have more than 15 clusters for financial data. Well, for sake of simplicity and education purpose lets use only 5.

####
#Let's start with 5 clusters
###
kmeanObject=kmeans(nasa,5,iter.max=10)
kmeanObject$centers
autocorrelation=head(cbind(kmeanObject$cluster,lag(as.xts(kmeanObject$cluster),-1)),-1)
xtabs(~autocorrelation[,1]+(autocorrelation[,2]))

y=apply(xtabs(~autocorrelation[,1]+(autocorrelation[,2])),1,sum)
x=xtabs(~autocorrelation[,1]+(autocorrelation[,2]))

z=x
for(i in 1:5)
{
  z[i,]=(x[i,]/y[i])
}
round(z,2)


######
#Predict with newdata
#####
library(clue)
result = cl_predict(kmeanObject,newdata=nasa[sample(size=10,x=nrow(nasa))])
result


result1 = cl_predict(kmeanObject,newdata=tail(nasa,1))
result1 
kmeanObject$centers
####Interpretation####

