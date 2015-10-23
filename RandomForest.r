library(randomForest)
## Classification:
data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
## Look at variable importance:
round(importance(iris.rf), 2)
varImpPlot(iris.rf)

###MDS Plot
MDSplot(iris.rf, iris$Species)
MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))
## Do MDS on 1 - proximity:
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)

## Regression:
data(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)
varImpPlot(ozone.rf)


############
#Parameters
###########
set.seed(71)
iris.rf1 <- randomForest(Species ~ ., data=iris, 
                         ntree=500,
                         mtry=sqrt(ncol(iris)-1), #default
                         replace=TRUE,
                         sampsize = 0.7*nrow(iris),#how many sample should be drawn from the given data. For example, c(20,30), which mean, class1-20,class2-30. But, it replace is true, then all the sample will be use
                         nodesize = 1, #default. The final terminal minimum size should leave only one class
                         maxnodes = NULL, #depth. It need to make iteration
                         importance=TRUE, #Importance of variables
                         proximity=FALSE, #Proximity rows will be calculated
                         norm.votes=TRUE, #If true, then the probability will be return.So, for final prediction
                         #if False, then the raw vote counts are returned.So,for making features
                         do.trace=FALSE)

###Prediction
print(iris.rf1)
test.iris = predict(iris.rf1,iris)
head(test.iris)
test.iris_p = predict(iris.rf1,iris,type="Prob")
head(test.iris_p)

###Variance Plot
varImpPlot(iris.rf1)


