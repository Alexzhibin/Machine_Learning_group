require(nnet)
data(iris)
attach(iris)
##Train data will be 100 
##Test data will be 50
index = sample(x=nrow(iris),size=100)
model <- multinom(Species ~ ., data = iris[index,])
summary(model)
exp(coef(model))
#Predict
head(pred<- fitted(model))
y_p = predict(model, newdata = iris[-index,1:4], "probs")
head(y_p,5)
y_c = predict(model, newdata = iris[-index,1:4], "class")
head(y_c,5)
##Check
table(y_c,iris[-index,5])

