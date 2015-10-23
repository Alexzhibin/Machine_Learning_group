#################
#Simulation
#################
set.seed(88)
x= rnorm(50,mean=5,sd=2)
y= sample(0:1,50,replace=T)
dat = data.frame(x=x,y=y)
test =  data.frame(test=rnorm(50,mean=2,sd=1))
plot(x=x,y=y,type="l")
#################
#1.Built the linear regression
#################
lm.fit = lm(y~x,data=dat)
summary(lm.fit)
y.pred = predict(lm.fit,newdata=test)
plot(y.pred,type="l")
#Is that right?
##The program will throw out an Error, because the y is not fall into (0,1)

#################
#2.Make the Y fall into(0,1)
#################
#The linear model is y = 0.01782*x + 0.55235
h= function(x){
z= 0.01782*x+0.55235
g= 1/(1+exp(-z))
return (g)
}
#Let's test it 
pred = data.frame(pred.y=rep(NA,50),x=rep(NA,50))
for (i in (1:50)){
  pred$x[i] = x[i]
  pred$pred.y[i] = h(x[i]) 
}
plot(x=pred$x,y=pred$pred.y,type="l")
##What is the meaning of pred.y here?

##3.Cost function
plot(pred$pred.y,type="l",ylab="pred.y",xlab="x")
plot(-log(pred$pred.y),type="l",ylab="Cost_Error",xlab="x")
#Also, I find we just simple use 1- pred.y, which could represent the cost as well
plot(1-pred$pred.y,type="l",ylab="1-pred.y",xlab="x")

######
###Find the best parameter: Gradient Descent
######

######
#4.Logistic Regression Function 
######
mod.fit = glm(y~x,family="binomial"(link = "logit"),data=dat)
summary(mod.fit)
pred.y1 = predict(mod.fit,newdata=test)
plot(pred.y1,type="l")
#What is the difference between two plots?

###How to predict which is 1 which 0?
######>0.5?###
pred.y2 = pred.y1
for (i in (1:length(pred.y1))){
if (pred.y1[i]>0.5){
  pred.y2[i] = 1
}
else{
  pred.y2[i] = 0
}
}
table(pred.y2)
############
#5.F1 Score
###########
library(caret)
confusionMatrix(pred.y2,y)
###What is the accuracy?How it come?
(8+19)/50
###Would it suitable? Cancer example
table(y)
32/50
##So, we need F1 score. And we need to predict accuracy of 0
p = 8/(8+13)
r = 10/(8+10)
F1 = 2*(p*r/(p+r))
F1

####In the Dignose lecture, I will talk about how to find out the best threshold using F1