###################
#Run all the data and function first
###################

set.seed(10086)
x = rnorm(50,mean=0,sd=1)
x1 = rnorm(50,mean=3,sd=6)
y = as.factor(sample(rep(0:1,50)))
train = data.frame(x=x,x1=x1,y=y)
set.seed(10000)
test_data = data.frame(test=rnorm(50,mean=0,sd=1))
test_y = sample(rep(0:1,25))
##Transfer your y
transfer_y = function(y_pre){
  for(i in (1:length(y_pre))){
    if(y_pre[i]>=0.5){
      y_pre[i]=1
    }
    else{
      y_pre[i]=0
    }}}

###################
#HomeWork Start
###################
#1.Built the logistic model, and apply to the test data. 


#2.Compare the result with test_y
#Note: Transfer your y first
#For example: y_pre = transfer_y(y_pre)
#hint confusionMatrix function
library(caret)



#3. calculate the F1 score for 1 and 0 


#4. calculate the Auc area and plot the roc 
#Hint:roc function
library(pROC)
