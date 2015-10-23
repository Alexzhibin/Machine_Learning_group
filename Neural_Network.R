library(neuralnet)
AND <- c(rep(0,7),1)
OR <- c(0,rep(1,7))
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
print(net <- neuralnet(AND+OR~Var1+Var2+Var3,  binary.data, hidden=0, 
                       rep=10, err.fct="ce", linear.output=FALSE))
summary(net)
plot(net,rep="best")
#The first round result
net$net.result[1]
##What happen if the layers increased ?
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
print(net <- neuralnet(AND+OR~Var1+Var2+Var3,  binary.data, hidden=4, 
                       rep=10, err.fct="ce", linear.output=FALSE))
summary(net)
plot(net,rep="best")

#Case 2
data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, 
                              err.fct="ce", linear.output=FALSE, likelihood=TRUE))
net.infert$net.result[1]
plot(net.infert,rep="best")
###Hidden layer plus 1
print(net.infert <- neuralnet(case~parity+induced+spontaneous, infert, hidden=3,
                              err.fct="ce", linear.output=FALSE, likelihood=TRUE))
net.infert$net.result[1]
plot(net.infert,rep="best")



#####Prediction
net <- neuralnet(AND+OR~Var1+Var2+Var3,  binary.data, hidden=0, 
                 rep=10, err.fct="ce", linear.output=FALSE)
test.binary.data <- data.frame(expand.grid(c(0,1), c(1,0), c(1,1)))
compute(net,test.binary.data)
result = compute(net,test.binary.data)$net.result
nn.predict1 = data.frame(test.binary.data,result )


####What we do if the data is multi-class data set?
#1.Split the data into two class like the AND and OR examples
#2.Multivariates Models 
################
###1.Split 
nn.train<- cbind(subset(training,select=-c(state_colunm,y)), class.ind(training$y))
names(nn.train)[60]="class_0"
names(nn.train)[61]="class_1"
###2.Model
mod.nn = neuralnet(class_0+class_1~PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + PC43 + PC44 + PC45 +
                     PC46+rowsum.raw+non_zero_raw+mean_raw + sd_raw + max_raw + min_raw + max_diff_raw + median_raw + var_raw + mode_raw +  knn1 + knn2 + knn4 ,
                   data=nn.train, hidden = c(86),        
                   stepmax = 1e+05, rep = 5, 
                   err.fct = "ce", act.fct = "logistic", 
                   linear.output = FALSE)

##231231231

