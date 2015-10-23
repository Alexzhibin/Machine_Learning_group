###Normal Equation
set.seed(20)
x = rnorm(50,mean=0,sd=1)
y = rnorm(50,mean=10,sd=5)
x1 = data.frame(x0=1,x=x)
parameters = rev(t(x1)*x1)*t(x1)*y
parameters = data.frame(b=parameters$x,m=parameters$x0,y=y,x=x)

compute_error_Noraml_Equation<- function(points){
  totalError  = data.frame(error=NA,b=NA,m=NA)
  for(i in 1:nrow(points)){
    m = points[i,"m"]
    b = points[i,"b"]
    y = points[i,"y"]
    x = points[i,"x"]
    totalError[i,"error"] = (y-(m*x+b))^2
    totalError[i,"b"] = b
    totalError[i,"m"] = m
  }
  return (totalError)
}
new_para = compute_error_Noraml_Equation(parameters)
plot(new_para$error,type="l",ylab="error",xlab="index")
para1 = new_para[which.min(new_para$error),]
plot(x=x,y=y)
compute_error_for_line_given_points(para1[,"b"], para1[,"m"], points)
##Standard Error error = 2.50622
