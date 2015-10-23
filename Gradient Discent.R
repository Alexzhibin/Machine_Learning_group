set.seed(20)
x = rnorm(50,mean=0,sd=1)
y = rnorm(50,mean=10,sd=5)
data = data.frame(x=x,y=y)
mod.fit = lm(y~x,data=data)
mod.fit
summary(mod.fit)

#Part 1
compute_error_for_line_given_points<- function(b,m,points){
  totalError  = 0 
  for(i in 1:nrow(points)){
    x = points[i,1]
    y = points[i,1]
    totalError = totalError + (y-(m*x+b))^2
  }
  return (totalError/nrow(points))
}
compute_error_for_line_given_points(1,0.1,data)

#Part 2

step_gradient<- function(b_current,m_current,points,learningRate){
  b_gradient = 0
  m_gradient = 0
  N = nrow(points)
  best = data.frame(new_b=NA,new_m=NA)
 for(i in 1:nrow(points)){
    x = points[i,1]
    y = points[i,2]
    b_gradient = b_gradient - (2/N)*(y-(m_current*x+b_current))
    m_gradient = m_gradient - (2/N)*x*(y-(m_current*x+b_current))
 }
    new_b = b_current - (learningRate*b_gradient)
    new_m = m_current - (learningRate*m_gradient)
  best[1,1]= new_b 
  best[1,2]= new_m
 return (best)
}
step_gradient(0.5,0.1,points,0.01)
#part 3
gradient_descent_runner<- function(points, starting_b, starting_m, learning_rate, num_iterations){
  best_m_n = data.frame(new_b=rep(NA,num_iterations),new_m=rep(NA,num_iterations),
                        num_iterations=rep(NA,num_iterations),
                        learning_rate=rep(NA,num_iterations),
                        error = rep(NA,num_iterations)
                        )
  b = starting_b
  m = starting_m
    for (i in 1:num_iterations){
      new_b  = step_gradient(b, m, points, learning_rate)[1]
      new_m  = step_gradient(b, m, points, learning_rate)[2]
      best_m_n[i,1]= new_b 
      best_m_n[i,2]= new_m
      best_m_n[i,3]= i
      best_m_n[i,4]=learning_rate
      best_m_n[i,5]=compute_error_for_line_given_points(new_b, new_m, points)
      learning_rate = learning_rate + 0.001
    }
  return(best_m_n)
}

set.seed(20)
points = data
learning_rate = 0.001
initial_b = 0.6 # initial y-intercept guess
initial_m = 0.2 # initial slope guess
num_iterations = 1000

compute_error_for_line_given_points(initial_b, initial_m, points)
best_m_n = gradient_descent_runner(points, initial_b, initial_m, learning_rate, num_iterations)
plot(x=best_m_n$learning_rate, y=best_m_n$error,type="l",ylim=c(1,400),ylab="Error",xlab="learning_rate")
###From the plot, we can see the higher learning rate, the error will be higher, 
#but we cannot set the learning rate too small, because the process will take time
#So, usually we will find out the point with a lowest deriative. In this case, 0.2 looks great
best_parameters = best_m_n[best_m_n$learning_rate==0.002,] #does not work?


