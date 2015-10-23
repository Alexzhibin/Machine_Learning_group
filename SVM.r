library(e1071)
data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y,probability=TRUE) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, probability=TRUE)
attr(pred, "probabilities")[1:4,]
y[1:4]
# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## try regression mode on two dimensions

# create data
x1 <- seq(0.1, 5, by = 0.05)
y1 <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x1, y1)
new <- predict(m, x1)

# visualize
plot(x1, y1)
points(x1, log(x1), col = 2)
points(x1, new, col = 4)

## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- svm(X, gamma = 0.1)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

# weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)



#######################
#Parameter
######################
#Change the parameters--Cost
model <- svm(Species ~ ., data = iris,cost=10)
# test with train data
pred <- predict(model, x)
pred <- fitted(model)
# Check accuracy:
table(pred, y)

#Change the parameters--gamma 
default = 1/ncol(iris[,-5])
model <- svm(Species ~ ., data = iris,gamma=5)
# test with train data
pred <- predict(model, x)
pred <- fitted(model)
# Check accuracy:
table(pred, y)
