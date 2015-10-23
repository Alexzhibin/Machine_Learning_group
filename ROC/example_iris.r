library(caret)
library(pROC)
data(iris)

# Make this two class classification.
iris <- iris[iris$Species == "virginica" | iris$Species == "versicolor", ]
iris$Species <- factor(iris$Species)  # setosa should be removed from factor
levels(iris$Species)


# Get train and test datasets.
# I'm making train set very small intentionally so that we get some wrong classification
 # results. This will give ROC graph some curves.
samples <- sample(NROW(iris), NROW(iris) * .5)
data.train <- iris[samples, ]
data.test <- iris[-samples, ]

# Random forest
forest.model <- train(Species ~., data.train)
forest.model


# Prediction.
result.predicted.prob <- predict(forest.model, data.test, type="prob")

# Draw ROC curve.
result.roc <- roc(data.test$Species, result.predicted.prob$versicolor)
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))


# Make prediction using the best top-left cutoff.
result.predicted.label <- factor(ifelse(result.predicted.prob[,1] > result.coords[1], "versicolor", "virginica"))

xtabs(~  result.predicted.label + data.test$Species)
