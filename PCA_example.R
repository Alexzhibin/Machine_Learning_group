train = read.csv("train_words_sample.csv")
require(caret)
training = preProcess(train[,-1], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PCA = predict(training, train[,-1])
# Retained PCs
head(PCA)

####################
##Method 2
####################
# log transform 

train.y <- train[, 1]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
train.pca <- prcomp(train[,-1],center = TRUE,scale. = TRUE) 
# print method
print(train.pca)

# plot method
plot(train.pca, type = "l")

# summary method
summary(train.pca)
head(summary(train.pca)$center)
summary(train.pca)$importance
# Predict PCs
predict(train.pca, newdata=tail(train[,-1], 2))

library(devtools)
#install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(train.pca, 
              groups = train.y, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)




############
#Models
###########
library(randomForest)
# training = train[1:70,]
# testing= train[71:100,]
# testing_x = testing[,-1]
# testing_y = testing[,1]
# #1:03-2:00
# words_model <- randomForest(sentiment ~ ., data=training,
#                             ntree=20,
#                             sampsize = 0.8*nrow(training),
#                             importance=FALSE,
#                             proximity=FALSE)
# pred1 = predict(words_model,newdata=testing_x)
# confusionMatrix(testing_y,pred1)
####Accuracy:0.3667

###PCA
data_pca = cbind(sentiment=train[,1],PCA)
data_pca_training = data_pca[1:70,]
data_pca_testing= data_pca[71:100,]
data_pca_testing_x = data_pca_testing[,2:3]
data_pca_testing_y = data_pca_testing[,1]

words_model1 <- randomForest(sentiment ~ ., data=data_pca_training,
                            ntree=20,
                            sampsize = 0.8*nrow(data_pca_training),
                            importance=FALSE,
                            proximity=FALSE)
pred2 = predict(words_model1,newdata=data_pca_testing_x )
confusionMatrix(data_pca_testing_y,pred2)
## Accuracy 0.5333