data(iris)
iris.features = iris[,1:4]
result = kmeans(iris.features,3)
result 
result$size
result$cluster

table(iris$Species,result$cluster)

plot(iris[c("Petal.Length","Petal.Width")],col=result$cluster)
plot(iris[c("Petal.Length","Petal.Width")],col=iris$Species)

plot(iris[c("Sepal.Length","Sepal.Width")],col=result$cluster)
plot(iris[c("Sepal.Length","Sepal.Width")],col=iris$Species)
