rm(list=ls())

# Set working directory to "Lab2 Part2" folder

# Exercise 1 - Naive Bayes Analysis w/ Abalone Dataset
# ----------------------------------------------------

# get abalone data, add "age group" column (based on "rings" column) and remove "sex" and "rings" columns
abalone <- read.csv("./data/abalone/abalone.data")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight',
                       'shucked_weight', 'viscera_weight', 'shell_weight', 'rings')
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
abalone$age.group <- as.factor(abalone$age.group)
abalone <- abalone[,-c(1,9)]

# Naive Bayes library
library("e1071")

# classifier1 - length, diameter, height
classifier1 <- naiveBayes(abalone[,c("length", "diameter", "height")], abalone[,8])

# classifier2 - whole_weight, shucked_weight, viscera_weight, shell_weight
classifier2 <- naiveBayes(abalone[,c("whole_weight", "shucked_weight", "viscera_weight",
                                  "shell_weight")], abalone[,8])

# classifier3 - height, whole_weight, shell_weight
classifier3 <- naiveBayes(abalone[,c("height", "whole_weight", "shell_weight")], abalone[,8])

# evaluate classifications
table(predict(classifier1, abalone[,-8]),abalone[,8],dnn=list('predicted','actual'))
table(predict(classifier2, abalone[,-8]),abalone[,8],dnn=list('predicted','actual'))
table(predict(classifier3, abalone[,-8]),abalone[,8],dnn=list('predicted','actual'))

# get class means and standard deviations for height
height.young.mean = classifier1$tables$height["young",1][[1]]
height.adult.mean = classifier1$tables$height["adult",1][[1]]
height.old.mean = classifier1$tables$height["old",1][[1]]

height.young.sd = classifier1$tables$height["young",2][[1]]
height.adult.sd = classifier1$tables$height["adult",2][[1]]
height.old.sd = classifier1$tables$height["old",2][[1]]

# get class means and standard deviations for whole_weight
whole_weight.young.mean = classifier2$tables$whole_weight["young",1][[1]]
whole_weight.adult.mean = classifier2$tables$whole_weight["adult",1][[1]]
whole_weight.old.mean = classifier2$tables$whole_weight["old",1][[1]]

whole_weight.young.sd = classifier2$tables$whole_weight["young",2][[1]]
whole_weight.adult.sd = classifier2$tables$whole_weight["adult",2][[1]]
whole_weight.old.sd = classifier2$tables$whole_weight["old",2][[1]]

# get class means and standard deviations for shell_weight
shell_weight.young.mean = classifier3$tables$shell_weight["young",1][[1]]
shell_weight.adult.mean = classifier3$tables$shell_weight["adult",1][[1]]
shell_weight.old.mean = classifier3$tables$shell_weight["old",1][[1]]

shell_weight.young.sd = classifier3$tables$shell_weight["young",2][[1]]
shell_weight.adult.sd = classifier3$tables$shell_weight["adult",2][[1]]
shell_weight.old.sd = classifier3$tables$shell_weight["old",2][[1]]

# plot normal distributions for height
plot(function(x) dnorm(x, height.young.mean, height.young.sd),
     xlim=c(0,0.3),ylim=c(0,16), col="red",
     main="Height distribution for the 3 different age groups")
curve(dnorm(x, height.adult.mean, height.adult.sd), add=TRUE, col="blue")
curve(dnorm(x, height.old.mean, height.old.sd), add=TRUE, col="green")
     
# plot normal distributions for whole_weight
plot(function(x) dnorm(x, whole_weight.young.mean, whole_weight.young.sd),
     xlim=c(0,3), col="red",
     main="Whole weight distribution for the 3 different age groups")
curve(dnorm(x, whole_weight.adult.mean, whole_weight.adult.sd), add=TRUE, col="blue")
curve(dnorm(x, whole_weight.old.mean, whole_weight.old.sd), add=TRUE, col="green")

# plot normal distributions for shell_weight
plot(function(x) dnorm(x, shell_weight.young.mean, shell_weight.young.sd), col="red",
     main="Shell weight distribution for the 3 different age groups")
curve(dnorm(x, shell_weight.adult.mean, shell_weight.adult.sd), add=TRUE, col="blue")
curve(dnorm(x, shell_weight.old.mean, shell_weight.old.sd), add=TRUE, col="green")

# Exercise 2 - K-Nearest Neighbors w/ Iris Dataset
# -----------------------------------------------

# sample 105 from 150 (70%)
s_iris <- sample(150,105)

# create train & test sets based on sampled indexes
iris.train <- iris[s_iris,]
iris.test <- iris[-s_iris,]

k = 55

# KNN library
library(class)

# train 2 models & predict
KNNpred1 <- knn(train=iris.train[1:2], test=iris.test[1:2], cl=iris.train$Species, k=k)
KNNpred2 <- knn(train=iris.train[3:4], test=iris.test[3:4], cl=iris.train$Species, k=k)

# create contingency tables/confusion matrices
contingency.table1 <- table(KNNpred1, iris.test$Species)
contingency.table2 <- table(KNNpred2, iris.test$Species)
contingency.matrix1 <- as.matrix(contingency.table1)
contingency.matrix2 <- as.matrix(contingency.table2)

contingency.matrix1
contingency.matrix2

sum(diag(contingency.matrix1))/length(iris.test$Species)
sum(diag(contingency.matrix2))/length(iris.test$Species)

accuracy1 <- c()
accuracy2 <- c()
ks <- c(1,10,20,30,40,50,60,70,80,90,100)
for(k in ks) {
  KNNpred1 <- knn(train=iris.train[1:2], test=iris.test[1:2], cl=iris.train$Species, k=k)
  KNNpred2 <- knn(train=iris.train[3:4], test=iris.test[3:4], cl=iris.train$Species, k=k)
  cm1 = as.matrix(table(Actual=KNNpred1, Predicted=iris.test$Species,
                        dnn=list('predicted', 'actual')))
  cm2 = as.matrix(table(Actual=KNNpred2, Predicted=iris.test$Species,
                        dnn=list('predicted', 'actual')))
  accuracy1 <- c(accuracy1, sum(diag(cm1))/length(iris.test$Species))
  accuracy2 <- c(accuracy2, sum(diag(cm2))/length(iris.test$Species))
}

plot(ks, accuracy1, type="b")
plot(ks, accuracy2, type="b")

# Exercise 3 - K-Means w/ Abalone & Iris Datasets
# -----------------------------------------------
library(ggplot2)

# set seed for RNG
set.seed(123)

# run k-means
abalone.km <- kmeans(abalone[,-8],centers=3)
iris.km <- kmeans(iris[,-5], centers=3)
abalone.assigned.clusters <- as.factor(abalone.km$cluster)
iris.assigned.clusters <- as.factor(iris.km$cluster)
ggplot(abalone, aes(x=length, y=diameter, color=abalone.assigned.clusters)) + geom_point()
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=iris.assigned.clusters)) + geom_point()

abalone.wss <- c()
iris.wss <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10)
for(k in ks) {
  abalone.km <- kmeans(abalone[,-8],centers=k)
  abalone.wss <- c(abalone.wss, abalone.km$tot.withinss)
  iris.km <- kmeans(iris[,-5],centers=k)
  iris.wss <- c(iris.wss,iris.km$tot.withinss)
}
plot(ks,abalone.wss,type="b")
plot(ks,iris.wss,type="b")

table(abalone.assigned.clusters, abalone[,8])
table(iris.assigned.clusters, iris[,5])