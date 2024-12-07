##########################################
### Principal Component Analysis (PCA) ###
##########################################

rm(list=ls())

library(ggfortify)
library(e1071)
library(class)
library(psych)
library("caret")

# load wine dataset
wine <- read.csv("data/wine.data", header=FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

wine.X <- wine[,-1]

# PCA with wine dataset
principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)
summary(principal_components)

# plot dataset using 1st & 2nd PCs
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings

# Classifier: kNN
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Model 1: Predict wine type using all attributes
## split train/test
data1 <- wine
mod1.train.indexes <- sample(nrow(data1),0.7*nrow(data1))
mod1.train <- data1[mod1.train.indexes,]
mod1.test <- data1[-mod1.train.indexes,]
mod1.knn <- train(Type~., data=mod1.train, method="knn", metric=metric, trControl=control)

# Model 2: Predict wine type using data projected into first 3 PCs
data2 <- data.frame(principal_components$scores[,1:3], wine$Type)
mod2.train.indexes <- sample(nrow(data2),0.7*nrow(data2))
mod2.train <- data2[mod2.train.indexes,]
mod2.test <- data2[-mod2.train.indexes,]
mod2.knn <- train(wine.Type~., data=mod2.train, method="knn", metric=metric, trControl=control)

# drop variables least contributing to 1st PC & rerun PCA
wine.pc1 <- wine[,-c(3,7,8)]
wine.pc1.X <- wine.pc1[,-1]
principal_components2 <- princomp(wine.pc1.X, cor = TRUE, score = TRUE)
summary(principal_components2)
# plot dataset using 1st & 2nd PCs
autoplot(principal_components2, data = wine.pc1, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# Model 3: Predict wine type using data projected into first 3 PCs
# after dropping least contributing variables (to 1st PC) & re-running PCA
data3 <- data.frame(principal_components2$scores[,1:3], wine$Type)
mod3.train.indexes <- sample(nrow(data3),0.7*nrow(data3))
mod3.train <- data3[mod3.train.indexes,]
mod3.test <- data3[-mod3.train.indexes,]
mod3.knn <- train(wine.Type~., data=mod3.train, method="knn", metric=metric, trControl=control)

# evaluate models
pred1.knn <- predict(mod1.knn,mod1.test[,-1])
pred2.knn <- predict(mod2.knn,mod2.test[,-4])
pred3.knn <- predict(mod3.knn,mod3.test[,-4])

# contingency tables
cm1 = as.matrix(table(Actual = mod1.test$Type, Predicted = pred1.knn))
cm2 = as.matrix(table(Actual = mod2.test$wine.Type, Predicted = pred2.knn))
cm3 = as.matrix(table(Actual = mod3.test$wine.Type, Predicted = pred3.knn))
cm1
cm2
cm3

# metrics - precision, recall, f1
diag1 = diag(cm1)
diag2 = diag(cm2)
diag3 = diag(cm3)
rowsums1 = apply(cm1, 1, sum)
rowsums2 = apply(cm2, 1, sum)
rowsums3 = apply(cm3, 1, sum)
colsums1 = apply(cm1, 2, sum)
colsums2 = apply(cm2, 2, sum)
colsums3 = apply(cm3, 2, sum)

recall1 = diag1 / rowsums1
recall2 = diag2 / rowsums2
recall3 = diag3 / rowsums3
precision1 = diag1 / colsums1
precision2 = diag2 / colsums2
precision3 = diag3 / colsums3
f11 = 2 * precision1 * recall1 / (precision1 + recall1)
f12 = 2 * precision2 * recall2 / (precision2 + recall2)
f13 = 2 * precision3 * recall3 / (precision3 + recall3)

data.frame(recall1, precision1, f11)
data.frame(recall2, precision2, f12)
data.frame(recall3, precision3, f13)

# knn accuracies
plot(mod1.knn$results$k,mod1.knn$results$Accuracy,type="b",ylim=c(0,1))
plot(mod2.knn$results$k,mod2.knn$results$Accuracy,type="b",ylim=c(0,1))
plot(mod3.knn$results$k,mod3.knn$results$Accuracy,type="b",ylim=c(0,1))