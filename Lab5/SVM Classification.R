rm(list=ls())

library("caret")
library(e1071)

wine <- read.csv("data/wine.data",header=FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
wine$Type <- as.factor(wine$Type)

# subset data
wine.subset <- wine[,1:7]

## train SVM model - linear kernel
svm.mod1 <- svm(Type ~ ., data = wine.subset, kernel = 'linear')

svm.pred1 <- predict(svm.mod1, wine.subset[,-1])

svm.cm1 = as.matrix(table(Actual = wine.subset$Type, Predicted = svm.pred1))

diag = diag(svm.cm1) # number of correctly classified instances per class 
rowsums = apply(svm.cm1, 1, sum) # number of instances per class
colsums = apply(svm.cm1, 2, sum) # number of predictions per class

svm.recall1 = diag / rowsums 
svm.precision1 = diag / colsums
svm.f11 = 2 * svm.precision1 * svm.recall1 / (svm.precision1 + svm.recall1) 

## Tuned SVM - polynomial
#set.seed(99)
#tuned.svm <- tune.svm(Type~., data = wine.subset, kernel = 'polynomial',gamma = seq(0,1,0.1), cost = seq(0.1,1,0.1))
## train SVM model - polynomial kernel (using gamma & cost from tuned SVM)
svm.mod2 <- svm(Type ~ ., data = wine.subset, kernel = 'polynomial', gamma = 0.7, cost = 0.2)

svm.pred2 <- predict(svm.mod2, wine.subset[,-1])

svm.cm2 = as.matrix(table(Actual = wine.subset$Type, Predicted = svm.pred2))

diag = diag(svm.cm2) # number of correctly classified instances per class 
rowsums = apply(svm.cm2, 1, sum) # number of instances per class
colsums = apply(svm.cm2, 2, sum) # number of predictions per class

svm.recall2 = diag / rowsums 
svm.precision2 = diag / colsums
svm.f12 = 2 * svm.precision2 * svm.recall2 / (svm.precision2 + svm.recall2) 

# kNN classifier
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
knn.mod <- train(Type~., data=wine.subset, method="knn", metric=metric, trControl=control)
knn.pred <- predict(knn.mod,wine.subset[,-1])

knn.cm = as.matrix(table(Actual = wine.subset$Type, Predicted = knn.pred))

diag = diag(knn.cm) # number of correctly classified instances per class 
rowsums = apply(knn.cm, 1, sum) # number of instances per class
colsums = apply(knn.cm, 2, sum) # number of predictions per class

knn.recall = diag / rowsums 
knn.precision = diag / colsums
knn.f1 = 2 * knn.precision * knn.recall / (knn.precision + knn.recall)

# compare all 3 models
# SVM - linear kernel
data.frame(svm.precision1, svm.recall1, svm.f11)
# SVM - polynomial kernel - optimized gamma & cost using tuned SVM
data.frame(svm.precision2, svm.recall2, svm.f12)
# kNN
data.frame(knn.precision, knn.recall, knn.f1)