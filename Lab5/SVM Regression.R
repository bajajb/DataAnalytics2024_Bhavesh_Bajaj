rm(list=ls())

library("caret")
library(e1071)

ny.housing <- read.csv("data/NY-House-Dataset.csv")

# SVM regression model
svm.regression.mod <- svm(log10(ny.housing$PRICE)~log10(ny.housing$PROPERTYSQFT),ny.housing)
pred.svm.regression <- predict(svm.regression.mod, ny.housing)
plot(log10(ny.housing$PRICE)~log10(ny.housing$PROPERTYSQFT))
points(log10(ny.housing$PROPERTYSQFT),pred.svm.regression,col="red")

# linear model
lin.mod <- lm(log10(ny.housing$PRICE)~log10(ny.housing$PROPERTYSQFT),ny.housing)
plot(log10(ny.housing$PRICE)~log10(ny.housing$PROPERTYSQFT))
abline(lin.mod)