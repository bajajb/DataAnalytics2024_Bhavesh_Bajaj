# 3. Classification (kNN)

rm(list=ls())

# Set working directory to "Lab3" folder
epi <- read.csv("data/epi2024results_DA_F24_lab03.csv")

# kNN library
library(class)

epi.subset <- epi[5:10]

# replacing missing values with 0
epi.subset[is.na(epi.subset)] <- 0

epi.subset.region1 <- subset(epi.subset, region==c("Global West","Asia-Pacific","Latin America & Caribbean"))
epi.subset.region2 <- subset(epi.subset, region==c("Eastern Europe","Sub-Saharan Africa","Greater Middle East"))

# sample 125 from 180 (~70%)

s_epi.region1 <- sample(nrow(epi.subset.region1),as.integer(nrow(epi.subset.region1)*0.7))
s_epi.region2 <- sample(nrow(epi.subset.region2),as.integer(nrow(epi.subset.region2)*0.7))

# create train & test sets based on sampled indexes
epi.subset.region1.train <- epi.subset.region1[s_epi.region1,]
epi.subset.region1.test <- epi.subset.region1[-s_epi.region1,]
epi.subset.region2.train <- epi.subset.region2[s_epi.region2,]
epi.subset.region2.test <- epi.subset.region2[-s_epi.region2,]

# kNN algorithm
accuracy1 <- c()
accuracy2 <- c()
ks <- c(1,5,10,15)
for(k in ks) {
  KNNpred1 <- knn(train=epi.subset.region1.train[2:6], test=epi.subset.region1.test[2:6], cl=epi.subset.region1.train$region, k=k)
  KNNpred2 <- knn(train=epi.subset.region2.train[2:6], test=epi.subset.region2.test[2:6], cl=epi.subset.region2.train$region, k=k)
  cm1 = as.matrix(table(Actual=KNNpred1, Predicted=epi.subset.region1.test$region,
                        dnn=list('predicted', 'actual')))
  cm2 = as.matrix(table(Actual=KNNpred2, Predicted=epi.subset.region2.test$region,
                        dnn=list('predicted', 'actual')))
  accuracy1 <- c(accuracy1, sum(diag(cm1))/length(epi.subset.region1.test$region))
  accuracy2 <- c(accuracy2, sum(diag(cm2))/length(epi.subset.region2.test$region))
}

plot(ks, accuracy1, type="b")
plot(ks, accuracy2, type="b")