# 4. Clustering (k-Means)

rm(list=ls())

# Set working directory to "Lab3" folder
epi <- read.csv("data/epi2024results_DA_F24_lab03.csv")

# set seed for RNG
set.seed(123)

# data subset
epi.subset <- epi[5:10]

# replacing missing values with 0
epi.subset[is.na(epi.subset)] <- 0

epi.subset.region1 <- subset(epi.subset, region==c("Global West","Asia-Pacific","Latin America & Caribbean"))
epi.subset.region2 <- subset(epi.subset, region==c("Eastern Europe","Sub-Saharan Africa","Greater Middle East"))

# run k-means
epi.km1.wcss <- c()
epi.km2.wcss <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10)
for(k in ks) {
  epi.km1 <- kmeans(epi.subset.region1[,-1],centers=k)
  epi.km2 <- kmeans(epi.subset.region2[,-1],centers=k)
  epi.km1.wcss <- c(epi.km1.wcss, epi.km1$tot.withinss)
  epi.km2.wcss <- c(epi.km2.wcss, epi.km2$tot.withinss)
}

# plot WCSS
plot(ks,epi.km1.wcss,type="b")
plot(ks,epi.km2.wcss,type="b")