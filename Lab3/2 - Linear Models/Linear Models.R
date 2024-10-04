# 2. Linear Models

rm(list=ls())

# Set working directory to "Lab3" folder
epi <- read.csv("data/epi2024results_DA_F24_lab03.csv")

# subset data & create linear model, for 5 variables
epi.5var <- epi[7:11]

lin.mod.epi.5var <- lm(epi$EPI~epi.5var[,1]+
                       epi.5var[,2]+
                       epi.5var[,3]+
                       epi.5var[,4]+
                       epi.5var[,5])
summary(lin.mod.epi.5var)
plot(epi$EPI~epi.5var[,1])
abline(lin.mod.epi.5var)

# subset epi data by region & create linear model, for 5 variables
epi.global_west <- subset(epi, region=="Global West")[,!(names(epi) %in% c("region"))]
epi.global_west.5var <- epi.global_west[7:11]

lin.mod.epi.global_west.5var <- lm(epi.global_west$EPI~epi.global_west.5var[,1]+
                        epi.global_west.5var[,2]+
                        epi.global_west.5var[,3]+
                        epi.global_west.5var[,4]+
                        epi.global_west.5var[,5])
summary(lin.mod.epi.global_west.5var)
plot(epi.global_west$EPI~epi.global_west.5var[,1])
abline(lin.mod.epi.global_west.5var)