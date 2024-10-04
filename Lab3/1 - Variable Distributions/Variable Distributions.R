# 1. Variable Distributions

rm(list=ls())

# Set working directory to "Lab3" folder
epi <- read.csv("data/epi2024results_DA_F24_lab03.csv")

# create subsets for Global West & Asia-Pacific regions
epi.global_west <- subset(epi, region=="Global West")[,!(names(epi) %in% c("region"))]
epi.asia_pacific <- subset(epi, region=="Asia-Pacific")[,!(names(epi) %in% c("region"))]

# 1.1. Histograms for variable "EPI" for Global West & Asia-Pacific regions
hist(epi.global_west$EPI,seq(50.,80.,1.0),prob=TRUE)
lines(density(epi.global_west$EPI,na.rm=TRUE,bw=1.))
rug(epi.global_west$EPI)
hist(epi.asia_pacific$EPI,seq(20.,70.,1.0),prob=TRUE)
lines(density(epi.asia_pacific$EPI,na.rm=TRUE,bw=1.))
rug(epi.asia_pacific$EPI)

# 1.2. QQ plots for Global West & Asia-Pacific Regions for normal
#      distributions
qqplot(rnorm(250),epi.global_west$EPI,
       xlab="Global West Q-Q plot, for norm. dsn")
qqline(epi.global_west$EPI)
qqplot(rnorm(250),epi.asia_pacific$EPI,
       xlab="Asia-Pacific Q-Q plot for norm. dsn")
qqline(epi.asia_pacific$EPI)