rm(list=ls())

# covid-19 datasets
us.counties.2020 <- read.csv("data/us-counties-2020.csv")
us.counties.2021 <- read.csv("data/us-counties-2021.csv")

# boxplots
boxplot(us.counties.2020$cases,us.counties.2021$cases,names=c("2020","2021"))
boxplot(us.counties.2020$deaths,us.counties.2021$deaths,names=c("2020","2021"))

# histograms
hist(us.counties.2020$cases,seq(0.,1700000.,10000.0),prob=TRUE,ylim=c(0,10E-5))
curve(2.5*dnorm(x, mean=mean(us.counties.2020$cases),sd=sd(us.counties.2020$cases)), add=TRUE)

hist(us.counties.2021$cases,seq(0.,1700000.,10000.0),prob=TRUE,ylim=c(0,10E-5))
curve(8*dnorm(x, mean=mean(us.counties.2021$cases),sd=sd(us.counties.2021$cases)), add=TRUE)

hist(us.counties.2020$deaths,seq(0.,36000.,500.0),prob=TRUE,ylim=c(0,0.002))
curve(2.5*dnorm(x, mean=mean(us.counties.2020$deaths,na.rm=TRUE),sd=sd(us.counties.2020$deaths,na.rm=TRUE)), add=TRUE)

hist(us.counties.2021$deaths,seq(0.,36000.,500.0),prob=TRUE,ylim=c(0,0.002))
curve(4*dnorm(x, mean=mean(us.counties.2021$deaths,na.rm=TRUE),sd=sd(us.counties.2021$deaths,na.rm=TRUE)), add=TRUE)
