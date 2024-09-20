library(ggplot2)

dev.off()

EPI_data <- read.csv("./data/epi2024results06022024.csv", header=TRUE)
attach(EPI_data)

# Box plot comparing 3 variables
boxplot(EPI.new, BDH.new, OZD.new, names=c("EPI.new","BDH.new","OZD.new"))

# Normal Q-Q plot for EPI.new
x <- seq(20.,80.,1.0)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

# Beta Q-Q plot for BDH.new
qqplot(qbeta(ppoints(200),shape1=1,shape2=2),BDH.new)
qqline(BDH.new)

# Chi-Squared Q-Q plot for OZD.new
qqplot(qchisq(ppoints(200),5),OZD.new)
qqline(OZD.new)

# ECDF plot for EPI.new vs. BDH.new
plot(ecdf(EPI.new),main="EPI.new vs. BDH.new ECDF")
lines(ecdf(BDH.new))

# ECDF plot for BDH.new vs. OZD.new
plot(ecdf(BDH.new),main="BDH.new vs. OZD.new ECDF")
lines(ecdf(OZD.new))

# ECDF plot for EPI.new vs. OZD.new
plot(ecdf(EPI.new),main="EPI.new vs. OZD.new ECDF")
lines(ecdf(OZD.new))

# Integrate EPI & populations datasets
populations_2023 <- read.csv("./data/countries_populations_2023.csv")
populations <- populations_2023[-which(!populations_2023$Country %in% EPI_data$country),]
populations <- populations[order(populations$Country),]
EPI_data.sub <- EPI_data[-which(!EPI_data$country %in% populations$Country),]
EPI_data.sub <- EPI_data.sub[order(EPI_data.sub$country),]
EPI_data.sub <- EPI_data.sub[,c("country","EPI.old","EPI.new", "BDH.old", "BDH.new", "OZD.old", "OZD.new")]
EPI_data.sub$population <- as.numeric(populations$Population)
EPI_data.sub$population_log <- log10(EPI_data.sub$population)

# Summary stats and select plots from 3 linear models
attach(EPI_data.sub)
lin.mod.epinew <- lm(EPI.new~population_log,EPI_data.sub)
lin.mod.bdhnew <- lm(BDH.new~population_log,EPI_data.sub)
lin.mod.ozdnew <- lm(OZD.new~population_log,EPI_data.sub)

summary(lin.mod.epinew)
summary(lin.mod.bdhnew)
summary(lin.mod.ozdnew)

plot(lin.mod.epinew)
plot(lin.mod.bdhnew)
plot(lin.mod.ozdnew)

ggplot(EPI_data.sub, aes(x=population_log, y=EPI.new)) + geom_point() + stat_smooth(method="lm")
ggplot(EPI_data.sub, aes(x=population_log, y=BDH.new)) + geom_point() + stat_smooth(method="lm")
ggplot(EPI_data.sub, aes(x=population_log, y=OZD.new)) + geom_point() + stat_smooth(method="lm")

ggplot(lin.mod.epinew, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0) + labs(title="Residual vs. Fitted Values Plot (EPI.new)", x="Fitted Values", y="Residuals")
ggplot(lin.mod.bdhnew, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0) + labs(title="Residual vs. Fitted Values Plot (BDH.new)", x="Fitted Values", y="Residuals")
ggplot(lin.mod.ozdnew, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0) + labs(title="Residual vs. Fitted Values Plot (OZD.new)", x="Fitted Values", y="Residuals")