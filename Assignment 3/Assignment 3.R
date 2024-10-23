rm(list=ls())

# covid-19 datasets
us.counties.2020.full <- read.csv("data/us-counties-2020.csv")
us.counties.2021.full <- read.csv("data/us-counties-2021.csv")

# Drop NAs and remove outliers from Cases & Deaths
us.counties.2020 <- us.counties.2020.full
us.counties.2021 <- us.counties.2021.full

us.counties.2020 <- subset(us.counties.2020, !is.na(cases))
us.counties.2020 <- subset(us.counties.2020, cases < 2000)

us.counties.2021 <- subset(us.counties.2021, !is.na(cases))
us.counties.2021 <- subset(us.counties.2021, cases < 8500)

us.counties.2020 <- subset(us.counties.2020, !is.na(deaths))
us.counties.2020 <- subset(us.counties.2020, deaths < 75)

us.counties.2021 <- subset(us.counties.2021, !is.na(deaths))
us.counties.2021 <- subset(us.counties.2021, deaths < 200)

# boxplots
us.counties.cases.boxplot <- boxplot(us.counties.2020$cases,us.counties.2021$cases,names=c("2020","2021"),main="Cases Boxplot")
us.counties.deaths.boxplot <- boxplot(us.counties.2020$deaths,us.counties.2021$deaths,names=c("2020","2021"),main="Deaths Boxplot")

# boxplot summaries
colnames(us.counties.cases.boxplot$stats) <- c("2020","2021")
rownames(us.counties.cases.boxplot$stats) <- c("Minimum","First Quartile","Median","Third Quartile","Maximum")
us.counties.cases.boxplot$stats

colnames(us.counties.deaths.boxplot$stats) <- c("2020","2021")
rownames(us.counties.deaths.boxplot$stats) <- c("Minimum","First Quartile","Median","Third Quartile","Maximum")
us.counties.deaths.boxplot$stats

# histograms
hist(us.counties.2020$cases,prob=TRUE)
curve(dexp(x,1/mean(us.counties.2020$cases)), add=TRUE)

hist(us.counties.2021$cases,prob=TRUE)
curve(dexp(x,1/mean(us.counties.2021$cases)), add=TRUE)

hist(us.counties.2020$deaths,prob=TRUE)
curve(dexp(x,1/mean(us.counties.2020$deaths)), add=TRUE)

hist(us.counties.2021$deaths,prob=TRUE)
curve(dexp(x,1/mean(us.counties.2021$deaths)), add=TRUE)

# ECDFs
plot(ecdf(us.counties.2020$cases),main="2020 (Black) vs. 2021 (Brown) Cases (ECDF)",col='black')
lines(ecdf(us.counties.2021$cases),col='brown')

plot(ecdf(us.counties.2020$deaths),main="2020 (Black) vs. 2021 (Brown) Deaths (ECDF)",col='black')
lines(ecdf(us.counties.2021$deaths),col='brown')

# QQ-plots
qqplot(qexp(ppoints(200)),us.counties.2020$cases)
qqline(us.counties.2020$cases)

qqplot(qexp(ppoints(200)),us.counties.2021$cases)
qqline(us.counties.2021$cases)

qqplot(qexp(ppoints(200)),us.counties.2020$deaths)
qqline(us.counties.2020$deaths)

qqplot(qexp(ppoints(200)),us.counties.2021$deaths)
qqline(us.counties.2021$deaths)

# filter by California as the state
us.California.2020 = us.counties.2020.full[us.counties.2020.full$state=='California',]
us.California.2021 = us.counties.2021.full[us.counties.2021.full$state=='California',]

# repeat histograms
hist(us.California.2020$cases,prob=TRUE)
curve(dexp(x,1/mean(us.California.2020$cases)), add=TRUE)

hist(us.California.2021$cases,prob=TRUE)
curve(dexp(x,1/mean(us.California.2021$cases)), add=TRUE)

hist(us.California.2020$deaths,prob=TRUE)
curve(dexp(x,1/mean(us.California.2020$deaths)), add=TRUE)

hist(us.California.2021$deaths,prob=TRUE)
curve(dexp(x,1/mean(us.California.2021$deaths)), add=TRUE)

# repeat ECDFs
plot(ecdf(us.California.2020$cases),main="2020 (Black) vs. 2021 (Brown) Cases in California (ECDF)",col='black')
lines(ecdf(us.California.2021$cases),col='brown')

plot(ecdf(us.California.2020$deaths),main="2020 (Black) vs. 2021 (Brown) Deaths in California (ECDF)",col='black')
lines(ecdf(us.California.2021$deaths),col='brown')

# repeat QQ-plots
qqplot(qexp(ppoints(200)),us.California.2020$cases)
qqline(us.California.2020$cases)

qqplot(qexp(ppoints(200)),us.California.2021$cases)
qqline(us.California.2021$cases)

qqplot(qexp(ppoints(200)),us.California.2020$deaths)
qqline(us.California.2020$deaths)

qqplot(qexp(ppoints(200)),us.California.2021$deaths)
qqline(us.California.2021$deaths)

# NY House Dataset
ny.house <- read.csv("data/NY-House-Dataset.csv")

# linear model
lin.mod.ny.house <- lm(ny.house$PRICE~ny.house$BEDS+ny.house$BATH+ny.house$PROPERTYSQFT,ny.house)
summary(lin.mod.ny.house)
plot(ny.house$PRICE~ny.house$BATH,ylim=c(-1E8,1E8)) # limit Y to remove outliers
abline(lin.mod.ny.house)

plot(lin.mod.ny.house$residuals,ylim=c(-0.5E7,0.5E7))

# subset dataset & repeat linear model
ny.house.subset <- subset(ny.house, PRICE < 1000000)
lin.mod.ny.house.subset <- lm(ny.house.subset$PRICE~ny.house.subset$BEDS+ny.house.subset$BATH+ny.house.subset$PROPERTYSQFT,ny.house.subset)
summary(lin.mod.ny.house.subset)
plot(ny.house.subset$PRICE~ny.house.subset$BATH)
abline(lin.mod.ny.house.subset)

plot(lin.mod.ny.house.subset$residuals)
