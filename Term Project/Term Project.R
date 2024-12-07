rm(list=ls())

# load data
youth.education <- read.csv("data/SYB66_309_202310_Education.csv")
youth.unemployment <- read.csv("data/Youth unemployment, both sexes.csv")

# convert youth education data
# complex conversion here:
#   Original data has only 2 columns, a "Value" column,
#   and a "Series" column.
#   The Series column identifies which series a value belongs to.
#   On top of that, a Series can be repeated for a particular Region,
#   for multiple years.
#   To convert, I first created a data frame with all the unique values
#   for region & year (in 2 columns respectively).
#   I then went through each possible series, then filtered the
#   the data by that series, then went through each row and added them
#   to the new data frame, matching on the row's Region & Year columns.
youth.education.cleaned <- data.frame(unique(youth.education[2:nrow(youth.education),c(2,3)]))
colnames(youth.education.cleaned) <- c("Region/Country/Area","Year")
series.names <- unique(youth.education[2:nrow(youth.education),"X.1"])
for(series.name in series.names) {
  youth.education.cleaned[,series.name] <- -1
  series.rows <- youth.education[youth.education$X.1 == series.name,]
  for(i in 1:nrow(series.rows)) {
    youth.education.cleaned[youth.education.cleaned$`Region/Country/Area` == series.rows[i,2] & youth.education.cleaned$Year == series.rows[i,3],series.name] <- series.rows[i,5]
  }
}

youth.unemployment.cleaned <- youth.unemployment

drops <- c("Country..code.","Repository..code.","Type.of.source..code.","Coverage..code.","Notes")
youth.unemployment.cleaned <- youth.unemployment.cleaned[,!(names(youth.unemployment.cleaned) %in% drops)]

youth.unemployment.cleaned$Youth.StartAge <- as.numeric(substring(youth.unemployment.cleaned$Age,8,9))
youth.unemployment.cleaned$Youth.EndAge <- as.numeric(substring(youth.unemployment.cleaned$Age,11,12))