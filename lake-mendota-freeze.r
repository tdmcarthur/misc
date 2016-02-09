
current.date <- as.Date("2015-12-24")

enso.1950 <- read.table("http://www.esrl.noaa.gov/psd/data/correlation/mei.data", sep="", nrows= 2015-1950+1, skip=1,  header=FALSE)
#enso.1950[, 1] <- as.character(enso.1950[, 1])

enso.1950 <- reshape(enso.1950, varying=list(2:13), idvar="V1", direction="long")
names(enso.1950) <- c("year", "month", "enso.1950")
enso.1950 <- enso.1950[order(enso.1950$year, enso.1950$month ), ]
enso.1950$enso.1950[enso.1950$enso.1950 == -999] <- NA

enso.1871 <- read.table("http://www.esrl.noaa.gov/psd/data/correlation/censo.long.data", sep="", nrows= 2003-1871+1, skip=1,  header=FALSE)

enso.1871 <- reshape(enso.1871, varying=list(2:13), idvar="V1", direction="long")
names(enso.1871) <- c("year", "month", "enso.1871")
enso.1871 <- enso.1871[order(enso.1871$year, enso.1871$month ), ]
enso.1871$enso.1871[enso.1871$enso.1871 == -9.99] <- NA

enso.1950.for.calib <- enso.1950[enso.1950$year <= 2002, ]
enso.1950.sd <- sd(enso.1950.for.calib$enso.1950)
enso.1950.mean <- mean(enso.1950.for.calib$enso.1950)

enso.1871.for.calib <- enso.1871[enso.1871$year <= 2002 & enso.1871$year >=1950, ]
enso.1871.sd <- sd(enso.1871.for.calib$enso.1871)
enso.1871.mean <- mean(enso.1871.for.calib$enso.1871)

enso.1950$enso.1950 <- (enso.1950$enso.1950-enso.1950.mean)/enso.1950.sd
enso.1871$enso.1871 <- (enso.1871$enso.1871-enso.1871.mean)/enso.1871.sd

enso.merged <- merge(enso.1871, enso.1950, all=TRUE)

enso.merged$enso <- apply(enso.merged[, c("enso.1871", "enso.1950")], 1, FUN=mean, na.rm=TRUE)


# 1950 to 2002
#row.names(enso.1950) <- enso.1950[, 1]
#enso.1950 <- enso.1950[, -1]
#row.names(enso.1871) <- enso.1871[, 1]
#enso.1871 <- enso.1871[, -1]
#enso.1950.limited <- enso.1950[as.numeric(rownames(enso.1950)) >= 1950 & as.numeric(rownames(enso.1950)) <= 2002, ]
#enso.1871.limited <- enso.1871[as.numeric(rownames(enso.1871)) >= 1950 & as.numeric(rownames(enso.1871)) <= 2002, ]
#cor(scale(unlist(enso.1871)), scale(unlist(enso.1950)))
#use scale() then take average where they overlap and use it as the input data.



nao.df <- read.csv("/Users/travismcarthur/Desktop/Misc/Lake Mendota Freeze/NAO.csv", stringsAsFactor=FALSE)
# Source: https://zenodo.org/record/9979#.Vli74IQR_dl

nao.1950 <- read.fwf("/Users/travismcarthur/Desktop/Misc/Lake Mendota Freeze/norm.daily.nao.index.b500101.current.ascii", 
         widths=c(4, 3, 3, 7), stringsAsFactor=FALSE, header=FALSE)
# a bunch of asterisks at line 20753 force me to do this
# from ftp://ftp.cpc.ncep.noaa.gov/cwlinks/
# http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/nao.shtml

colnames(nao.1950) <- c("Year", "Month", "Day", "nao")
nao.1950$nao <- as.numeric(nao.1950$nao)
nao.1950$nao[is.na(nao.1950$nao)] <- nao.1950$nao[ which(is.na(nao.1950$nao)) - 1 ]
table(is.na(nao.1950$nao))
# Just impute the NA's

test <- merge(nao.df, nao.1950)
cor(test$Daily.Natural.NAO, test$nao)

nao.1950.for.calib <- nao.1950[nao.1950$Year <= 2014, ]
nao.1950.sd <- sd(nao.1950.for.calib$nao, na.rm=TRUE)
nao.1950.mean <- mean(nao.1950.for.calib$nao, na.rm=TRUE)

nao.df.for.calib <- nao.df[nao.df$Year <= 2014 & nao.df$Year >=1950, ]
nao.df.sd <- sd(nao.df.for.calib$Daily.Natural.NAO, na.rm=TRUE)
nao.df.mean <- mean(nao.df.for.calib$Daily.Natural.NAO, na.rm=TRUE)

nao.1950$nao.1950 <- (nao.1950$nao-nao.1950.mean)/nao.1950.sd
nao.df$nao.1871 <- (nao.df$Daily.Natural.NAO-nao.df.mean)/nao.df.sd

nao.merged <- merge(nao.df, nao.1950, all=TRUE)

nao.merged$nao.combined <- apply(nao.merged[, c("nao.1871", "nao.1950")], 1, FUN=mean, na.rm=TRUE)





temperature.df <- read.csv("/Users/travismcarthur/Desktop/Misc/Lake Mendota Freeze/Madison temperature data.csv", stringsAsFactor=FALSE)

# Source: http://www1.ncdc.noaa.gov/pub/orders/cdo/645115.csv
# This was a custom order. See email order form
# "*** Values denoted with “***” (i.e. temperature values) are in Celsius degrees to tenths on csv and text forms and Fahrenheit to tenths on Daily Form pdf file."
# "DATE is the year of the record (4 digits) followed by month (2 digits) and day (2 digits)."



temperature.1869.df <- read.csv("/Users/travismcarthur/Desktop/Misc/Lake Mendota Freeze/Madison temperature data - city bureau.csv", stringsAsFactor=FALSE)
# Source: http://www1.ncdc.noaa.gov/pub/orders/cdo/645434.csv

temperature.1869.df$TOBS <- NA

temperature.df$DATE <- as.Date(as.character(temperature.df$DATE), format="%Y%m%d")

table(temperature.df$STATION_NAME)

temperature.df <- temperature.df[temperature.df$STATION_NAME== "MADISON DANE CO REGIONAL AIRPORT WI US", ]

# Adding temp data day-by-day:

additional.temp.data <- data.frame(
  STATION= "GHCND:USW00014837",
  STATION_NAME = "MADISON DANE CO REGIONAL AIRPORT WI US",
  DATE= as.Date(paste0("2015-12-", 21:24), format = "%Y-%m-%d"),
  TMAX= c( 5.0, 2.8,  12.8,  1.1) * 10,
  TMIN= c( 0.6,  0.0, 1.7, -0.6 ) * 10,
  TOBS=NA, stringsAsFactors=FALSE)
# Source: http://w1.weather.gov/data/obhistory/metric/KMSN.html


 temperature.df <- rbind(temperature.df, additional.temp.data)


temperature.df$TMAX <- temperature.df$TMAX / 10
temperature.df$TMIN<- temperature.df$TMIN / 10
temperature.df$TOBS <- temperature.df$TOBS / 10
temperature.df$t.mean <- (temperature.df$TMAX + temperature.df$TMIN) / 2





temperature.1869.df$TMAX[which( temperature.1869.df$TMAX == -9999)] <- 
  ( temperature.1869.df$TMAX[ which( temperature.1869.df$TMAX == -9999) + 1 ] +
  temperature.1869.df$TMAX[ which( temperature.1869.df$TMAX == -9999) - 1 ] ) /2

temperature.1869.df$TMIN[which( temperature.1869.df$TMIN == -9999)] <- 
  ( temperature.1869.df$TMIN[ which( temperature.1869.df$TMIN == -9999) + 1 ] +
  temperature.1869.df$TMIN[ which( temperature.1869.df$TMIN == -9999) - 1 ] ) /2
# rudamentry imputation for missings


temperature.1869.df$TMAX <- temperature.1869.df$TMAX / 10
temperature.1869.df$TMIN<- temperature.1869.df$TMIN / 10
temperature.1869.df$t.mean <- (temperature.1869.df$TMAX + temperature.1869.df$TMIN) / 2

temperature.1869.df$DATE <- as.Date(as.character(temperature.1869.df$DATE), format="%Y%m%d")




temperature.combined.df <- merge(temperature.df, temperature.1869.df, by="DATE")

mean.temperature.adjust <- mean(temperature.combined.df$t.mean.x - temperature.combined.df$t.mean.y)
max.temperature.adjust <- mean(temperature.combined.df$TMAX.x - temperature.combined.df$TMAX.y)
min.temperature.adjust <- mean(temperature.combined.df$TMIN.x - temperature.combined.df$TMIN.y)

# summary(temperature.combined.df$t.mean.x - temperature.combined.df$t.mean.y)
# View(temperature.combined.df[, c("t.mean.x", "t.mean.y")])
# cor(temperature.combined.df[, c("t.mean.x", "t.mean.y")])

temperature.1869.df$t.mean <- temperature.1869.df$t.mean + mean.temperature.adjust 
temperature.1869.df$TMAX <- temperature.1869.df$TMAX + max.temperature.adjust 
temperature.1869.df$TMIN <- temperature.1869.df$TMIN + min.temperature.adjust

temperature.df <- rbind(temperature.1869.df[temperature.1869.df$DATE < min(temperature.df$DATE), ], temperature.df)


temperature.df$season <- NA

# Let June 1 starts a season
for (i in 1860:2015) {
  begin.interval <- as.Date(paste0(i, "-06-01"), format = "%Y-%m-%d")
  end.interval <- as.Date(paste0(i+1, "-05-31"), format = "%Y-%m-%d")
  temperature.df$season[temperature.df$DATE >= begin.interval & temperature.df$DATE <= end.interval] <- i
}
temperature.df$season <-  temperature.df$season + 1




closure.dates.df <- read.csv("/Users/travismcarthur/Desktop/Misc/Lake Mendota Freeze/Lake mendota freeze dates.csv", stringsAsFactor=FALSE)


#table(gsub("-.*", "", closure.dates.df$closure.date))
closure.dates.df$closure.month <- gsub("-.*", "", closure.dates.df$closure.date)
closure.dates.df$closure.day <- gsub(".*-", "", closure.dates.df$closure.date)
closure.dates.df$closure.year <- ifelse(closure.dates.df$closure.month %in% c("Jan", "Feb", "Mar"), 
                                closure.dates.df$season, closure.dates.df$season - 1 )

closure.dates.df$closure.date.full <- with(closure.dates.df, as.Date(paste(closure.year, closure.month, closure.day, sep="-"), format="%Y-%b-%d"))

closure.dates.df$begin.season.date <- as.Date(paste0(closure.dates.df$season-1, "-11-1")) 

library("lubridate")

closure.dates.df$days.to.closure <- as.duration(new_interval(start = closure.dates.df$begin.season.date, end=closure.dates.df$closure.date.full))

closure.dates.df$days.to.closure <- round(as.numeric(closure.dates.df$days.to.closure)/86400)
# Number of seconds in a day: 86400
# I can't beleive that lubridate makes me do this.


#plot(closure.dates.df$season, closure.dates.df$days.to.closure)

#summary(lm(days.to.closure ~ season, data=closure.dates.df))

enso.months <- seq(1, 11)
nao.months <- seq(1, 11)


enso.year.agg <- aggregate(enso ~ year + month , data=enso.merged[enso.merged$month %in% enso.months, ], FUN=mean, na.rm=TRUE)
enso.year.agg$season <- enso.year.agg$year  + 1
enso.year.agg <- enso.year.agg[, colnames(enso.year.agg)!="year"]

enso.year.agg <- reshape(enso.year.agg, v.names="enso", idvar = "season", timevar="month", direction="wide")

closure.dates.df.2 <- merge(enso.year.agg, closure.dates.df, all=TRUE)

closure.dates.df.2$begin.season.date[closure.dates.df.2$season==2016] <- as.Date("2015-11-01")

# closure.dates.df.2$enso.1yr.lag <- c(NA, closure.dates.df.2$enso[-nrow(closure.dates.df.2)])
#summary(lm(days.to.closure ~ season + enso + enso.1yr.lag, closure.dates.df.2))
#summary(lm(days.to.closure ~ season + enso , closure.dates.df.2))

nao.agg <- aggregate(nao.combined ~ Year + Month, data = nao.merged[nao.merged$Month %in% nao.months, ], FUN=mean, na.rm=TRUE) 

nao.agg$season <- nao.agg$Year  + 1
names(nao.agg)[names(nao.agg)=="nao.combined"] <- "nao"

nao.agg <- nao.agg[, colnames(nao.agg)!="Year"]

nao.agg <- reshape(nao.agg, v.names="nao", idvar = "season", timevar="Month", direction="wide")

closure.dates.df.3 <- merge(nao.agg, closure.dates.df.2, all=TRUE)


summary(lm(days.to.closure ~ season + enso + enso.1yr.lag + nao, closure.dates.df.3))
summary(lm(days.to.closure ~ season + nao, closure.dates.df.3))


closure.dates.df.3.agg <- merge(closure.dates.df.3, temperature.df)

FDD.agg <- by(closure.dates.df.3.agg, INDICES=list(closure.dates.df.3.agg$season), FUN=function(x) {
  targ.indices <- x$DATE <= x$closure.date.full & x$DATE >=  x$begin.season.date
#  targ.temps <- x$t.mean[targ.indices]
  targ.temps <- x$t.mean[targ.indices]
  targ.temps <- targ.temps[targ.temps < 0]
  (-1) * sum(targ.temps)
})

FDD.df <- data.frame(FDD.to.closure = as.vector(FDD.agg), season=names(FDD.agg))

closure.dates.df.4 <- merge(closure.dates.df.3, FDD.df, all=TRUE)

DD.agg <- by(closure.dates.df.3.agg, INDICES=list(closure.dates.df.3.agg$season), FUN=function(x) {
  targ.indices <- x$DATE <= x$closure.date.full & 
    x$DATE >=  x$begin.season.date # as.Date(paste0(year(x$begin.season.date), ""), format="%Y-%m-%d")
#  targ.temps <- x$t.mean[targ.indices]
    targ.temps <- x$t.mean[targ.indices]
#  targ.temps <- targ.temps[targ.temps < 0]
  mean(targ.temps)
})

DD.df <- data.frame(DD.to.closure = as.vector(DD.agg), season=names(DD.agg))

closure.dates.df.4 <- merge(closure.dates.df.4, DD.df, all=TRUE)


FDD.agg <- by(closure.dates.df.3.agg, INDICES=list(closure.dates.df.3.agg$season), FUN=function(x) {
  targ.indices <- x$DATE <= as.Date(gsub("2015", year(x$begin.season.date[1]), as.character(current.date))) & 
                                           x$DATE >=  x$begin.season.date
#  cat(sum(targ.indices), "\n")
#  targ.temps <- x$t.mean[targ.indices]
    targ.temps <- x$t.mean[targ.indices]
  targ.temps <- targ.temps[targ.temps < 0]
  (-1) * sum(targ.temps)
})

FDD.df <- data.frame(FDD.to.today = as.vector(FDD.agg), season=names(FDD.agg))

closure.dates.df.4 <- merge(closure.dates.df.4, FDD.df, all=TRUE)

DD.agg <- by(closure.dates.df.3.agg, INDICES=list(closure.dates.df.3.agg$season), FUN=function(x) {
  targ.indices <- x$DATE <= as.Date(gsub("2015", year(x$begin.season.date[1]), as.character(current.date))) & 
    x$DATE >=  x$begin.season.date # as.Date(paste0(year(x$begin.season.date), ""), format="%Y-%m-%d")
#   targ.temps <- x$t.mean[targ.indices]
    targ.temps <- x$t.mean[targ.indices]
#  targ.temps <- targ.temps[targ.temps < 0]
  mean(targ.temps)
})

DD.df <- data.frame(DD.to.today = as.vector(DD.agg), season=names(DD.agg))

closure.dates.df.4 <- merge(closure.dates.df.4, DD.df, all=TRUE)




closure.dates.df.4 <- closure.dates.df.4[closure.dates.df.4$season!=1940, ]

best.lm <- lm(days.to.closure ~ season + enso + enso.1yr.lag + nao + FDD.to.closure + DD.to.closure, closure.dates.df.4)

summary(best.lm )

plot(best.lm$fitted.values, best.lm$model[[1]])

summary(resid(best.lm))

# install.packages("mgcv")
library(mgcv)

summary(gam(days.to.closure ~ s(season) + s(enso) + enso.1yr.lag + s(nao) + s(FDD.to.closure) + s(DD.to.closure), data=closure.dates.df.4))

summary(gam(days.to.closure ~ s(season) + s(enso) + 
              enso.1yr.lag + s(nao) + s(FDD.to.closure) + s(DD.to.closure) + ti(enso, nao), data=closure.dates.df.4))


summary(todays.best.gam <- gam(days.to.closure ~ s(season) + s(enso) + s(enso.1yr.lag) + s(nao) + s(FDD.to.today) + s(DD.to.today), data=closure.dates.df.4))

summary(todays.best.gam <- gam(days.to.closure ~ s(season) + s(enso) + s(enso.1yr.lag) + s(nao) + s(FDD.to.today) + s(DD.to.today), data=closure.dates.df.4, family=poisson ))


summary(todays.best.gam <- gam(days.to.closure ~ s(season) + enso.1 + enso.2 + enso.3 + enso.4 + enso.5 + enso.6 + enso.7+ enso.8+ enso.9 + enso.10 + nao.4 + nao.5 + nao.6 + nao.7 + nao.8 + nao.9 + nao.10  + s(FDD.to.today) + s(DD.to.today), data=closure.dates.df.4, family=poisson ))


#summary(todays.best.gam <- gam(days.to.closure ~ s(season) + s(I( enso.8 + enso.9 + enso.10)) + s(I( nao.8 + nao.9 + nao.10)) + s(FDD.to.today) + s(DD.to.today), data=closure.dates.df.4, family=poisson ))

summary(todays.best.gam <- gam(days.to.closure ~ s(season) + te(I(enso.11), I(   nao.11)) + s(FDD.to.today) + s(DD.to.today), data=closure.dates.df.4, family=poisson ))

# summary(todays.best.gam <- gam(days.to.closure ~ s(season)  + s(DD.to.today), data=closure.dates.df.4, family=poisson ))

best.lm <- lm(days.to.closure ~ season + I(enso.11)*I(nao.11) + FDD.to.today + DD.to.today, closure.dates.df.4)

summary(best.lm )

# enso.8*0.25 + enso.9*0.5 + enso.10*0.75 + nao.10*0.75 +
#   nao.8*0.25 + nao.9*0.5 +

# enso.1 + enso.2 + enso.3 + enso.4 + enso.5 + enso.6 + enso.7
# nao.4 + nao.5 + nao.6 + nao.7 +

# min temp: R-sq.(adj) =  0.311   Deviance explained = 49.5%
# max temp: R-sq.(adj) =  0.306   Deviance explained = 48.4%
# mean temp: R-sq.(adj) =  0.379   Deviance explained = 56.7%


#summary(todays.best.gam <- gam(days.to.closure ~ s(season) +  s(enso.4)  + s(enso.7) + 
#           s(nao.6) + s(nao.7) + s(FDD.to.today) + s(DD.to.today), data=closure.dates.df.4, family=poisson ))


# todays.best.gam
# best.lm
predicted.day.2016 <- predict(todays.best.gam, newdata=closure.dates.df.4[closure.dates.df.4$season==2016, ], type="response", se.fit=TRUE)
predicted.days <- predict(todays.best.gam, type="response")
mean(predicted.days)
predicted.day.2016$fit
predicted.day.2016$se.fit

as.Date("2015-11-01") + ddays(predicted.day.2016$fit )

summary(resid(todays.best.gam, type="response"))
sd(resid(todays.best.gam, type="response"))

plot(todays.best.gam$fitted.values, todays.best.gam$model[[1]])


summary( todays.best.lm <- lm(days.to.closure ~ season + enso + enso.1yr.lag + nao + FDD.to.today + DD.to.today, closure.dates.df.4))






