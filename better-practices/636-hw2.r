###################################################################################
#                               Homework #2                                       #
###################################################################################
# updated 9/29/14

## Preliminaries
setwd("C:\\Users\\Jared\\Documents\\HW2")
lapply(c('foreign','MASS'), require, character.only = TRUE)
housedat <- read.dta("house636.dta")

## Question 1
## part A.
descriptives <- function(x) {
  c(stdev =sd(x), mean = mean(x), minimum = min(x), maximum = max(x))
}
sapply(housedat, descriptives)

## part B.
cor(housedat)
cor.test(housedat$price, housedat$dhws)
cor.test(housedat$dhws, housedat$dcom)

## part C.
hist(housedat$price,
     main = "Histogram of Price",
     xlab = "Price",
     ylab = "Frequency")
hist(housedat$dhws,
     main = "Histogram of Distance to Waste Site",
     xlab = "Distance in Miles",
     ylab = "Frequency")

## part D.
plot(housedat$price,housedat$dhws,
     main = "Scatter between Price and Distance to Waste",
     xlab = "Price",
     ylab = "Distance")

## part E.
housedat$lnprice <- log(housedat$price)
plot(housedat$lnprice,housedat$dhws,
     main = "Scatter between Ln(Price) and Distance to Waste",
     xlab = "Log of Price",
     ylab = "Distance"
)

## part F.
housedat$close <- as.numeric(housedat$dhws<3) 
margin.table(table(housedat[,c('close')]),1)

## part G.
housedat.c <- subset(housedat, close==1)
housedat.nc <- subset(housedat, close==0)
cor.test(housedat.c$price, housedat.c$dhws)
cor.test(housedat.nc$price, housedat.nc$dhws)

## part H.

lm1 <- lm(price~dhws,data = housedat)
summary(lm1)
lm2 <- lm(lnprice~dhws, data=housedat)
summary(lm2)

## part I.

lm2c <- lm(lnprice~dhws,
            data = subset(housedat,close==1))
lm2nc <- lm(lnprice~dhws,
            data = subset(housedat,close==0))
summary(lm2c)
summary(lm2nc)