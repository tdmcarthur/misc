###################################################################################
#                               Homework #2                                       #
###################################################################################
# updated 9/29/14

## Preliminaries
setwd("C:\\Users\\Jared\\Documents\\HW2")
lapply(c('foreign','MASS'), require, character.only = TRUE)
# nice use of lapply()
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
# Keeping this as logical would have made more sense, but I know that the
# assignment says to make it 0-1.
margin.table(table(housedat[,c('close')]),1)

## part G.
cor.test(housedat$price[housedat$close==1], housedat$dhws[housedat$close==1])
cor.test(housedat$price[housedat$close==0], housedat$dhws[housedat$close==0])

# Or a neater alternative:
with(housedat[housedat$close==1, ], cor.test(price, dhws) )
with(housedat[housedat$close==0, ], cor.test(price, dhws) )


## part H.

# A nice trick to fit things on one line:
summary(lm1 <- lm(price ~ dhws, data = housedat))
summary(lm2 <- lm(lnprice ~ dhws, data=housedat))


## part I.

summary(lm2c <- lm(lnprice ~ dhws, data = housedat[housedat$close==1, ]))
summary(lm2nc <- lm(lnprice ~ dhws, data = housedat[housedat$close==0, ]))