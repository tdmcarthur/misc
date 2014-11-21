##################################################################################
#                               Homework #5                                      #
##################################################################################
#updated 11/13/2014

# preliminaries
setwd("C:\\Users\\Jared\\Documents\\HW5")
lapply(c('foreign','MASS','sandwich','lmtest','car','plm'), require, character.only = TRUE)
lakedata <- read.dta("IA_long.dta")

clse<-function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

# Part a
descriptives <- function(x) {
  c(stdev =sd(x), mean = mean(x), minimum = min(x), maximum = max(x))
}
sapply(lakedata, descriptives)
gomeanbysite<-aggregate(lakedata$go, by=list(lakedata$site), FUN=mean )
gomean<- gomeanbysite[with(gomeanbysite, order(-x)), ]
names(gomean)[1:2] <- c("lake_id","mean_participation")
head(gomean, n=5)

# Part b 
sapply(lakedata[,6:12], descriptives)
# These variables are measured on the lake level and will be the same across
# individuals.  The real sample size when looking at the variation in the lake
# level variables is the number of lakes:
length(unique(lakedata$site))

# Part c.i
lpm1 <-lm(data=lakedata, go ~ pr + ltp)
summary(lpm1)
# B1 is the change in the probability of visiting lake j given $10 increase
# in the price of traveling.  B2 is the change in the probability given a one percentage
# point increase in the total phosphorous level in lake j.

# Part c.ii
# Two reasons to doubt the conventional standard errors are that 
# 1) They assume homoskedasticity, and with a linear probability model
# we inevitably have heteroskedasticity; it depends on the value of each X.
# 2) there will likely be covariance between u's in different groups
# which neccesitates a different estimation of standard errors.

# Part c.iii
# We would doubt the point estimate of B2 because there are likely several factors
# unique to the region where the lake is located that will affect environmental conditions.
# This would result in a non-zero conditional mean and a bias in B2.

# Part c.iv.
lakedata$pred <-predict(object= lpm1)
summary(lakedata$pred) 

# the percentage of observations that were predicted outside the [0,1] interval is
a<-length(lakedata[which(lakedata$pred<0 | lakedata$pred>1),]$pred)
b<-length(lakedata$pred)
(a/b)*100

# Averaging predicted values for the given lakes results in:
predbysite<-aggregate(lakedata$pred, by=list(lake_id=lakedata$site), FUN=mean )
predbysite<- predbysite[with(predbysite, order(-x)), ]
predbysite[predbysite$lake_id %in% gomean$lake_id[1:5],]
names(predbysite)[2] <- "mean_prediction"
compar <-merge(gomean, predbysite, sort=F)
head(compar, n=5)
# The model has low prediction power; it is underestimating, as the vast majority
# of observations are 0 so this is weighted towards 0.

# part c.v.
lakedata$res <- (lpm1$residuals)^2
lpm1.res <- lm(data=lakedata,res ~ pr + ltp)
summary(lpm1.res)
lht(lpm1.res, c("pr=0","ltp=0"))
# we reject the null of homoskedasticity, or that the variance is constant 
# over different values of the variables.

# part d.
coeftest(lpm1, vcov = vcovHC(lpm1, type="HC1"))
# There is very little difference in the standard errors.
felm1 <- lm(go ~ pr + ltp + factor(region), data=lakedata)
summary(felm1)
summary(lpm1)
coeftest(lpm1, vcov=vcovHC(felm1, type="HC1"))
# part i.
# The main difference is the marginal effect of ltp nearly doubles when region
# fixed effects are estimated.  The standard errors have changed little.
# The estimate of B2 with fixed effects is likely more accurate since we have
# now accounted for environmental variation unique to each region; this does not
# mean that the zero conditional mean assumption necessarily holds, as there me
# be other factors correlated with the phosphourus level that are not included
# in this regression and would make us doubt our estimate of B2.

# part ii.
# Lake level fixed effects would not be advisable because the variable we interested
# in is measured on the lake level.  Including lake level fixed effects would
# take away all of the variation we interested in using to estimate B2, that is
# estimating water quality.

# part e.
coeftest(lpm1,vcov=clse(lpm1,lakedata$id))
coeftest(lpm1,vcov=vcovHC(felm1, type="HC1"))
coeftest(lpm1,vcov=clse(lpm1,lakedata$site))
# part i.
# Mathematically, the difference in standard errors is due to an extra covariance
# term being added to the white standard errors.  Intuitively, the standard errors
# are bigger because we have assumed the existence of a covariance term between
# the two lakes which is added to the standard error.

# part ii.
# The standard errors are higher at the lake level for phospohorous because
# the variable is measured at the lake level, so for two people visiting the same
# lake there is high covariance between of value of phosphorous (because they are
# the same).  Nevertheless, we are interested in understanding how people react
# to lake pollution so it would make sense to take advantage of the covariance
# between two people visiting the same lake as opposed to two lakes visited by
# the same person.  However, this must be balanced with the consideration that
# the standard errors are much higher when clustering on the lake level.


# part f.
lpm2 <- lm(go ~ pr + ltp +lchl+secchi, data=lakedata)
coeftest(lpm2,vcov=clse(lpm2,lakedata$site))
lht(lpm2, vcov = clse(lpm2,lakedata$site), c("ltp=0","lchl=0","secchi=0"))
# We fail to reject the null of joint significance.

# part g.
lpm3 <- lm(go ~ pr +lchl, data=lakedata)
coeftest(lpm3,vcov=clse(lpm3,lakedata$site))
coeftest(lpm1,vcov=clse(lpm1,lakedata$site))
# Chlorophyll is more visible than phosphourous, and since chlorophyll
# is usually a result of concentration of phosphorous this may influence 
# consumer behavior more than the phosphourous content.  
