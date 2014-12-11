##################################################################################
#                                 Homework #7                                    #
##################################################################################
# Updated 12-08-14

# preliminaries
setwd("C:\\Users\\Jared\\Documents\\HW7")
lapply(c('foreign','MASS','lmtest','car','plm','AER'), require, character.only = TRUE)
murddf <- read.dta("murder.dta")
airdf <- read.dta("airfare.dta")
elemdf <- read.dta("elem94_95.dta")
fertdf <- read.dta("fertil2.dta")

haus <-function(m1, m2){
  cf_d <- coef(m1) - coef(m2)
  vc_d <- vcov(m1) - vcov(m2)
  x2_d <- as.vector(t(cf_d) %*% solve(vc_d) %*% cf_d)
  return(pchisq(x2_d, df = 2, lower.tail = FALSE))
}
# Question 14.C.7
# Part i.
# The sign of beta1 should be <0, as more executions should be deterring murders.
# The sign of beta2 should be >0, as states with higher unemployment are likely
# to have more of their population in lower income brackets where crime is likely
# to be a symptom.

# Part ii.
summary(plm1 <-lm(mrdrte ~ exec + unem, data=murddf[murddf$year!=87,]))
# No evidence of deterrent effect.
# Putting the formula before all other arguments is much more R-like.

# Part iii.
summary(fdm1 <-plm(data=murddf[murddf$year!=87,],mrdrte~exec+unem, model="fd",index=c("id","year")))
# Now there is evidence of a deterrence effect; for every execution there is a
# a .1 decrease in the murder rate, or 1 less murder per 1 million people.

# Part iv.
coeftest(fdm1, vcov=vcovHC(fdm1, type="HC1"))

# Part v.
exec93 <- murddf[murddf$year==93, c("exec","state")]
exec93 <- murddf[order(murddf$exec, decreasing = TRUE)), ]
head(exec93,2)
exec93[1,1]-exec93[2,1]
# Generally, don't use head() except to view things.

# Part vi.
summary(fdm1c <-plm(data=murddf[murddf$year!=87 & murddf$state!='TX',],mrdrte~exec+unem, model="fd",index=c("id","year")))
# Separate logical statements with a space. I'm surprised R didn't choke without the spaces
coeftest(fdm1c, vcov=vcovHC(fdm1c, type="HC1"))
# Executions are not actually deterring murder, rather the large amount of executions
# in Texas is biasing the estimator upwards; Texas has likely a lower murder rate
# which biases the results.

# Part vii.
murdpl<-plm.data(murddf, indexes = "id")
summary(fdm2 <-plm(data=murdpl,mrdrte~exec+unem+d90+ d93, model="within",effect= "individual",index=c("id","year")))
# The coefficient is no insignificant

# Question 14.C.10
# part i.
summary(lm1<-plm(data=airdf,lfare~concen+ldist+ldistsq+factor(year),model="pooling"))

# part ii.
# It is not reliable as pooling requires no autocorrelation which is not likely.
rstd<-coeftest(lm1, vcov=vcovHC(lm1, type="HC1"))
conf <-cbind(rstd[,"Estimate"] - rstd[,"Std. Error"]*1.96,rstd[,"Estimate"] + rstd[,"Std. Error"]*1.96)
# It's probably better to get the standard errors by calling theirnames in the coeftest object
# than by just their numbers, since you may be confused about what you are doing when you read 
# your code later.

# part iii.
# Take e(-b2/b3*2)

# part iv.
summary(rlm1<-plm(data=airdf,lfare~concen+ldist+ldistsq+factor(year),model="random",index=c("id","year")))
# The coefficient is smaller but still statistically significant

# part v. 
summary(flm1<-plm(data=airdf,lfare~concen+ldist+ldistsq+factor(year),model="within",index=c("id","year")))
phtest(flm1, rlm1)
# They are similar, as the theta paremeter is close to 1 (.85)

# part vi.
# One characteristic of the locations could be geographic characteristics

# part vii.
# Fixed effect estimate is better estimate, concen does positively affect fares.

# Question 14.C.12
# part i.
freqtb <- as.data.frame(table(elemdf$distid))
max(freqtb$Freq)
min(freqtb$Freq)
mean(freqtb$Freq)

# The question didnt ask this, but if I wanted to display the district number with
# the most schools would I do this?
freqtb[which.max(freqtb$Freq), ]
# This is much easier. Be careful of ties (i.e. multiple observations that are all the max), though.
# For that, you would do freqtb[which(freqtb$Freq == max(freqtb$Freq)), ]

# part ii.
elemplm<-plm.data(elemdf, indexes = "distid")
summary(plm3 <-plm(data=elemplm, lavgsal~bs+lenrol+lstaff+lunch, model="pooling"))

# part iii.
coeftest(plm3,vcov=vcovHC(plm3,type="HC1",cluster="group"))

# part iv.
summary(plm4 <-plm(data=elemplm[elemplm$bs<=.5,], lavgsal~bs+lenrol+lstaff+lunch, model="pooling"))
coeftest(plm4,vcov=vcovHC(plm3,type="HC1",cluster="group"))

# part v.
summary(plm5 <-plm(effect = "individual",data=elemplm[elemplm$bs<=.5,], lavgsal~bs+lenrol+lstaff+lunch, model="within"))
# Now there is a salary-benefits tradeoff

# part vi.
# Explain why districts affect

# Question 15.C.2
# part i.
summary(lm2<-lm(data=fertdf, children~educ+age+agesq))
# A year increase in education translates as a .09 decrease in children. 
# If 100 women were to recieve one more year of education then that group of
# of women would have nine less children.

# part ii.
summary(lm3<-lm(data=fertdf, educ~ frsthalf))
# We reject the null that the effect of frsthalf on educ is zero, so it is clear
# Cov(educ,frsthalf)!=0 which means the instrument passes the test of relevance.
# We have already assumed that exogeneity holds.

# Frsthalf is reasonable as an instrument because it is randomly assigned (not 
# correlated with u1) but will determine how much education they recieve.

# part iii.
summary(ivlm <- ivreg(data=fertdf,children~educ+age+agesq|frsthalf+age+agesq ))
summary(ivlm)
summary(lm2)
# Hausman test by hand (this code is in the vignette for AER.  Is there a better
# way without using systemfit?)
# I don't know of a better way without systemfit. Systemfit is not difficult though. It
# can do single-equation models fine.

haus(ivlm,lm2)
# The effects are statistically different and post-IV value is larger and still
# negative.

# part iv.
summary(lmadd<-lm(data=fertdf, children~educ+age+agesq+electric+tv+bicycle))
summary(ivlmadd<-ivreg(data=fertdf, children~educ+age+agesq+electric+tv+bicycle|frsthalf+age+agesq+electric+tv+bicycle))
haus(ivlmadd,lmadd)
# TV may have a negative effect on fertility because it is a proxy for socioeconomic
# status, and those with more income have less children.  Likely what has happened
# is that educ was biased downwards 
