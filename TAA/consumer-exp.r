

# http://www.bls.gov/cex/pumdhome.htm
library(plyr)
library(foreign)

# /TAA research/consumer exp/intrvw12/fmli121x.dta

i <- interview.round[cohort]

# c(paste0(year, 2:4), paste0(year+1, 1))

interview.round <- c( paste0( rep(10:12, each=5), c(1, "1x", 2:4) )[-1], "131")

master.list <-list()

# 12 cohorts

cohort <- 10

for (cohort in 1:11) {

if (substr(interview.round[cohort], 3, 4)=="1x" & cohort!=1) {next}
if (substr(interview.round[cohort], 3, 4)=="1"  & cohort!=1) {
   c.exp.for.ids.df <- rbind( read.dta(paste0(work.dir, "/fmli", interview.round[cohort], ".dta"))[, c("inclass", "age_ref", "age2", "educ_ref", "educa2", "cuid", "newid")],  
    read.dta(paste0(work.dir, "/fmli", interview.round[cohort], "x.dta"))[, c("inclass", "age_ref", "age2", "educ_ref", "educa2", "cuid", "newid")] )
   c.exp.for.ids.df<-c.exp.for.ids.df[ !duplicated(c.exp.for.ids.df$cuid), ]    
} else {  
  c.exp.for.ids.df<-read.dta(paste0(work.dir, "/fmli", interview.round[cohort], ".dta")) 
}

c.exp.ids <- c.exp.for.ids.df[
  c.exp.for.ids.df$inclass %in% c("02", "03", "04") & 
  rowMeans(c.exp.for.ids.df[, c("age_ref", "age2")], na.rm = TRUE) <= 40 & 
  rowMeans(c.exp.for.ids.df[, c("age_ref", "age2")], na.rm = TRUE) >= 22 &
  apply(c.exp.for.ids.df[, c("educ_ref", "educa2")], 1, FUN=function(x) max(as.numeric(x), na.rm=TRUE))  %in% 15:16 & 
  substr(x=c.exp.for.ids.df$newid, start=7, stop=7)==2
  , "cuid"]

c.exp.list<-list()
# c(paste0(year, 2:4), paste0(year+1, 1))
  
for (i in  interview.round[cohort:(cohort+4)]) {
  if (substr(i, 3, 4)=="1x" & cohort!=1) {next}
  if (substr(i, 3, 4)=="1"  & cohort!=1) {
     temp.c.exp.list <- rbind.fill( read.dta(paste0(work.dir, "/fmli", i, ".dta")),  
      read.dta(paste0(work.dir, "/fmli", i, "x.dta")) )
     temp.c.exp.list<-temp.c.exp.list[ !duplicated(temp.c.exp.list$cuid), ]    
  } else {  
    temp.c.exp.list<-read.dta(paste0(work.dir, "/fmli", i, ".dta")) 
  }
  c.exp.list[[i]] <- temp.c.exp.list[temp.c.exp.list$cuid %in% c.exp.ids, 
    c("newid", "cuid", "houscq", "houspq", "fdhomecq", "fdhomepq", "apparcq", "apparpq", "transcq", "transpq")]
}

c.exp.df<-do.call(rbind, c.exp.list  )

# table(substr(x=c.exp.df$newid, start=7, stop=7))
# definitely have attrition

interview.times.tab <- table(c.exp.df$cuid)

cat( names(interview.times.tab)[interview.times.tab == 5], "\n")

c.exp.df<- c.exp.df[c.exp.df$cuid %in% names(interview.times.tab)[interview.times.tab == 4], ]
  
if (nrow(c.exp.df)==0) {cat(cohort); next}

c.exp.df<-aggregate(c.exp.df[, c("houscq", "houspq", "fdhomecq", "fdhomepq", "apparcq", "apparpq", "transcq", "transpq")], by=list(c.exp.df$cuid), FUN=sum, na.rm=TRUE)

c.exp.df$hous <- rowSums(c.exp.df[, c("houscq", "houspq")])
c.exp.df$fdhome <- rowSums(c.exp.df[, c("fdhomecq", "fdhomepq")])
c.exp.df$appar <- rowSums(c.exp.df[, c("apparcq", "apparpq")])
c.exp.df$trans <- rowSums(c.exp.df[, c("transcq", "transpq")])

master.list[[cohort]] <- c.exp.df

}


master.df<-do.call(rbind, master.list )

master.df<-master.df[, c("hous", "fdhome", "appar", "trans")]

master.df <- master.df/12

medians.df <- apply(master.df, 2, FUN=median)


median.CIs.df<-apply(master.df, 2, FUN=function(x) {
  bootmed <- apply(matrix(sample(x,rep=TRUE,10^5*length(x)),nrow=10^5),1,median)
  quantile(bootmed, c(.05,0.95))
  }
)
# Thanks to http://stats.stackexchange.com/questions/21103/confidence-interval-for-median

medians.df<-rbind(median.CIs.df[1, , drop=FALSE], 
  median=medians.df, 
  median.CIs.df[2, , drop=FALSE])

barplot(medians.df, beside=TRUE)

medians.df


# Alterantive confidence intervals:
# apply(master.df, 2, FUN=function(x) wilcox.test(x, conf.int=TRUE, conf.level=.90)$conf.int)
# However, according to this, it is no good: http://r.789695.n4.nabble.com/Simple-95-confidence-interval-for-a-median-td3517769.html




# Housing costs, including utilities, etc.
# "houscq"]] 

#FDHOMECQ

# APPARCQ Apparel and services this quarter 
  
# TRANSCQ: Transportation this quarter: CARTKNPQ + CARTKUPQ + OTHVEHPQ + GASMOPQ + VEHFINPQ + MAINRPPQ + VEHINSPQ + VRNTLOPQ + PUBTRAPQ



# "Demographic characteristics, such as family size, refer to the CU status on the date of the interview. Demographic characteristic information may change between interviews if, for example, a member enters or leaves the CU. Income variables contain annual values. Income data are collected in the second and fifth interviews only and cover the 12 months prior to the date of interview. Income data collected in the second interview are copied to the third and fourth interviews. Income data are updated only if a CU member over 13 is new to the CU or has not worked in previous interviews and has now started working."

# http://stackoverflow.com/questions/3357743/replacing-character-values-with-na-in-a-data-frame?rq=1


