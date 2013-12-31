


library(plyr)

# /TAA research/consumer exp/intrvw12/fmli121x.dta

i <- interview.round[cohort]

c(paste0(year, 2:4), paste0(year+1, 1))

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
  rowMeans(c.exp.for.ids.df[, c("age_ref", "age2")], na.rm = TRUE) <= 35 & 
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

# TODO: need to do something about the x years, maybe

}


master.df<-do.call(rbind, master.list )

master.df<-master.df[, c("hous", "fdhome", "appar", "trans")]

master.df <- master.df/12

medians.df <- apply(master.df, 2, FUN=median)


median.CIs.df<-apply(master.df, 2, FUN=function(x) {
  bootmed <-apply(matrix(sample(x,rep=TRUE,10^4*length(x)),nrow=10^4),1,median)
  quantile(bootmed,c(.05,0.95))
  }
)
# Thanks to http://stats.stackexchange.com/questions/21103/confidence-interval-for-median

medians.df<-rbind(median.CIs.df[1, , drop=FALSE], 
  median=medians.df, 
  median.CIs.df[2, , drop=FALSE])

barplot(medians.df, beside=TRUE)

medians.df






x <- master.df$fdhome
bootmed <- 
quantile(bootmed,c(.05,0.95))
# Thanks to http://stats.stackexchange.com/questions/21103/confidence-interval-for-median




test <-as.data.frame(matrix(runif(50), 5))

test[test<.5]<-NA
test


# Housing costs, including utilities, etc.
median.exp.ls[["houscq"]] <- median(c.exp.subset.df$houscq[c.exp.subset.df$houscq>0], na.rm=TRUE)
# length(c.exp.subset.df$fdhomecq[c.exp.subset.df$fdhomecq>0])

#FDHOMECQ
# Food at home current quarter
median.exp.ls[["fdhomecq"]] <- median(c.exp.subset.df$fdhomecq[c.exp.subset.df$fdhomecq>0], na.rm=TRUE)
# length(c.exp.subset.df$fdhomecq[c.exp.subset.df$fdhomecq>0])

# Food away from home current quarter
median.exp.ls[["fdawaycq"]] <- median(c.exp.subset.df$fdawaycq[c.exp.subset.df$fdawaycq>0], na.rm=TRUE)
# length(c.exp.subset.df$fdawaycq[c.exp.subset.df$fdawaycq>0])

# APPARCQ Apparel and services this quarter 
median.exp.ls[["apparcq"]] <- median(c.exp.subset.df$apparcq[c.exp.subset.df$apparcq>0], na.rm=TRUE)
# length(c.exp.subset.df$apparcq[c.exp.subset.df$apparcq>0])
  
# TRANSCQ: Transportation this quarter: CARTKNPQ + CARTKUPQ + OTHVEHPQ + GASMOPQ + VEHFINPQ + MAINRPPQ + VEHINSPQ + VRNTLOPQ + PUBTRAPQ
median.exp.ls[["transcq"]] <- median(c.exp.subset.df$transcq[c.exp.subset.df$transcq>0], na.rm=TRUE)








year <-12

  
library(foreign)

c.exp.for.ids.df<-read.dta(paste0(work.dir, "/intrvw", year, "/fmli", year, "2.dta"))

c.exp.ids <- c.exp.for.ids.df[
  c.exp.for.ids.df$inclass %in% c("02", "03", "04") & 
  rowMeans(c.exp.for.ids.df[, c("age_ref", "age2")], na.rm = TRUE) <= 35 & 
  rowMeans(c.exp.for.ids.df[, c("age_ref", "age2")], na.rm = TRUE) >= 22 &
  c.exp.for.ids.df$high_edu %in% c( "15", "16"), "cuid"]

c.exp.list<-list()

for (i in c(paste0(year, 2:4), paste0(year+1, 1)) ) {
  temp.c.exp.list<-read.dta(paste0(work.dir, "/intrvw", year, "/fmli", i, ".dta"))  
  c.exp.list[[i]] <- temp.c.exp.list[temp.c.exp.list$cuid %in% c.exp.ids, ]
}

c.exp.df<-do.call(rbind, c.exp.list)

table(duplicated(c.exp.df$newid))


nrow(c.exp.df)
nrow(c.exp.list[[1]])
nrow(c.exp.list[[2]])
nrow(c.exp.list[[3]])
nrow(c.exp.list[[4]])



# Revamp:


year <-12

full.yr.medians.ls <- list()

for (i in c(paste0(year, 2:4), paste0(year+1, 1)) ) {
  
# Data dictionary at http://www.bls.gov/cex/2012/csxintvwdata.pdf
  
median.exp.ls <-list()

c.exp.temp.df<-read.dta(paste0(work.dir, "/intrvw", year, "/fmli", i, ".dta"))  

c.exp.subset.df <- c.exp.temp.df[
  c.exp.temp.df$inclass %in% c("02", "03", "04") & 
  rowMeans(c.exp.temp.df[, c("age_ref", "age2")], na.rm = TRUE) <= 35 & 
  rowMeans(c.exp.temp.df[, c("age_ref", "age2")], na.rm = TRUE) >= 22 &
  c.exp.temp.df$high_edu %in% c( "15", "16"), ]

  
# Housing costs, including utilities, etc.
median.exp.ls[["houscq"]] <- median(c.exp.subset.df$houscq[c.exp.subset.df$houscq>0], na.rm=TRUE)
# length(c.exp.subset.df$fdhomecq[c.exp.subset.df$fdhomecq>0])

#FDHOMECQ
# Food at home current quarter
median.exp.ls[["fdhomecq"]] <- median(c.exp.subset.df$fdhomecq[c.exp.subset.df$fdhomecq>0], na.rm=TRUE)
# length(c.exp.subset.df$fdhomecq[c.exp.subset.df$fdhomecq>0])

# Food away from home current quarter
median.exp.ls[["fdawaycq"]] <- median(c.exp.subset.df$fdawaycq[c.exp.subset.df$fdawaycq>0], na.rm=TRUE)
# length(c.exp.subset.df$fdawaycq[c.exp.subset.df$fdawaycq>0])

# APPARCQ Apparel and services this quarter 
median.exp.ls[["apparcq"]] <- median(c.exp.subset.df$apparcq[c.exp.subset.df$apparcq>0], na.rm=TRUE)
# length(c.exp.subset.df$apparcq[c.exp.subset.df$apparcq>0])
  
# TRANSCQ: Transportation this quarter: CARTKNPQ + CARTKUPQ + OTHVEHPQ + GASMOPQ + VEHFINPQ + MAINRPPQ + VEHINSPQ + VRNTLOPQ + PUBTRAPQ
median.exp.ls[["transcq"]] <- median(c.exp.subset.df$transcq[c.exp.subset.df$transcq>0], na.rm=TRUE)

full.yr.medians.ls[[i]] <- as.data.frame(median.exp.ls)
  
}

full.yr.medians.df<- do.call(rbind, full.yr.medians.ls)

full.yr.medians.df



  c.exp.list[[i]] <- temp.c.exp.list[temp.c.exp.list$cuid %in% c.exp.ids, ]
}



# "Demographic characteristics, such as family size, refer to the CU status on the date of the interview. Demographic characteristic information may change between interviews if, for example, a member enters or leaves the CU. Income variables contain annual values. Income data are collected in the second and fifth interviews only and cover the 12 months prior to the date of interview. Income data collected in the second interview are copied to the third and fourth interviews. Income data are updated only if a CU member over 13 is new to the CU or has not worked in previous interviews and has now started working."




c.exp.df[1:10 , c("newid", "cuid")]

substr(x=c.exp.df[1:10 , c("newid")], start=7, stop=7)
  












c.exp.comp.df <- c.exp.df[
  c.exp.df$inclass=="03" & 
  c.exp.df$region=="2" & 
  rowMeans(c.exp.df[, c("age_ref", "age2")], na.rm = TRUE) <= 35 & 
  rowMeans(c.exp.df[, c("age_ref", "age2")], na.rm = TRUE) >= 22 &
  c.exp.df$high_edu %in% c( "15", "16"), ]
  
c.exp.comp.df <- c.exp.df[
  c.exp.df$inclass %in% c("02", "03", "04") & 
  rowMeans(c.exp.df[, c("age_ref", "age2")], na.rm = TRUE) <= 35 & 
  rowMeans(c.exp.df[, c("age_ref", "age2")], na.rm = TRUE) >= 22 &
  c.exp.df$high_edu %in% c( "15", "16"), ]

nrow(c.exp.comp.df)
# [1] 42


# Housing costs, including utilities, etc.
median(c.exp.comp.df$houscq[c.exp.comp.df$houscq>0], na.rm=TRUE)
length(c.exp.comp.df$houscq[c.exp.comp.df$houscq>0])


#FDHOMECQ
# Food at home current quarter
median(c.exp.comp.df$fdhomecq[c.exp.comp.df$fdhomecq>0], na.rm=TRUE)
length(c.exp.comp.df$fdhomecq[c.exp.comp.df$fdhomecq>0])

# Food away from home current quarter
median(c.exp.comp.df$fdawaycq[c.exp.comp.df$fdawaycq>0], na.rm=TRUE)
length(c.exp.comp.df$fdawaycq[c.exp.comp.df$fdawaycq>0])

# APPARCQ Apparel and services this quarter 
median(c.exp.comp.df$apparcq[c.exp.comp.df$apparcq>0], na.rm=TRUE)
length(c.exp.comp.df$apparcq[c.exp.comp.df$apparcq>0])





