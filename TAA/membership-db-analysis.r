





















# full.outer.merge.df <- read.csv("/Users/travismcarthur/Desktop/TAA work/Grad student database/Outer merge Enr Emp Mem.csv",  fileEncoding="Latin1", stringsAsFactors=FALSE) #, na.strings=c("", "NA"))


t(t(table(full.outer.merge.df$Fte.EM)))

t(t(table(full.outer.merge.df$Uw.Jobcode.Descr.EM)))


thirty.three.appt.prop.table<- with(full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="LECTURER (SA)", ], 
prop.table(table( ACAD_PLAN_LONG_DESCR, Fte.EM <= .4 &  Fte.EM >= .3), margin=1))[, 2]

thirty.three.appt.freq.table <- with(full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="LECTURER (SA)", ], 
table( ACAD_PLAN_LONG_DESCR, Fte.EM <= .4 &  Fte.EM >= .3))

stopifnot(all(names(thirty.three.appt.prop.table) == names(thirty.three.appt.freq.table)))
# The above _must_ be true, since we are going to combine based only on position

thirty.three.appt.ENROLLMENT.dept.df <- data.frame(enrolled.department=names(thirty.three.appt.prop.table),
  proportion.at.30_40.perc=thirty.three.appt.prop.table, number.of.grads.at.30_40.perc=thirty.three.appt.freq.table[, 2], 
  number.of.grads.at.ANOTHER.perc=thirty.three.appt.freq.table[, 1], stringsAsFactors=FALSE) 




thirty.three.appt.prop.table<- with(full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="LECTURER (SA)", ], 
prop.table(table( department_for_match.EM, Fte.EM <= .4 &  Fte.EM >= .3), margin=1))[, 2]

thirty.three.appt.freq.table <- with(full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="LECTURER (SA)", ], 
table( department_for_match.EM, Fte.EM <= .4 &  Fte.EM >= .3))

stopifnot(all(names(thirty.three.appt.prop.table) == names(thirty.three.appt.freq.table)))
# The above _must_ be true, since we are going to combine based only on position

thirty.three.appt.EMPLOYMENT.dept.df <- data.frame(employing.department=names(thirty.three.appt.prop.table),
  proportion.at.30_40.perc=thirty.three.appt.prop.table, number.of.grads.at.30_40.perc=thirty.three.appt.freq.table[, 2], 
  number.of.grads.at.ANOTHER.perc=thirty.three.appt.freq.table[, 1], stringsAsFactors=FALSE) 



write.csv(thirty.three.appt.ENROLLMENT.dept.df, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Enrolled-based percentage at 30-40 percent.csv", row.names=FALSE, fileEncoding="Latin1")
write.csv(thirty.three.appt.EMPLOYMENT.dept.df, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Employment-based percentage at 30-40 percent.csv", row.names=FALSE, fileEncoding="Latin1")



t(t(table(full.outer.merge.df[full.outer.merge.df$department_for_match.EM=="ENGLISH", "ACAD_PLAN_LONG_DESCR"])))

t(t(table(full.outer.merge.df[full.outer.merge.df$ACAD_PLAN_LONG_DESCR=="English PHD", "department_for_match.EM"])))










table(membership.after.dedup.df$membership.employment.key!="No Match w Emp")
prop.table(table(membership.after.dedup.df$membership.employment.key!="No Match w Emp"))
table(employment.after.dedup.df$enrollment.employment.key!="No Match w Enr")
prop.table(table(employment.after.dedup.df$enrollment.employment.key!="No Match w Enr"))

table(employment.after.dedup.df$Uw.Jobcode.Descr.First, employment.after.dedup.df$enrollment.employment.key!="No Match w Enr")

View(employment.after.dedup.df[employment.after.dedup.df$enrollment.employment.key=="No Match w Enr", ])


table(Emp = membership.after.dedup.df$membership.employment.key!="No Match w Emp", 
      Enr=membership.after.dedup.df$membership.enrollment.key!="No Match w Enr")
prop.table(table(Emp = membership.after.dedup.df$membership.employment.key!="No Match w Emp", 
      Enr=membership.after.dedup.df$membership.enrollment.key!="No Match w Enr"))
# So about 16% of members can't be matched in either database

table(Enr = employment.after.dedup.df$enrollment.employment.key!="No Match w Enr", 
      Mem=employment.after.dedup.df$membership.employment.key!="No Match w Mem")



table(Emp = enrollment.after.dedup.df$enrollment.employment.key!="No Match w Emp", 
      Mem=enrollment.after.dedup.df$membership.enrollment.key!="No Match w Mem")

# TODO: SLight diferences in the numbers in the lower-right corner of the three table
# Shouldn't all three match, since it's just saying "can I find matches in BOTH other databases"?

# membership.employment.key	membership.enrollment.key
# No Match w Emp	No Match w Enr


# getpairs.output[getpairs.output$L_name_for_rec_linkage.2=="BARANOWSKI",][1:3, ]

# TODO: and at the end sort it by last name, first name?



# summary(glm(is.TAA.member ~ gre + gpa + rank, family=binomial(link="probit"), data=full.outer.merge.df) )


full.outer.merge.df$BIRTHDATE.formatted <- 
d.tmp <- as.Date(full.outer.merge.df$BIRTHDATE, format="%m/%d/%y")
full.outer.merge.df$BIRTHDATE.formatted <- as.Date(ifelse(d.tmp > Sys.Date(), format(d.tmp, "19%y-%m-%d"), format(d.tmp)))
# This prevents "births" in the future - i.e. it converts births after '15 to 1915 and later, not 2015 and later
# Thanks to http://stackoverflow.com/questions/9508747/add-correct-century-to-dates-with-year-provided-as-year-without-century-y
# hist(full.outer.merge.df$BIRTHDATE.formatted, breaks="years", las=3, cex.axis=.7 )
# hist(log((-1)*as.numeric(full.outer.merge.df$BIRTHDATE.formatted) + as.numeric(max(full.outer.merge.df$BIRTHDATE.formatted, na.rm = TRUE))), las=3, cex.axis=.7 )
# Wow. Like log-triangular. http://ecolego.facilia.se/ecolego/show/Log-Triangular%20Distribution
# http://www.minem.gob.pe/minem/archivos/file/dgaam/publicaciones/curso_cierreminas/02_Técnico/03_Calidad%20de%20Aguas/TecCalAg-L4_GoldSim_App%20A-B.pdf
# summary(full.outer.merge.df$BIRTHDATE.formatted)

full.outer.merge.df$is.intl.student <- (! full.outer.merge.df$Home.Country.EN %in% c("", "United States Territory", "US Minor Outlying Islands", "Puerto Rico" ) )
# So excluding US territories from the classifoctaion of intl student here.
# Seems there is a problem here, since none of these are
# table(full.outer.merge.df$is.intl.student)

full.outer.merge.df$Uw.Jobcode.Descr.EM[full.outer.merge.df$Uw.Jobcode.Descr.EM=="PRG AST-GRADER/READER" ] <- "PRJ AST-GRADER/READER"
full.outer.merge.df$Uw.Jobcode.Descr.EM[full.outer.merge.df$Uw.Jobcode.Descr.EM=="PROGRAM ASST-REG" ] <- "PROJECT ASST-REG"
# Consolidating program assistants into project assistants because it is messing up our regressions

table(is.na(full.outer.merge.df$Home.Country.EN))
# For now, none are NA

# install.packages("mfx")
library("mfx")
# install.packages("erer")
library("erer")





# install.packages("glmx")
library("glmx")

targ.formula <- as.numeric(is.TAA.member) ~ log(age.EN) +  Uw.Jobcode.Descr.EM + is.intl.student + degree.type.simple 

# Note that hetglm will make the variance and mean eqn the same if only the mean eqn is specified.

het.glm.comparison <- hetglm(targ.formula    , 
                             data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="", ],
      family = binomial(link = "probit"), control = hetglm.control(method="BFGS", trace=T, reltol=1e-12, maxit = 2000 )) #, link.scale = "identity") 


oglmx.output.robust.con <- oglmx.controlled(formulaMEAN=  targ.formula   , 
                      formulaSD =targ.formula , 
                      data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ], 
                      link = "probit", constantMEAN = T, analhessian = T, savemodelframe=F, robust=TRUE, SameModelMEANSD=TRUE,

        constantSD = TRUE, delta = 0, threshparam = 0, method="NR", print.level=3, iterlim = 0, start=coef(het.glm.comparison)) #, SameModelMEANSD=TRUE)


margins.oglmx(oglmx.output.robust.con, outcomes=1, dummyzero = TRUE)

McFaddensR2.oglmx(oglmx.output.robust.con)
summary(oglmx.output.robust.con)

with(full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ],
     ftable(Uw.Jobcode.Descr.EM, is.intl.student, is.TAA.member) )

degree.type.simple, 













# age.EN + 
maBina(glm(is.TAA.member ~ age.EN + Uw.Jobcode.Descr.EM + is.intl.student, family=binomial(link="probit"), data=full.outer.merge.df, x=TRUE), x.mean = F, rev.dum = F, digits = 3,
      subset.name = NULL, subset.value)

bptest(is.TAA.member ~   age.EN + Uw.Jobcode.Descr.EM + is.intl.student, varformula = ~ is.intl.student,  data = full.outer.merge.df)

# Below is an attempt to make sense of where the heteroskedastciity it coming from (probably age).
# But I can make neither hide not tail of it. I used these resources:
# http://stats.stackexchange.com/questions/82682/how-do-i-interpret-this-fitted-vs-residuals-plot
# http://stats.stackexchange.com/questions/33028/measures-of-residuals-heteroscedasticity

test.lm <- lm(is.TAA.member ~ age.EN + Uw.Jobcode.Descr.EM + is.intl.student,  data=full.outer.merge.df)
summary(test.lm)$adj.r.squared

plot(resid(test.lm), test.lm$model$age.EN)

 plot(fitted(test.lm), residuals(test.lm))

res <- residuals(test.lm)
pred <- predict(test.lm)
n.bins <- 30
bins <- cut(pred, quantile(pred, probs = seq(0, 1, 1/n.bins)))
b <- boxplot(res ~ bins, boxwex=1/2, main="Residuals vs. Predicted",
             xlab="Predicted", ylab="Residual")
colors <- hsv(seq(2/6, 1, 1/6))
temp <- sapply(1:5, function(i) lines(lowess(1:n.bins, b$stats[i,], f=.25), 
        col=colors[i], lwd=2))





summary(glm(is.TAA.member ~ age.EN + Uw.Jobcode.Descr.EM + is.intl.student, family=binomial(link="probit"), data=full.outer.merge.df) )


probitmfx(formula = is.TAA.member ~ age.EN + Uw.Jobcode.Descr.EM + is.intl.student, 
          data=full.outer.merge.df, atmean = TRUE) # , robust = TRUE)
# robust = TRUE changes everything above. not sure what to do

logitmfx(formula = is.TAA.member ~  is.intl.student, 
          data=full.outer.merge.df, atmean = TRUE, robust = TRUE)

Acad.Group.Long.Descr.EN

table(full.outer.merge.df$is.intl.student, full.outer.merge.df$is.TAA.member)
with(full.outer.merge.df[with(full.outer.merge.df,complete.cases(BIRTHDATE.formatted, is.intl.student, Uw.Jobcode.Descr.EM)), ],
     table( Uw.Jobcode.Descr.EM,  is.intl.student) )
with(full.outer.merge.df[with(full.outer.merge.df,complete.cases(BIRTHDATE.formatted, is.intl.student, Uw.Jobcode.Descr.EM)), ],
     table( is.TAA.member,  is.intl.student) )
round(prop.table(table(full.outer.merge.df$Uw.Jobcode.Descr.EM, full.outer.merge.df$is.TAA.member), margin=1)*100, 1)
round(prop.table(table(full.outer.merge.df$degree.type.simple, full.outer.merge.df$is.TAA.member), margin=1)*100, 1)


round(prop.table(table(full.outer.merge.df$Acad.Group.Long.Descr.EN, full.outer.merge.df$is.TAA.member), margin=1)*100, 1)
round(prop.table(table(full.outer.merge.df$PRIMARY_ACADEMIC_GROUP, full.outer.merge.df$is.TAA.member), margin=1)*100, 2)
table(full.outer.merge.df$Uw.Jobcode.Descr.EM, full.outer.merge.df$is.intl.student)
chisq.test(table(full.outer.merge.df$Uw.Jobcode.Descr.EM, full.outer.merge.df$is.TAA.member))
# sample(1:3, nrow(full.outer.merge.df), replace=T)
# View(full.outer.merge.df[1:10, ])

# install.packages("lubridate")
library("lubridate")
# install.packages("oglmx")
library("oglmx")
# Awesome package!!! # as.numeric(is.TAA.member)

full.outer.merge.df$age.EN <- year(as.period(as.Date(Sys.Date()) - full.outer.merge.df$BIRTHDATE.formatted, unit="year"))

mail.state.EN=="WI"

full.outer.merge.df$is.intl.student <- as.factor(full.outer.merge.df$is.intl.student)

with(full.outer.merge.df, cor(age.EN, as.numeric(is.TAA.member), use="complete.obs"))

str(full.outer.merge.df$age.EN)
summary(full.outer.merge.df$age.EN)
with(full.outer.merge.df, summary(lm(as.numeric(is.TAA.member) ~ I(age.EN/100) ))
with(full.outer.merge.df, summary(lm(as.numeric(is.TAA.member) ~ is.intl.student )))
with(full.outer.merge.df, summary(lm(as.numeric(is.TAA.member) ~ Fte.EM )))
with(full.outer.merge.df, summary(lm(as.numeric(is.TAA.member) ~ age.EN + Fte.EM + (Mail.State.EN=="WI") + is.intl.student + as.factor(Uw.Jobcode.Descr.EM) )))

levels(as.factor(full.outer.merge.df$Uw.Jobcode.Descr.EM))

full.outer.merge.df$Fte.EM[is.na(full.outer.merge.df$Fte.EM)] <- 0
full.outer.merge.df$is.intl.student <- as.character(full.outer.merge.df$is.intl.student)
full.outer.merge.df$is.intl.student[1:200] <- "Maybe"

# + as.numeric(is.intl.student)
# PRIMARY_ACADEMIC_GROUP # as.factor(Uw.Jobcode.Descr.EM)
oglmx.output.robust.con <- oglmx.controlled(formulaMEAN= as.numeric(is.TAA.member) ~ log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student + degree.type.simple    , 
                      formulaSD = ~ log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student + degree.type.simple , 
                      data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ], 
                      link = "probit", constantMEAN = T, analhessian = T, savemodelframe=F, robust=TRUE,

        constantSD = TRUE, delta = 0, threshparam = 0, method="NR", print.level=0, iterlim = 1, start=coef(het.glm.comparison)) #, SameModelMEANSD=TRUE)
# robust=TRUE, savemodelframe=TRUE, 
# Must do the complete.cases stuff since oglmx drops the factor levels that do not appear in the final dataset _after_ checking
# the error condition, but hetglm does it before
# Got the above extra arguments from examining the source code of probit.reg, because I have just binary outcome. Seems
# that the delta = 0 and threshparam = 0  arguments are important to make this actually work.
# Leacing out + as.factor(Uw.Jobcode.Descr.EM) for now because it doesn't seems to like too mnay vars in the variance eqn
# Not sure what to do with the "robust" argument
McFaddensR2.oglmx(oglmx.output.robust.con)
summary(oglmx.output.robust.con)
margins.oglmx(oglmx.output, dummyzero = TRUE)
margins.oglmx(oglmx.output.robust, dummyzero = TRUE)
margins.oglmx(oglmx.output.robust.con, outcomes=1, dummyzero = TRUE)
probitmfx(formula = is.TAA.member ~ log(age.EN) + Uw.Jobcode.Descr.EM + is.intl.student, data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ], atmean = TRUE)


              , ascontinuous=TRUE)
              # AME=TRUE, ascontinuous=TRUE)

probitmfx(formula = as.numeric(degree.type.simple=="Doctorate") ~ log(age.EN) , data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ], atmean = TRUE)


summary(lm(as.numeric(degree.type.simple=="Doctorate") ~ log(age.EN) , data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ]))

probitmfx(formula = as.numeric(is.TAA.member) ~ degree.type.simple , data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ], atmean = TRUE)

summary(lm(as.numeric(is.TAA.member) ~ degree.type.simple , data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="" & with(full.outer.merge.df,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ]))





#ok do bootstrap:


n.straps <- 100

boot.collect <- list()
set.seed(100)
for ( boot.num in 1:n.straps) {
# reltol=1e-12, 
  
  full.outer.merge.df.boot <- full.outer.merge.df[sample(nrow(full.outer.merge.df), replace=TRUE),]
  
  het.glm.comparison <- hetglm(as.numeric(is.TAA.member) ~  log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student  | log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student   , 
                             data=full.outer.merge.df.boot,
      family = binomial(link = "probit"), control = hetglm.control(method="BFGS", trace=F, reltol=1e-12, maxit = 20000 )) 
  

  oglmx.output.robust.con <- oglmx.controlled(formulaMEAN= as.numeric(is.TAA.member) ~ log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student    , 
                      formulaSD = ~ log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student , 
                      data=full.outer.merge.df.boot[with(full.outer.merge.df.boot,complete.cases(age.EN, Uw.Jobcode.Descr.EM, is.intl.student)), ], 
                      link = "probit", constantMEAN = T, analhessian = T, 

        constantSD = TRUE, delta = 0, threshparam = 0, method="NR", print.level=3, iterlim = 0, start=coef(het.glm.comparison), robust=TRUE) #, SameModelMEANSD


  boot.collect[[boot.num]]<- margins.oglmx(oglmx.output.robust.con, outcomes=1, dummyzero = TRUE)[[1]][, 1]
  cat(boot.num, as.character(Sys.time()), "\n")

}


cor(log(full.outer.merge.df$age.EN), as.numeric(full.outer.merge.df$is.TAA.member), use="complete.obs" )

margins.oglmx(oglmx.output.robust.con, outcomes=1, location=c(log(25), rep(0, 7)), AME=TRUE)
dummyzero = TRUE, 

table(sapply(boot.collect, length))
# A-ok

boot.collect.df <- do.call(rbind, boot.collect)

summary(as.data.frame(boot.collect.df))











new.employment.data.df <- read.csv(file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Employment data - Nov 2015 Records Request.csv", fileEncoding="Latin1", stringsAsFactors = FALSE)

# table(new.employment.data.df$Uw.Jobcode.Descr)
new.employment.data.df.only.TAs <- new.employment.data.df[new.employment.data.df$Uw.Jobcode.Descr %in% c("TEACH ASST STANDARD", "TEACH ASST SENIOR"),  ]

length(unique(new.employment.data.df.only.TAs$Emplid))

length(unique(new.employment.data.df$Emplid))
nrow(new.employment.data.df)

round(prop.table(table(table(new.employment.data.df$Emplid)))*100, 2)
table(table(new.employment.data.df$Emplid))


with(new.employment.data.df, table( Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3))

with(new.employment.data.df, round(prop.table(table( Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3))*100, digits=2))
# with(new.employment.data.df, prop.table(table( Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3))) #DOuple-checking something here. we get the same 20.49% below for a different calc - want to triple check that there is no error by looking at all the decimal digits, not just two of them.


with(new.employment.data.df, length(unique(Emplid[Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3])))
with(new.employment.data.df, length(unique(Emplid)))

round(100*(with(new.employment.data.df, length(unique(Emplid[Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3]))) /
  with(new.employment.data.df, length(unique(Emplid))) ), digits=2)
 
# new.employment.data.df <- new.employment.data.df[!new.employment.data.df$Uw.Jobcode.Descr %in% c("PRG AST-GRADER/READER", "PRJ AST-GRADER/READER"), ]
fte.sum.by.student.agg <- aggregate(Uw.Dv.Job.Fte ~ Emplid, data= new.employment.data.df, FUN=sum)
# This will be a problem if there are any NA's in data received later from HR
# summary(fte.sum.by.student.agg$Uw.Dv.Job.Fte)
# summary(new.employment.data.df$Uw.Dv.Job.Fte)

# Journal-Sentinel (JS) Milwaukee:
 fte.sum.by.student.agg <- aggregate(Uw.Dv.Job.Fte ~ Emplid, data= new.employment.data.df[!new.employment.data.df$Uw.Jobcode.Descr %in% c("PRG AST-GRADER/READER", "PRJ AST-GRADER/READER"), ], FUN=sum)
# This will be a problem if there are any NA's in data received later from HR
 summary(fte.sum.by.student.agg$Uw.Dv.Job.Fte)
 summary(new.employment.data.df$Uw.Dv.Job.Fte)
new.employment.data.df$Uw.Comprate <- as.numeric(gsub(",", "", new.employment.data.df$Uw.Comprate))
new.employment.data.df$compensation <- new.employment.data.df$Uw.Dv.Job.Fte * new.employment.data.df$Uw.Comprate
compensation.by.student.agg <- aggregate(compensation ~ Emplid, data= new.employment.data.df, FUN=sum)
summary(compensation.by.student.agg$compensation)
compensation.by.student.agg <- aggregate(compensation ~ Emplid, data= new.employment.data.df[!new.employment.data.df$Uw.Jobcode.Descr %in% c("PRG AST-GRADER/READER", "PRJ AST-GRADER/READER"), ], FUN=sum)
summary(compensation.by.student.agg$compensation)





with(fte.sum.by.student.agg, length(Emplid[Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3]))
nrow(fte.sum.by.student.agg)

round(100*(with(fte.sum.by.student.agg, length(Emplid[Uw.Dv.Job.Fte <= .4 &  Uw.Dv.Job.Fte >= .3]) /
  nrow(fte.sum.by.student.agg))), digits=2)


hist(new.employment.data.df$Uw.Dv.Job.Fte*100, breaks=20, axes = FALSE, 
     main="Fig 1: Histogram of number of appointments\nby appointment percentage", 
     xlab="Appointment percentage", ylab="Number of appointments", col="red", cex.main=.7, cex.lab=.7)
axis(1, at= (0:10)*10, labels = (0:10)*10, cex.axis=.5)
axis(2, cex.axis=.55)

#png(filename = "/Users/travismcarthur/Desktop/TAA work/Grad student database/Hist num appointments by perc.png", width = 300, height=300)
# Xquartz is choking on this for some reason

fte.sum.by.student.agg.topcoded <- fte.sum.by.student.agg
fte.sum.by.student.agg.topcoded$Uw.Dv.Job.Fte[fte.sum.by.student.agg.topcoded$Uw.Dv.Job.Fte > 1] <- 1

hist(fte.sum.by.student.agg.topcoded$Uw.Dv.Job.Fte*100, breaks=20, axes = FALSE, 
     main="Fig 2: Histogram of sum of appointment levels\nby number of grad assistants holding\nsuch appointments", 
     xlab="Sum of appointment percentage", ylab="Number of assistants", col="red", cex.main=.7, cex.lab=.7)
axis(1, at= (0:10)*10, labels = (0:10)*10, cex.axis=.5)
axis(2, cex.axis=.55)




table(new.employment.data.df$Uw.Dv.Job.Fte[new.employment.data.df$Uw.Jobcode.Descr %in% c("PRJ AST-GRADER/READER", "PRG AST-GRADER/READER")])

with(new.employment.data.df[new.employment.data.df$Uw.Jobcode.Descr %in% c("PRJ AST-GRADER/READER", "PRG AST-GRADER/READER"), ], table( Uw.Dv.Job.Fte,  Uw.Jobcode.Descr ))
# Ok, it seems that all grader/readers are set at 0% FTE

table(new.employment.data.df$Uw.Dv.Job.Fte)



fte.sum.by.student.agg.by.dept <- aggregate(Uw.Dv.Job.Fte ~ Emplid + Uw.Deptid.Descr, data= new.employment.data.df, FUN=sum)

t(t(sort(table(fte.sum.by.student.agg.by.dept$Uw.Deptid.Descr[fte.sum.by.student.agg.by.dept$Uw.Dv.Job.Fte > 0.50]))))


t(t(sort(table(fte.sum.by.student.agg.by.dept$Uw.Deptid.Descr[fte.sum.by.student.agg.by.dept$Uw.Dv.Job.Fte > 0.50]))))

fte.sum.by.student.agg.by.dept.proportion <- aggregate(Uw.Dv.Job.Fte ~ Uw.Deptid.Descr, data = fte.sum.by.student.agg.by.dept, FUN=function(x) c(sum(x > 0.50)/length(x), length(x))  )
# Can do this, but it creates an abomination

fte.sum.by.student.agg.by.dept.proportion <- 
  data.frame(Uw.Deptid.Descr=fte.sum.by.student.agg.by.dept.proportion$Uw.Deptid.Descr, 
             g.t.50.prop=fte.sum.by.student.agg.by.dept.proportion$Uw.Dv.Job.Fte[, 1],
             num.students=fte.sum.by.student.agg.by.dept.proportion$Uw.Dv.Job.Fte[, 2],
             stringsAsFactors=FALSE)


fte.sum.by.student.agg.by.dept.proportion <- fte.sum.by.student.agg.by.dept.proportion[
  order(fte.sum.by.student.agg.by.dept.proportion[, 2], fte.sum.by.student.agg.by.dept.proportion[, 3], decreasing=TRUE), ]


write.csv( fte.sum.by.student.agg.by.dept.proportion, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/More than 50 perc assistants by dept.csv", row.names=FALSE, fileEncoding="Latin1")



fte.sum.by.student.agg.by.dept.proportion[grepl("ECON", fte.sum.by.student.agg.by.dept.proportion[, 1]), ]



fte.sum.by.student.math.agg <- aggregate(Uw.Dv.Job.Fte ~ Emplid, 
   data= new.employment.data.df[new.employment.data.df$Uw.Deptid.Descr=="L&S/MATHEMATICS/MATH", ], FUN=sum)

sort(table(fte.sum.by.student.math.agg$Uw.Dv.Job.Fte))


t(t(sort(table(new.employment.data.df$Uw.Deptid.Descr))))


#



str(new.employment.data.df )

round(prop.table(table(table(new.employment.data.df$Emplid)))*100, 2)
table(table(new.employment.data.df$Emplid))


with(new.employment.data.df, table(Uw.Jobcode.Descr,Uw.Pay.Basis) )
# Pay Basis is explained here: https://kb.wisc.edu/page.php?id=29426
# Maybe also useful: http://www.bussvc.wisc.edu/acct/codes/ccsalary.html
# http://www.ohr.wisc.edu/polproced/UPPP/How_Use_Sal_Rngs.htm
# https://kb.wisc.edu/hrs/page.php?id=29890


#with(new.employment.data.df, table(Uw.Jobcode.Descr, Empl.Class) )

#t(t(sort(table(new.employment.data.df$Uw.Comprate))))

#View(new.employment.data.df[new.employment.data.df$Uw.Comprate=="55,587", ])


#

#table(new.employment.data.df$Uw.Comprate, new.employment.data.df$Uw.Pay.Basis)



#


















      
levels(as.factor(full.outer.merge.df$Uw.Jobcode.Descr.EM))
# Find the omitted category
# So the omitted category is just "not employed", since it is blank

library("lmtest")
library("sandwich")
# class(oglmx.output) <- c(class(oglmx.output), "glm")
# class(oglmx.output) <- class(oglmx.output)[1]
# oglmx.output$family$family <- "logit" ; oglmx.output$family$family <- NULL
coeftest(coef(oglmx.output), vcov. = vcovHC(oglmx.output)))


library("lmtest")
    # likelihood ratio test to compare model with and without heteroskedasticity.
oglmx.output.homosk <- oglmx(formulaMEAN= as.numeric(is.TAA.member) ~ as.numeric(BIRTHDATE.formatted) + as.factor(Uw.Jobcode.Descr.EM) + as.factor(is.intl.student),
                      data=full.outer.merge.df, savemodelframe=TRUE, link = "logit", constantMEAN = TRUE, 

        constantSD = TRUE, delta = 0, threshparam = 0)
lrtest(oglmx.output.homosk,oglmx.output)

test.for.difference.oglmx <- probit.reg(as.numeric(is.TAA.member) ~ as.numeric(BIRTHDATE.formatted) + as.factor(Uw.Jobcode.Descr.EM) + as.factor(is.intl.student), data=full.outer.merge.df, savemodelframe=TRUE)
summary(test.for.difference.oglmx)
homosk.probit.for.comp <- glm(is.TAA.member ~ age.EN + Uw.Jobcode.Descr.EM + is.intl.student, family=binomial(link="probit"), data=full.outer.merge.df) 
summary(homosk.probit.for.comp)
# AIC: 1676.4
summary(oglmx.output.robust)
full.outer.merge.df$Acad.Group.Long.Descr.EN.fac <- relevel(as.factor(full.outer.merge.df$Acad.Group.Long.Descr.EN), ref="College of Letters and Science")




lm.for.comp <- lm(as.numeric(is.TAA.member) ~  Uw.Jobcode.Descr.EM + is.intl.student + Acad.Group.Long.Descr.EN.fac, data=full.outer.merge.df) 
summary(lm.for.comp)
coeftest(lm.for.comp, vcov. = vcovHC)
summary(predict(lm.for.comp))

prob.mfx.output <- probitmfx(formula = is.TAA.member ~ age.EN +  is.intl.student + Acad.Group.Long.Descr.EN.fac, 
          data=full.outer.merge.df, atmean = TRUE, robust=TRUE)
(prob.mfx.output)
summary(predict(prob.mfx.output, type="response"))

# install.packages("glmnet")
library("glmnet")
# http://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
targ.formula <- is.TAA.member ~ Uw.Jobcode.Descr.EM + is.intl.student + Acad.Group.Long.Descr.EN.fac
fit3=glmnet(model.matrix(targ.formula, model.frame(targ.formula, full.outer.merge.df)), as.numeric(full.outer.merge.df$is.TAA.member),family="binomial")
# Ok, this package is some weird stuff

# install.packages("logistf")
library("logistf")
logistf.output <- logistf(is.TAA.member ~ Uw.Jobcode.Descr.EM + is.intl.student + Acad.Group.Long.Descr.EN.fac, full.outer.merge.df)
summary(logistf.output)[, c("coefficients", "ci.lower", "ci.upper")]

round(do.call(cbind, logistf.output[c("coefficients", "prob","ci.lower", "ci.upper")]), 2)

plot(profile(logistf.output, variable="age.EN"))

lm.for.comp <- lm(as.numeric(is.TAA.member) ~ age.EN +Uw.Jobcode.Descr.EM + is.intl.student + ACAD_PLAN_LONG_DESCR, data=full.outer.merge.df) 
summary(lm.for.comp)
coeftest(lm.for.comp, vcov. = vcovHC)

lm.for.comp <- lm(as.numeric(is.TAA.member) ~  is.intl.student , data=full.outer.merge.df) 
summary(lm.for.comp)
coeftest(lm.for.comp, vcov. = vcovHC)




is_TAA_member age_EN Uw_Jobcode_Descr_EM is_intl_student


levels(as.factor(full.outer.merge.df$Acad.Group.Long.Descr.EN))

# install.packages("glmx")
library("glmx")
het.glm.comparison <- hetglm(as.numeric(is.TAA.member) ~  log(age.EN) + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student + degree.type.simple  | log(age.EN)  + as.factor(Uw.Jobcode.Descr.EM) + is.intl.student + degree.type.simple   , 
                             data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="", ],
      family = binomial(link = "probit"), control = hetglm.control(method="BFGS", trace=T, reltol=1e-12, maxit = 2000 )) #, link.scale = "identity") 
summary(het.glm.comparison)
summary(oglmx.output.robust.con)
logLik(het.glm.comparison)
lrtest(het.glm.comparison, het.glm.comparison.2)
logLik(oglmx.output.robust.con)
logLik(oglmx.output.robust)

glmx:::predict.hetglm(het.glm.comparison, newdata=, type=response)

as.factor(Uw.Jobcode.Descr.EM) + 
het.glm.comparison.hom <- hetglm(as.numeric(is.TAA.member) ~  log(age.EN) + Uw.Jobcode.Descr.EM + is.intl.student | 1   , 
                             data=full.outer.merge.df[full.outer.merge.df$Uw.Jobcode.Descr.EM!="", ],
      family = binomial(link = "probit"), control = hetglm.control(method="BFGS", trace=T, reltol=1e-12 )) #, link.scale = "identity") 
het.glm.comparison.hom.2 <- hetglm(as.numeric(is.TAA.member) ~  age.EN + is.intl.student | 1   , 
                             data=full.outer.merge.df,
      family = binomial(link = "probit"), control = hetglm.control(method="BFGS", trace=F, reltol=1e-12 )) #, link.scale = "identity") 
lrtest(het.glm.comparison,het.glm.comparison.hom)
summary(het.glm.comparison.hom)
summary(het.glm.comparison.hom.2)





This is log lik with  BFGS - 
logLik(oglmx.output.robust.con) [1] -817.1183


sort(unique(full.outer.merge.df$Home.Country.EN))


library("foreign")
full.outer.merge.df.missing <- full.outer.merge.df
full.outer.merge.df.missing <- as.data.frame(lapply(full.outer.merge.df.missing, FUN=function(x) {
  if (is.Date(x)) return(x)
  x[x==""] <- "missing"
  x}))
write.dta(full.outer.merge.df.missing, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Outer merge Enr Emp Mem.dta")




sum(full.outer.merge.df$Home.Country.EN=="Serbia - DO NOT USE")
# Database fail. Lol.





























colnames(membership.after.dedup.df)[colnames(membership.after.dedup.df)==""] <- ""

# Purpose of this is to know where the columns come from, when columns in other daatbases could be confused.

# TODO: DO I put the "anchor (person's) name" as the cleaned up name or not? probably yes.
# Then which columns are these?




summary( employment.df$Fte )

breaks=c(0, .20, )

hist(employment.df$Fte*100, breaks=20, col="red", cex.axis=2 )

hist(employment.df$Fte*employment.df$UW.Pay.Rate, breaks=20, col="red", cex.axis=2 )



# No fellows in database









# All below is scratch/unused code







# Try:
# membership.employment.link.key.df
# enrollment.employment.link.key.df
# membership.enrollment.link.key.df

master.key.df <- merge(membership.enrollment.link.key.df[!is.na(membership.enrollment.link.key.df$enrollment.db.id), ], 
                    enrollment.employment.link.key.df[!is.na(enrollment.employment.link.key.df$enrollment.db.id), ])

intersect(names(master.key.df), names(membership.employment.link.key.df))

master.key.df <- merge(master.key.df[!is.na(membership.enrollment.link.key.df$enrollment.db.id), ], 
                    membership.employment.link.key.df[!is.na(enrollment.employment.link.key.df$enrollment.db.id), ])

# The key thing that arises is that employment.db.id and membership.db.id are in both datasets



dim( merge(enrollment.after.dedup.df[!is.na(enrollment.after.dedup.df$membership.enrollment.key), ], 
           membership.after.dedup.df[!is.na(membership.after.dedup.df$membership.enrollment.key), ],  by="membership.enrollment.key") )

dim( merge(enrollment.after.dedup.df[!is.na(enrollment.after.dedup.df$membership.enrollment.key), ], 
           membership.after.dedup.df[!is.na(membership.after.dedup.df$membership.enrollment.key), ],  by="membership.enrollment.key") )




membership.after.dedup.df[, c("membership.db.id", "membership.employment.key", "membership.enrollment.key")]
enrollment.after.dedup.df[, c("enrollment.db.id", "enrollment.employment.key", "membership.enrollment.key")]
employment.after.dedup.df[, c("employment.db.id", "enrollment.employment.key", "membership.employment.key")]

final.key.MP.EN.df <- merge(
  membership.after.dedup.df[!is.na(membership.after.dedup.df$membership.enrollment.key), 
                            c("membership.db.id", "membership.employment.key", "membership.enrollment.key")], 
  enrollment.after.dedup.df[!is.na(enrollment.after.dedup.df$membership.enrollment.key), 
                            c("enrollment.db.id", "enrollment.employment.key", "membership.enrollment.key")],
  all=FALSE
)
# Needed to remove the incomparables, so used the is.na cleaning

final.key.MP.EN.df <- merge(
  membership.after.dedup.df[!is.na(membership.after.dedup.df$membership.enrollment.key), 
                            c("membership.db.id", "membership.employment.key", "membership.enrollment.key")], 
  employment.after.dedup.df[!is.na(enrollment.after.dedup.df$membership.enrollment.key), 
                            c("enrollment.db.id", "enrollment.employment.key", "membership.enrollment.key")],
  all=FALSE
)
# Needed to remove the incomparables, so used the is.na cleaning


final.key.df <- merge(
  final.key.df[!is.na(final.key.df$enrollment.employment.key), ], 
  employment.after.dedup.df[!is.na(enrollment.after.dedup.df$membership.enrollment.key), 
                            c("enrollment.db.id", "enrollment.employment.key", "membership.employment.key")],
  all=FALSE
)
# TODO: Ok, technically I can merge on two columns since dataframe 
# should have enrollment.employment.key and membership.employment.key
# but for now I won't since it will add a bit of a complication
# Need to handle the 


final.key.df <- merge(final.key.df, 
  employment.after.dedup.df[, c("employment.db.id", "enrollment.employment.key", "membership.employment.key")],
  all=TRUE
)










# TODO: Need to come up with a strategy to "complete the circle" - probably just prioritize one
# link in the network above the other, right? Like which one is more likely to be accurate




enrollment.db.id.crossref







strcmp.getpairs

trainSupv



model=trainSupv(test.linkage, method="rpart", minsplit=5)
result=classifySupv(model=model, newdata=l$valid)
summary(result)



#rpairs.test <- fsWeights(test.linkage)
# classify and show results
#summary(fsClassify(rpairs.test,0))
#getPairs(fsClassify(rpairs.test,1), filter.link="possible",
                            filter.match=c("unknown", "nonmatch"))



rpairs <- RLBigDataLinkage(




rpairs <- compare.dedup(inconsistent.recs[, c("address.clean", "contributor.clean", "name.parts.must.match")],
		  blockfld = list(c("address.clean", "name.parts.must.match")),
			identity = inconsistent.recs$contribution.id, strcmp = TRUE)


rpairs.weights <- epiWeights(rpairs)
	
	rpairs.classified <- epiClassify(rpairs.weights, 
	  threshold.upper=.99999999999999999, threshold.lower=grouping.threshold)
	
	rpairs.classified<-rpairs.classified[rpairs.classified$prediction %in% c("P", "L")] 
	
#	unique.addr<-unique(rpairs.classified$data$address.clean)
#	unique.addr<-which(rpairs.classified$data$address.clean %in% unique.addr)
	
#	df.temp<-rpairs.classified$pairs[rpairs.classified$pairs$id1 %in% unique.addr |
#		rpairs.classified$pairs$id2 %in% unique.addr,]
	
	df.temp<-rpairs.classified$pairs
	
	m<-as.matrix(apply(df.temp[, c("id1", "id2")], MARGIN=2, FUN=as.character))
	if (ncol(m)==1) {m<-t(m)}
	dimnames(m)<-NULL	
	g<-graph.edgelist(m)
	clust.g<-clusters(g)
	g.df<-data.frame(indices=V(g)$name, membership=clust.g$membership,
    contributor=rpairs.classified$data$contributor.clean[as.numeric(V(g)$name)],
	  stringsAsFactors=FALSE)
	
	contributor.freq.agg<-aggregate(g.df$membership, 
	  by=list(membership=g.df$membership, contributor=g.df$contributor),
	  FUN=length)
	
	unique.components<-unique(contributor.freq.agg$membership)
	
	name.replacements.df<-data.frame(component=unique.components, contributor=NA, stringsAsFactors=FALSE)
















merge(enrollment.after.dedup.df[, "names.combined", drop=FALSE], membership.after.dedup.df[, "names.combined", drop=FALSE])

merge(employment.after.dedup.df[, "names.combined", drop=FALSE], membership.after.dedup.df[, "names.combined", drop=FALSE])

1:

formatC(as.numeric(NA), width = 5)


merge(enrollment.after.dedup.df[, c("NAME_FIRST_PREFERRED", "names.combined"), drop=FALSE], membership.after.dedup.df[, c("First.Name", "names.combined"), drop=FALSE] )



## Older::
ftable(enrol=!is.na(merge.temp$enrollment.db.id), emp=!is.na(merge.temp$employment.db.id), memb=!is.na(merge.temp$membership.db.id))
            memb FALSE TRUE
enrol emp                  
FALSE FALSE          0  173
      TRUE        1830  109
TRUE  FALSE       4844   96
      TRUE        3048  276

### Newer:
  ftable(enrol=!is.na(merge.temp$enrollment.db.id), emp=!is.na(merge.temp$employment.db.id), memb=!is.na(merge.temp$membership.db.id))
            memb FALSE TRUE
enrol emp                  
FALSE FALSE          0  146
      TRUE        1821  118
TRUE  FALSE       4837  103
      TRUE        3037  287


      
      
t(t(with(enrollment.df[enrollment.df$ACAD_PLAN_LONG_DESCR=="Agricultural & Appl Econ PHD", ], prop.table(table(Country)))))

t(t(with(enrollment.df[enrollment.df$ACAD_PLAN_LONG_DESCR %in%
    c("Agricultural & Appl Econ MA",  "Agricultural & Appl Econ MS", "Agricultural & Appl Econ PHD"), ], prop.table(table(Country)))))




# Produces table of number of students who came to fair grad pay rally, by degree program / department

load("/Users/travismcarthur/Desktop/TAA work/Grad student database/Outer merge Enr Emp Mem version 11-16-15.Rdata")


rally.names.df <- read.csv("/Users/travismcarthur/Desktop/TAA work/Grad student database/fair grad pay Rally Names.csv", stringsAsFactor=FALSE, header=TRUE, fileEncoding="Latin1")

rally.names.ls <- as.list(rally.names.df)

rally.names.ls <- lapply(rally.names.ls, FUN=function(x) x[x!=""])

rally.names.ls <- lapply(rally.names.ls, FUN=function(x) gsub(" .+ ", " ", x)  )

rally.participants.df <- merge(full.outer.merge.df[, c("Name.Master.Key", "PLAN_DESCR.combined.EN")], data.frame(Name.Master.Key=rally.names.ls[["YES"]]))

t(t(sort(table(rally.participants.df$PLAN_DESCR.combined.EN), decreasing = TRUE)))

rally.interested.df <- merge(full.outer.merge.df[, c("Name.Master.Key", "PLAN_DESCR.combined.EN")], data.frame(Name.Master.Key=rally.names.ls[["INTERESTED"]]))

t(t(sort(table(rally.interested.df$PLAN_DESCR.combined.EN), decreasing = TRUE)))



# TODO: make combined dept for the employment data

full.outer.merge.df$Uw.Deptid.Descr.combined <- do.call(paste, full.outer.merge.df[, grepl("Uw.Deptid.Descr", colnames(full.outer.merge.df))])

# TAs being employed by math department, by academic department

set.of.math.tas.by.dept <- with(full.outer.merge.df, full.outer.merge.df$PLAN_DESCR.combined.EN[
  (grepl("L&S/MATHEMATICS/MATH", Uw.Deptid.Descr.First.EM) & grepl("TEACH ASST", Uw.Jobcode.Descr.First.EM) ) | 
        (grepl("L&S/MATHEMATICS/MATH", Uw.Deptid.Descr.Second.EM) & grepl("TEACH ASST", Uw.Jobcode.Descr.Second.EM) )]) 
                                                        
                                                        
t(t(sort(table(set.of.math.tas.by.dept ), decreasing = TRUE)))

round(prop.table(t(t(sort(table(set.of.math.tas.by.dept ), decreasing = TRUE))))*100, digits=1)


with(full.outer.merge.df, full.outer.merge.df[
  ((grepl("L&S/MATHEMATICS/MATH", Uw.Deptid.Descr.First.EM) & grepl("TEACH ASST", Uw.Jobcode.Descr.First.EM) ) | 
        (grepl("L&S/MATHEMATICS/MATH", Uw.Deptid.Descr.Second.EM) & grepl("TEACH ASST", Uw.Jobcode.Descr.Second.EM) )) &
    PLAN_DESCR.combined.EN %in% c("Curriculum and Instruction PHD", "Educational Policy Studies PHD"), 
    c("Name.Master.Key", "PLAN_DESCR.combined.EN")]) 




# ECE appointment percentages

library("stringr")

with(full.outer.merge.df[ grepl("Electrical Engineering", full.outer.merge.df$PLAN_DESCR.combined.EN) , ],  {
  tmp <- str_extract_all(PLAN_DESCR.combined.EN[order(sum.appointment.perc.EM)], 
                         "(Electrical Engineering PHD)|(Electrical Engineering MS)", simplify = FALSE)
  tmp <- sapply(tmp, FUN=paste, collapse=";")
  tmp <- gsub("Electrical Engineering", "EE", tmp)
  table(sum.appt.perc=sum.appointment.perc.EM[order(sum.appointment.perc.EM)], degree=tmp)
}
)
# A kind of complicated piece of code to strip out all the other degrees that people may be enrolled in





enrollment.file.location <- "/Users/travismcarthur/Desktop/TAA work/Grad student database/taa_fall 2015.csv"
enrollment.df <- read.csv(enrollment.file.location, stringsAsFactor=FALSE, fileEncoding="Latin1")

t(t(sort(table(enrollment.df$PLAN_DESCR), decreasing = TRUE)))
t(t(sort(table(full.outer.merge.df$PLAN_DESCR.combined.EN), decreasing = TRUE)))



paste3 <- function(...,sep=", ") {
     L <- list(...)
     L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
     ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                 gsub(paste0(sep,sep),sep,
                      do.call(paste,c(L,list(sep=sep)))))
     is.na(ret) <- ret==""
     # Uses this feature (explained in the help file): "The generic function is.na<- sets elements to NA."
     ret
}


# Appointments for electrical engineering students, by employing department

with(full.outer.merge.df[ grepl("Electrical Engineering", full.outer.merge.df$PLAN_DESCR.combined.EN) , ],  {
  tmp1 <- c(Uw.Deptid.Descr.First.EM, Uw.Deptid.Descr.Second.EM)
  tmp2 <- c(Uw.Dv.Job.Fte.First.EM, Uw.Dv.Job.Fte.Second.EM)
  table(employing.dept=tmp1, appt.perc=cut(tmp2, seq(0, .7, by=.1), include.lowest = TRUE))
}
)


table(full.outer.merge.df$num.appointments.EM[ grepl("Electrical Engineering", full.outer.merge.df$PLAN_DESCR.combined.EN)])


table(full.outer.merge.df$num.appointments.EM[ grepl("Electrical Engineering", full.outer.merge.df$PLAN_DESCR.combined.EN)],
      non.salaried=full.outer.merge.df$num.non.salaried.appts.held.EM[ grepl("Electrical Engineering", full.outer.merge.df$PLAN_DESCR.combined.EN)])



# >50% appointments by job type

paste3 <- function(...,sep=", ") {
     L <- list(...)
     L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
     ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                 gsub(paste0(sep,sep),sep,
                      do.call(paste,c(L,list(sep=sep)))))
     is.na(ret) <- ret==""
     # Uses this feature (explained in the help file): "The generic function is.na<- sets elements to NA."
     ret
}


table(full.outer.merge.df$sum.appointment.perc.EM>.5)

with(full.outer.merge.df[full.outer.merge.df$sum.appointment.perc.EM > .5, ],
  t(t(sort(table(paste3(Uw.Jobcode.Descr.First.EM, Uw.Jobcode.Descr.Second.EM, sep=";")), decreasing = TRUE)))
)


# Lots of stacked TA/PA/Lectureship appointments totalling greater than 50%?


with(full.outer.merge.df[ (full.outer.merge.df$Uw.Dv.Job.Fte.Second.EM + full.outer.merge.df$Uw.Dv.Job.Fte.First.EM) > 0.5 & 
                         full.outer.merge.df$Uw.Jobcode.Descr.First.EM!="RESEARCH ASSISTANT" & 
                         full.outer.merge.df$Uw.Jobcode.Descr.Second.EM!="RESEARCH ASSISTANT", ],
  t(t(sort(table(paste3(Uw.Jobcode.Descr.First.EM, Uw.Jobcode.Descr.Second.EM, sep=";")), decreasing = TRUE)))
)


with(full.outer.merge.df[ (full.outer.merge.df$Uw.Dv.Job.Fte.Second.EM + full.outer.merge.df$Uw.Dv.Job.Fte.First.EM) > 0.5 & 
                         full.outer.merge.df$Uw.Jobcode.Descr.First.EM!="RESEARCH ASSISTANT" & 
                         full.outer.merge.df$Uw.Jobcode.Descr.Second.EM!="RESEARCH ASSISTANT", ],
  t(t(na.omit(cbind(Name.Master.Key, is.TAA.member)[order(is.TAA.member), ])))
)


stacked.appt.ta.pa.g.t.50.perc <- full.outer.merge.df[ 
  (full.outer.merge.df$Uw.Dv.Job.Fte.Second.EM + full.outer.merge.df$Uw.Dv.Job.Fte.First.EM) > 0.5 & 
                         full.outer.merge.df$Uw.Jobcode.Descr.First.EM!="RESEARCH ASSISTANT" & 
                         full.outer.merge.df$Uw.Jobcode.Descr.Second.EM!="RESEARCH ASSISTANT", ]


summary(stacked.appt.ta.pa.g.t.50.perc$age.EN)
summary(full.outer.merge.df$age.EN)


t(full.outer.merge.df[full.outer.merge.df$Name.Master.Key=="Shui-Yin Yam", ])



# Madelaine L'Esperance
# Lindsay Weymouth
# Kathryn Boonstra

# Old way to do this:

with(full.outer.merge.df[full.outer.merge.df$sum.appointment.perc.EM > 0.5 & 
                         full.outer.merge.df$num.appointments.EM == 2 & 
                         full.outer.merge.df$Uw.Jobcode.Descr.First.EM!="RESEARCH ASSISTANT" & 
                         full.outer.merge.df$Uw.Jobcode.Descr.Second.EM!="RESEARCH ASSISTANT", ],
  t(t(sort(table(paste3(Uw.Jobcode.Descr.First.EM, Uw.Jobcode.Descr.Second.EM, sep=";")), decreasing = TRUE)))
)


credits.of.tas.tab<- with(full.outer.merge.df, {
  select.index <- is.enrolled & is.employed & Uw.Dv.Job.Fte.First.EM >= .33 &
    grepl("TEACH ASST", Uw.Jobcode.Descr.First.EM)
  table(TERM_TAKEN_CREDITS_PROGRESS.EN[select.index])
} )
# The above is an estimate, because it just examines the first position if it is TA and at least 33%

credits.of.tas.tab[["8"]] <- sum(credits.of.tas.tab[as.character(8:100)], na.rm=TRUE)
credits.of.tas.tab <- credits.of.tas.tab[as.character(1:8)]
credits.of.tas.tab[is.na(credits.of.tas.tab)] <- 0
# Turn the 1-credit slot into zero instead of NA


# Hard to get residency:
#"If you enter and remain in Wisconsin principally to attend an educational institution during the twelve months you are attempting to establish bona fide residence in Wisconsin, you are presumed to continue to reside outside the state. This presumption remains in effect until you are able to demonstrate you have overcome the presumption with clear and convincing evidence of bona fide residence in the state for the year next preceding a term for which you wish to enroll at the University of Wisconsin-Madison. Wisconsin Statutes 36.27.(2) Section (e). 

#"Generally, a student who enters Wisconsin to attend any educational institution will continue in the nonresident status until the student's reason for being in Wisconsin is clearly shown to be non-educational. Twelve months after the change in the student's reason for being in Wisconsin, the student may wish to inquire about appealing their nonresident status. "
# https://registrar.wisc.edu/residence_frequently_asked_questions.htm
# BTW: "GUEST STUDENT: A Wisconsin resident age 60 and over only taking courses for audit."

# Fall 2015 Term. Top is 8+ credits:
tuition.nonresident.by.credits <- c(12598.36,
11029.94,
9461.52,
7893.10,
6324.68,
4756.26,
3187.84,
1619.42)

tuition.resident.by.credits <- c(5934.92,
5199.43,
4463.94,
3728.45,
2992.96,
2257.47,
1521.98,
786.49)

# Non-resident grad students: 6478
# Resident grad students: 2524
# So just spread proportionally
# Numbers from https://registrar.wisc.edu/documents/Stats_all_2015-2016Fall.pdf

average.tuition <- (tuition.nonresident.by.credits * 6478 + tuition.resident.by.credits * 2524) / (6478 + 2524)


average.tuition  <- rev(average.tuition )
# Sort from lowest to highest

sum(average.tuition  * credits.of.tas.tab) / sum(credits.of.tas.tab)








cat(t(t(unique(sort(full.outer.merge.df$Uw.Jobcode.Descr.First.EM)))), sep="\n")


t(t(table(full.outer.merge.df$sum.appointment.perc.EM[full.outer.merge.df$is.intl.student.EN])))



t(t(table(full.outer.merge.df$sum.appointment.perc.EM[full.outer.merge.df$is.intl.student.EN] > .5)))

t(t(table(full.outer.merge.df$sum.appointment.perc.EM[
  full.outer.merge.df$is.intl.student.EN & !is.na(full.outer.merge.df$is.intl.student.EN) ] > .5 )))


with( full.outer.merge.df[ full.outer.merge.df$sum.appointment.perc.EM > .5 & 
  full.outer.merge.df$is.intl.student.EN ,  ], {
  t(t(table(Uw.Jobcode.Descr.First.EM ))) }
)


with( full.outer.merge.df[ full.outer.merge.df$sum.appointment.perc.EM > .5 & 
  full.outer.merge.df$is.intl.student.EN ,  ], {
  t(t(table(Uw.Dv.Job.Fte.First.EM, Uw.Jobcode.Descr.First.EM ))) }
)


full.outer.merge.df$Jobcode.Decr.combined <- paste3(full.outer.merge.df$Uw.Jobcode.Descr.First.EM,
                                                    full.outer.merge.df$Uw.Jobcode.Descr.Second.EM, sep=";")


with( full.outer.merge.df[ full.outer.merge.df$sum.appointment.perc.EM > .5 & 
  full.outer.merge.df$is.intl.student.EN ,  ], {
  t(t(table(Jobcode.Decr.combined ))) }
)




t(table(full.outer.merge.df$Uw.Dv.Job.Fte.First.EM[full.outer.merge.df$is.intl.student.EN] > .5,
          full.outer.merge.df$Uw.Jobcode.Descr.First.EM[full.outer.merge.df$is.intl.student.EN]))






# Gender regressions:


summary(lm(sum.estimated.annual.compensation.EM ~ probable.gender, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ) )

summary(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender , 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ) )

summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender , 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )



summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ PLAN_DESCR.1.EN +
                    probable.gender + CUM_CREDITS_TOTAL.EN + TERM_TAKEN_CREDITS_PROGRESS.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )

table(full.outer.merge.df$probable.gender)


summary.lm.h.c(lm(log(sum.appointment.perc.EM + 1) ~ PLAN_DESCR.1.EN +
                    probable.gender + CUM_CREDITS_TOTAL.EN + TERM_TAKEN_CREDITS_PROGRESS.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


summary.lm.h.c(lm(I(sum.appointment.perc.EM*100) ~ PLAN_DESCR.1.EN +
                    probable.gender + CUM_CREDITS_TOTAL.EN + TERM_TAKEN_CREDITS_PROGRESS.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


 
summary.lm.h.c(lm(I(sum.appointment.perc.EM*100) ~ 
                    probable.gender , 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )




summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender.0 , 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


summary.lm.h.c(lm(I(sum.appointment.perc.EM*100) ~ probable.gender.0 , 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


table(grepl("Luisa", full.outer.merge.df$Name.Master.Key))

View(full.outer.merge.df[grepl("Luisa", full.outer.merge.df$Name.Master.Key), ])



summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender.0 + Discipline.Area.Descr.1.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


summary.lm.h.c(lm(I(sum.appointment.perc.EM*100) ~ probable.gender.0 + Discipline.Area.Descr.1.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


no.graderships.index<- apply(full.outer.merge.df[, grepl("Jobcode", names(full.outer.merge.df) )], 1, FUN=function(x) {!any(grepl("GRADER", x))} )


summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender.0  + Discipline.Area.Descr.1.EN ,
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN & no.graderships.index & 
                                      !is.na(full.outer.merge.df$Discipline.Area.Descr.1.EN), ] ), white.cor = TRUE )
# Want to do !is.na(full.outer.merge.df$Discipline.Area.Descr.1.EN) when we have just gender in there

summary.lm.h.c(lm(I(sum.appointment.perc.EM*100) ~ probable.gender.0 + Discipline.Area.Descr.1.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN & no.graderships.index & 
                                      !is.na(full.outer.merge.df$Discipline.Area.Descr.1.EN), ] ), white.cor = TRUE )





summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender.0 + Discipline.Area.Descr.1.EN, 
           data=full.outer.merge.df ), white.cor = TRUE )


summary.lm.h.c(lm(I(sum.appointment.perc.EM*100) ~ probable.gender.0 + Discipline.Area.Descr.1.EN, 
           data=full.outer.merge.df ), white.cor = TRUE )








# "Other is just:
#                PLAN_DESCR.1.EN Discipline.Area.Descr.1.EN      Department.Name.1.EN
#    Second Lang Acquisition PHD                      Other GENERAL INTERDEPARTMENTAL


unique(full.outer.merge.df[full.outer.merge.df$Discipline.Area.Descr.1.EN %in% "Other", 
                           c("PLAN_DESCR.1.EN", "Discipline.Area.Descr.1.EN", "Department.Name.1.EN")])




sort(unique(full.outer.merge.df$Discipline.Area.Descr.1.EN))




summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender + PRIMARY_ACADEMIC_GROUP.1.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), white.cor = TRUE )


summary.lm.h.c(lm(log(sum.estimated.annual.compensation.EM + 1) ~ probable.gender + Uw.Deptid.Descr.First.EM +
                    CUM_CREDITS_TOTAL.EN + ACADEMIC_LOAD.EN, 
           data=full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, ] ), 
           cluster= full.outer.merge.df[!full.outer.merge.df$is.intl.student.EN, "Uw.Deptid.Descr.First.EM"]) 


# full.outer.merge.df$age.EN <- as.period(new_interval(full.outer.merge.df$BIRTHDATE.formatted.EN, as.Date(Sys.Date())))@year

summary(full.outer.merge.df$age.EN[full.outer.merge.df$probable.gender=="female" & 
                                     !full.outer.merge.df$is.intl.student.EN ] )

summary(full.outer.merge.df$age.EN[full.outer.merge.df$probable.gender=="female" & 
                                     !full.outer.merge.df$is.intl.student.EN &
                                     full.outer.merge.df$is.employed ] )



# table(full.outer.merge.df$HOME_COUNTRY_CODE.EN %in% country.code.df$iso3 | full.outer.merge.df$HOME_COUNTRY_CODE.EN=="")

# View(full.outer.merge.df[!(full.outer.merge.df$HOME_COUNTRY_CODE.EN %in% country.code.df$iso3 | full.outer.merge.df$HOME_COUNTRY_CODE.EN %in% "") & !is.na(full.outer.merge.df$HOME_COUNTRY_CODE.EN), ])

# Only 4 people are not captured by the ISO 3 - two Serbia (SER) and two Puerto Rico (UST). So just ignore non-matches.

# round(vcov(lm(rnorm(100000) ~ runif(100000) + rpois(100000, 1))), digits=5)



# Intl student emails for Luisa
paste( full.outer.merge.df$Email.Address.EN[full.outer.merge.df$is.intl.student.EN], collapse="; ")




