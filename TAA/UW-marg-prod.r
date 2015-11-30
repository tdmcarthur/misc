

uw.df <- read.csv("/Users/travismcarthur/Desktop/TAA work/UW marginal product/UW_marg_prod_data.csv", 
                  stringsAsFactors = FALSE, comment.char = "")
# from https://apir.wisc.edu/deptplanningprof.htm

# What does academic staff mean, after all?
# http://www.ohr.wisc.edu/polproced/UPPP/0102.html
# http://www.ohr.wisc.edu/polproced/utg/SumUnclApptTypes.html
# https://www.ohr.wisc.edu/polproced/UTG/TitleDesc2.html#instr


# INSTR_AS_AND_TA_ALL_FUNDS
# FTE total of all funded splits within department of instructional academic staff titles (EEO category 2-2) plus teaching assistants (titles beginning with Y30, Y32 or Y33)

# INSTR_ACAD_STAFF_GPR
# FTE total of all GPR-funded splits within department (on all activities) of instructional academic staff titles (EEO category 2-2), regardless of the duration of the appointment.
# GPR is "GPR funded (funds 101, 104, 108, 115, 118, 117, and 402)"
# Small problem: this will not capture non-GPR-funded people. but I;m not sure
# how common that is, or even if it exists.
# See also http://siftingandwinnowing.org/2015/02/14/the-history-of-tuition-and-state-support-at-uw-madison/


# FACULTY_GPR_2
# FTE total of all GPR-funded, Activity 2 (instructional) splits within department of tenure/tenure-track faculty titles. 


# Add support staff? - which lines to add?


# TOTAL_CREDITS
# Total of all credits generated within department by undergraduate, graduate, professional and special students.  Calculation = Undergrad Credits + Total Graduate Credits + Professional Credits + Special Credits.

# TOTAL_FACULTY_ROSTER
# FACULTY_ALL_FUNDS
# Faculty
# INDEFINITE_APPTS_HEAD_CT
# ROLLING_HORIZON_HEAD_CT
# INSTR_ACAD_STAFF_101


# INSTR_AS_AND_TA_ALL_FUNDS
# INSTR_ACAD_STAFF_GPR
# FACULTY_GPR_2

# faculty: FACULTY_GPR_2
# INSTR_AS_AND_TA_ALL_FUNDS
# academ.staff: INSTR_ACAD_STAFF_GPR
# ta.s: INSTR_AS_AND_TA_ALL_FUNDS - INSTR_ACAD_STAFF_GPR, i.e. ta.s.temp - academ.staff


uw.df <- uw.df[ grepl("(FACULTY GPR-2)|(INSTR AS & TA ALL FUNDS)|(INSTR ACAD STAFF GPR)|(-TOTAL CREDITS)",  
             uw.df$Line...Description), ]




uw.df$input <- NA

uw.df$input[grepl("(FACULTY GPR-2)", uw.df$Line...Description)] <- "faculty"
uw.df$input[grepl("(INSTR ACAD STAFF GPR)", uw.df$Line...Description)] <- "academ.staff"
uw.df$input[grepl("(INSTR AS & TA ALL FUNDS)", uw.df$Line...Description)] <- "ta.s.temp"
uw.df$input[grepl("(-TOTAL CREDITS)", uw.df$Line...Description)] <- "credits"
# Need to add dash to TOTAL CREDITS because otherwise it will pick up the percentage
# of total credits in college variable, which we do not want

uw.df <- reshape(uw.df, varying=list(which(grepl("X_[0-9]{4}", colnames(uw.df)))), 
                 direction="long", v.names="value", times = colnames(uw.df)[grepl("X_[0-9]{4}", colnames(uw.df))] )

attr(uw.df, "reshapeLong") <- NULL
# Need to do this or the below will just attempt to simply undo the above operation

uw.df$Topic <- NULL
uw.df$Line...Description <- NULL
uw.df$id <- NULL
#uw.df$time <- NULL

# View(uw.df[uw.df$Department.Name=="URBAN AND REGIONAL PLANNING", ] )
# This is to handle the fact that URBAN AND REGIONAL PLANNING is in two colleges
# So must get rid of the School.College variable
urb.plan.fix <- aggregate(value ~  Department.Name + Term + input + time, 
          data = uw.df[uw.df$Department.Name=="URBAN AND REGIONAL PLANNING", ], FUN=sum, na.rm=TRUE)

urb.plan.fix <- cbind(data.frame(School.College=rep("LETTERS & SCIENCE", nrow(urb.plan.fix)), 
                 UDDS=rep("A4894", nrow(urb.plan.fix))), urb.plan.fix )

uw.df <- rbind(uw.df[uw.df$Department.Name!="URBAN AND REGIONAL PLANNING", ], urb.plan.fix)

genetics.fix <- aggregate(value ~  Department.Name + Term + input + time, 
          data = uw.df[uw.df$Department.Name=="GENETICS", ], FUN=sum, na.rm=TRUE)

genetics.fix <- cbind(data.frame(School.College=rep("AGRICULTURAL & LIFE SCIENCES", nrow(genetics.fix)), 
                 UDDS=rep("A0742", nrow(genetics.fix))), genetics.fix )

uw.df <- rbind(uw.df[uw.df$Department.Name!="GENETICS", ], genetics.fix)




uw.df <- reshape(uw.df,  
                 direction="wide", timevar="input", idvar=c("School.College", "UDDS", "Department.Name", "Term", "time"))

# The start recording the data for each semester separately in 1995, so eliminate before then:
uw.df <- uw.df[ ! uw.df$time %in% paste0("X_", 1990:1994), ]

colnames(uw.df) <- gsub("value[.]", "", colnames(uw.df))

uw.df$ta.s <- uw.df$ta.s.temp - uw.df$academ.staff
uw.df$ta.s.temp <- NULL

attr(uw.df, "reshapeLong") <- NULL

# install.packages("micEcon")
library("micEcon")

#<- quadFuncEst( yName, xNames, data, shifterNames = NULL,
#       linear = FALSE, homWeights = NULL,
#       regScale = 1, ... )

# table(colSums( model.matrix(credits ~  - 1 + Department.Name, data=uw.df) ) )
# Remove intercept
# table(colSums( model.matrix(credits ~  - 1 + Department.Name, data=uw.df)[, -1] ) )

# sort(table(uw.df$Department.Name))

# View(uw.df[uw.df$Department.Name=="URBAN AND REGIONAL PLANNING", ] )
# View(uw.df[uw.df$Department.Name=="GENETICS", ] )

uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + Department.Name, data=uw.df)[, -1, drop=FALSE] ) 
uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + time, data=uw.df)[, -1, drop=FALSE] ) 
uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + Term , data=uw.df)[, -1, drop=FALSE] ) 

# uw.df[, c("ta.s", "faculty", "academ.staff")] <- uw.df[, c("ta.s", "faculty", "academ.staff")] * 10

quad.output <- quadFuncEst( "credits", c("ta.s", "faculty", "academ.staff"), 
  data=uw.df, 
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)

coef(quad.output$est)[1:4]

# SO for shifter names, we take all columns that were created by the model.matrix,
# but don;t take the original source names, i.e. column names that exactly match dept name, etc.

# Maybe useful for getting to fized effects more elegantly:
# https://r-forge.r-project.org/scm/viewvc.php/pkg/tests/quadFunc.R?annotate=1066&root=micecon&pathrev=1094
# https://r-forge.r-project.org/forum/forum.php?max_rows=25&style=nested&offset=6&forum_id=841&group_id=257

# Apparently we can use a plm object here

quad.output.year.interact <- quadFuncEst( "credits", c("ta.s", "faculty", "academ.staff",
  colnames(uw.df)[grepl( "(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ] ), 
  data=uw.df, 
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)


coef(quad.output)
summary(quad.output$est)

summary(quad.output.year.interact$est)


shifter.coefs<- data.frame(coef=round(coef(quad.output)[grepl("d", names(coef(quad.output)))]), 
      std.err = sqrt(diag(vcov(quad.output$est)))[grepl("d", names(coef(quad.output)))],
      names=colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") )]
)

shifter.coefs$t.stat <- shifter.coefs$coef / shifter.coefs$std.err

shifter.coefs <- shifter.coefs[order(shifter.coefs$coef), ]

summary(aggregate(credits ~ Department.Name, uw.df[uw.df$time=="X_2015", ], FUN=sum, na.rm=TRUE))

library("sandwich")
library("lmtest")
library("car")

margProducts.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff"),
          data=uw.df, coef=coef( quad.output ), coefCov = vcovHC(quad.output$est ) )


str(margProducts.output)
colMeans(margProducts.output)
# Going to be the same as below, since marginal product formula is
# linear, so dont run into Jensen's inequality

margProducts.mean.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff"),
          data=colMeans(uw.df[!sapply(uw.df, FUN=is.character)], na.rm=TRUE), 
          coef=coef( quad.output ), coefCov= vcovHC(quad.output$est ) )

margProducts.2015.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff"),
          data=colMeans(uw.df[uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)], na.rm=TRUE), 
          coef=coef( quad.output ), coefCov = vcovHC(quad.output$est ) )

attr(margProducts.2015.output, "stdDev")

corrected.varcov <- matrix(0, nrow=length(coef( quad.output.year.interact )), 
                            ncol=length(coef( quad.output.year.interact )))
corrected.varcov[!is.na(coef( quad.output.year.interact )),
                 !is.na(coef( quad.output.year.interact ))] <- vcovHC(quad.output.year.interact$est )
# Brilliant! It (appears to) work!
dimnames(corrected.varcov)[[1]] <- names(coef( quad.output.year.interact ))
dimnames(corrected.varcov)[[2]] <- names(coef( quad.output.year.interact ))


margProducts.2015.output.year.interact <- quadFuncDeriv( xNames = 
          c("ta.s", "faculty", "academ.staff",
          colnames(uw.df)[grepl( "(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ] ),
          data=colMeans(uw.df[uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)], na.rm=TRUE), 
          coef= coef( quad.output.year.interact ), 
          coefCov=corrected.varcov 
) # 
# [!is.na(coef( quad.output.year.interact ))]
data.frame(coef=unlist(margProducts.2015.output.year.interact), 
           st.dev=unlist(attr(margProducts.2015.output.year.interact, "stdDev") ))[1:3, ]

data.frame(coef=unlist(margProducts.2015.output), 
           st.dev=unlist(attr(margProducts.2015.output, "stdDev") ))[1:3, ]


# 5207.88 per 15 credits for WI residents (actually, 12-18 credits)
# https://registrar.wisc.edu/documents/1162tuition.pdf
revenue.per.credit.if.all.residents <- 5207.88/15

# $ 489 million from tuition, 2014-2015
# https://www.vc.wisc.edu/documents/Budget-in-Brief.pdf
total.fy.2014.2014.credits <- sum(uw.df$credits[uw.df$time=="X_2015"])
total.tuition.revenue <- 489 * 10^6
revenue.per.credit.from.budget <- total.tuition.revenue / total.fy.2014.2014.credits

margProducts.mean.output$ta.s * revenue.per.credit.if.all.residents
margProducts.mean.output$ta.s * revenue.per.credit.from.budget

margProducts.2015.output$ta.s * revenue.per.credit.if.all.residents
margProducts.2015.output$ta.s * revenue.per.credit.from.budget

margProducts.mean.output$faculty * revenue.per.credit.if.all.residents
margProducts.mean.output$faculty * revenue.per.credit.from.budget

margProducts.2015.output$faculty * revenue.per.credit.if.all.residents
margProducts.2015.output$faculty * revenue.per.credit.from.budget

ta.mp.sd.year.interact <- attr(margProducts.2015.output.year.interact, "stdDev")$ta.s

margProducts.2015.output.year.interact$ta.s * revenue.per.credit.if.all.residents
c(
  (margProducts.2015.output.year.interact$ta.s + ta.mp.sd.year.interact * 1.6) * revenue.per.credit.from.budget,
  (margProducts.2015.output.year.interact$ta.s - ta.mp.sd.year.interact * 1.6) * revenue.per.credit.from.budget
)
# 90% C.I.


8.559627
    
#View(uw.df[uw.df$time=="X_2015",])
# NOTE: X_2015 = FY 2014-2015


# TODO: I could actually separate out the profesh, grad, and undergrad credits
# by multiplying each credit type by the average revenue per credit.

# TODO: This is probably not a balanced panel, so what do we do with that?



# elaFit <- quadFuncEla( xNames = c( "qLabor", "land", "qVarInput", "time" ),
#          data = germanFarms, coef = coef( estResult ) )


elaFit.mean.output <- quadFuncEla( xNames = c("ta.s", "faculty", "academ.staff"),
          data=colMeans(uw.df[!sapply(uw.df, FUN=is.character)], na.rm=TRUE), 
          coef=coef( quad.output ), yName="credits" )

elaFit.2015.output <- quadFuncEla( xNames = c("ta.s", "faculty", "academ.staff"),
          data=colMeans(uw.df[uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)], na.rm=TRUE), 
          coef=coef( quad.output ), yName="credits" )

# quadFuncCalc( c( "qLabor", "land", "qVarInput", "time" ), germanFarms,
#           coef( estResult ) )


output.eval.point <- colMeans(uw.df[, !sapply(uw.df, FUN=is.character)], na.rm=TRUE)

output.eval.point <- colMeans(uw.df[uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)], na.rm=TRUE)

#output.eval.point <- output.eval.point * 1.5

# h.dif <- 10^-8
# h.dif <- 0.10
 h.dif <- 0.01
# h.dif <- 4
# h.dif <- (-0.5)

quad.output.base <- quadFuncCalc(  xNames = c("ta.s", "faculty", "academ.staff"),
          data=output.eval.point, 
          coef=coef( quad.output ),
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)

quad.output.change <- quadFuncCalc(  xNames = c("ta.s", "faculty", "academ.staff"),
          data=output.eval.point * (1 +
            ifelse(names(output.eval.point) %in% c("ta.s", "faculty", "academ.staff") , h.dif, 0)
            ), 
          coef=coef( quad.output ),
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)

# Returns to scale:
quad.output.change / ( (h.dif + 1) * quad.output.base )

(quad.output.change - quad.output.base) / quad.output.base

# Best returns to scale (output elasticity w.r.t. all inputs):
((quad.output.change - quad.output.base) / quad.output.base ) / h.dif

log(quad.output.change) -  log( quad.output.base )

(log(quad.output.change) -  log( quad.output.base )) / (log(h.dif + 1) - log(1)) 


quad.fun.hessian <- with(as.list(coef(quad.output)), 
  matrix(c(b_1_1, b_1_2, b_1_3,
           b_1_2, b_2_2, b_2_3,
           b_1_3, b_2_3, b_3_3), ncol=3)
)
# WARNING: I'm not sure if I'm supposed to multiply the diag elements
# by 2 (or even 1/2). Depends on how this all was fit.

library("matrixcalc")

is.negative.definite(quad.fun.hessian, tol=1e-8)
eigen(quad.fun.hessian)



# abolute change 
# log(quad.output.change) - log(quad.output.base)


names(uw.df)

summary( lm(credits ~  ta.s + faculty + academ.staff + ta.s:Department.Name  + Department.Name + Term + time, 
  data=uw.df))

summary( lm(credits ~  ta.s + faculty + academ.staff + ta.s:School.College  + Department.Name + Term + time, 
  data=uw.df))

levels(factor(uw.df$School.College))[1]
levels(factor(uw.df$Department.Name))[1]
# 166 departments

summary(test.full.dept.interact.lm <- lm(credits ~  
              Department.Name * (I(ta.s^2) + I(faculty^2) + I(academ.staff^2) + (ta.s + faculty + academ.staff )^2 ) + 
              Term + time, 
  data=uw.df))








compare.dept.ta.MPs <- function(dept1, dept2, one.more.labor.unit, targ.labor.unit, data, model) {
  data <- data[, c("Department.Name", "ta.s", "faculty", "academ.staff")]
  data1 <- data.frame( ta.s = mean(data$ta.s), 
              faculty = mean(data$faculty),
              academ.staff = mean(data$academ.staff),
              Term = "Fall",
              time = "X_2015",
              Department.Name = dept1)
  
   data2 <- data.frame( ta.s = mean(data$ta.s), 
              faculty = mean(data$faculty),
              academ.staff = mean(data$academ.staff),
              Term = "Fall",
              time = "X_2015",
              Department.Name = dept2) 
  
  marg.prod.base <- predict(model, newdata = data1)
  data1.ch <- data1
  data1.ch[, targ.labor.unit] <- data1.ch[, targ.labor.unit] + one.more.labor.unit
  marg.prod.change <- predict(model, newdata = data1.ch) 
  marg.prod1 <- (marg.prod.change - marg.prod.base) / one.more.labor.unit
  
  marg.prod.base <- predict(model, newdata = data2)
  data2.ch <- data2
  data2.ch[, targ.labor.unit] <- data2.ch[, targ.labor.unit] + one.more.labor.unit
  marg.prod.change <- predict(model, newdata = data2.ch) 
  marg.prod2 <- (marg.prod.change - marg.prod.base) / one.more.labor.unit
  
  ret <- c( marg.prod1, marg.prod2)
  names(ret) <- c(dept1, dept2)
  ret
}

# So how much does MP of TAs change when going from dept 1 to dept 2?

compare.dept.ta.MPs(dept1 = "ENGLISH", dept2 = "MATHEMATICS",
  one.more.labor.unit = 1, targ.labor.unit = "faculty",
  data=uw.df, model=test.full.dept.interact.lm)



compare.dept.ta.MPs(dept1 = "ENGLISH", dept2 = "MATHEMATICS",
  one.more.labor.unit = 1, targ.labor.unit = "faculty",
  data=uw.df[uw.df$time=="X_2015", ], model=test.full.dept.interact.lm)

compare.dept.ta.MPs(dept1 = "ENGLISH", dept2 = "MATHEMATICS",
  one.more.labor.unit = 1, targ.labor.unit = "ta.s",
  data=uw.df[uw.df$time=="X_2015", ], model=test.full.dept.interact.lm)

sort(unique(uw.df$Department.Name))


compare.dept.ta.MPs(dept1 = "AGRICULTURAL & APPLIED ECONOMICS", dept2 = "ECONOMICS",
  one.more.labor.unit = 1, targ.labor.unit = "ta.s",
  data=uw.df[uw.df$time=="X_2015", ], model=test.full.dept.interact.lm)


fn.of.MP <- function(fn.to.apply, one.more.labor.unit, targ.labor.unit, data, model) {
  marg.prod.base <- fn.to.apply(predict(model, newdata = data))
  data.ch <- data
  data.ch[, targ.labor.unit] <- data.ch[, targ.labor.unit] + one.more.labor.unit
  marg.prod.change <- fn.to.apply(predict(model, newdata = data.ch))
  marg.prod1 <- (marg.prod.change - marg.prod.base) / one.more.labor.unit
  marg.prod1
}

fn.of.MP(fn.to.apply= median, one.more.labor.unit = 10^-6, targ.labor.unit = "ta.s",
  data=uw.df, model=test.full.dept.interact.lm)
# [uw.df$time=="X_2015", ]

fn.of.MP(fn.to.apply= median, one.more.labor.unit = 10^-6, targ.labor.unit = "faculty",
  data=uw.df, model=test.full.dept.interact.lm)

fn.of.MP(fn.to.apply= median, one.more.labor.unit = 10^-6, targ.labor.unit = "academ.staff",
  data=uw.df, model=test.full.dept.interact.lm)


summary(test.quad.lm <- lm(credits ~  
              I(ta.s^2) + I(faculty^2) + I(academ.staff^2) + (ta.s + faculty + academ.staff )^2  + 
              Department.Name + Term + time, model=FALSE, qr=TRUE,
  data=uw.df))

fn.of.MP(fn.to.apply= median, one.more.labor.unit = 10^-6, targ.labor.unit = "ta.s",
  data=uw.df, model=test.quad.lm)

fn.of.MP(fn.to.apply= median, one.more.labor.unit = 10^-6, targ.labor.unit = "faculty",
  data=uw.df, model=test.quad.lm)

fn.of.MP(fn.to.apply= median, one.more.labor.unit = 10^-6, targ.labor.unit = "academ.staff",
  data=uw.df, model=test.quad.lm)


marg.prod.base <- mean(predict(test.full.dept.interact.lm))

one.more.ta.unit <- 1

uw.one.more.ta <- uw.df
uw.one.more.ta$ta.s <- uw.one.more.ta$ta.s + one.more.ta.unit
marg.prod.change <- mean( predict(test.full.dept.interact.lm, newdata = uw.one.more.ta) )

(marg.prod.change - marg.prod.base) / one.more.ta.unit




quad.output.dept.interact <- quadFuncEst( "credits", c("ta.s", "faculty", "academ.staff",
  colnames(uw.df)[grepl( "(Department.Name)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ] ), 
  data=uw.df, 
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)







