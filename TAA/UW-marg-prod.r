

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

# TA_GPR
#FTE total of all GPR-funded splits within department for teaching assistant titles (titles beginning with Y30, Y32 or Y33). 


# Add support staff? - which lines to add?


# TOTAL_CREDITS
# Total of all credits generated within department by undergraduate, graduate, professional and special students. Calculation = Undergrad Credits + Total Graduate Credits + Professional Credits + Special Credits.

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
# NOTE: I changed the definition after looking into MEDICINE

# TOT UNCL SUPPORT ALL FUNDS	
# FTE total of all splits within department of unclassified titles defined as "executive, managerial, administrative" (EEO category 1-1), "doctoral" (EEO category 3-1) , "other" (EEO category 3-3), "employees in training" (EEO category 3-2) and "student staff" (EEO category 3-4, excluding teaching assistants)

# TOT CLASSIFIED ALL FUNDS	
# FTE total of all splits within department of classified titles defined as "executive, managerial, administrative" (EEO category 1-2), "professional" (EEO category 3-5), "secretarial/clerical" (EEO category 4), "technical/paraprofessional" (EEO category 5),  "skilled crafts" (EEO category 6) and "service/maintenance" (EEO category 7)
# NOTE: There are also GPR versions of the two above - should we get those?



# Master's cost per student includes law students; doctoral cost per student excludes medical and veterinary students.
# 2013-14 Undergrad cost per student: 13,710
# 2013-14 Master's cost per student: 23,559
# 2013-14 PhD cost per student: 21,244

# Master's cost per student includes law students; doctoral cost per student excludes medical and veterinary students.
# Source: http://legis.wisconsin.gov/lfb/publications/informational-papers/documents/2015/33_uw%20tuition.pdf

# 13710 / 15
# 23559 / 9
# 21244 / 9
# These calcs above sort of assume everyone is full time

# UGRAD_CREDITS
# Total of all credits generated within department by undergraduates (freshman, sophomore, junior or senior)
# MASTERS_CREDITS
# Total of all credits generated within department by masters-level students
# PHD_CREDITS	
# Total of all credits generated within department by Ph.D. students

# SCHOOL OF PHARMACY; MEDICINE; FAMILY MEDICINE-GEN ; SCHOOL OF VETERINARY MEDICINE






#uw.df <- uw.df[ grepl("(FACULTY GPR-2)|(INSTR AS & TA ALL FUNDS)|(INSTR ACAD STAFF GPR)|(-TOTAL CREDITS)",  
#             uw.df$Line...Description), ]

#uw.df <- uw.df[ grepl("(FACULTY GPR-2)|(-TA GPR)|(INSTR ACAD STAFF GPR)|(-TOTAL CREDITS)",  
#             uw.df$Line...Description), ]

uw.df <- uw.df[ 
  grepl("(FACULTY GPR-2)|(-TA GPR)|(INSTR ACAD STAFF GPR)|(-TOTAL CREDITS)|(-UNDERGRAD CREDITS)|(-MASTERS CREDITS)|(-PHD CREDITS)|(-TOT UNCL SUPPORT ALL FUNDS)|(-TOT CLASSIFIED ALL FUNDS)|(-SPECIAL CREDITS)",  
             uw.df$Line...Description), ]


uw.df$input <- NA

uw.df$input[grepl("(FACULTY GPR-2)", uw.df$Line...Description)] <- "faculty"
uw.df$input[grepl("(INSTR ACAD STAFF GPR)", uw.df$Line...Description)] <- "academ.staff"
# uw.df$input[grepl("(INSTR AS & TA ALL FUNDS)", uw.df$Line...Description)] <- "ta.s.temp"
uw.df$input[grepl("(-TA GPR)", uw.df$Line...Description)] <- "ta.s.temp"
uw.df$input[grepl("(-TOTAL CREDITS)", uw.df$Line...Description)] <- "credits"
# Need to add dash to TOTAL CREDITS because otherwise it will pick up the percentage
# of total credits in college variable, which we do not want

uw.df$input[grepl("(-MASTERS CREDITS)", uw.df$Line...Description)] <- "masters.credits"
uw.df$input[grepl("(-UNDERGRAD CREDITS)", uw.df$Line...Description)] <- "undergrad.credits"
uw.df$input[grepl("(-PHD CREDITS)", uw.df$Line...Description)] <- "phd.credits"
uw.df$input[grepl("(-SPECIAL CREDITS)", uw.df$Line...Description)] <- "special.credits"
uw.df$input[grepl("(-TOT UNCL SUPPORT ALL FUNDS)", uw.df$Line...Description)] <- "unclassified.supp.staff"
uw.df$input[grepl("(-TOT CLASSIFIED ALL FUNDS)", uw.df$Line...Description)] <- "classified.supp.staff"

# The only credits not represented directy here are professional credits.




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


colnames(uw.df) <- gsub("value[.]", "", colnames(uw.df))



uw.df <- uw.df[ ! uw.df$Department.Name %in% c("SCHOOL OF PHARMACY", "MEDICINE", "FAMILY MEDICINE-GEN", "SCHOOL OF VETERINARY MEDICINE"), ]
# Get rid of professonal schools, although not law since it is included in average of master's student cost.
# Note: business school MBA's seem to be counted as Masters rather than professional


credit.agg <- aggregate(
  cbind( (undergrad.credits + masters.credits + phd.credits + special.credits) , credits) ~ Department.Name,
  uw.df, FUN=sum)

depts.to.exclude <- credit.agg[ (credit.agg[, 2, drop=FALSE] /credit.agg[, 3, drop=FALSE]) < 0.9, 1 ]
# Exclude departments where more than 10% of credits are professional
# This is about 13% of departments

uw.df <- uw.df[! uw.df$Department.Name %in% depts.to.exclude, ]


# uw.df$ta.s <- uw.df$ta.s.temp - uw.df$academ.staff
uw.df$ta.s <- uw.df$ta.s.temp 
uw.df$ta.s.temp <- NULL

uw.df$credit.value <- with(uw.df, 
  undergrad.credits * (13710 / (14.1*2)) + masters.credits * (23559 / (9.0*2)) + phd.credits * (21244 / (6.6*2))  )
# See above for the source on cost per student
# Since these cost-per-student-year, I have the number of credits for a typical full-timer, times two
# The average student credit load is from https://registrar.wisc.edu/documents/Stats_all_2015-2016Fall.pdf

# Delete cells that have no credits. But do not do wholesale exclusion of 
# departments unless all their cells are zero

uw.df <- uw.df[uw.df$credit.value > 0 , ]



perc.dif.fall.spring.df <- aggregate(cbind(faculty, academ.staff, ta.s, 
                                        unclassified.supp.staff, classified.supp.staff ) ~ Term + Department.Name, 
          uw.df[ ! uw.df$time %in% paste0("X_", 1990:1994), ], FUN=mean)

# NOTE: The ordering of these operations matter since the adjustment term
# is now based on only the nonzero values.

# TODO: What do we do with credits that come from, for example "CALS - COLLEGE-WIDE",
# which has no apparent associated faculty?
# For now, just exclude cells without any TAs, faculty, or academ.staff:
uw.df <- uw.df[uw.df$faculty + uw.df$academ.staff + uw.df$ta.s > 0 , ]
uw.df <- uw.df[uw.df$faculty + uw.df$academ.staff + uw.df$ta.s >= 1 , ]
# Ok, actually let's force it to exclude observations where the total FTE is less than 1



impute.1990.94 <- TRUE

if ( impute.1990.94 ) {
  
  for (targ.dept in unique(uw.df$Department.Name) ) {
    for (targ.year in paste0("X_", 1990:1994)) {
      
      perc.dif.fall.spring.adjust <- 
         perc.dif.fall.spring.df[perc.dif.fall.spring.df$Term=="Spring" & 
                                   perc.dif.fall.spring.df$Department.Name==targ.dept, -(1:2)] /
         perc.dif.fall.spring.df[perc.dif.fall.spring.df$Term=="Fall"   & 
                                   perc.dif.fall.spring.df$Department.Name==targ.dept, -(1:2)] 
      # Estimate average decline in the Spring semester numbers so the imputation can be improved
      perc.dif.fall.spring.adjust[, !is.finite(unlist(perc.dif.fall.spring.adjust))] <- 1
      # Handle divide-by-zero case
      
      if ( (sum(uw.df$Department.Name == targ.dept & uw.df$time == targ.year & uw.df$Term == "Fall") +
          sum(uw.df$Department.Name == targ.dept & uw.df$time == targ.year & uw.df$Term == "Spring") ) != 2) {
        next
        # Handles issue if one of the semesters was removed due to a zero credit production
      }
      
      temp.impute <- uw.df[uw.df$Department.Name == targ.dept & uw.df$time == targ.year & uw.df$Term == "Fall", 
            c("faculty", "academ.staff", "ta.s", "unclassified.supp.staff", "classified.supp.staff")]
      uw.df[uw.df$Department.Name == targ.dept & uw.df$time == targ.year & uw.df$Term == "Spring", 
            c("faculty", "academ.staff", "ta.s", "unclassified.supp.staff", "classified.supp.staff")] <- 
          temp.impute * perc.dif.fall.spring.adjust
      stopifnot(all(names(temp.impute) == names(perc.dif.fall.spring.adjust)))
    }
  }
} else {
  # They start recording the data for each semester separately in 1995, so eliminate before then:
  uw.df <- uw.df[ ! uw.df$time %in% paste0("X_", 1990:1994), ]
}


uw.df$support.staff <- with(uw.df, unclassified.supp.staff + classified.supp.staff )

attr(uw.df, "reshapeLong") <- NULL

# Descriptive stats

summary(uw.df)
cor(uw.df[,-(1:5)])
uw.df[uw.df$ta.s > 100, ]
# Seems to be a problem with MEDICINE. I looked into it and
# it seems that we want to change TA defn to TA GPR


 
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

uw.df$term.year <- paste(uw.df$time, uw.df$Term, sep="_")
# NOTE: IMPORTANT: This is not exactly the right listings, since the 2015_Fall is actually 2014 Fall, for example, since
# 2015 refers to the 2014-2015 academic year. It doesn't matter for estimation, though

uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + Department.Name, data=uw.df)[, -1, drop=FALSE] ) 
uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + time, data=uw.df)[, -1, drop=FALSE] ) 
uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + Term , data=uw.df)[, -1, drop=FALSE] ) 
# uw.df <- cbind(uw.df, model.matrix(credits ~  - 1 + term.year , data=uw.df)[, -1, drop=FALSE] ) 

# uw.df[, c("ta.s", "faculty", "academ.staff")] <- uw.df[, c("ta.s", "faculty", "academ.staff")] * 10





# 
quad.output <- quadFuncEst( "credit.value", c("ta.s", "faculty", "academ.staff", "support.staff"), 
  data=uw.df, 
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)

library("plm")



#quad.output <- quadFuncEst( "credit.value", c("ta.s", "faculty", "academ.staff", "support.staff"), 
#  data=plm.data(uw.df, indexes=c("Department.Name", "term.year")), 
#  #shifterNames = colnames(uw.df)[grepl( "(Term)|(time)", colnames(uw.df) ) &
#               #                   (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ],
#  effect = "individual", model = "random" # , random.method ="swar"
#)

#quad.output <- quadFuncEst( "credit.value", c("ta.s", "faculty", "academ.staff", "support.staff"), 
#  data=uw.df, 
#  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(term.year)", colnames(uw.df) ) &
#                                  (! colnames(uw.df) %in% c("Department.Name", "term.year") ) ]
#)

# The term X year model has lower R-sq than the model with individual dummies for term and year


coef(quad.output$est)[1:5]

# SO for shifter names, we take all columns that were created by the model.matrix,
# but don;t take the original source names, i.e. column names that exactly match dept name, etc.

# Maybe useful for getting to fized effects more elegantly:
# https://r-forge.r-project.org/scm/viewvc.php/pkg/tests/quadFunc.R?annotate=1066&root=micecon&pathrev=1094
# https://r-forge.r-project.org/forum/forum.php?max_rows=25&style=nested&offset=6&forum_id=841&group_id=257

# Apparently we can use a plm object here

quad.output.year.interact <- quadFuncEst( "credit.value", c("ta.s", "faculty", "academ.staff", "support.staff",
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


# install.packages("multiwayvcov")
library("multiwayvcov")


margProducts.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"),
          data=uw.df, coef=coef( quad.output ), coefCov = vcov(quad.output$est ) )
# vcovHC(quad.output$est )


str(margProducts.output)
colMeans(margProducts.output)
# Going to be the same as below, since marginal product formula is
# linear, so dont run into Jensen's inequality

margProducts.mean.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"),
          data=sapply(uw.df[!sapply(uw.df, FUN=is.character)], FUN=mean, na.rm=TRUE), 
          coef=coef( quad.output ), coefCov= vcovHC(quad.output$est, type="HC0" ) ) 

margProducts.2015.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"),
          data=sapply(uw.df[uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)], FUN=mean, na.rm=TRUE), 
          coef=coef( quad.output ), coefCov = vcovHC(quad.output$est, type="HC0" ) )

data.frame(coef=unlist(margProducts.mean.output), 
           st.err=unlist(attr(margProducts.mean.output, "stdDev") ))[1:4, ]

data.frame(coef=unlist(margProducts.2015.output), 
           st.err=unlist(attr(margProducts.2015.output, "stdDev") ))[1:4, ]


uw.df.test <- uw.df
colMeans(uw.df.test[, c("ta.s", "faculty", "academ.staff", "support.staff")])
uw.df.test[, c("ta.s", "faculty", "academ.staff", "support.staff")] <- rowMeans(uw.df.test[, c("ta.s", "faculty", "academ.staff", "support.staff")])
# Surpringly, the above actually works
colMeans(uw.df.test[, c("ta.s", "faculty", "academ.staff", "support.staff")])

margProducts.mean.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"),
          data=sapply(uw.df.test[!sapply(uw.df, FUN=is.character)], FUN=mean, na.rm=TRUE), 
          coef=coef( quad.output ), coefCov= vcovHC(quad.output$est, type="HC0" ) )




sum(quadFuncCalc( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"), 
                  data=uw.df[uw.df$time=="X_2015", ], coef=coef( quad.output ), shifterNames = 
                    colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
))



total.credit.value.2015 <- sum(uw.df[uw.df$time=="X_2015", "credit.value"])
# This is the same as the quadFuncCalc, since the regression should exactly predict the mean anyway.

no.labor.counterfactual <- function(labor.component, data) {

  data[, labor.component] <- 0

  total.credit.value.2015.no.labor.component <- sum(
    quadFuncCalc( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"), 
                  data=data, coef=coef( quad.output ), shifterNames = 
                    colnames(data)[grepl( "(Department.Name)|(Term)|(time)", colnames(data) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
  ))
  
  total.credit.value.2015.no.labor.component
}

total.credit.value.2015.no.tas <- no.labor.counterfactual("ta.s", uw.df[uw.df$time=="X_2015", ])

1 - total.credit.value.2015.no.tas / total.credit.value.2015
prettyNum( total.credit.value.2015 - total.credit.value.2015.no.tas, big.mark="," )

prettyNum( 35425  * sum(uw.df[uw.df$time=="X_2015", "ta.s"]), big.mark="," )
# Calc assuming all TAs are senior TAs 
# https://www.ohr.wisc.edu/polproced/UTG/SalRng.html#stuasst

# Keep in mind that both these measures deal with the sum of both
# semesters in 2015

total.credit.value.2015.no.faculty <- no.labor.counterfactual("faculty", uw.df[uw.df$time=="X_2015", ])
1 - total.credit.value.2015.no.faculty / total.credit.value.2015
prettyNum( total.credit.value.2015 - total.credit.value.2015.no.faculty, big.mark="," )
total.credit.value.2015.no.academ.staff <- no.labor.counterfactual("academ.staff", uw.df[uw.df$time=="X_2015", ])
1 - total.credit.value.2015.no.academ.staff / total.credit.value.2015
prettyNum( total.credit.value.2015 - total.credit.value.2015.no.academ.staff, big.mark="," )
total.credit.value.2015.no.support.staff <- no.labor.counterfactual("support.staff", uw.df[uw.df$time=="X_2015", ])
1 - total.credit.value.2015.no.support.staff / total.credit.value.2015
prettyNum( total.credit.value.2015 - total.credit.value.2015.no.support.staff, big.mark="," )

colSums(uw.df[uw.df$time=="X_2015", c("ta.s", "faculty", "academ.staff", "support.staff")])

total.credit.value.2015.no.labor <- no.labor.counterfactual(c("ta.s", "faculty", "academ.staff", "support.staff"), uw.df[uw.df$time=="X_2015", ])
1 - total.credit.value.2015.no.labor / total.credit.value.2015
prettyNum( total.credit.value.2015 - total.credit.value.2015.no.labor, big.mark="," )



attr(margProducts.2015.output, "stdDev")

corrected.varcov <- matrix(0, nrow=length(coef( quad.output.year.interact )), 
                            ncol=length(coef( quad.output.year.interact )))
corrected.varcov[!is.na(coef( quad.output.year.interact )),
                 !is.na(coef( quad.output.year.interact ))] <- vcovHC(quad.output.year.interact$est, type = "HC0" )
# Brilliant! It (appears to) work!
dimnames(corrected.varcov)[[1]] <- names(coef( quad.output.year.interact ))
dimnames(corrected.varcov)[[2]] <- names(coef( quad.output.year.interact ))


margProducts.2015.output.year.interact <- quadFuncDeriv( xNames = 
          c("ta.s", "faculty", "academ.staff", "support.staff",
          colnames(uw.df)[grepl( "(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ] ),
          data=colMeans(uw.df[uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)], na.rm=TRUE), 
          coef= coef( quad.output.year.interact ), 
          coefCov=corrected.varcov 
) # 
# [!is.na(coef( quad.output.year.interact ))]
data.frame(coef=unlist(margProducts.2015.output.year.interact), 
           st.err=unlist(attr(margProducts.2015.output.year.interact, "stdDev") ))[1:4, ]




anova(quad.output$est, quad.output.year.interact$est)
# var.test(quad.output$est, quad.output.year.interact$est)
# I dont think that var.test() is what I want
# waldtest(quad.output.year.interact$est, quad.output$est)
lrtest(quad.output.year.interact$est, quad.output$est)



# My statement says that the employer healthcare contrib was $4818 this year
# Nonresident full-time graduate tuition per semester was $12598.36 this fall semester
# So total non-wage cost of grad student is 4818 + 12598.36 * 2



# 5207.88 per 15 credits for WI residents (actually, 12-18 credits)
# https://registrar.wisc.edu/documents/1162tuition.pdf
revenue.per.credit.if.all.residents <- 5207.88/15

# $ 489 million from tuition, 2014-2015
# https://www.vc.wisc.edu/documents/Budget-in-Brief.pdf
total.fy.2014.2014.credits <- sum(uw.df$credits[uw.df$time=="X_2015"])
total.tuition.revenue <- 489 * 10^6
revenue.per.credit.from.budget <- total.tuition.revenue / total.fy.2014.2014.credits

# Master's cost per student includes law students; doctoral cost per student excludes medical and veterinary students.
# 2013-14 Undergrad cost per student: 13,710
# 2013-14 Master's cost per student: 23,559
# 2013-14 PhD cost per student: 21,244

# Master's cost per student includes law students; doctoral cost per student excludes medical and veterinary students.
# Source: http://legis.wisconsin.gov/lfb/publications/informational-papers/documents/2015/33_uw%20tuition.pdf

# 13710 / 15
# 23559 / 9
# 21244 / 9

# UGRAD_CREDITS
# Total of all credits generated within department by undergraduates (freshman, sophomore, junior or senior)
# MASTERS_CREDITS
# Total of all credits generated within department by masters-level students
# PHD_CREDITS	Total of all credits generated within department by Ph.D. students

# SCHOOL OF PHARMACY; MEDICINE; FAMILY MEDICINE-GEN

revenue.per.credit.from.cost.UG <- 13710 / 15



margProducts.mean.output$ta.s * revenue.per.credit.if.all.residents
margProducts.mean.output$ta.s * revenue.per.credit.from.budget
margProducts.mean.output$ta.s * revenue.per.credit.from.cost.UG

margProducts.2015.output$ta.s * revenue.per.credit.if.all.residents
margProducts.2015.output$ta.s * revenue.per.credit.from.budget
margProducts.2015.output$ta.s * revenue.per.credit.from.cost.UG

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

quad.fun.hessian <- with(as.list(coef(quad.output)), 
  matrix(c(b_1_1, b_1_2, b_1_3, b_1_4,
           b_1_2, b_2_2, b_2_3, b_2_4,
           b_1_3, b_2_3, b_3_3, b_3_4,
           b_1_4, b_2_4, b_3_4, b_4_4), ncol=4)
)

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



system.time(
quad.output.dept.interact <- quadFuncEst( "credits", c("ta.s", "faculty", "academ.staff",
  colnames(uw.df)[grepl( "(Department.Name)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ] ), 
  data=uw.df, 
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)
)


# install.packages("micEconCES")
library("micEconCES")

cesKmenta <- cesEst( yName="credit.value", xNames=c("ta.s", "faculty"), 
  data=uw.df, method = "BFGS",  vrs = TRUE )

, "academ.staff", "support.staff"




quad.output <- quadFuncEst( "credit.value", c("ta.s", "faculty", "academ.staff", "support.staff"), 
  data=uw.df, 
  shifterNames = colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
)


fn.test <- function(params, data, y.var, x.var) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  pred <- params[1] + params[2] * log( quad.matrix %*% params[3:length(params)] )
  resid <- log(data[, y.var]) - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
  sum(resid^2)
}

test.params <- 2 + ncol( poly(as.matrix(uw.df[, c("ta.s", "faculty", "academ.staff", "support.staff")]), degree=2, raw=TRUE)   )

test.params  <- rep(0.1 , times=test.params )

optimal.params <- optim(par=almost.params  , fn=fn.test, data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"),
       method="Ne", control=list(trace=10, maxit=500000))

1 - 1683.449/sum((log(uw.df$credit.value) - mean(log(uw.df$credit.value)) ) ^2)

1 - 667.041707/sum((log(uw.df$credit.value) - mean(log(uw.df$credit.value)) ) ^2)


almost.params <- c(11.667801539574, 0.534141969405913, 7.8208577844026, -0.701011407512023, 
0.493584857421176, 2.60645344648932, 0.218575469175654, 1.52691620624216, 
-1.29290869346299, 1.25334694465767, -0.0120064328616552, 0.43024330698524, 
0.0328566229187893, -0.108916458506194, 0.144886872276533, 0.0053361234986971
)

summary(lm(log(credit.value) ~ (ta.s + faculty + academ.staff + support.staff)^2 +
          I(ta.s^2) + I(faculty^2) + I(academ.staff^2) + I(support.staff^2), data=uw.df))


almost.params <- c(7.52258221554237, 1.33131440020552, 2.41090487152244, 0.28491365470118, 
0.106994199936277, -0.575533751791787, 1.02895241072983, -0.126068096389511, 
-0.284133632811112, 0.977752339413508, 0.999141378109766, 4.31148443833218, 
0.346248754214827, -0.420460541661333, -0.114618851697518, 0.000342831377700391
)


fn.test(almost.params, data=uw.df, y.var = "credit.value", x.var=c("ta.s", "faculty", "academ.staff", "support.staff"))


fn.test.aug <- function(params, data, y.var, x.var, shifters) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
  pred <- params[1] + params[2] * log( quad.matrix %*% params[3:(length(params)-shift.num)] ) + 
    as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)]
  resid <- log(data[, y.var]) - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
  sum(resid^2)
}

optimal.params



optimal.params.for.input <- optimal.params$par
optimal.params.for.input <- c(optimal.params.for.input, rep(0, length(targ.shifters)) )

targ.shifters <- colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") )]


fn.test.aug(optimal.params.for.input, data=uw.df, y.var = "credit.value", 
        x.var=c("ta.s", "faculty", "academ.staff", "support.staff"),
        shifters=targ.shifters)
     
shifter.optimal.params <- optim(par=optimal.params.for.input , fn=fn.test.aug, data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       method="Ne", control=list(trace=10, maxit=500000))


fn.test.aug(shifter.optimal.params$par, data=uw.df, y.var = "credit.value", 
        x.var=c("ta.s", "faculty", "academ.staff", "support.staff"),
        shifters=targ.shifters)

shifter.optimal.params.2 <- optim(par=shifter.optimal.params$par , fn=fn.test.aug, data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       method="BFGS", control=list(trace=10, maxit=500000))

shifter.optimal.params$par[3:20]

fn.test.non.log <- function(params, data, y.var, x.var, shifters) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
  pred <- exp(params[1]) * (quad.matrix %*% params[3:(length(params)-shift.num)] )^(params[2]) * 
    exp( as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)] )
  resid <- data[, y.var] - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
  sum(resid^2)
}

shifter.optimal.params.2 <- optim(par=new.params.2 , fn=fn.test.non.log, data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       method="Ne", control=list(trace=10, maxit=500000))

shifter.optimal.params.3 <- optim(par=shifter.optimal.params.2$par , fn=fn.test.non.log, data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       method="BFGS", control=list(trace=10, maxit=500000))

fn.test.non.log(new.params.2, data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)

1 - 2237234182506884 / sum(( uw.df$credit.value - mean(uw.df$credit.value) ) ^2)

new.params.2 <- c(13.1183885972668, 0.284581152591662, 0.713441422333855, 2.37027530961392, 
1.75110065825376, 0.207581543128229, -0.0160077215355074, -0.184931217543229, 
0.432503808876937, 3.71395049722403, 0.623027982437357, 3.73375597364969, 
0.288536168437156, 0.0685964731849235, -0.563270258170996, 0.00028693582305341, 
-0.422437161386807, -0.240855733696475, -0.15085457404465, -0.405041290339102, 
-1.28662156006031, -1.17551852581742, -0.806154596016622, 0.448053399217603, 
0.0457361184382769, 0.167148291233333, -1.18031734319452, -0.487077165224957, 
-0.28281083970194, 0.0622456461068939, -0.1539318130982, -1.3656491498713, 
-0.468126252240487, -0.462441521597743, -1.053806546488, -0.668560347220134, 
-1.43182771131749, -0.313534954125872, 0.583416178430731, -1.47293674444014, 
0.0332273806943589, -1.14667496570523, -0.327587937503595, -1.46051727362102, 
0.181042222127766, -0.261615490439157, -1.45446030340369, -0.130134837695238, 
0.244913466709701, -0.355993346991657, -0.0926441545282713, -0.45641778168837, 
0.161439586908479, -1.33064766369021, -0.922038198984554, -1.55958704571214, 
-0.663829175815794, -0.297788391047215, -0.728500708529445, 0.75761279088269, 
-0.19678650031491, -0.352494433350036, 0.00446078888154527, 0.348560805162847, 
-0.187381013903906, -0.368874108443704, 0.286523885631566, -1.1362246033611, 
-0.996474101684807, 0.239967786265024, -0.396243555343432, -0.548794644910289, 
-0.0151867768575302, -0.0233221045245571, -0.0291594622693402, 
-0.460248490897924, 0.175773302157896, -0.282618071567036, -0.300224914407073, 
-0.777541153477223, 0.752238221965789, -0.280896868797053, -0.914177280476445, 
-0.211152029756349, -0.0722607995512241, -0.379161628196381, 
-0.192814699590111, -0.904134758272507, -0.349762167387513, -0.528994452553099, 
0.182898985024964, -0.13942076612236, -0.441551302905191, -0.499542535426664, 
-1.19816346706973, -0.727215470012257, -0.296369916480129, 0.308762428963124, 
0.282896634283556, -0.452683858799667, 0.696298849985999, 0.28905473672264, 
-0.741848869917808, -2.07863014727409, -0.791819366212008, -0.608452541970791, 
-0.690351523438011, -4.23459475565939, 0.0727265887531966, -0.989020918289071, 
0.0383123948439294, -1.39408803112943, 0.388093283137662, 0.266838376700037, 
-3.47919458070935, -1.40934563217479, 0.83449722200771, 0.714497871280494, 
-0.388538149434937, -0.347345424751104, 0.204487056043244, -1.12923420997281, 
1.31624566679725, 0.0991076813121336, 0.163627346879028, -0.160520179827547, 
-0.74652951425608, 0.324523815333044, 0.718985401378717, -1.00386769792474, 
0.260869654609912, 0.340728459928907, -0.395055954827015, -0.390272806622859, 
0.392385108207412, 0.246329602189456, 0.145779195362864, 0.188511941591603, 
0.105864640873972, 0.113810254890191, 0.0458782044779149, 0.0562132584569619, 
-0.0314706233263025, -0.0194685024153917, -0.0376058160319968, 
-0.0100269263481557, -0.0327727021598597, -0.0211960005842973, 
-0.0028773435225812, -0.0163238279514044, -0.0384844354418711, 
-0.0395240113576193, -0.0600667834541221, -0.0619938952701653, 
-0.0627721756432277, -0.0456104689116487, -0.0611886135732647, 
-0.0568547844897651, -0.077836226907728, -0.0808480486253997, 
-0.0621618507665233, -0.122350859081196)


mp.factor <- 2


quad.params <- 1:ncol(polym(1:2, 2:3, 3:4, 4:5, degree =2, raw=T))

quad.matrix <- polym(1:2, 2:3, 3:4, 4:5, degree =2, raw=T)
ploy.names.split <- strsplit(colnames(quad.matrix ), "[.]")
columns.to.keep <- sapply(ploy.names.split, FUN=function(x) x[mp.factor]!=0)
quad.matrix.for.chain.rule <- quad.matrix
quad.params[ sapply(ploy.names.split, FUN=function(x) x[mp.factor]==2) ] <- 
  quad.params[ sapply(ploy.names.split, FUN=function(x) x[mp.factor]==2) ]

quad.params <- quad.params[columns.to.keep]

temp.chain.rule <- as.matrix(data[, x.var])
temp.chain.rule[, mp.factor] <- temp.chain.rule[, mp.factor] * 2
temp.chain.rule <- cbind(rep(1, nrow(temp.chain.rule)), temp.chain.rule)

chain.rule <- temp.chain.rule %*% quad.params




quad.matrix.for.chain.rule


fn.test.non.log.MP <- function(params, data, y.var, x.var, shifters, mp.factor) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
  
  quad.params <- params[3:(length(params)-shift.num)]
  
  ploy.names.split <- strsplit(colnames(quad.matrix ), "[.]")
  columns.to.keep <- sapply(ploy.names.split, FUN=function(x) x[mp.factor]!=0)
  quad.matrix.for.chain.rule <- quad.matrix
  quad.params[ sapply(ploy.names.split, FUN=function(x) x[mp.factor]==2) ] <- 
    quad.params[ sapply(ploy.names.split, FUN=function(x) x[mp.factor]==2) ]

  quad.params <- quad.params[columns.to.keep]

  temp.chain.rule <- as.matrix(data[, x.var])
  temp.chain.rule[, mp.factor] <- temp.chain.rule[, mp.factor] * 2
  temp.chain.rule <- cbind(rep(1, nrow(temp.chain.rule)), temp.chain.rule)

  chain.rule <- temp.chain.rule %*% quad.params
  
  ret <- params[2] * exp(params[1]) * (quad.matrix %*% params[3:(length(params)-shift.num)] )^(params[2]-1) * 
    exp( as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)] ) *
    chain.rule
    
#  resid <- data[, y.var] - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
#  sum(resid^2)
  ret
}

mp.test.output <- fn.test.non.log.MP(shifter.optimal.params.2$par, 
       data=uw.df[uw.df$time=="X_2015", ], y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       mp.factor = 4 )

#mp.test.output

summary(mp.test.output )


uw.df$Department.Name=="ECONOMICS" & 



margProducts.output <- quadFuncDeriv( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"),
          data=uw.df[uw.df$time=="X_2015", ], coef=coef( quad.output ), coefCov = vcovHC(quad.output$est ) )


# Just predicted value:

fn.test.non.log.pred <- function(params, data, y.var, x.var, shifters) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
#  pred <- exp(params[1]) * (quad.matrix %*% params[3:(length(params)-shift.num)] )^(params[2]) * 
#    exp( as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)] )
    pred <- exp(params[1]) * ( 1 + quad.matrix %*% c(34.4473003, params[3:(length(params)-shift.num)]) )^(params[2]) * 
    exp( as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)] )
#  resid <- data[, y.var] - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
#  sum(resid^2)
  pred
}


summary(fn.test.non.log.pred(shifter.optimal.params.2$par, 
       data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)
)

table(0 > quadFuncCalc( xNames = c("ta.s", "faculty", "academ.staff", "support.staff"),
          data=uw.df, coef=coef( quad.output ), shifterNames = 
                    colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ] ))
# Ok, so only 1% have negative predictions


fn.test.non.log.quad.part.pred <- function(params, data, y.var, x.var, shifters) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
  pred <-  (quad.matrix %*% params[3:(length(params)-shift.num)] )
#  resid <- data[, y.var] - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
#  sum(resid^2)
  pred
}

summary(fn.test.non.log.quad.part.pred(shifter.optimal.params.2$par, 
       data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)
)
# Seems like taking the power of the quad is foring this to be non-negative. There is one observation
# with barely non-neg: 0.005


fn.test.non.log.resid <- function(params, data, y.var, x.var, shifters) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
  pred <- exp(params[1]) * ( 1 + quad.matrix %*% c(34.4473003, params[3:(length(params)-shift.num)]) )^(params[2]) * 
    exp( as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)] )
#  resid <- data[, y.var] - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
#  sum(resid^2)
  data[, y.var] - pred
}


library("minpack.lm")
test.nls.lm <- nls.lm(par=shifter.optimal.params.2$par , fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter=300, maxfev=10000000), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)


test.nls.lm.2 <- nls.lm(par=coef(test.nls.lm), fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter=300, maxfev=10000000), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)


targ.shifters <- colnames(uw.df)[grepl( "(Department.Name)|(Term)|(time)", colnames(uw.df) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") )]

test.nls.lm.11 <- nls.lm(par=coef(test.nls.lm.7)[-(159:length(coef(test.nls.lm.7)))], fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter=500, maxfev=10000000), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)

test.nls.lm.11 <- nls.lm(par=coef(test.nls.lm.10), fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter=500, maxfev=10000000), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)

test.nls.lm.12 <- nls.lm(par=coef(test.nls.lm.11)[-3], fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter=500, maxfev=10000000), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)


c(coef(test.nls.lm.12)[1:2], 34.4473003, 
                                     coef(test.nls.lm.12)[3:length(coef(test.nls.lm.12))])
mp.test.output <- fn.test.non.log.MP.fixed(coef(test.nls.lm.12),
       data=uw.df[uw.df$time=="X_2015", ], y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       mp.factor = 1 )

#mp.test.output

summary(mp.test.output )

# Thanks to http://stats.stackexchange.com/questions/122066/how-to-use-delta-method-for-standard-errors-of-marginal-effects

require(numDeriv) # Load numerical derivative package

grad_g <-  jacobian(fn.test.non.log.MP.fixed, coef(test.nls.lm.12),
                    data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters,
       mp.factor = 1 ) # Jacobian gives dimensions, otherwise same as
                               # gradient 

grad_g <-  jacobian(no.labor.counterfactual.nested, coef(test.nls.lm.12), labor.component="ta.s", data=uw.df[uw.df$time=="X_2015", ])

# [uw.df$time=="X_2015", !sapply(uw.df, FUN=is.character)]

summary(diag(sqrt(grad_g %*% vcov(test.nls.lm.12) %*% t(grad_g)))) # Should be exactly the same )









test.nls.lm.random <- nls.lm(par=rep(.1, 158), fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter=500, maxfev=10000000), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)

1 - 1.70342e+15 / sum(( uw.df$credit.value - mean(uw.df$credit.value) ) ^2)

quad.output$r2


library("numDeriv")
rankMatrix library("Matrix")
# coef(test.nls.lm.7)
fn.test.jac <- jacobian(fn.test.non.log.pred, coef(test.nls.lm.11), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)
# fn.test.non.log.resid
fn.test.jac[!is.finite(fn.test.jac)] <- 0
# This is a hack - but just two elements that are non-finite

rankMatrix(fn.test.jac)
ncol(fn.test.jac)

table(apply(fn.test.jac, 2, FUN=function(x) sum(!is.finite(x)) ))
which(apply(fn.test.jac, 2, FUN=function(x) sum(!is.finite(x)) ) >0)
which(apply(fn.test.jac, 1, FUN=function(x) sum(!is.finite(x)) ) >0)

uw.df[which(apply(fn.test.jac, 1, FUN=function(x) sum(!is.finite(x)) ) >0), 1:17]


# fn.test.jac.2 <- fn.test.jac[ apply(fn.test.jac, 1, FUN=function(x) sum(!is.finite(x)) ) == 0, ]

set.seed(100)
fn.test.jac.2 <- fn.test.jac
fn.test.jac.2[!is.finite(fn.test.jac.2)] <- runif(sum(!is.finite(fn.test.jac.2)))

rankMatrix(fn.test.jac.2)
ncol(fn.test.jac.2)


[, -3]
var.cov.est <-  solve(crossprod(fn.test.jac.2), tol=10^-30) * as.vector(var(fn.test.non.log.resid(
       coef(test.nls.lm.11), data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)))

# format(sqrt(diag(var.cov.est)))[1:12]


coef(test.nls.lm.11)[1:12]/sqrt(diag(var.cov.est))[1:12]
summary(quad.output$est)$coefficients[,"t value"][1:12]

 testcols <- function(ee) {
       ## split eigenvector matrix into a list, by columns
       evecs <- split(zapsmall(ee$vectors),col(ee$vectors))
       ## for non-zero eigenvalues, list non-zero evec components
       mapply(function(val,vec) {
           if (val!=0) NULL else which(vec!=0)
       },zapsmall(ee$values),evecs)
       # Thanks to http://stackoverflow.com/questions/12304963/using-eigenvalues-to-test-for-singularity-identifying-collinear-columns
   }
   
#lambda.and.07.cols <- stacked.jacobian[ , grepl("(lambda)|(07)", colnames(stacked.jacobian) )] 

testcols(eigen(crossprod(fn.test.jac.2)))

str(solve(crossprod(fn.test.jac.2), tol=10^-30))

diag(solve(crossprod(fn.test.jac.2), tol=10^-30)

#testcols(eigen(crossprod(lambda.and.07.cols)))
 # 

cor(test.poly <-  poly(as.matrix(uw.df[, c("ta.s", "faculty", "academ.staff", "support.staff")]), degree=2, raw=TRUE))

summary(cor(test.poly)[cor(test.poly)!=1])


test.nls.lm.final <- nls.lm(par=coef(test.nls.lm.8), fn=fn.test.non.log.resid, 
           control = nls.lm.control(nprint=5, maxiter = -1, maxfev=10000000), 
           data=uw.df, y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), shifters=targ.shifters)


[apply(fn.test.jac, 1, FUN=function(x) sum(!is.finite(x)) ) == 0, ]


summary(rowSums(uw.df[, c("ta.s", "faculty", "academ.staff")])<1)


min(uw.df$credit.value)



fn.test.non.log.MP.fixed <- function(params, data, y.var, x.var, shifters, mp.factor) {
  quad.matrix <- poly(as.matrix(data[, x.var]), degree=2, raw=TRUE)
  shift.num <- length(shifters)
  
  quad.params <- c(34.4473003, params[3:(length(params)-shift.num)])
  
  ploy.names.split <- strsplit(colnames(quad.matrix ), "[.]")
  columns.to.keep <- sapply(ploy.names.split, FUN=function(x) x[mp.factor]!=0)
  quad.matrix.for.chain.rule <- quad.matrix
  quad.params[ sapply(ploy.names.split, FUN=function(x) x[mp.factor]==2) ] <- 
    quad.params[ sapply(ploy.names.split, FUN=function(x) x[mp.factor]==2) ]

  quad.params <- quad.params[columns.to.keep]

  temp.chain.rule <- as.matrix(data[, x.var])
  temp.chain.rule[, mp.factor] <- temp.chain.rule[, mp.factor] * 2
  temp.chain.rule <- cbind(rep(1, nrow(temp.chain.rule)), temp.chain.rule)

  chain.rule <- temp.chain.rule %*% quad.params
  
  ret <- params[2] * exp(params[1]) * (quad.matrix %*% c(34.4473003, params[3:(length(params)-shift.num)]) )^(params[2]-1) * 
    exp( as.matrix(data[, shifters]) %*% params[(length(params)-shift.num + 1):length(params)] ) *
    chain.rule
    
#  resid <- data[, y.var] - pred
#  cat(dput(params), "\n")
#  assign("global.parm", params, envir = .GlobalEnv)
#  sum(resid^2)
  ret
}




total.credit.value.2015 <- sum(uw.df[uw.df$time=="X_2015", "credit.value"])
# This is the same as the quadFuncCalc, since the regression should exactly predict the mean anyway.

no.labor.counterfactual.nested <- function(params, labor.component, data) {

  data[, labor.component] <- 0

  total.credit.value.2015.no.labor.component <- sum(
    fn.test.non.log.pred( params,
                    y.var = "credit.value", 
       x.var=c("ta.s", "faculty", "academ.staff", "support.staff"), 
                  data=data,  shifters = 
                    colnames(data)[grepl( "(Department.Name)|(Term)|(time)", colnames(data) ) &
                                  (! colnames(uw.df) %in% c("Department.Name", "Term", "time") ) ]
  ))
  
  total.credit.value.2015.no.labor.component
}

total.credit.value.2015.no.tas <- no.labor.counterfactual(coef(test.nls.lm.12), "ta.s", uw.df[uw.df$time=="X_2015", ])

1 - total.credit.value.2015.no.tas / total.credit.value.2015
prettyNum( total.credit.value.2015 - total.credit.value.2015.no.tas, big.mark="," )

prettyNum( 35425  * sum(uw.df[uw.df$time=="X_2015", "ta.s"]), big.mark="," )

test.nls.lm.12$m$resid <- test.nls.lm.12$fvec

(2*158 - 2* logLik.nls.repaired(test.nls.lm.12)) -
(2*158 - 2* logLik(quad.output$est))

qqnorm(test.nls.lm.12$m$resid);qqline(test.nls.lm.12$m$resid, col = 2)
qqnorm(resid(quad.output$est));qqline(resid(quad.output$est), col = 2)

shapiro.test(test.nls.lm.12$m$resid)
shapiro.test(resid(quad.output$est))
# "The user may reject the null hypothesis if W is below a predetermined threshold."
# shapiro.test(rnorm(5000))


logLik.nls.repaired <- function(object, REML = FALSE, ...) {
    if (REML) 
        stop("cannot calculate REML log-likelihood for \"nls\" objects")
    res <- object$m$resid
    N <- length(res)
    if (is.null(w <- object$weights)) 
        w <- rep_len(1, N)
    zw <- w == 0
    val <- -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw)) + 
        log(sum(w * res^2)))/2
    attr(val, "df") <- 1L + length(coef(object))
    attr(val, "nobs") <- attr(val, "nall") <- sum(!zw)
    class(val) <- "logLik"
    val
}



