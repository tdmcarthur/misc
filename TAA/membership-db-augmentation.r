

# This all is a little hack-y since I dont keep the three datasets in w big list, which would have made this more elegant

membership.file.location <- "/Users/travismcarthur/Desktop/TAA work/Grad student database/20151022 - TAA membership.csv"
employment.file.location <- "/Users/travismcarthur/Desktop/TAA work/Grad student database/TAA Open Records Request 2015.csv"
enrollment.file.location <- "/Users/travismcarthur/Desktop/TAA work/Grad student database/GRAD_Enrolled_Home_Mail_LvngOnCampus_Phone_Email_AcadGroup_Major.csv"

membership.df <- read.csv(membership.file.location, stringsAsFactor=FALSE, header=FALSE, fileEncoding="Latin1")
membership.df <- membership.df[-1, ]
colnames(membership.df) <- make.names(strsplit(readLines(membership.file.location, n=1), split=",")[[1]])
# It has a problem with reading in the column names, so have to do this.
# Seems the root problem is the weird "\xca" strings in the EmployeeId/PayrollId column

employment.df <- read.csv(employment.file.location, stringsAsFactor=FALSE, fileEncoding="Latin1")
enrollment.df <- read.csv(enrollment.file.location, stringsAsFactor=FALSE, fileEncoding="Latin1")
# NOTE: Must have the input files be saved in Excel as "Windows Comma Separated"


membership.df$membership.db.id <- 1:nrow(membership.df)
employment.df$employment.db.id <- 1:nrow(employment.df)
enrollment.df$enrollment.db.id <- 1:nrow(enrollment.df)


# Splitting the name column in the employment dataset




employment.names.split.ls <- lapply(strsplit(employment.df$Name, split=","),   
       function(x) {
         data.frame(first.name=strsplit(x[2], " ")[[1]][1], 
                    last.name=x[1], stringsAsFactors=FALSE)
         } 
)
# Have to do a second split since don't want to split up any spaces in the last name

employment.df <- cbind(employment.df, do.call(rbind, employment.names.split.ls) )

# Setting all names to upper so comparison is easier below

#membership.df[, c("First.Name", "Last.Name")] <- lapply(membership.df[, c("First.Name", "Last.Name")] , FUN=toupper)
#employment.df[, c("first.name", "last.name")] <- lapply(employment.df[, c("first.name", "last.name")] , FUN=toupper)
#enrollment.df[, c("NAME_FIRST_PREFERRED", "NAME_LAST_PREFERRED")] <- lapply(enrollment.df[, c("NAME_FIRST_PREFERRED", "NAME_LAST_PREFERRED")] , FUN=toupper)


membership.df[, c("F.name.for.match.MP", "L.name.for.match.MP")] <- lapply(membership.df[, c("First.Name", "Last.Name")] , FUN=toupper)
employment.df[, c("F.name.for.match.EM", "L.name.for.match.EM")] <- lapply(employment.df[, c("first.name", "last.name")] , FUN=toupper)
enrollment.df[, c("F.name.for.match.EN", "L.name.for.match.EN")] <- lapply(enrollment.df[, c("NAME_FIRST_PREFERRED", "NAME_LAST_PREFERRED")] , FUN=toupper)

# MP = membership
# EM = employment
# EN = enrollment

# TODO: May want to cut out possible middle names for the "display" names for the database, rather
# than naively taking NAME_FIRST_PREFERRED, etc.

membership.df$F.name.for.match.MP <- gsub("(^ +)|( +$)", "", membership.df$F.name.for.match.MP)
employment.df$F.name.for.match.EM <- gsub("(^ +)|( +$)", "", employment.df$F.name.for.match.EM)
enrollment.df$F.name.for.match.EN <- gsub("(^ +)|( +$)", "", enrollment.df$F.name.for.match.EN)
# Remove any trailing or leading spaces
membership.df$F.name.for.match.MP <- gsub("( +.+)", "", membership.df$F.name.for.match.MP)
employment.df$F.name.for.match.EM <- gsub("( +.+)", "", employment.df$F.name.for.match.EM)
enrollment.df$F.name.for.match.EN <- gsub("( +.+)", "", enrollment.df$F.name.for.match.EN)
# Cutting out any middle names. May create a bit of a problem with international names.
# We should do this for enrollment, too, correct? Have done it above.





# Splitting email into the pure and composite wisc emails

membership.df$pure.wisc.email <- ifelse(grepl("@wisc.edu", membership.df$Primary.Email, 
                                              fixed=TRUE), membership.df$Primary.Email, NA)
membership.df$pure.wisc.email <- ifelse(grepl("@wisc.edu", membership.df$Secondary.Email, 
  fixed=TRUE) & is.na(membership.df$pure.wisc.email),
  membership.df$Secondary.Email, membership.df$pure.wisc.email)

membership.df$composite.wisc.email <- ifelse(grepl(".wisc.edu", membership.df$Primary.Email, 
                                              fixed=TRUE), membership.df$Primary.Email, NA)
membership.df$composite.wisc.email <- ifelse(grepl(".wisc.edu", membership.df$Secondary.Email, 
  fixed=TRUE) & is.na(membership.df$composite.wisc.email),
  membership.df$Secondary.Email, membership.df$composite.wisc.email)


enrollment.df$pure.wisc.email <- ifelse(grepl("@wisc.edu", enrollment.df$EMAIL_ADDRESS, 
                                              fixed=TRUE), enrollment.df$EMAIL_ADDRESS, NA)

enrollment.df$composite.wisc.email <- ifelse(grepl(".wisc.edu", enrollment.df$EMAIL_ADDRESS, 
                                              fixed=TRUE), enrollment.df$EMAIL_ADDRESS, NA)












membership.dup.names <- membership.df[ 
    duplicated(membership.df[, c("F.name.for.match.MP", "L.name.for.match.MP")] ),
    c("F.name.for.match.MP", "L.name.for.match.MP")]

membership.dup.names <- membership.dup.names[!duplicated(membership.dup.names), ]

employment.dup.names <- employment.df[ 
    duplicated(employment.df[, c("F.name.for.match.EM", "L.name.for.match.EM")] ),
    c("F.name.for.match.EM", "L.name.for.match.EM")]

employment.dup.names <- employment.dup.names[!duplicated(employment.dup.names), ]


enrollment.dup.names <- enrollment.df[ 
    duplicated(enrollment.df[, c("F.name.for.match.EN", "L.name.for.match.EN")] ),
    c("F.name.for.match.EN", "L.name.for.match.EN")]

enrollment.dup.names <- enrollment.dup.names[!duplicated(enrollment.dup.names), ]



colnames(membership.dup.names) <- colnames(employment.dup.names) <- colnames(enrollment.dup.names) <- c("first.name", "last.name")
# Being a bit "clever" - maybe too clever for my own good

combined.dup.names.df <- rbind( membership.dup.names, employment.dup.names, enrollment.dup.names)
combined.dup.names.df <- combined.dup.names.df[!duplicated(combined.dup.names.df), ]

combined.dup.names.df$names.combined <- paste(combined.dup.names.df$first.name, combined.dup.names.df$last.name, sep="|")

membership.df$names.combined <- paste(membership.df$F.name.for.match.MP, membership.df$L.name.for.match.MP, sep="|")
employment.df$names.combined <- paste(employment.df$F.name.for.match.EM, employment.df$L.name.for.match.EM, sep="|")
enrollment.df$names.combined <- paste(enrollment.df$F.name.for.match.EN, enrollment.df$L.name.for.match.EN, sep="|")



membership.dups.records.df <- membership.df[membership.df$names.combined %in% combined.dup.names.df$names.combined, ]
membership.after.dedup.df <- membership.df[ ! membership.df$names.combined %in% combined.dup.names.df$names.combined, ]
  

employment.dups.records.df <- employment.df[employment.df$names.combined %in% combined.dup.names.df$names.combined, ]
employment.after.dedup.df <- employment.df[ ! employment.df$names.combined %in% combined.dup.names.df$names.combined, ]


enrollment.dups.records.df <- enrollment.df[enrollment.df$names.combined %in% combined.dup.names.df$names.combined, ]
enrollment.after.dedup.df <- enrollment.df[ ! enrollment.df$names.combined %in% combined.dup.names.df$names.combined, ]


# One good measure of inaccuracy of matching is below - how many 
# names duplicated in each dataset
nrow(membership.dups.records.df)
nrow(employment.dups.records.df)
nrow(enrollment.dups.records.df)

# Ok, below trying naive matching - hopefully it will get the lion's share

# Iterative 3-way merge:
merge.temp <- merge(enrollment.after.dedup.df[, c("names.combined", "enrollment.db.id")], 
                    membership.after.dedup.df[, c("names.combined", "membership.db.id")], all=TRUE)

merge.temp <- merge(merge.temp, 
                    employment.after.dedup.df[, c("names.combined", "employment.db.id")], all=TRUE)


# Nice summary of the status of the match as of now:
ftable(enrol=!is.na(merge.temp$enrollment.db.id), emp=!is.na(merge.temp$employment.db.id), memb=!is.na(merge.temp$membership.db.id))

# Do we need to circle back around to the beginning?, i.e. merge back in enrollment?
# I think we are ok, given we have only 3 datasets, and therefore the rekationship is
# triangular, i.e. circular

enrollment.merge.names.df <- merge.temp[!is.na(merge.temp$enrollment.db.id), ]
employment.merge.names.df <- merge.temp[!is.na(merge.temp$employment.db.id), ]
membership.merge.names.df <- merge.temp[!is.na(merge.temp$membership.db.id), ]

#enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, enrollment.merge.temp.df)
#employment.after.dedup.df <- merge(employment.after.dedup.df, employment.merge.temp.df)
#membership.after.dedup.df <- merge(membership.after.dedup.df, membership.merge.temp.df)





#enrollment.df$pure.wisc.email
#enrollment.df$composite.wisc.email

employment.after.dedup.df$pure.wisc.email <- "NOMATCH@wisc.edu"
employment.after.dedup.df$composite.wisc.email <- "NOMATCH@nomatch.wisc.edu"

# TODO: NA's for now, before we work out the webscrape.


# Iterative 3-way merge:
merge.temp <- merge(enrollment.after.dedup.df[
  !is.na(enrollment.after.dedup.df$pure.wisc.email), 
  c("pure.wisc.email", "enrollment.db.id")], 
                    membership.after.dedup.df[
  !is.na(membership.after.dedup.df$pure.wisc.email), 
  c("pure.wisc.email", "membership.db.id")], all=TRUE)

merge.temp <- merge(merge.temp, 
                    employment.after.dedup.df[, c("pure.wisc.email", "employment.db.id")], all=TRUE)

merge.temp <- merge.temp[!is.na(merge.temp$pure.wisc.email), ]

# Nice summary of the status of the match as of now:
ftable(enrol=!is.na(merge.temp$enrollment.db.id), memb=!is.na(merge.temp$membership.db.id))
#ftable(enrol=!is.na(merge.temp$enrollment.db.id), emp=!is.na(merge.temp$employment.db.id), memb=!is.na(merge.temp$membership.db.id))

enrollment.merge.pure.email.df <- merge.temp[!is.na(merge.temp$enrollment.db.id), ]
employment.merge.pure.email.df <- merge.temp[!is.na(merge.temp$employment.db.id), ]
membership.merge.pure.email.df <- merge.temp[!is.na(merge.temp$membership.db.id), ]

#enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, enrollment.merge.temp.df)
#employment.after.dedup.df <- merge(employment.after.dedup.df, employment.merge.temp.df)
#membership.after.dedup.df <- merge(membership.after.dedup.df, membership.merge.temp.df)



# Iterative 3-way merge:
merge.temp <- merge(enrollment.after.dedup.df[
  !is.na(enrollment.after.dedup.df$composite.wisc.email), 
  c("composite.wisc.email", "enrollment.db.id")], 
                    membership.after.dedup.df[
  !is.na(membership.after.dedup.df$composite.wisc.email), 
  c("composite.wisc.email", "membership.db.id")], all=TRUE)

merge.temp <- merge(merge.temp, 
  employment.after.dedup.df[, c("composite.wisc.email", "employment.db.id")], all=TRUE)

merge.temp <- merge.temp[!is.na(merge.temp$composite.wisc.email), ]

# Nice summary of the status of the match as of now:
ftable(enrol=!is.na(merge.temp$enrollment.db.id), memb=!is.na(merge.temp$membership.db.id))
#ftable(enrol=!is.na(merge.temp$enrollment.db.id), emp=!is.na(merge.temp$employment.db.id), memb=!is.na(merge.temp$membership.db.id))

enrollment.merge.composite.email.df <- merge.temp[!is.na(merge.temp$enrollment.db.id), ]
employment.merge.composite.email.df <- merge.temp[!is.na(merge.temp$employment.db.id), ]
membership.merge.composite.email.df <- merge.temp[!is.na(merge.temp$membership.db.id), ]

#enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, enrollment.merge.temp.df)
#employment.after.dedup.df <- merge(employment.after.dedup.df, employment.merge.temp.df)
#membership.after.dedup.df <- merge(membership.after.dedup.df, membership.merge.temp.df)

#For blah set - iterate so that each merging method replaces the merge key if its not NA


#############
#############
# Enrollment

colnames(enrollment.merge.names.df) <- paste0(colnames(enrollment.merge.names.df), ".names")
colnames(enrollment.merge.names.df)[grepl("enrollment", colnames(enrollment.merge.names.df))] <- "enrollment.db.id"

colnames(enrollment.merge.pure.email.df) <- paste0(colnames(enrollment.merge.pure.email.df), ".pure.email")
colnames(enrollment.merge.pure.email.df)[grepl("enrollment", colnames(enrollment.merge.pure.email.df))] <- "enrollment.db.id"

colnames(enrollment.merge.composite.email.df) <- paste0(colnames(enrollment.merge.composite.email.df), ".composite.email")
colnames(enrollment.merge.composite.email.df)[grepl("enrollment", colnames(enrollment.merge.composite.email.df))] <- "enrollment.db.id"



enrollment.merge.master.df<- merge( merge(enrollment.merge.names.df, enrollment.merge.pure.email.df, all=TRUE), enrollment.merge.composite.email.df,  all=TRUE)



table( 
  apply(enrollment.merge.master.df[, c("membership.db.id.names", "membership.db.id.pure.email","membership.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) <= 1
} )
)   
# IMPORTANT - TODO: Need to have user-readable output about this. If any of these are FALSE, 
# then we have a problem, 
# since the different ways of finding matches somehow do not agree

enrollment.merge.master.df$membership.db.id.crossref <- apply(enrollment.merge.master.df[, c("membership.db.id.names", "membership.db.id.pure.email","membership.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
    ifelse(length(unique(x))==1, x, NA)
} )

enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, enrollment.merge.master.df[, c("membership.db.id.crossref", "enrollment.db.id")] )
# NOTE: Should have same number of rows in each dataset. Maybe TODO: have a nrow check here




table( 
  apply(enrollment.merge.master.df[, c("employment.db.id.names", "employment.db.id.pure.email","employment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) <= 1
} )
)   
# IMPORTANT - TODO: Need to have user-readable output about this. If any of these are FALSE, 
# then we have a problem, 
# since the different ways of finding matches somehow do not agree

enrollment.merge.master.df$employment.db.id.crossref <- apply(enrollment.merge.master.df[, c("employment.db.id.names", "employment.db.id.pure.email","employment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
    ifelse(length(unique(x))==1, x, NA)
} )

enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, enrollment.merge.master.df[, c("employment.db.id.crossref", "enrollment.db.id")] )
# NOTE: Should have same number of rows in each dataset. Maybe TODO: have a nrow check here

#############
#############
### Membership


colnames(membership.merge.names.df) <- paste0(colnames(membership.merge.names.df), ".names")
colnames(membership.merge.names.df)[grepl("membership", colnames(membership.merge.names.df))] <- "membership.db.id"

colnames(membership.merge.pure.email.df) <- paste0(colnames(membership.merge.pure.email.df), ".pure.email")
colnames(membership.merge.pure.email.df)[grepl("membership", colnames(membership.merge.pure.email.df))] <- "membership.db.id"

colnames(membership.merge.composite.email.df) <- paste0(colnames(membership.merge.composite.email.df), ".composite.email")
colnames(membership.merge.composite.email.df)[grepl("membership", colnames(membership.merge.composite.email.df))] <- "membership.db.id"




membership.merge.master.df<- merge( merge(membership.merge.names.df, membership.merge.pure.email.df, all=TRUE), membership.merge.composite.email.df,  all=TRUE)



table( 
  apply(membership.merge.master.df[, c("enrollment.db.id.names", "enrollment.db.id.pure.email","enrollment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) <= 1
} )
)   
# IMPORTANT - TODO: Need to have user-readable output about this. If any of these are FALSE, 
# then we have a problem, 
# since the different ways of finding matches somehow do not agree

membership.merge.master.df$enrollment.db.id.crossref <- apply(membership.merge.master.df[, c("enrollment.db.id.names", "enrollment.db.id.pure.email","enrollment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  ifelse(length(unique(x))==1, x, NA)
} )

membership.after.dedup.df <- merge(membership.after.dedup.df, membership.merge.master.df[, c("enrollment.db.id.crossref", "membership.db.id")] )
# NOTE: Should have same number of rows in each dataset. Maybe TODO: have a nrow check here

 table(!is.na(membership.after.dedup.df$enrollment.db.id.crossref))
# 163


table( 
  apply(membership.merge.master.df[, c("employment.db.id.names", "employment.db.id.pure.email","employment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) <= 1
} )
)   
# IMPORTANT - TODO: Need to have user-readable output about this. If any of these are FALSE, 
# then we have a problem, 
# since the different ways of finding matches somehow do not agree

membership.merge.master.df$employment.db.id.crossref <- apply(membership.merge.master.df[, c("employment.db.id.names", "employment.db.id.pure.email","employment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
    ifelse(length(unique(x))==1, x, NA)
} )

membership.after.dedup.df <- merge(membership.after.dedup.df, membership.merge.master.df[, c("employment.db.id.crossref", "membership.db.id")] )
# NOTE: Should have same number of rows in each dataset. Maybe TODO: have a nrow check here



##########
##########
### Employment




colnames(employment.merge.names.df) <- paste0(colnames(employment.merge.names.df), ".names")
colnames(employment.merge.names.df)[grepl("employment", colnames(employment.merge.names.df))] <- "employment.db.id"

colnames(employment.merge.pure.email.df) <- paste0(colnames(employment.merge.pure.email.df), ".pure.email")
colnames(employment.merge.pure.email.df)[grepl("employment", colnames(employment.merge.pure.email.df))] <- "employment.db.id"

colnames(employment.merge.composite.email.df) <- paste0(colnames(employment.merge.composite.email.df), ".composite.email")
colnames(employment.merge.composite.email.df)[grepl("employment", colnames(employment.merge.composite.email.df))] <- "employment.db.id"


employment.merge.master.df<- merge( merge(employment.merge.names.df, employment.merge.pure.email.df, all=TRUE), employment.merge.composite.email.df,  all=TRUE)


table( 
  apply(employment.merge.master.df[, c("enrollment.db.id.names", "enrollment.db.id.pure.email","enrollment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) <= 1
} )
)   
# IMPORTANT - TODO: Need to have user-readable output about this. If any of these are FALSE, 
# then we have a problem, 
# since the different ways of finding matches somehow do not agree

employment.merge.master.df$enrollment.db.id.crossref <- apply(employment.merge.master.df[, c("enrollment.db.id.names", "enrollment.db.id.pure.email","enrollment.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
    ifelse(length(unique(x))==1, x, NA)
} )

employment.after.dedup.df <- merge(employment.after.dedup.df, employment.merge.master.df[, c("enrollment.db.id.crossref", "employment.db.id")] )
# NOTE: Should have same number of rows in each dataset. Maybe TODO: have a nrow check here



table( 
  apply(employment.merge.master.df[, c("membership.db.id.names", "membership.db.id.pure.email","membership.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) <= 1
} )
)   
# IMPORTANT - TODO: Need to have user-readable output about this. If any of these are FALSE, 
# then we have a problem, 
# since the different ways of finding matches somehow do not agree

employment.merge.master.df$membership.db.id.crossref <- apply(employment.merge.master.df[, c("membership.db.id.names", "membership.db.id.pure.email","membership.db.id.composite.email" )], 1, FUN=function(x) {
  x <- x[!is.na(x)]
    ifelse(length(unique(x))==1, x, NA)
} )

employment.after.dedup.df <- merge(employment.after.dedup.df, employment.merge.master.df[, c("membership.db.id.crossref", "employment.db.id")] )
# NOTE: Should have same number of rows in each dataset. Maybe TODO: have a nrow check here


############
############
# checking goodness of linkage after name and email match

#### membership-enrollment 

master.for.goodness.of.link.df <- merge(
  enrollment.after.dedup.df[, c("membership.db.id.crossref", "enrollment.db.id")], 
  membership.after.dedup.df[, c("enrollment.db.id.crossref", "membership.db.id")], 
  by.x="enrollment.db.id",
  by.y="enrollment.db.id.crossref", all=TRUE)
# Will do below when employment scraper is up and running
# master.for.goodness.of.link.df <- merge(master.for.goodness.of.link.df, employment.after.dedup.df[something], all=TRUE)

ftable(enrol=!is.na(master.for.goodness.of.link.df$enrollment.db.id), 
              memb=!is.na(master.for.goodness.of.link.df$membership.db.id))


membership.enrollment.link.key.df <- merge(
  enrollment.after.dedup.df[, c("membership.db.id.crossref", "enrollment.db.id")], 
  membership.after.dedup.df[, c("enrollment.db.id.crossref", "membership.db.id")], 
  by.x="enrollment.db.id",
  by.y="enrollment.db.id.crossref", all=TRUE)

membership.enrollment.link.key.df$membership.enrollment.key <- NA
membership.enrollment.link.key.df$membership.enrollment.key[
  !is.na(membership.enrollment.link.key.df$membership.db.id.crossref) ] <-
    1:sum(!is.na(membership.enrollment.link.key.df$membership.db.id.crossref))

enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, 
      membership.enrollment.link.key.df[
        !is.na(membership.enrollment.link.key.df$membership.enrollment.key), 
        c("enrollment.db.id", "membership.enrollment.key")], all.x=TRUE)

membership.after.dedup.df <- merge(membership.after.dedup.df, 
      membership.enrollment.link.key.df[
        !is.na(membership.enrollment.link.key.df$membership.enrollment.key), 
        c("membership.db.id", "membership.enrollment.key")], all.x=TRUE)


#### membership-employment link

master.for.goodness.of.link.df <- merge(
  employment.after.dedup.df[, c("membership.db.id.crossref", "employment.db.id")], 
  membership.after.dedup.df[, c("employment.db.id.crossref", "membership.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)
# Will do below when employment scraper is up and running
# master.for.goodness.of.link.df <- merge(master.for.goodness.of.link.df, employment.after.dedup.df[something], all=TRUE)

ftable(enrol=!is.na(master.for.goodness.of.link.df$employment.db.id), 
              memb=!is.na(master.for.goodness.of.link.df$membership.db.id))


membership.employment.link.key.df <- merge(
  employment.after.dedup.df[, c("membership.db.id.crossref", "employment.db.id")], 
  membership.after.dedup.df[, c("employment.db.id.crossref", "membership.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)

membership.employment.link.key.df$membership.employment.key <- NA
membership.employment.link.key.df$membership.employment.key[
  !is.na(membership.employment.link.key.df$membership.db.id.crossref) ] <-
    1:sum(!is.na(membership.employment.link.key.df$membership.db.id.crossref))

employment.after.dedup.df <- merge(employment.after.dedup.df, 
      membership.employment.link.key.df[
        !is.na(membership.employment.link.key.df$membership.employment.key), 
        c("employment.db.id", "membership.employment.key")], all.x=TRUE)

membership.after.dedup.df <- merge(membership.after.dedup.df, 
      membership.employment.link.key.df[
        !is.na(membership.employment.link.key.df$membership.employment.key), 
        c("membership.db.id", "membership.employment.key")], all.x=TRUE)



#### enrollment-employment link

master.for.goodness.of.link.df <- merge(
  employment.after.dedup.df[, c("enrollment.db.id.crossref", "employment.db.id")], 
  enrollment.after.dedup.df[, c("employment.db.id.crossref", "enrollment.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)
# Will do below when employment scraper is up and running
# master.for.goodness.of.link.df <- merge(master.for.goodness.of.link.df, employment.after.dedup.df[something], all=TRUE)

ftable(enrol=!is.na(master.for.goodness.of.link.df$employment.db.id), 
              memb=!is.na(master.for.goodness.of.link.df$enrollment.db.id))


enrollment.employment.link.key.df <- merge(
  employment.after.dedup.df[, c("enrollment.db.id.crossref", "employment.db.id")], 
  enrollment.after.dedup.df[, c("employment.db.id.crossref", "enrollment.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)

enrollment.employment.link.key.df$enrollment.employment.key <- NA
enrollment.employment.link.key.df$enrollment.employment.key[
  !is.na(enrollment.employment.link.key.df$enrollment.db.id.crossref) ] <-
    1:sum(!is.na(enrollment.employment.link.key.df$enrollment.db.id.crossref))

employment.after.dedup.df <- merge(employment.after.dedup.df, 
      enrollment.employment.link.key.df[
        !is.na(enrollment.employment.link.key.df$enrollment.employment.key), 
        c("employment.db.id", "enrollment.employment.key")], all.x=TRUE)

enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, 
      enrollment.employment.link.key.df[
        !is.na(enrollment.employment.link.key.df$enrollment.employment.key), 
        c("enrollment.db.id", "enrollment.employment.key")], all.x=TRUE)




##### Ok, now onto RecordLinkage matching
#####

# Dont's need these, now that we have done the above with membership.enrollment.link.key.df
#enrollment.after.exact.linkage.membership.link.df <- 
#  enrollment.after.dedup.df[is.na(enrollment.after.dedup.df$membership.db.id.crossref), ]
#membership.after.exact.linkage.enrollment.link.df <- 
#  enrollment.after.dedup.df[is.na(enrollment.after.dedup.df$membership.db.id.crossref), ]

library("RecordLinkage")



#rpairs2 <- RLBigDataLinkage(RLdata500, dataset, identity1 = identity.RLdata500,
#+   identity2 = identity2, phonetic = 1:4, exclude = "lname_c2")
#getPairs(result, min.weight=0.7, filter.link="link")

######
######

enrollment.after.dedup.df$F_name_for_rec_linkage <- enrollment.after.dedup.df$F.name.for.match.EN
enrollment.after.dedup.df$L_name_for_rec_linkage <- enrollment.after.dedup.df$L.name.for.match.EN

membership.after.dedup.df$F_name_for_rec_linkage <- membership.after.dedup.df$F.name.for.match.MP
membership.after.dedup.df$L_name_for_rec_linkage <- membership.after.dedup.df$L.name.for.match.MP

employment.after.dedup.df$F_name_for_rec_linkage <- employment.after.dedup.df$F.name.for.match.EM
employment.after.dedup.df$L_name_for_rec_linkage <- employment.after.dedup.df$L.name.for.match.EM


membership.after.dedup.df$department_for_match <- membership.after.dedup.df$Department
membership.after.dedup.df$department_for_match[membership.after.dedup.df$department_for_match==""] <- NA
enrollment.after.dedup.df$department_for_match <- enrollment.after.dedup.df$ACAD_PLAN_LONG_DESCR
enrollment.after.dedup.df$department_for_match <- gsub("( PHD)|( MS)|( MFA)|( MA)" , "", enrollment.after.dedup.df$department_for_match)

employment.after.dedup.df$department_for_match <- employment.after.dedup.df$Uw.Deptid.Descr
employment.after.dedup.df$department_for_match <- sapply(strsplit(employment.after.dedup.df$department_for_match, "/"), FUN=function(x) {
  if(length(x)==1) {
    return(x[1])
  } else {
    return(x[2])
  } } )

employment.after.dedup.df$department_for_match <- gsub("[^ ]&[^ ]", " & ", employment.after.dedup.df$department_for_match)

membership.after.dedup.df$department_for_match <- gsub("&", "and", membership.after.dedup.df$department_for_match, fixed=TRUE)
enrollment.after.dedup.df$department_for_match <- gsub("&", "and", enrollment.after.dedup.df$department_for_match, fixed=TRUE)
employment.after.dedup.df$department_for_match <- gsub("&", "and", employment.after.dedup.df$department_for_match, fixed=TRUE)

membership.after.dedup.df$department_for_match <- toupper(membership.after.dedup.df$department_for_match)
enrollment.after.dedup.df$department_for_match <- toupper(enrollment.after.dedup.df$department_for_match)
employment.after.dedup.df$department_for_match <- toupper(employment.after.dedup.df$department_for_match)





enrollment.after.dedup.df$PREFERRED_PHONE <- gsub("[^0-9]", "", enrollment.after.dedup.df$PREFERRED_PHONE)
membership.after.dedup.df$Home.Phone <-  gsub("[^0-9]", "", membership.after.dedup.df$Home.Phone)
membership.after.dedup.df$Mobile.Phone <-  gsub("[^0-9]", "", membership.after.dedup.df$Mobile.Phone)

enrollment.after.dedup.df$PREFERRED_PHONE <- gsub("^1", "", enrollment.after.dedup.df$PREFERRED_PHONE)
membership.after.dedup.df$Home.Phone <-  gsub("^1", "", membership.after.dedup.df$Home.Phone)
membership.after.dedup.df$Mobile.Phone <-  gsub("^1", "", membership.after.dedup.df$Mobile.Phone)

enrollment.after.dedup.df$PREFERRED_PHONE[enrollment.after.dedup.df$PREFERRED_PHONE==""] <- NA
membership.after.dedup.df$Home.Phone[membership.after.dedup.df$Home.Phone==""] <- NA
membership.after.dedup.df$Mobile.Phone[membership.after.dedup.df$Mobile.Phone==""] <- NA

membership.after.dedup.df$phone_for_match <- ifelse(is.na(membership.after.dedup.df$Mobile.Phone),
  membership.after.dedup.df$Home.Phone, membership.after.dedup.df$Mobile.Phone)
enrollment.after.dedup.df$phone_for_match <- enrollment.after.dedup.df$PREFERRED_PHONE

membership.after.dedup.df$phone_for_match[nchar(membership.after.dedup.df$phone_for_match)==7] <-
  paste0("608", membership.after.dedup.df$phone_for_match[nchar(membership.after.dedup.df$phone_for_match)==7])

# Address

membership.after.dedup.df$address_to_match <- gsub("([^0-9A-Z ])", "", toupper(membership.after.dedup.df$Address) )
enrollment.after.dedup.df$address_to_match <- gsub("([^0-9A-Z ])", "", toupper(enrollment.after.dedup.df$MAIL_ADDRESS_LINE1) )



#c( colnames(enrollment.after.dedup.df)[
#  !colnames(enrollment.after.dedup.df) %in% c("F.name.for.rec.linkage", "L.name.for.rec.linkage") ],
#  colnames(membership.after.dedup.df)[
#  !colnames(membership.after.dedup.df) %in% c("F.name.for.rec.linkage", "L.name.for.rec.linkage") ]
#)

# "department_for_match", 
vars.to.match.on <- c("F_name_for_rec_linkage", "L_name_for_rec_linkage", "phone_for_match", "address_to_match")

est.err.by.column <- c(.10, .01, .10, .20) * 3
# Make the above a param, maybe

#membership.after.dedup.df.decimated <- membership.after.dedup.df[, vars.to.match.on]
#membership.after.dedup.df.decimated$department_for_match[1:640] <- NA


linkage.output <- RLBigDataLinkage(
  enrollment.after.dedup.df[, vars.to.match.on], 
  membership.after.dedup.df[, vars.to.match.on], 
  identity1 = enrollment.after.dedup.df$membership.enrollment.key, 
  identity2 = membership.after.dedup.df$membership.enrollment.key,
  strcmp = TRUE, strcmpfun = "jarowinkler" )



rpairs.weights <- epiWeights(linkage.output, e = est.err.by.column)
# e is the estimated error weight of each column, seems quite useful. 
# See the documentation for info on this

grouping.threshold <- 0.60
# TODO: This should be a parameter above

rpairs.classified <- epiClassify(rpairs.weights, 
	  threshold.upper=.99999999999999999, threshold.lower=grouping.threshold)
# I think putting upper threshold at .9999 means that it does not include the exact matches

#getTable(rpairs.classified)
# getPairs( rpairs.classified, min.weight=grouping.threshold, filter.match=c("unknown"))[1:300, ]

getpairs.output <- getPairs( rpairs.classified, min.weight=grouping.threshold, #filter.link="possible",
                            filter.match=c("unknown"), single.rows=TRUE)
# Not sure what min.weight means

# getpairs.output[getpairs.output$L_name_for_rec_linkage.2=="BARANOWSKI",][1:3, ]

# Elim any duplicates by $ id.1  and $ id.2  use duplicated() on them . 
# SInce they are sorted, we will only get the best matches

getpairs.output <- getpairs.output[ !duplicated(getpairs.output$id.1) & !duplicated(getpairs.output$id.2), ]

getpairs.output <- getpairs.output[ ! getpairs.output$id.1 %in% membership.after.dedup.df$membership.enrollment.key[getpairs.output$id.1] &
                                    ! getpairs.output$id.2 %in% enrollment.after.dedup.df$membership.enrollment.key[getpairs.output$id.2], 
                                      ]
# A bit complicated piece of code to make sure that we do not overwrite the previous correct matches with estimated
# (incorrect) matches

membership.after.dedup.df$enrollment.db.id.crossref[getpairs.output$id.2] <- getpairs.output$id.1
# So insert the id values for the other database in the corresponding rows of the other database
enrollment.after.dedup.df$membership.db.id.crossref[getpairs.output$id.1] <- getpairs.output$id.2




# And there is no reason why we can't re-calculate the key ID numbers after doing this


# Ok, will use name and dept for enroll-employ link and just name for membership-employ link




########
### Now with do RecordLinkage for membership-employ:
#######


vars.to.match.on <- c("F_name_for_rec_linkage", "L_name_for_rec_linkage")

est.err.by.column <- c(.10, .01) * 3
# TODO: Make the above a param, maybe

#membership.after.dedup.df.decimated <- membership.after.dedup.df[, vars.to.match.on]
#membership.after.dedup.df.decimated$department_for_match[1:640] <- NA


linkage.output <- RLBigDataLinkage(
  employment.after.dedup.df[, vars.to.match.on], 
  membership.after.dedup.df[, vars.to.match.on], 
  identity1 = employment.after.dedup.df$membership.employment.key, 
  identity2 = membership.after.dedup.df$membership.employment.key,
  strcmp = TRUE, strcmpfun = "jarowinkler" )



rpairs.weights <- epiWeights(linkage.output, e = est.err.by.column)
# e is the estimated error weight of each column, seems quite useful. 
# See the documentation for info on this

grouping.threshold <- 0.94
# grouping.threshold <- 0.5
# TODO: This should be a parameter above

rpairs.classified <- epiClassify(rpairs.weights, 
	  threshold.upper=.99999999999999999, threshold.lower=grouping.threshold)
# I think putting upper threshold at .9999 means that it does not include the exact matches

#getTable(rpairs.classified)

# getPairs( rpairs.classified, min.weight=grouping.threshold, filter.match=c("unknown"))[1:300, ]

getpairs.output <- getPairs( rpairs.classified, min.weight=grouping.threshold, #filter.link="possible",
                            filter.match=c("unknown"), single.rows=TRUE)
# Not sure what min.weight means

# Elim any duplicates by $ id.1  and $ id.2  use duplicated() on them . 
# SInce they are sorted, we will only get the best matches

getpairs.output <- getpairs.output[ !duplicated(getpairs.output$id.1) & !duplicated(getpairs.output$id.2), ]

getpairs.output <- getpairs.output[ ! getpairs.output$id.1 %in% membership.after.dedup.df$membership.employment.key[getpairs.output$id.1] &
                                    ! getpairs.output$id.2 %in% employment.after.dedup.df$membership.employment.key[getpairs.output$id.2], 
                                      ]
# A bit complicated piece of code to make sure that we do not overwrite the previous correct matches with estimated
# (incorrect) matches

membership.after.dedup.df$employment.db.id.crossref[getpairs.output$id.2] <- getpairs.output$id.1
# So insert the id values for the other database in the corresponding rows of the other database
employment.after.dedup.df$membership.db.id.crossref[getpairs.output$id.1] <- getpairs.output$id.2



########
### Now with do RecordLinage for enrollment-employ:
#######


vars.to.match.on <- c("F_name_for_rec_linkage", "L_name_for_rec_linkage", "department_for_match")

est.err.by.column <- c(.05, .01, .20) * 3
# TODO: Make the above a param, maybe

#membership.after.dedup.df.decimated <- membership.after.dedup.df[, vars.to.match.on]
#membership.after.dedup.df.decimated$department_for_match[1:640] <- NA


linkage.output <- RLBigDataLinkage(
  employment.after.dedup.df[, vars.to.match.on], 
  enrollment.after.dedup.df[, vars.to.match.on], 
  identity1 = employment.after.dedup.df$enrollment.employment.key, 
  identity2 = enrollment.after.dedup.df$enrollment.employment.key,
  strcmp = TRUE, strcmpfun = "jarowinkler" )



rpairs.weights <- epiWeights(linkage.output, e = est.err.by.column)
# e is the estimated error weight of each column, seems quite useful. 
# See the documentation for info on this

grouping.threshold <- 0.98
# TODO: This should be a parameter above
# TODO: Probably should increase this substantially

rpairs.classified <- epiClassify(rpairs.weights, 
	  threshold.upper=.99999999999999999, threshold.lower=grouping.threshold)
# I think putting upper threshold at .9999 means that it does not include the exact matches

#getTable(rpairs.classified)

# getPairs( rpairs.classified, min.weight=grouping.threshold, filter.match=c("unknown"))[1:300, ]

getpairs.output <- getPairs( rpairs.classified, min.weight=grouping.threshold, #filter.link="possible",
                            filter.match=c("unknown"), single.rows=TRUE)
# Not sure what min.weight means

# Elim any duplicates by $ id.1  and $ id.2  use duplicated() on them . 
# SInce they are sorted, we will only get the best matches

getpairs.output <- getpairs.output[ !duplicated(getpairs.output$id.1) & !duplicated(getpairs.output$id.2), ]

getpairs.output <- getpairs.output[ ! getpairs.output$id.1 %in% enrollment.after.dedup.df$enrollment.employment.key[getpairs.output$id.1] &
                                    ! getpairs.output$id.2 %in% employment.after.dedup.df$enrollment.employment.key[getpairs.output$id.2], 
                                      ]
# A bit complicated piece of code to make sure that we do not overwrite the previous correct matches with estimated
# (incorrect) matches

enrollment.after.dedup.df$employment.db.id.crossref[getpairs.output$id.2] <- getpairs.output$id.1
# So insert the id values for the other database in the corresponding rows of the other database
employment.after.dedup.df$enrollment.db.id.crossref[getpairs.output$id.1] <- getpairs.output$id.2





##### Ok, re-doing the linkage key. This is the exact same code as above, so maybe I should 
# put it into another script file


############
############
# checking goodness of linkage after name and email match

#### membership-enrollment 

master.for.goodness.of.link.df <- merge(
  enrollment.after.dedup.df[, c("membership.db.id.crossref", "enrollment.db.id")], 
  membership.after.dedup.df[, c("enrollment.db.id.crossref", "membership.db.id")], 
  by.x="enrollment.db.id",
  by.y="enrollment.db.id.crossref", all=TRUE)
# Will do below when employment scraper is up and running
# master.for.goodness.of.link.df <- merge(master.for.goodness.of.link.df, employment.after.dedup.df[something], all=TRUE)

ftable(enrol=!is.na(master.for.goodness.of.link.df$enrollment.db.id), 
              memb=!is.na(master.for.goodness.of.link.df$membership.db.id))


membership.enrollment.link.key.df <- merge(
  enrollment.after.dedup.df[, c("membership.db.id.crossref", "enrollment.db.id")], 
  membership.after.dedup.df[, c("enrollment.db.id.crossref", "membership.db.id")], 
  by.x="enrollment.db.id",
  by.y="enrollment.db.id.crossref", all=TRUE)

membership.enrollment.link.key.df$membership.enrollment.key <- NA
membership.enrollment.link.key.df$membership.enrollment.key[
  !is.na(membership.enrollment.link.key.df$membership.db.id.crossref) ] <-
    1:sum(!is.na(membership.enrollment.link.key.df$membership.db.id.crossref))

enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, 
      membership.enrollment.link.key.df[
        !is.na(membership.enrollment.link.key.df$membership.enrollment.key), 
        c("enrollment.db.id", "membership.enrollment.key")], all.x=TRUE)

membership.after.dedup.df <- merge(membership.after.dedup.df, 
      membership.enrollment.link.key.df[
        !is.na(membership.enrollment.link.key.df$membership.enrollment.key), 
        c("membership.db.id", "membership.enrollment.key")], all.x=TRUE)


#### membership-employment link

master.for.goodness.of.link.df <- merge(
  employment.after.dedup.df[, c("membership.db.id.crossref", "employment.db.id")], 
  membership.after.dedup.df[, c("employment.db.id.crossref", "membership.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)
# Will do below when employment scraper is up and running
# master.for.goodness.of.link.df <- merge(master.for.goodness.of.link.df, employment.after.dedup.df[something], all=TRUE)

ftable(enrol=!is.na(master.for.goodness.of.link.df$employment.db.id), 
              memb=!is.na(master.for.goodness.of.link.df$membership.db.id))


membership.employment.link.key.df <- merge(
  employment.after.dedup.df[, c("membership.db.id.crossref", "employment.db.id")], 
  membership.after.dedup.df[, c("employment.db.id.crossref", "membership.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)

membership.employment.link.key.df$membership.employment.key <- NA
membership.employment.link.key.df$membership.employment.key[
  !is.na(membership.employment.link.key.df$membership.db.id.crossref) ] <-
    1:sum(!is.na(membership.employment.link.key.df$membership.db.id.crossref))

employment.after.dedup.df <- merge(employment.after.dedup.df, 
      membership.employment.link.key.df[
        !is.na(membership.employment.link.key.df$membership.employment.key), 
        c("employment.db.id", "membership.employment.key")], all.x=TRUE)

membership.after.dedup.df <- merge(membership.after.dedup.df, 
      membership.employment.link.key.df[
        !is.na(membership.employment.link.key.df$membership.employment.key), 
        c("membership.db.id", "membership.employment.key")], all.x=TRUE)



#### enrollment-employment link

master.for.goodness.of.link.df <- merge(
  employment.after.dedup.df[, c("enrollment.db.id.crossref", "employment.db.id")], 
  enrollment.after.dedup.df[, c("employment.db.id.crossref", "enrollment.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)
# Will do below when employment scraper is up and running
# master.for.goodness.of.link.df <- merge(master.for.goodness.of.link.df, employment.after.dedup.df[something], all=TRUE)

ftable(enrol=!is.na(master.for.goodness.of.link.df$employment.db.id), 
              memb=!is.na(master.for.goodness.of.link.df$enrollment.db.id))


enrollment.employment.link.key.df <- merge(
  employment.after.dedup.df[, c("enrollment.db.id.crossref", "employment.db.id")], 
  enrollment.after.dedup.df[, c("employment.db.id.crossref", "enrollment.db.id")], 
  by.x="employment.db.id",
  by.y="employment.db.id.crossref", all=TRUE)

enrollment.employment.link.key.df$enrollment.employment.key <- NA
enrollment.employment.link.key.df$enrollment.employment.key[
  !is.na(enrollment.employment.link.key.df$enrollment.db.id.crossref) ] <-
    1:sum(!is.na(enrollment.employment.link.key.df$enrollment.db.id.crossref))

employment.after.dedup.df <- merge(employment.after.dedup.df, 
      enrollment.employment.link.key.df[
        !is.na(enrollment.employment.link.key.df$enrollment.employment.key), 
        c("employment.db.id", "enrollment.employment.key")], all.x=TRUE)

enrollment.after.dedup.df <- merge(enrollment.after.dedup.df, 
      enrollment.employment.link.key.df[
        !is.na(enrollment.employment.link.key.df$enrollment.employment.key), 
        c("enrollment.db.id", "enrollment.employment.key")], all.x=TRUE)


#########



#intersect(names(enrollment.after.dedup.df), names(membership.after.dedup.df))


enrollment.after.dedup.df$membership.enrollment.key[is.na(enrollment.after.dedup.df$membership.enrollment.key)] <- "No Match w Mem"
employment.after.dedup.df$membership.employment.key[is.na(employment.after.dedup.df$membership.employment.key)] <- "No Match w Mem"

membership.after.dedup.df$membership.enrollment.key[is.na(membership.after.dedup.df$membership.enrollment.key)] <- "No Match w Enr"
employment.after.dedup.df$enrollment.employment.key[is.na(employment.after.dedup.df$enrollment.employment.key)] <- "No Match w Enr"

enrollment.after.dedup.df$enrollment.employment.key[is.na(enrollment.after.dedup.df$enrollment.employment.key)] <- "No Match w Emp"
membership.after.dedup.df$membership.employment.key[is.na(membership.after.dedup.df$membership.employment.key)] <- "No Match w Emp"

# Hmm. Ok, with this paradigm, we do not have to resolve the problem of circularity,
# since within fusion tables it would be resolved by 

dim( merge(enrollment.after.dedup.df, 
           membership.after.dedup.df,  by="membership.enrollment.key", all.x=TRUE) )

dim( merge(membership.after.dedup.df, 
           enrollment.after.dedup.df,  by="membership.enrollment.key", all.x=TRUE) )

library("plyr")
membership.after.dedup.df <- rename(membership.after.dedup.df, c(
  "Name"="Combined.Name.MP",
  "First.Name"="First.Name.MP",
  "Last.Name"="Last.Name.MP",
  "Primary.Email"="Primary.Email.MP",
  "Home.Phone"="Home.Phone.MP",
  "Mobile.Phone"="Mobile.Phone.MP",
  "Address"="Address.MP",
  "City"="City.MP",
  "State"="State.MP",
  "Zip"="Zip.MP",
  "Department"="Department.MP",
  "Job.Class"="Job.Class.MP",
  "Employer"="Employer.MP",
  "Secondary.Email"="Secondary.Email.MP",
  "Job.Title"="Job.Title.MP",
  "EmployeeId.PayrollId"="EmployeeId.PayrollId.MP",
  "pure.wisc.email"="pure.wisc.email.MP", 
  "composite.wisc.email"="composite.wisc.email.MP", 
  "names.combined"="cleaned.names.combined.MP", 
  "enrollment.db.id.crossref"="enrollment.db.id.crossref.MP", 
  "employment.db.id.crossref"="employment.db.id.crossref.MP", 
  "F_name_for_rec_linkage"="F_name_for_rec_linkage.MP", 
  "L_name_for_rec_linkage"="L_name_for_rec_linkage.MP",
  "department_for_match"="department_for_match.MP",
  "phone_for_match"="phone_for_match.MP",
  "address_to_match"="address_to_match.MP"))


enrollment.after.dedup.df <- rename(enrollment.after.dedup.df, c(
  "NAME_LAST_PREFERRED"="Last.Name.EN",
  "NAME_FIRST_PREFERRED"="First.Name.EN",
  "NAME_MIDDLE_PREFERRED"="Middle.Name.EN",
  "Name"="Combined.Name.EN",
  "EMAIL_ADDRESS"="Email.Address.EN",
  "HOME_ADDRESS_LINE1"="Home.Address.Line.1.EN",
  "HOME_ADDRESS_LINE2"="Home.Address.Line.2.EN",
  "HOME_ADDRESS_LINE3"="Home.Address.Line.3.EN",
  "HOME_ADDRESS_LINE4"="Home.Address.Line.4.EN",
  "HOME_CITY"="Home.City.EN",
  "HOME_STATE"="Home.State.EN",
  "HOME_ZIP_CODE"="Home.Zip.EN",
  "Country"="Home.Country.EN",
  "MAIL_ADDRESS_LINE1"="Mail.Address.Line.1.EN",
  "MAIL_ADDRESS_LINE2"="Mail.Address.Line.2.EN",
  "MAIL_ADDRESS_LINE3"="Mail.Address.Line.3.EN",
  "MAIL_ADDRESS_LINE4"="Mail.Address.Line.4.EN",
  "MAIL_CITY"="Mail.City.EN",
  "MAIL_STATE"="Mail.State.EN",
  "MAIL_ZIP_CODE"="Mail.Zip.EN",
  "MailCountryDesc"="Mail.Country.EN",
  "PREFERRED_PHONE"="Phone.EN",
  "CAREER_DESCR"="Career.Description.EN",
  "AcadLevelDesc"="Acad.Level.Desc.EN",
  "ACAD_GROUP_LONG_DESCR"="Acad.Group.Long.Descr.EN",
  "PLAN_CODE"="Plan.Code.EN",
  "pure.wisc.email"="pure.wisc.email.EN", 
  "composite.wisc.email"="composite.wisc.email.EN", 
  "names.combined"="cleaned.names.combined.EN", 
  "membership.db.id.crossref"="membership.db.id.crossref.EN", 
  "employment.db.id.crossref"="employment.db.id.crossref.EN", 
  "F_name_for_rec_linkage"="F_name_for_rec_linkage.EN", 
  "L_name_for_rec_linkage"="L_name_for_rec_linkage.EN",
  "department_for_match"="department_for_match.EN",
  "phone_for_match"="phone_for_match.EN",
  "address_to_match"="address_to_match.EN"
))


employment.after.dedup.df <- rename(employment.after.dedup.df, c(
  "Name"="Combined.Name.EM",
  "Uw.Jobcode.Descr"="Uw.Jobcode.Descr.EM",
  "Deptid"="Deptid.EM",
  "Uw.Deptid.Descr"="Uw.Deptid.Descr.EM",
  "Fte"="Fte.EM",
  "UW.Pay.Rate"="UW.Pay.Rate.EM",
  "first.name"="First.Name.EM",
  "last.name"="Last.Name.EM",
  "names.combined"="cleaned.names.combined.EM", 
  "pure.wisc.email"="pure.wisc.email.EM", 
  "composite.wisc.email"="composite.wisc.email.EM", 
  "membership.db.id.crossref"="membership.db.id.crossref.EM", 
  "enrollment.db.id.crossref"="enrollment.db.id.crossref.EM", 
  "F_name_for_rec_linkage"="F_name_for_rec_linkage.EM", 
  "L_name_for_rec_linkage"="L_name_for_rec_linkage.EM",
  "department_for_match"="department_for_match.EM"
))
  
# TODO: Probably want some error handling if not all of the columns exist in future datasets
# 



simpleCap <- function(x) {
    s <- strsplit(x, "[^A-Za-z]")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}
# Not ending up using this simpleCap

# simpleCap("the quick red fox jumps over the lazy brown dog")

# Standardizing names, below

membership.after.dedup.df$First.Name.OLD.MP <- membership.after.dedup.df$First.Name.MP
membership.after.dedup.df$Last.Name.OLD.MP <- membership.after.dedup.df$Last.Name.MP

#membership.after.dedup.df$First.Name.MP <- membership.after.dedup.df$F_name_for_rec_linkage.MP
#membership.after.dedup.df$Last.Name.MP <- membership.after.dedup.df$L_name_for_rec_linkage.MP

enrollment.after.dedup.df$First.Name.OLD.EN <- enrollment.after.dedup.df$First.Name.EN
enrollment.after.dedup.df$Last.Name.OLD.EN <- enrollment.after.dedup.df$Last.Name.EN

#enrollment.after.dedup.df$First.Name.EN <- enrollment.after.dedup.df$F_name_for_rec_linkage.EN
#enrollment.after.dedup.df$Last.Name.EN <- enrollment.after.dedup.df$L_name_for_rec_linkage.EN

employment.after.dedup.df$First.Name.OLD.EM <- employment.after.dedup.df$First.Name.EM
employment.after.dedup.df$Last.Name.OLD.EM <- employment.after.dedup.df$Last.Name.EM

#employment.after.dedup.df$First.Name.EM <- employment.after.dedup.df$F_name_for_rec_linkage.EM
#employment.after.dedup.df$Last.Name.EM <- employment.after.dedup.df$L_name_for_rec_linkage.EM

# Ok, so don't use the names that have been stripped of any middle names or compound names
# because we would not be able to put the proper full first names
# into the database while still allowing them to link up

# membership.employment.key
# membership.enrollment.key

#match(x, table=membership.after.dedup.df$
        
#merge(membership.after.dedup.df[ , c("membership.enrollment.key", "First.Name.MP")], 
#      enrollment.after.dedup.df[ , c("membership.enrollment.key", "First.Name.EN")])

# Ok, giving names precedence if they are different in the database.
# Among EN-MP matches, MP name has precedence
# Among EM-MP matches, MP name has precedence
# Among EM-EN matches, EN names has precedence
# This syntax below is excessively complicated, but whatevs.


enrollment.after.dedup.df$First.Name.EN[match( 
    membership.after.dedup.df$membership.enrollment.key[membership.after.dedup.df$membership.enrollment.key!="No Match w Enr"], 
    enrollment.after.dedup.df$membership.enrollment.key) ]  <- 
  membership.after.dedup.df$First.Name.MP[membership.after.dedup.df$membership.enrollment.key!="No Match w Enr"]

enrollment.after.dedup.df$Last.Name.EN[match( 
    membership.after.dedup.df$membership.enrollment.key[membership.after.dedup.df$membership.enrollment.key!="No Match w Enr"], 
    enrollment.after.dedup.df$membership.enrollment.key) ]  <- 
  membership.after.dedup.df$Last.Name.MP[membership.after.dedup.df$membership.enrollment.key!="No Match w Enr"]

  
employment.after.dedup.df$First.Name.EM[match( 
    membership.after.dedup.df$membership.employment.key[membership.after.dedup.df$membership.employment.key!="No Match w Emp"], 
    employment.after.dedup.df$membership.employment.key) ]  <- 
  membership.after.dedup.df$First.Name.MP[membership.after.dedup.df$membership.employment.key!="No Match w Emp"]

employment.after.dedup.df$Last.Name.EM[match( 
    membership.after.dedup.df$membership.employment.key[membership.after.dedup.df$membership.employment.key!="No Match w Emp"], 
    employment.after.dedup.df$membership.employment.key) ]  <- 
  membership.after.dedup.df$Last.Name.MP[membership.after.dedup.df$membership.employment.key!="No Match w Emp"]

  
employment.after.dedup.df$First.Name.EM[match( 
    enrollment.after.dedup.df$enrollment.employment.key[enrollment.after.dedup.df$enrollment.employment.key!="No Match w Emp"], 
    employment.after.dedup.df$enrollment.employment.key) ]  <- 
  enrollment.after.dedup.df$First.Name.EN[enrollment.after.dedup.df$enrollment.employment.key!="No Match w Emp"]

employment.after.dedup.df$Last.Name.EM[match( 
    enrollment.after.dedup.df$enrollment.employment.key[enrollment.after.dedup.df$enrollment.employment.key!="No Match w Emp"], 
    employment.after.dedup.df$enrollment.employment.key) ]  <- 
  enrollment.after.dedup.df$Last.Name.EN[enrollment.after.dedup.df$enrollment.employment.key!="No Match w Emp"]

# TODO: Double check that each of these above are doing the corect thing
# TODO: Maybe have the employment database be lower case in the names



# employment.after.dedup.df

membership.after.dedup.df$is.TAA.member <- TRUE
employment.after.dedup.df$is.employed <- TRUE
enrollment.after.dedup.df$is.enrolled <- TRUE

# TODO: Maybe set all NA's to "" to get good with googly
# And the non-merged NA's (due to all-TRUE above) to FALSE


write.csv(employment.after.dedup.df, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Employment final database.csv", row.names=FALSE, fileEncoding="Latin1")

write.csv(membership.after.dedup.df, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Membership final database.csv", row.names=FALSE, fileEncoding="Latin1")

write.csv(enrollment.after.dedup.df, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Enrollment final database.csv", row.names=FALSE, fileEncoding="Latin1")





full.outer.merge.df <- merge(enrollment.after.dedup.df, 
      merge(employment.after.dedup.df, membership.after.dedup.df, by="membership.employment.key", all=TRUE),
      by="enrollment.employment.key", all=TRUE ) 
# Ok, so this last "by=" argument would involve which merge key we have the
# most confidence in. Choosing enrollment.employment.key for now.
# Outcome looks like:
# names(full.outer.merge.df)[grepl("key", names(full.outer.merge.df))]
# [1] "enrollment.employment.key"   "membership.enrollment.key.x" "membership.employment.key"   "membership.enrollment.key.y"
# for this reason.

table(full.outer.merge.df$membership.enrollment.key.x==full.outer.merge.df$membership.enrollment.key.y)
# It appears that there is no inconsistence arising from which direction we go
# (clockwise or counter-clockwise) around the circle network, since these are all TRUE.







# TODO: Can implement some error test code below.
# The objective is that the table "matrix" below is completely diagona. All
# off-diagonal elements should be zero.
# Otherwise, would have to do more complicated NA checking below
table(is.na(full.outer.merge.df$Last.Name.EN), is.na(full.outer.merge.df$First.Name.EN))
table(is.na(full.outer.merge.df$Last.Name.EM), is.na(full.outer.merge.df$First.Name.EM))
table(is.na(full.outer.merge.df$Last.Name.MP), is.na(full.outer.merge.df$First.Name.MP))

full.outer.merge.df$Name.Master.Key <- NA
full.outer.merge.df$First.Name.0 <- NA
full.outer.merge.df$Last.Name.0 <- NA
# Must do the above because otherwise it will want to paste NA's together


# The "precedence" code above should allow the code below to "automatically" respect precedence:

full.outer.merge.df$First.Name.0[!is.na(full.outer.merge.df$First.Name.EM)] <- 
   gsub("(^ +)|( +$)", "", full.outer.merge.df$First.Name.EM)[!is.na(full.outer.merge.df$First.Name.EM)]
full.outer.merge.df$Last.Name.0[!is.na(full.outer.merge.df$Last.Name.EM)] <- 
   gsub("(^ +)|( +$)", "", full.outer.merge.df$Last.Name.EM)[!is.na(full.outer.merge.df$Last.Name.EM)]
# "(^ +)|( +$)" cuts out any trailing or leading spaces.

full.outer.merge.df$First.Name.0[!is.na(full.outer.merge.df$First.Name.EN)] <- 
   gsub("(^ +)|( +$)", "", full.outer.merge.df$First.Name.EN)[!is.na(full.outer.merge.df$First.Name.EN)]
full.outer.merge.df$Last.Name.0[!is.na(full.outer.merge.df$Last.Name.EN)] <- 
   gsub("(^ +)|( +$)", "", full.outer.merge.df$Last.Name.EN)[!is.na(full.outer.merge.df$Last.Name.EN)]

full.outer.merge.df$First.Name.0[!is.na(full.outer.merge.df$First.Name.MP)] <- 
   gsub("(^ +)|( +$)", "", full.outer.merge.df$First.Name.MP)[!is.na(full.outer.merge.df$First.Name.MP)]
full.outer.merge.df$Last.Name.0[!is.na(full.outer.merge.df$Last.Name.MP)] <- 
   gsub("(^ +)|( +$)", "", full.outer.merge.df$Last.Name.MP)[!is.na(full.outer.merge.df$Last.Name.MP)]




full.outer.merge.df$Name.Master.Key <- paste(full.outer.merge.df$First.Name.0, full.outer.merge.df$Last.Name.0, sep=" ")

table(is.na(full.outer.merge.df$Name.Master.Key))
table(is.na(full.outer.merge.df$First.Name.0))
table(is.na(full.outer.merge.df$Last.Name.0))
# TODO: Error check - should all be false



full.outer.merge.df$is.TAA.member[is.na(full.outer.merge.df$is.TAA.member)] <- FALSE
full.outer.merge.df$is.employed[is.na(full.outer.merge.df$is.employed)] <- FALSE
full.outer.merge.df$is.enrolled[is.na(full.outer.merge.df$is.enrolled)] <- FALSE

full.outer.merge.df <- cbind(full.outer.merge.df[ , c("Name.Master.Key", "Last.Name.0", "First.Name.0"), drop=FALSE], 
  full.outer.merge.df[, ! colnames(full.outer.merge.df) %in% c("Name.Master.Key", "Last.Name.0", "First.Name.0") ] )
# Just reordering the columns a bit

full.outer.merge.df <- full.outer.merge.df[
  order(full.outer.merge.df$Last.Name.0, full.outer.merge.df$First.Name.0), ]


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
paste3(c("a","b", "c", NA), c("A","", "", NA), c(1:3, NA)) 
# Thanks to http://stackoverflow.com/questions/13673894/suppress-nas-in-paste

# Note that we are using sep=", " for all this
full.outer.merge.df$Home.Address.Combined.EN <- with(full.outer.merge.df,
     paste0( Home.Address.Line.1.EN, Home.Address.Line.2.EN, Home.Address.Line.3.EN, 
       Home.Address.Line.4.EN, Home.City.EN, Home.State.EN, Home.Zip.EN, Home.Country.EN) )

full.outer.merge.df$Mail.Address.Combined.EN <- with(full.outer.merge.df,
     paste3( Mail.Address.Line.1.EN, Mail.Address.Line.2.EN, Mail.Address.Line.3.EN, 
       Mail.Address.Line.4.EN, Mail.City.EN, Mail.State.EN, Mail.Zip.EN, Mail.Country.EN) )

full.outer.merge.df$Address.Combined.MB <- with(full.outer.merge.df,
     paste3( Address.MP, City.MP, State.MP, Zip.MP) )

tail(full.outer.merge.df$Mail.Address.Line.4.EN)

full.outer.merge.df$Address.0 <- NA

full.outer.merge.df$Address.0[!is.na(full.outer.merge.df$Address.Combined.MB)] <- 
  full.outer.merge.df$Address.Combined.MB[!is.na(full.outer.merge.df$Address.Combined.MB)]
full.outer.merge.df$Address.0[!is.na(full.outer.merge.df$Mail.Address.Combined.EN)] <- 
  full.outer.merge.df$Mail.Address.Combined.EN[!is.na(full.outer.merge.df$Mail.Address.Combined.EN)]
# So the enrollment address data has precedence because it is probably more up to date than
# the membership data

# Not sure why we are still getting the commas between empty strings, but I don't care enough
# to fix it now

# TODO: convert all missing values to ""


write.csv(full.outer.merge.df, file="/Users/travismcarthur/Desktop/TAA work/Grad student database/Outer merge Enr Emp Mem.csv", row.names=FALSE, fileEncoding="Latin1")

# , na=""



table(Emp = membership.after.dedup.df$membership.employment.key!="No Match w Emp", 
      Enr=membership.after.dedup.df$membership.enrollment.key!="No Match w Enr")


table(Enr = employment.after.dedup.df$enrollment.employment.key!="No Match w Enr", 
      Mem=employment.after.dedup.df$membership.employment.key!="No Match w Mem")



table(Emp = enrollment.after.dedup.df$enrollment.employment.key!="No Match w Emp", 
      Mem=enrollment.after.dedup.df$membership.enrollment.key!="No Match w Mem")


# membership.employment.key	membership.enrollment.key
# No Match w Emp	No Match w Enr


# getpairs.output[getpairs.output$L_name_for_rec_linkage.2=="BARANOWSKI",][1:3, ]

# TODO: and at the end sort it by last name, first name?





















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








