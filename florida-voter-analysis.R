



read.table("/Users/travismcarthur/Desktop/Misc/voter-history/VoterHistory/SAR_H_20120602.txt", header=F, nrows = 5)


voter.hist.2012.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/VoterHistory/SAR_H_20120602.txt")
# Seems these are all tab-delimited

voter.hist.2012.df[which(voter.hist.2012.df$V2 == 100282648), ]

voter.hist.2020.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/20200211_VoterHistory/ALA_H_20200211.txt")
# https://flvoters.com/download/20200131/
# http://flvoters.com/downloads.html
# 100282648 is me
voter.hist.2020.df[which(voter.hist.2020.df$V2 == 100282648), ]


voter.reg.2020.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/20200211_VoterDetail/ALA_20200211.txt", 
  sep = "\t", comment.char = "", na.strings = "", quote = "", stringsAsFactors = FALSE)

voter.reg.2020.df[which(voter.reg.2020.df$V2 == 100282648), ]

voter.reg.2012.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/VoterExtract/SAR_20120602.txt",
  sep = "\t", comment.char = "", na.strings = "", quote = "", stringsAsFactors = FALSE)


voter.reg.2012.df[which(voter.reg.2012.df$V2 == 100282648), ]


voter.reg.2020.SAR.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/20200211_VoterDetail/SAR_20200211.txt", 
  sep = "\t", comment.char = "", na.strings = "", quote = "", stringsAsFactors = FALSE)

voter.reg.2020.SAR.df[toupper(voter.reg.2020.SAR.df$V3) == "MCARTHUR", ]
voter.reg.2020.SAR.df[toupper(voter.reg.2020.SAR.df$V3) == "MC ARTHUR", ]

voter.hist.2020.SAR.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/20200211_VoterHistory/SAR_H_20200211.txt")
voter.hist.2020.SAR.df$V3 <- as.Date(voter.hist.2020.SAR.df$V3, format = "%m/%d/%Y")
voter.hist.2020.SAR.df <- voter.hist.2020.SAR.df[order(voter.hist.2020.SAR.df$V3), ]

voter.hist.2020.SAR.df[which(voter.hist.2020.SAR.df$V2 == 118231803), ] # Julie
voter.hist.2020.SAR.df[which(voter.hist.2020.SAR.df$V2 == 100010955), ] # Mom
voter.hist.2020.SAR.df[which(voter.hist.2020.SAR.df$V2 == 100108878), ] # Dad


voter.reg.2020.MAN.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/20200211_VoterDetail/MAN_20200211.txt", 
  sep = "\t", comment.char = "", na.strings = "", quote = "", stringsAsFactors = FALSE)


voter.reg.2020.MAN.df[toupper(voter.reg.2020.MAN.df$V3) == "KING" & 
    toupper(voter.reg.2020.MAN.df$V5) == "SPENCER", ]

voter.reg.2020.MAN.df[toupper(voter.reg.2020.MAN.df$V8) == toupper("4935   80th Avenue Cir E"), ]


voter.hist.2020.MAN.df <- read.table("/Users/travismcarthur/Desktop/Misc/voter-history/20200211_VoterHistory/MAN_H_20200211.txt")
voter.hist.2020.MAN.df$V3 <- as.Date(voter.hist.2020.MAN.df$V3, format = "%m/%d/%Y")
voter.hist.2020.MAN.df <- voter.hist.2020.MAN.df[order(voter.hist.2020.MAN.df$V3), ]

voter.hist.2020.MAN.df[which(voter.hist.2020.MAN.df$V2 == 105440446), ] # Spencer
voter.hist.2020.MAN.df[which(voter.hist.2020.MAN.df$V2 == 105308671), ] # Auntie Lynne





