








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

APPARCQ Apparel and services this quarter 
median(c.exp.comp.df$apparcq[c.exp.comp.df$apparcq>0], na.rm=TRUE)
length(c.exp.comp.df$apparcq[c.exp.comp.df$apparcq>0])


