##### Purpose ####
# This script is for pre-processing the scale data for experiment 1b
# espeically for the psychological-distance data

# initial
source('Initial.r')

# load data
# set wd to sub-folder 'scale_preproc'
# setwd('./scale_preproc')

# load data
df3 <- read.csv("exp3_data_personal_distance.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))

# get the distnace data
df3$SelfGood   <- (df3$PerDis9 + df3$PerDis12 + df3$PerDis20 + df3$ PerDis26)/4
df3$SelfNormal <- (df3$PerDis3 + df3$PerDis15 + df3$PerDis23 + df3$ PerDis28)/4
df3$SelfBad    <- (df3$PerDis5 + df3$PerDis10 + df3$PerDis24 + df3$ PerDis25)/4
df3$SelfOther  <- (df3$PerDis6 + df3$PerDis14 + df3$PerDis21 + df3$ PerDis29)/4

df3$GoodBad    <- (df3$PerDis2 + df3$PerDis8  + df3$PerDis17 + df3$ PerDis18)/4
df3$GoodNormal <- (df3$PerDis1 + df3$PerDis11 + df3$PerDis16 + df3$ PerDis27)/4
df3$BadNormal  <- (df3$PerDis7 + df3$PerDis13 + df3$PerDis19 + df3$ PerDis22)/4

# set the self-other distance as NA for the earlier data
df3$SelfOther[df3$SelfOther == 0] <- NA

# remove participant that don't understand the instruction
df3_v <- df3[df3$PerDis4 < 50,]
df3_v <- df3_v[df3_v$GoodBad !=0,]

# extract the relevant data
df3_perdis <- df3_v[,c("expID","subID","SessionID", "SelfGood","SelfNormal",
                         "SelfBad","GoodBad","GoodNormal","BadNormal")]


colnames(df3_perdis)[colnames(df3_perdis) == 'SessionID'] <- 'session'
df3_perdis$session[is.na(df3_perdis$session)] <- 1

df3_perdis$totalDis <- rowSums(df3_perdis[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
df3_perdis$SelfGood_r <- df3_perdis$SelfGood/df3_perdis$totalDis
df3_perdis$SelfNormal_r <- df3_perdis$SelfNormal/df3_perdis$totalDis
df3_perdis$SelfBad_r <- df3_perdis$SelfBad/df3_perdis$totalDis
df3_perdis$GoodBad_r <- df3_perdis$GoodBad/df3_perdis$totalDis
df3_perdis$GoodNormal_r <- df3_perdis$GoodNormal/df3_perdis$totalDis
df3_perdis$BadNormal_r <- df3_perdis$BadNormal/df3_perdis$totalDis

# wide to long format to get the summary data
df3_perdis_r <- df3_perdis[,c("expID","subID","session", "SelfGood_r","SelfNormal_r",
                    "SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]
df3_perdis_l <- melt(df3_perdis_r, id.vars = c("expID","subID","session"),variable.name = "distLabel",
                      value.name = "distValue")
df3_perdis_l <- df3_perdis_l[order(df3_perdis_l$subID),]

# calculated the summary
df3_perdis.sum <- summarySEwithin(df3_perdis_l,measurevar = 'distValue', withinvar = c('distLabel'), 
                                 idvar = 'subID',na.rm = TRUE, .drop=FALSE)



# save
write.csv(df3_perdis,'exp3_perDis_wide.csv',row.names = F)
write.csv(df3_perdis_r,'exp3_perDis_wide_r.csv',row.names = F)
write.csv(df3_perdis_l,'exp3_perDis_long.csv',row.names = F)

## plot ####
install.packages("devtools")
library("devtools")
install_github("ndphillips/yarrr")
library(yarrr)
df3_perdis_l$distLabel <- as.character(df3_perdis_l$distLabel)
pirateplot(formula =  distValue~ distLabel,
           data = df3_perdis_l,
           pal = "southpark",
           xlab = "Persons",
           ylab = "Normalized distance",
           main = "Good to try",
           point.pch = 16,
           point.o = .2,
           hdi.o = .6,
           bar.o = .1,
           line.o = .5)
