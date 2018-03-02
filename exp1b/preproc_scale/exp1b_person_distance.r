##### Purpose ####
# This script is for pre-processing the scale data for experiment 1b
# espeically for the psychological-distance data

# initial
source('Initial.r')

# load data
# set wd to sub-folder 'scale_preproc'
# setwd('./scale_preproc')

# load data
df1b_1 <- read.csv("exp1b_data_personal_distance_2015.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b_2 <- read.csv("exp1b_data_personal_distance_2017.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))

df1b <- rbind(df1b_1,df1b_2)

# get the distnace data
df1b$SelfGood   <- (df1b$PerDis9 + df1b$PerDis12 + df1b$PerDis20 + df1b$ PerDis26)/4
df1b$SelfNormal <- (df1b$PerDis3 + df1b$PerDis15 + df1b$PerDis23 + df1b$ PerDis28)/4
df1b$SelfBad    <- (df1b$PerDis5 + df1b$PerDis10 + df1b$PerDis24 + df1b$ PerDis25)/4
df1b$SelfOther  <- (df1b$PerDis6 + df1b$PerDis14 + df1b$PerDis21 + df1b$ PerDis29)/4

df1b$GoodBad    <- (df1b$PerDis2 + df1b$PerDis8  + df1b$PerDis17 + df1b$ PerDis18)/4
df1b$GoodNormal <- (df1b$PerDis1 + df1b$PerDis11 + df1b$PerDis16 + df1b$ PerDis27)/4
df1b$BadNormal  <- (df1b$PerDis7 + df1b$PerDis13 + df1b$PerDis19 + df1b$ PerDis22)/4

# set the self-other distance as NA for the earlier data
df1b$SelfOther[df1b$SelfOther == 0] <- NA

# remove participant that don't understand the instruction
df1b_v <- df1b[df1b$PerDis4 < 50,]
df1b_v <- df1b_v[!(df1b_v$GoodBad ==0),]

# extract the relevant data
df1b_perdis <- df1b_v[,c("expID","subID","SessionID", "SelfGood","SelfNormal",
                         "SelfBad","GoodBad","GoodNormal","BadNormal")]

# calculated the summary
df1b_perdis$totalDis <- rowSums(df1b_perdis[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
df1b_perdis$SelfGood_r <- df1b_perdis$SelfGood/df1b_perdis$totalDis
df1b_perdis$SelfNormal_r <- df1b_perdis$SelfNormal/df1b_perdis$totalDis
df1b_perdis$SelfBad_r <- df1b_perdis$SelfBad/df1b_perdis$totalDis
df1b_perdis$GoodBad_r <- df1b_perdis$GoodBad/df1b_perdis$totalDis
df1b_perdis$GoodNormal_r <- df1b_perdis$GoodNormal/df1b_perdis$totalDis
df1b_perdis$BadNormal_r <- df1b_perdis$BadNormal/df1b_perdis$totalDis

df1b_perdis$expID <- 'exp1b'
df1b_perdis$SessionID[is.na(df1b_perdis$SessionID)] <- 1

df1b_perdis_normalized <- df1b_perdis[,c("expID","subID","SessionID","SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]

# save
write.csv(df1b_perdis,'exp1b_personaldistance.csv',row.names = F)
write.csv(df1b_perdis_normalized,'exp1b_personaldistance_r.csv',row.names = F)
