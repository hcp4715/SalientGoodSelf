##### Purpose ####
# This script is for pre-processing the personal distance data for experiment 4b
# 

# initial
source('Initial.r')

# load data
df4b <- read.csv("exp4.2_data_personal_distance.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
# get the distnace data
df4b$SelfGood   <- (df4b$PerDis9 + df4b$PerDis12 + df4b$PerDis20 + df4b$ PerDis26)/4
df4b$SelfNormal <- (df4b$PerDis3 + df4b$PerDis15 + df4b$PerDis23 + df4b$ PerDis28)/4
df4b$SelfBad    <- (df4b$PerDis5 + df4b$PerDis10 + df4b$PerDis24 + df4b$ PerDis25)/4
df4b$SelfOther  <- (df4b$PerDis6 + df4b$PerDis14 + df4b$PerDis21 + df4b$ PerDis29)/4

df4b$GoodBad    <- (df4b$PerDis2 + df4b$PerDis8  + df4b$PerDis17 + df4b$ PerDis18)/4
df4b$GoodNormal <- (df4b$PerDis1 + df4b$PerDis11 + df4b$PerDis16 + df4b$ PerDis27)/4
df4b$BadNormal  <- (df4b$PerDis7 + df4b$PerDis13 + df4b$PerDis19 + df4b$ PerDis22)/4

# set the self-other distance as NA for the earlier data
df4b$SelfOther[df4b$SelfOther == 0] <- NA

# remove participant that don't understand the instruction
df4b_v <- df4b[df4b$PerDis4 < 50,]
df4b_v <- df4b_v[df4b_v$GoodBad != 0,]

# remove participants' that have identical ratings for many items (visual scoll)
# sub_exld <- c(4146,4150) # to be excluded participants
# df4b_v <- df4b_v[!df4b_v$subID %in% sub_exld, ]

# extract the relevant data
df4b_perdis <- df4b_v[,c("expID","subID","SessionID", "SelfGood","SelfNormal",
                         "SelfBad","GoodBad","GoodNormal","BadNormal")]
df4b_perdis$expID <- 'exp4b'
df4b_perdis$SessionID <- 1

df4b_perdis$totalDis     <- rowSums(df4b_perdis[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
df4b_perdis$SelfGood_r   <- df4b_perdis$SelfGood/df4b_perdis$totalDis
df4b_perdis$SelfNormal_r <- df4b_perdis$SelfNormal/df4b_perdis$totalDis
df4b_perdis$SelfBad_r    <- df4b_perdis$SelfBad/df4b_perdis$totalDis
df4b_perdis$GoodBad_r    <- df4b_perdis$GoodBad/df4b_perdis$totalDis
df4b_perdis$GoodNormal_r <- df4b_perdis$GoodNormal/df4b_perdis$totalDis
df4b_perdis$BadNormal_r  <- df4b_perdis$BadNormal/df4b_perdis$totalDis

# select normalized data
df4b_perdis_r <- df4b_perdis[,c("expID","subID","SessionID", "SelfGood_r","SelfNormal_r",
                           "SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]

# wide to long format to get the summary data
df4b_perdis_lr <- melt(df4b_perdis_r, id.vars = c("expID","subID","SessionID"),variable.name = "distLabel",
                       value.name = "distValue")

df4b_perdis_lr <- df4b_perdis_lr[order(df4b_perdis_lr$subID),]

# calculated the summary
df4b_perdis.sum <- summarySEwithin(df4b_perdis_lr,measurevar = 'distValue', withinvar = c('distLabel'), 
                                 idvar = 'subID',na.rm = TRUE, .drop=FALSE)

# save fiel
write.csv(df4b_perdis,'exp4b_personaldistance_all.csv',row.names = F)
write.csv(df4b_perdis_r,'exp4b_personaldistance_r.csv',row.names = F)
write.csv(df4b_perdis_lr,'exp4b_personaldistance_r_long.csv',row.names = F)


# plot the data for visual scoll
library(yarrr)
# reorder the
df4b_perdis.sum <-df4b_perdis.sum[order(df4b_perdis.sum$distValue),]
neworder <- df4b_perdis.sum$distLabel
df4b_perdis_lr_or <- df4b_perdis_lr
df4b_perdis_lr_or$distLabel <- factor(df4b_perdis_lr_or$distLabel,levels = neworder)
pirateplot(formula =  distValue ~ distLabel,
           data = df4b_perdis_lr_or,
           pal = "southpark",
           xlab = "Persons",
           ylab = "Normalized distance",
           main = "exp4b_personal_distance",
           theme = 1)