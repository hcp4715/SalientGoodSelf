##### Purpose ####
# This script is for pre-processing the scale data for experiment 4a
# espeically for the psychological-distance data

# initial
source('Initial.r')

# load data
df4a_1 <- read.csv("exp4a_data_personal_distance_2015.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a_2 <- read.csv("exp4a_data_personal_distance_2017.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))

# combine the data from both sources
df4a <- rbind(df4a_1,df4a_2)

# get the distnace data
df4a$SelfGood   <- (df4a$PerDis9 + df4a$PerDis12 + df4a$PerDis20 + df4a$ PerDis26)/4
df4a$SelfNormal <- (df4a$PerDis3 + df4a$PerDis15 + df4a$PerDis23 + df4a$ PerDis28)/4
df4a$SelfBad    <- (df4a$PerDis5 + df4a$PerDis10 + df4a$PerDis24 + df4a$ PerDis25)/4
df4a$SelfOther  <- (df4a$PerDis6 + df4a$PerDis14 + df4a$PerDis21 + df4a$ PerDis29)/4

df4a$GoodBad    <- (df4a$PerDis2 + df4a$PerDis8  + df4a$PerDis17 + df4a$ PerDis18)/4
df4a$GoodNormal <- (df4a$PerDis1 + df4a$PerDis11 + df4a$PerDis16 + df4a$ PerDis27)/4
df4a$BadNormal  <- (df4a$PerDis7 + df4a$PerDis13 + df4a$PerDis19 + df4a$ PerDis22)/4

# set the self-other distance as NA for the earlier data
df4a$SelfOther[df4a$SelfOther == 0] <- NA

# remove participant that don't understand the instruction
df4a_v <- df4a[df4a$PerDis4 < 50,]
df4a_v <- df4a_v[df4a_v$GoodBad != 0,]

# remove participants' that have identical ratings for many items (visual scoll)
sub_exld <- c(4146,4150) # to be excluded participants
df4a_v <- df4a_v[!df4a_v$subID %in% sub_exld, ]

# extract the relevant data
df4a_perdis <- df4a_v[,c("expID","subID","SessionID", "SelfGood","SelfNormal",
                         "SelfBad","GoodBad","GoodNormal","BadNormal")]
df4a_perdis$expID <- 'exp4a'
df4a_perdis$SessionID <- 1

df4a_perdis$totalDis     <- rowSums(df4a_perdis[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
df4a_perdis$SelfGood_r   <- df4a_perdis$SelfGood/df4a_perdis$totalDis
df4a_perdis$SelfNormal_r <- df4a_perdis$SelfNormal/df4a_perdis$totalDis
df4a_perdis$SelfBad_r    <- df4a_perdis$SelfBad/df4a_perdis$totalDis
df4a_perdis$GoodBad_r    <- df4a_perdis$GoodBad/df4a_perdis$totalDis
df4a_perdis$GoodNormal_r <- df4a_perdis$GoodNormal/df4a_perdis$totalDis
df4a_perdis$BadNormal_r  <- df4a_perdis$BadNormal/df4a_perdis$totalDis

df4a_perdis_r <- df4a_perdis[,c("expID","subID","SessionID", "SelfGood_r","SelfNormal_r",
                           "SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]

# wide to long format to get the summary data
df4a_perdis_lr <- melt(df4a_perdis_r, id.vars = c("expID","subID","SessionID"),variable.name = "distLabel",
                       value.name = "distValue")

df4a_perdis_lr <- df4a_perdis_lr[order(df4a_perdis_lr$subID),]

# calculated the summary
df4a_perdis.sum <- summarySEwithin(df4a_perdis_l,measurevar = 'distValue', withinvar = c('distLabel'), 
                                 idvar = 'subID',na.rm = TRUE, .drop=FALSE)

# save fiel
write.csv(df4a_perdis,'exp4a_personaldistance.csv',row.names = F)
write.csv(df4a_perdis_r,'exp4a_personaldistance_r.csv',row.names = F)
write.csv(df4a_perdis_lr,'exp4a_personaldistance_r_long.csv',row.names = F)

library(yarrr)
df4a_perdis_l$distLabel <- as.character(df4a_perdis_l$distLabel)
pirateplot(formula =  distValue ~ distLabel,
           data = df4a_perdis_lr,
           pal = "southpark",
           xlab = "Persons",
           ylab = "Normalized distance",
           main = "Good to try",
           point.pch = 16,
           point.o = .2,
           hdi.o = .6,
           bar.o = .1,
           line.o = .5)


