## About ####
# this script is used for combined the data from exp1a and exp2 to assessing the stability of the effect
# 
# Initializing ####
source('Initial.r')

#
# load data ####
df1a <- read.csv("exp1a_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b <- read.csv("exp1b_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df2  <- read.csv("exp2_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df3  <- read.csv("exp3_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a <- read.csv("exp4a_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b <- read.csv("exp4b_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

colnames(df1a) <- colnames(df1b)

colnames(df3) == colnames(df4b)
colnames(df2)[colnames(df2) == "SubjectID"] <- "subID"
colnames(df2)[colnames(df2) == "session"] <- "SessionID"
df2 <- df2[,colnames(df1b)]
colnames(df3)[colnames(df3) == "session"] <- "SessionID"

df_perdis <- rbind(df1a,df1b,df2,df3,df4a,df4b)
df_perdis_long <- melt(df_perdis,id.vars = c('expID','subID','SessionID'),
                       variable.name = "condition",
                       value.name = "distance")
df_perdis.sum <- summarySEwithin(df_perdis_long,measurevar = 'distance', withinvar = c('condition'), 
                                  idvar = 'subID',na.rm = TRUE, .drop=FALSE)
df_perdis.sum <- df_perdis.sum[order(df_perdis.sum$distance),]
df_perdis_long$condition <- factor(df_perdis_long$condition, 
                                   levels = c('SelfNormal_r', 'SelfGood_r', 'GoodNormal_r', 'BadNormal_r',  'GoodBad_r','SelfBad_r'))
df_perdis_long <- df_perdis_long[order(df_perdis_long$condition),]
# write csv
write.csv(df_perdis, "exp1a_4b_df_perdis.csv",row.names = F)
write.csv(df_perdis_long, "exp1a_4b_df_perdis_long.csv",row.names = F)
