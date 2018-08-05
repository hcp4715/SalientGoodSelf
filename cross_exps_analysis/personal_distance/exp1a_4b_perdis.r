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

## plot distance ####
df_perdis_long <- read.csv('exp1a_4b_df_perdis_long.csv',header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df_perdis_long$condition[df_perdis_long$condition == 'SelfGood_r'] <- 'Self-Good'
df_perdis_long$condition[df_perdis_long$condition == 'SelfNormal_r'] <- 'Self-Ord'
df_perdis_long$condition[df_perdis_long$condition == 'SelfBad_r'] <- 'Self-Bad'
df_perdis_long$condition[df_perdis_long$condition == 'GoodBad_r'] <- 'Good-Bad'
df_perdis_long$condition[df_perdis_long$condition == 'GoodNormal_r'] <- 'Good-Ord'
df_perdis_long$condition[df_perdis_long$condition == 'BadNormal_r'] <- 'Bad-Ord'

df_perdis_long$condition <- factor(df_perdis_long$condition, 
                                   levels = c('Self-Ord', 'Self-Good', 'Good-Ord', 'Bad-Ord',  'Good-Bad','Self-Bad'))
p1 <- ggplot(data = df_perdis_long, aes(y = distance, x = condition,fill = condition)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  geom_point(aes(y = distance,color = condition), position = position_jitter(width = .1), size = 1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) + guides(color = FALSE)+
  #theme_bw() +
  raincloud_theme+scale_y_continuous(breaks = seq(0,0.6,0.1),limits = c(0,0.6))+labs(x = "Conditions",y = "Normalized personal distance")

tiff('exp_all_perdis.tiff', width = 9, height = 6, units = 'in', res = 300)
p1
dev.off()
