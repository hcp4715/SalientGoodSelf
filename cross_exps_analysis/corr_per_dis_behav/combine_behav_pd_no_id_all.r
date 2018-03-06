## this script is used for combining data from all exp.s

source('initial.r')

## load the personal distance data
df1a <- read.csv('exp1a_behav_scale_combined_wide.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b <- read.csv('exp1b_behav_scale_combined_wide.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df2 <- read.csv('exp2_behav_scale_combined_wide.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df3 <- read.csv('exp3_behav_scale_combined_wide_no_id.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df3 <- df3[,colnames(df1a)]
df4a <- read.csv('exp4a_behav_scale_combined_wide_no_id.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a <- df4a[,2:33]
colnames(df4a)[colnames(df4a) == 'SessionID'] <- 'session'
df4a <- df4a[,colnames(df1a)]

df4b <- read.csv('exp4b_behav_scale_combined_wide_no_id.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b <- df4b[,2:33]
colnames(df4b)[colnames(df4b) == 'SessionID'] <- 'session'
df4b <- df4b[,colnames(df1a)]

# merge the personal distance data to the behav data (all.x = T)
df_join <- rbind(df1a,df1b,df2,df3,df4a,df4b)
df_join_v <- df_join[!is.na(df_join$SelfGood_r),]

## extract the data for corrlation analysis
df_cor_rt <- df_join_v[,21:29]
df_cor_d <- df_join_v[,c(21:26,30:32)]

# calculate correlation
library(corrplot)
library(Hmisc)
library(ggplot2)
library(ggpubr)
cor_rt <- rcorr(as.matrix(df_cor_rt))
cor_d <- rcorr(as.matrix(df_cor_d))

# write file:
write.csv(df_join_v, 'behav_scale_combined_wide_no_id_all.csv', row.names = F)