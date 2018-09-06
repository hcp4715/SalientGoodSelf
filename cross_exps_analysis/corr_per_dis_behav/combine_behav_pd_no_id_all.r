## this script is used for combining data from all exp.s

source('initial.r')

## load the personal distance data
df1a <- read.csv('exp1a_behav_scale_combined_wide.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b <- read.csv('exp1b_behav_scale_combined_wide.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df2 <- read.csv('exp2_behav_scale_combined_wide.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df3a <- read.csv('exp3a_behav_scale_combined_wide_no_id.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df3a <- df3a[,colnames(df1a)]
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
df_join <- rbind(df1a,df1b,df2,df3a,df4a,df4b)
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
# corrplot(as.matrix(df_cor_rt), method = "number") # Display the correlation coefficient
cor_d <- rcorr(as.matrix(df_cor_d))


# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_rt_f <- flattenCorrMatrix(cor_rt$r,cor_rt$P)
cor_rt_f_v <- subset(cor_rt_f, p < 0.05)

cor_d_f <- flattenCorrMatrix(cor_d$r,cor_d$P)
cor_d_f_v <- subset(cor_d_f, p < 0.05)

# write file:
write.csv(df_join_v, 'behav_scale_combined_wide_no_id_all.csv', row.names = F)
