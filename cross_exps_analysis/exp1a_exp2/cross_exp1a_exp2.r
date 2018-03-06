## About ####
# this script is used for combined the data from exp1a and exp2 to assessing the stability of the effect
# 
# Initializing ####
source('Initial.r')

#
# load data ####
df1a <- read.csv("exp1a_behav_scale_combined_wide.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df2  <- read.csv("exp2_behav_scale_combined_wide.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
colnames(df1a)[6:31] <- paste( colnames(df1a[,6:31]),"1a", sep = "_")
colnames(df2)[6:31]  <- paste( colnames(df2[,6:31]),"2", sep = "_")

df_c <- merge(df1a,df2,by = 'Subject')

# write csv
write.csv(df_c, "exp1a_exp2_cross_data.csv",row.names = F)
