
source('initial.r')

## load the RT data and d prime data (without considering the effect of identity)
df3_behav <- read.csv('exp3_behav_moral_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))

## load the personal distance data
df3_perDis <- read.csv('exp3_perDis_wide_r.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df3_join <- merge(df3_behav,df3_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df3_join <- df3_join[,c(colnames(df3_join)[c(1:3,19:20,4:18,21:26)])]

# rename some of the columns
df3_join$expID   <- "exp3"  # the experiment was "exp3"
df3_join$session[is.na(df3_join$session)] <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df3_join$RT_Good_Bad  <- df3_join$RT_Match_Moral   - df3_join$RT_Match_Immoral
df3_join$RT_Good_Neut  <- df3_join$RT_Match_Moral   - df3_join$RT_Match_Neutral 
df3_join$RT_Neut_Bad   <- df3_join$RT_Match_Neutral - df3_join$RT_Match_Immoral

df3_join$d_Good_Bad   <- df3_join$d_Moral   - df3_join$d_Immoral
df3_join$d_Good_Neut  <- df3_join$d_Moral   - df3_join$d_Neutral 
df3_join$d_Neut_Bad   <- df3_join$d_Neutral - df3_join$d_Immoral

# write file:
write.csv(df3_join, 'exp3_behav_scale_combined_wide_no_id.csv', row.names = F)
