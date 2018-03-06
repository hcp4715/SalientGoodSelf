source('initial.r')

## load the RT data and d prime data (without considering the effect of identity)
df4b_behav <- read.csv('exp4b_behav_moral_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))

## load the personal distance data
df4b_perDis <- read.csv('exp4b_personaldistance_r.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df4b_join <- merge(df4b_behav,df4b_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df4b_join <- df4b_join[,c(colnames(df4b_join)[c(2,1,3,4,20:21,5:19,22:27)])]

# rename some of the columns
df4b_join$expID   <- "exp4b"  # the experiment was "exp3"
df4b_join$SessionID[is.na(df4b_join$SessionID)] <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df4b_join$RT_Good_Bad  <- df4b_join$RT_Match_Moral   - df4b_join$RT_Match_Immoral
df4b_join$RT_Good_Neut  <- df4b_join$RT_Match_Moral   - df4b_join$RT_Match_Neutral 
df4b_join$RT_Neut_Bad   <- df4b_join$RT_Match_Neutral - df4b_join$RT_Match_Immoral

df4b_join$d_Good_Bad   <- df4b_join$d_Moral   - df4b_join$d_Immoral
df4b_join$d_Good_Neut  <- df4b_join$d_Moral   - df4b_join$d_Neutral 
df4b_join$d_Neut_Bad   <- df4b_join$d_Neutral - df4b_join$d_Immoral

# write file:
write.csv(df4b_join, 'exp4b_behav_scale_combined_wide_no_id.csv', row.names = F)
