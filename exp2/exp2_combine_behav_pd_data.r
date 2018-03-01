
source('initial.r')

## load the RT data and d prime data

df2_behav <- read.csv('exp2_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df2_perDis <- read.csv('exp2_personaldistance.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df2_join <- merge(df2_behav,df2_perDis,by.x = "Subject", by.y = "SubjectID",all.x = T)
df2_join <- df2_join[,c("expID","Subject","session","Sex","Age",colnames(df2_join)[c(4:18,21:26)])]

# rename some of the columns
df2_join$expID   <- "exp2"  # the experiment was "exp1a"
df2_join$session <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df2_join$RT_Good_Bad <-  df2_join$RT_Match_Moral   - df2_join$RT_Match_Immoral
df2_join$RT_Good_Neut <- df2_join$RT_Match_Moral   - df2_join$RT_Match_Neutral 
df2_join$RT_Neut_Bad  <- df2_join$RT_Match_Neutral - df2_join$RT_Match_Immoral

df2_join$d_Good_Bad <-  df2_join$d_Moral   - df2_join$d_Immoral
df2_join$d_Good_Neut <- df2_join$d_Moral   - df2_join$d_Neutral 
df2_join$d_Neut_Bad  <- df2_join$d_Neutral - df2_join$d_Immoral

# write file:
write.csv(df2_join, 'exp2_behav_scale_combined_wide.csv', row.names = F)
