
source('initial.r')

## load the RT data and d prime data

df1a_behav <- read.csv('exp1a_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df1a_perDis <- read.csv('exp1a_personaldistance.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df1a_join <- merge(df1a_behav,df1a_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df1a_join <- df1a_join[,c("expID","Subject","session","Sex","Age",colnames(df1a_join)[c(4:18,21:26)])]

# rename some of the columns
df1a_join$expID   <- "exp1a"  # the experiment was "exp1a"
df1a_join$session <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df1a_join$RT_Good_Bad <-  df1a_join$RT_Match_Moral   - df1a_join$RT_Match_Immoral
df1a_join$RT_Good_Neut <- df1a_join$RT_Match_Moral   - df1a_join$RT_Match_Neutral 
df1a_join$RT_Neut_Bad  <- df1a_join$RT_Match_Neutral - df1a_join$RT_Match_Immoral

df1a_join$d_Good_Bad <-  df1a_join$d_Moral   - df1a_join$d_Immoral
df1a_join$d_Good_Neut <- df1a_join$d_Moral   - df1a_join$d_Neutral 
df1a_join$d_Neut_Bad  <- df1a_join$d_Neutral - df1a_join$d_Immoral

# write file:
write.csv(df1a_join, 'ex1a_behav_scale_combined_wide.csv', row.names = F)
