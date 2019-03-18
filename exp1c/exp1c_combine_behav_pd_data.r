
source('initial.r')

## load the RT data and d prime data

df1b_behav <- read.csv('exp1a_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b_perDis <- read.csv('exp1b_personaldistance_r.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))
colnames(df1b_perDis)[colnames(df1b_perDis) == 'SessionID'] <- 'session'

# merge the personal distance data to the behav data (all.x = T)
df1b_join <- merge(df1b_behav,df1b_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df1b_join <- df1b_join[,c("expID","Subject","session","Sex","Age",colnames(df1b_join)[c(4:18,21:26)])]


# rename some of the columns
df1b_join$expID   <- "exp1b"  # the experiment was "exp1b"
df1b_join$session <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df1b_join$RT_Good_Bad <-  df1b_join$RT_Match_Moral   - df1b_join$RT_Match_Immoral
df1b_join$RT_Good_Neut <- df1b_join$RT_Match_Moral   - df1b_join$RT_Match_Neutral 
df1b_join$RT_Neut_Bad  <- df1b_join$RT_Match_Neutral - df1b_join$RT_Match_Immoral

df1b_join$d_Good_Bad <-  df1b_join$d_Moral   - df1b_join$d_Immoral
df1b_join$d_Good_Neut <- df1b_join$d_Moral   - df1b_join$d_Neutral 
df1b_join$d_Neut_Bad  <- df1b_join$d_Neutral - df1b_join$d_Immoral

# write file:
write.csv(df1b_join, 'exp1b_behav_scale_combined_wide.csv', row.names = F)

cor(df1b_join[,21:29],use="pairwise.complete.obs")
