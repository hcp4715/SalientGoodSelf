
source('initial.r')

## load the RT data and d prime data

df4a_behav <- read.csv('exp4a_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a_perDis <- read.csv('exp4a_personaldistance_r.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df4a_join <- merge(df4a_behav,df4a_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df4a_join <- df4a_join[,colnames(df4a_join)[c(1:4,35,36,5:34,37:42)]]

colnames(df4a_join)[colnames(df4a_join) == 'SessionID'] <- "session"

# rename some of the columns
df4a_join$expID   <- "exp4a"  # the experiment was "exp3"
df4a_join$session[is.na(df4a_join$session)] <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df4a_join$RT_Good_Bad_Self  <- df4a_join$RT_Match_Self_Moral   - df4a_join$RT_Match_Self_Immoral
df4a_join$RT_Good_Neut_Self <- df4a_join$RT_Match_Self_Moral   - df4a_join$RT_Match_Self_Neutral 
df4a_join$RT_Neut_Bad_Self  <- df4a_join$RT_Match_Self_Neutral - df4a_join$RT_Match_Self_Immoral

df4a_join$RT_Good_Bad_Other  <- df4a_join$RT_Match_Other_Moral   - df4a_join$RT_Match_Other_Immoral
df4a_join$RT_Good_Neut_Other <- df4a_join$RT_Match_Other_Moral   - df4a_join$RT_Match_Other_Neutral 
df4a_join$RT_Neut_Bad_Other  <- df4a_join$RT_Match_Other_Neutral - df4a_join$RT_Match_Other_Immoral

df4a_join$RT_Self_Other_Moral <- df4a_join$RT_Match_Self_Moral - df4a_join$RT_Match_Other_Moral
df4a_join$RT_Self_Other_Neutral <- df4a_join$RT_Match_Self_Neutral - df4a_join$RT_Match_Other_Neutral
df4a_join$RT_Self_Other_Immoral <- df4a_join$RT_Match_Self_Immoral - df4a_join$RT_Match_Other_Immoral

df4a_join$d_Good_Bad_Self  <- df4a_join$d_Self_Moral   - df4a_join$d_Self_Immoral
df4a_join$d_Good_Neut_Self <- df4a_join$d_Self_Moral   - df4a_join$d_Self_Neutral 
df4a_join$d_Neut_Bad_Self  <- df4a_join$d_Self_Neutral - df4a_join$d_Self_Immoral

df4a_join$d_Good_Bad_Other  <- df4a_join$d_Other_Moral   - df4a_join$d_Other_Immoral
df4a_join$d_Good_Neut_Other <- df4a_join$d_Other_Moral   - df4a_join$d_Other_Neutral 
df4a_join$d_Neut_Bad_Other  <- df4a_join$d_Other_Neutral - df4a_join$d_Other_Immoral

df4a_join$d_Self_Other_Moral <- df4a_join$d_Self_Moral - df4a_join$d_Other_Moral
df4a_join$d_Self_Other_Neutral <- df4a_join$d_Self_Neutral - df4a_join$d_Other_Neutral
df4a_join$d_Self_Other_Immoral <- df4a_join$d_Self_Immoral - df4a_join$d_Other_Immoral

# write file:
write.csv(df4a_join, 'exp4a_behav_scale_combined_wide.csv', row.names = F)
