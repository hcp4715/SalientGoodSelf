## This script is used for combining the behavial data and personal distance

# initializing
source('initial.r')

## load data ####

df4b_behav <- read.csv('exp4b_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b_perDis <- read.csv('exp4b_personaldistance_r.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df4b_join <- merge(df4b_behav,df4b_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df4b_join <- df4b_join[,colnames(df4b_join)[c(1:4,35,36,5:34,37:42)]]

colnames(df4b_join)[colnames(df4b_join) == 'SessionID'] <- "session"

# rename some of the columns
df4b_join$expID   <- "exp4"  # the experiment was "exp3"
df4b_join$session[is.na(df4b_join$session)] <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df4b_join$RT_Good_Bad_Self  <- df4b_join$RT_Match_Self_Moral   - df4b_join$RT_Match_Self_Immoral
df4b_join$RT_Good_Neut_Self <- df4b_join$RT_Match_Self_Moral   - df4b_join$RT_Match_Self_Neutral 
df4b_join$RT_Neut_Bad_Self  <- df4b_join$RT_Match_Self_Neutral - df4b_join$RT_Match_Self_Immoral

df4b_join$RT_Good_Bad_Other  <- df4b_join$RT_Match_Other_Moral   - df4b_join$RT_Match_Other_Immoral
df4b_join$RT_Good_Neut_Other <- df4b_join$RT_Match_Other_Moral   - df4b_join$RT_Match_Other_Neutral 
df4b_join$RT_Neut_Bad_Other  <- df4b_join$RT_Match_Other_Neutral - df4b_join$RT_Match_Other_Immoral

df4b_join$RT_Self_Other_Moral <- df4b_join$RT_Match_Self_Moral - df4b_join$RT_Match_Other_Moral
df4b_join$RT_Self_Other_Neutral <- df4b_join$RT_Match_Self_Neutral - df4b_join$RT_Match_Other_Neutral
df4b_join$RT_Self_Other_Immoral <- df4b_join$RT_Match_Self_Immoral - df4b_join$RT_Match_Other_Immoral

df4b_join$d_Good_Bad_Self  <- df4b_join$d_Self_Moral   - df4b_join$d_Self_Immoral
df4b_join$d_Good_Neut_Self <- df4b_join$d_Self_Moral   - df4b_join$d_Self_Neutral 
df4b_join$d_Neut_Bad_Self  <- df4b_join$d_Self_Neutral - df4b_join$d_Self_Immoral

df4b_join$d_Good_Bad_Other  <- df4b_join$d_Other_Moral   - df4b_join$d_Other_Immoral
df4b_join$d_Good_Neut_Other <- df4b_join$d_Other_Moral   - df4b_join$d_Other_Neutral 
df4b_join$d_Neut_Bad_Other  <- df4b_join$d_Other_Neutral - df4b_join$d_Other_Immoral

df4b_join$d_Self_Other_Moral <- df4b_join$d_Self_Moral - df4b_join$d_Other_Moral
df4b_join$d_Self_Other_Neutral <- df4b_join$d_Self_Neutral - df4b_join$d_Other_Neutral
df4b_join$d_Self_Other_Immoral <- df4b_join$d_Self_Immoral - df4b_join$d_Other_Immoral

# write file:
write.csv(df4b_join, 'exp4b_behav_scale_combined_wide.csv', row.names = F)
