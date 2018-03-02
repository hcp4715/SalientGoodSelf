
source('initial.r')

## load the RT data and d prime data

df3_behav <- read.csv('exp3_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df3_perDis <- read.csv('exp3_perDis_wide_r.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df3_join <- merge(df3_behav,df3_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df3_join <- df3_join[,c("expID","Subject","session","Sex","Age",colnames(df3_join)[c(4:33,36:41)])]

# rename some of the columns
df3_join$expID   <- "exp3"  # the experiment was "exp3"
df3_join$session[is.na(df3_join$session)] <- 1        # all participants only attented once in this study, no re-test

# difference between conditions
df3_join$RT_Good_Bad_Self  <- df3_join$RT_Match_Self_Moral   - df3_join$RT_Match_Self_Immoral
df3_join$RT_Good_Neut_Self <- df3_join$RT_Match_Self_Moral   - df3_join$RT_Match_Self_Neutral 
df3_join$RT_Neut_Bad_Self  <- df3_join$RT_Match_Self_Neutral - df3_join$RT_Match_Self_Immoral

df3_join$RT_Good_Bad_Other  <- df3_join$RT_Match_Other_Moral   - df3_join$RT_Match_Other_Immoral
df3_join$RT_Good_Neut_Other <- df3_join$RT_Match_Other_Moral   - df3_join$RT_Match_Other_Neutral 
df3_join$RT_Neut_Bad_Other  <- df3_join$RT_Match_Other_Neutral - df3_join$RT_Match_Other_Immoral

df3_join$RT_Self_Other_Moral <- df3_join$RT_Match_Self_Moral - df3_join$RT_Match_Other_Moral
df3_join$RT_Self_Other_Neutral <- df3_join$RT_Match_Self_Neutral - df3_join$RT_Match_Other_Neutral
df3_join$RT_Self_Other_Immoral <- df3_join$RT_Match_Self_Immoral - df3_join$RT_Match_Other_Immoral

df3_join$d_Good_Bad_Self  <- df3_join$d_Self_Moral   - df3_join$d_Self_Immoral
df3_join$d_Good_Neut_Self <- df3_join$d_Self_Moral   - df3_join$d_Self_Neutral 
df3_join$d_Neut_Bad_Self  <- df3_join$d_Self_Neutral - df3_join$d_Self_Immoral

df3_join$d_Good_Bad_Other  <- df3_join$d_Other_Moral   - df3_join$d_Other_Immoral
df3_join$d_Good_Neut_Other <- df3_join$d_Other_Moral   - df3_join$d_Other_Neutral 
df3_join$d_Neut_Bad_Other  <- df3_join$d_Other_Neutral - df3_join$d_Other_Immoral

df3_join$d_Self_Other_Moral <- df3_join$d_Self_Moral - df3_join$d_Other_Moral
df3_join$d_Self_Other_Neutral <- df3_join$d_Self_Neutral - df3_join$d_Other_Neutral
df3_join$d_Self_Other_Immoral <- df3_join$d_Self_Immoral - df3_join$d_Other_Immoral

# write file:
write.csv(df3_join, 'exp3_behav_scale_combined_wide.csv', row.names = F)
