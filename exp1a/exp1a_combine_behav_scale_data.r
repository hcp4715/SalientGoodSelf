
source('initial.r')

## load the RT data and d prime data

df1a_behav <- read.csv('exp1a_behav_wide.csv',header = T, sep = ',',
                      stringsAsFactors=FALSE,na.strings=c("","NA"))
df1a_perDis <- read.csv('exp1a_personaldistance.csv',header = T, sep = ',',
                        stringsAsFactors=FALSE,na.strings=c("","NA"))

# merge the personal distance data to the behav data (all.x = T)
df1a_join <- merge(df1a_behav,df1a_perDis,by.x = "Subject", by.y = "subID",all.x = T)
df1a_join$RT_Good_Bad <- df1a_join$RT_Match_Immoral - df1a_join$RT_Match_Moral
df1a_join$RT_Good_Neut <- df1a_join$RT_Match_Neutral - df1a_join$RT_Match_Moral

corTry <- cor(df1a_join[,c('RT_Good_Bad','RT_Good_Neut',"SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")])
