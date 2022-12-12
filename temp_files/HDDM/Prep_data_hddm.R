##### prepare the data for HDDM analysis#####
#
# This script was used to prepare the behavioral data from experiment 3,4,and 5 for HDDM processing
#
# input: raw data from experiment
# output: csv file with clean data
# 
# pre-processing: 
#   1. exclude participants who failed to finish the experiment
#   2. exclude participants whose overall accuracy is lower than 50%
#   3. exclude trials without response
#   4. exclude trials with response time less than 200 ms

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientifc notion
options(digits=5)       # limit the number of reporting
rm(list = setdiff(ls(), lsf.str()))  # remove all data but keep functions

library(plyr)

############                          ###################### 
############ Prepare for experiment 3 ######################
############                          ######################
# load the data for matching task, there should be 18672 obs.
df3 <- read.csv("rawdata_exp_behav_moral_asso_3.csv",header = TRUE, sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA")) 

# render the numeric column
cols.num <- c("Age","BlockList.Sample","Target.RT","Target.ACC")
# Tcols.num <- c("SubjectID","Age","Block","Bin","Trial","RT","Accuracy")
df3[cols.num] <- sapply(df3[cols.num], as.numeric)

# rename the colnames
colnames(df3)[colnames(df3)=="Target.ACC"] <- "ACC"
colnames(df3)[colnames(df3)=="Target.RT"] <- "RT"
colnames(df3)[colnames(df3)=="Target.CRESP"] <- "CRESP"
colnames(df3)[colnames(df3)=="Target.RESP"] <- "RESP"
colnames(df3)[colnames(df3)=="YesNoResp"] <- "Matchness"
colnames(df3)[colnames(df3)=="self"] <- "Identity"
colnames(df3)[colnames(df3)=="morality"] <- "Morality"

df3$Morality[df3$Shape == 'Goodself'|df3$Shape == 'Goodother'] <- "Good"
df3$Morality[df3$Shape == 'Badself'|df3$Shape == 'Badother'] <- "Bad"
df3$Morality[df3$Shape == 'Normalself'|df3$Shape == 'Normalother'] <- "Neut"
df3$Identity[df3$Shape == 'Goodself'|df3$Shape == 'Badself'|df3$Shape == 'Normalself'] <- "Self"
df3$Identity[df3$Shape == 'Goodother' | df3$Shape == 'Badother'|df3$Shape == 'Normalother'] <- "Other"

df3$Morality[df3$Morality == "moral"] <- "Good"
df3$Morality[df3$Morality == "normal"] <- "Neut"
df3$Morality[df3$Morality == "immoral"] <- "Bad"
# df3$Morality <- factor(df3$Morality, levels=c("Good", "Neut","Bad")) # make the variables in a specified order
# df3$Identity[df3$Identity == "self"] <- "Self"
# df3$Identity[df3$Identity == "other"] <- "Other"
# df3$Identity <- factor(df3$Identity, levels=c("self", "other"))
df3$Matchness[df3$Matchness == "Yes"] <- "Match"
df3$Matchness[df3$Matchness == "No"] <- "Mismatch"
# df3$Matchness <- factor(df3$Matchness, levels=c("Match", "Mismatch"))
df3$RT <- df3$RT/1000  # change time scale to second

# remove the practice
df3.P <- df3[is.na(df3$BlockList.Sample),]
df3.T <- df3[complete.cases(df3$BlockList.Sample),]

print(count(df3.T, vars = "Subject"))

# calcuate the overall accuracy (just as in RT analysisi)
df3.T.acc.g <-  ddply(df3.T,.(Subject), summarise,
                      N = length(ACC),
                      countN = sum(ACC),
                      ACC = sum(ACC)/length(ACC))

excld.sub <- df3.T.acc.g$Subject[df3.T.acc.g$ACC < 0.60]

## prepare for hddm

df3.valid <- df3.T[!(df3.T$Subject %in% excld.sub),]  # exclude the subject whose over all accy is lower than 0.5
print(count(df3.valid, vars = "Subject"))             # check the number of each participant for the testing phase

### data for matching task
# separate the match and non-matching trials
# delete the no response trials
df3.match <- df3.valid[df3.valid$Match == 'Match',]
df3.nonmatch <- df3.valid[df3.valid$Match == 'Mismatch',]

df3.match.noResp <- subset(df3.match,is.na(RESP))    #  the trials without response
df3.match.tooFast <- subset(df3.match,RT <= 0.2 & RT != 0) #  the trials less than 200 ms
df3.match.v <- df3.match[df3.match$RT > 0.2,]
ratioExlcd.match <- (nrow(df3.match.noResp) + nrow(df3.match.tooFast))/nrow(df3.match)  # 0.375
nrow(df3.match.noResp) + nrow(df3.match.tooFast) + nrow(df3.match.v) == nrow(df3.match) # check the number is right
print(count(df3.match.v, vars = "Subject")) # print the trial number for each participants

df3.nonmatch.noResp <- subset(df3.nonmatch,is.na(RESP))  # exclude the trials without response
df3.nonmatch.tooFast <- subset(df3.nonmatch,RT <= 0.2 & RT != 0) # exclude the trials less than 200 ms
df3.nonmatch.v <- subset(df3.nonmatch,RT > 0.2)
ratioExlcd.mismatch <- (nrow(df3.nonmatch.noResp)+nrow(df3.nonmatch.tooFast))/nrow(df3.nonmatch) # 0.0544
nrow(df3.nonmatch.noResp) + nrow(df3.nonmatch.tooFast) + nrow(df3.nonmatch.v) == nrow(df3.nonmatch) # check the number is right
print(count(df3.nonmatch.v, vars = "Subject")) # print the trial number for each participants

# get the neccessary columns and re-name the columes according to HDDM's demand
df3.match.hddm <-  df3.match.v[,c("Subject","Morality","Identity","RT","ACC")]
df3.nonmatch.hddm <- df3.nonmatch.v[,c("Subject","Morality","Identity","RT","ACC")]

colnames(df3.match.hddm)<- c("subj_idx","moral","id","rt","response")
colnames(df3.nonmatch.hddm)<- c("subj_idx","moral","id","rt","response")


# save the files
write.csv(df3.match.hddm,"data_exp3_match_hddm.csv",row.names=FALSE)
write.csv(df3.nonmatch.hddm,"data_exp3_nonmatch_hddm.csv",row.names=FALSE)


############                          ###################### 
############ Prepare for experiment 4 ######################
############                          ######################
# load the data, there should be 18672 obs.
df4 <- read.csv("rawdata_exp_behav_moral_asso_4_170321.csv",header = TRUE, sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA")) 

# render the numeric column
cols.num <- c('Subject',"Age","BlockList.Sample","Target.RT","Target.ACC")
df4[cols.num] <- sapply(df4[cols.num], as.numeric)

# rename the colnames
colnames(df4)[colnames(df4)=="Target.ACC"] <- "ACC"
colnames(df4)[colnames(df4)=="Target.RT"] <- "RT"
colnames(df4)[colnames(df4)=="YesNoResp"] <- "Matchness"
colnames(df4)[colnames(df4)=="self"] <- "Identity"
colnames(df4)[colnames(df4)=="morality"] <- "Morality"

# rename the condition
df4$Morality[df4$Morality == "Normal"] <- "Neut"
df4$Matchness[df4$Matchness == "Yes"] <- "Match"
df4$Matchness[df4$Matchness == "No"] <- "Mismatch"
df4$Identity[df4$Identity == "self"] <- "Self"
df4$Identity[df4$Identity == "other"] <- "Other"

# change time scale to second
df4$RT <- df4$RT/1000

# remove the practice
df4.P <- df4[is.na(df4$BlockList.Sample),]
df4.T <- df4[complete.cases(df4$BlockList.Sample),]

# remove the data of session 1 of subject 30 because of this session was unfinished
df4.T <- subset(df4.T,!(Subject ==30 & Session == 1))

print(count(df4.T, vars = "Subject"))

# calcuate the overall accuracy (just as in RT analysisi)
df4.T.acc.g <-  ddply(df4.T,.(Subject), summarise,
                      N = length(ACC),
                      countN = sum(ACC),
                      ACC = sum(ACC)/length(ACC))

excld.sub <- df4.T.acc.g$Subject[df4.T.acc.g$ACC < 0.60]

## prepare for hddm

df4.valid <- df4.T[!(df4.T$Subject %in% excld.sub),]  # exclude the subject whose over all accy is lower than 0.5
print(count(df4.valid, vars = "Subject"))             # check the number of each participant for the testing phase

### data for matching task
# separate the match and non-matching trials
# delete the no response trials
df4.match <- df4.valid[df4.valid$Match == 'Match',]
df4.nonmatch <- df4.valid[df4.valid$Match == 'Mismatch',]

df4.match.noResp <- subset(df4.match,is.na(Target.RESP))    #  the trials without response
df4.match.tooFast <- subset(df4.match,RT <= 0.2 & RT != 0) #  the trials less than 200 ms
df4.match.v <- df4.match[df4.match$RT > 0.2,]
ratioExlcd.match <- (nrow(df4.match.noResp) + nrow(df4.match.tooFast))/nrow(df4.match)  # 0.0448
nrow(df4.match.noResp) + nrow(df4.match.tooFast) + nrow(df4.match.v) == nrow(df4.match) # check the number is right
print(count(df4.match.v, vars = "Subject")) # print the trial number for each participants

df4.nonmatch.noResp <- subset(df4.nonmatch,is.na(Target.RESP))  # exclude the trials without response
df4.nonmatch.tooFast <- subset(df4.nonmatch,RT <= 0.2 & RT != 0) # exclude the trials less than 200 ms
df4.nonmatch.v <- subset(df4.nonmatch,RT > 0.2)
ratioExlcd.mismatch <- (nrow(df4.nonmatch.noResp)+nrow(df4.nonmatch.tooFast))/nrow(df4.nonmatch) # 0.05178
nrow(df4.nonmatch.noResp) + nrow(df4.nonmatch.tooFast) + nrow(df4.nonmatch.v) == nrow(df4.nonmatch) # check the number is right
print(count(df4.nonmatch.v, vars = "Subject")) # print the trial number for each participants

# get the neccessary columns and re-name the columes according to HDDM's demand
df4.match.hddm <-  df4.match.v[,c("Subject","Morality","Identity","RT","ACC")]
df4.nonmatch.hddm <- df4.nonmatch.v[,c("Subject","Morality","Identity","RT","ACC")]

colnames(df4.match.hddm)<- c("subj_idx","moral","id","rt","response")
colnames(df4.nonmatch.hddm)<- c("subj_idx","moral","id","rt","response")
df4.match.hddm <- df4.match.hddm[order(df4.match.hddm$subj_idx),] 
df4.nonmatch.hddm <- df4.nonmatch.hddm[order(df4.nonmatch.hddm$subj_idx),] 

# save the files
write.csv(df4.match.hddm,"data_exp4_match_hddm.csv",row.names=FALSE)
write.csv(df4.nonmatch.hddm,"data_exp4_nonmatch_hddm.csv",row.names=FALSE)

############                          ###################### 
############ Prepare for experiment 5 ######################
############                          ######################
# load the data.
df5 <- read.csv("rawdata_exp_behav_moral_asso_5.csv",header = TRUE, sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA")) 

# render the numeric column
cols.num <- c('Subject',"Age","BlockList.Sample","Target.RT","Target.ACC")
df5[cols.num] <- sapply(df5[cols.num], as.numeric)
rm(cols.num)

# rename the colnames
colnames(df5)[colnames(df5)=="Target.ACC"] <- "ACC"
colnames(df5)[colnames(df5)=="Target.RT"] <- "RT"
colnames(df5)[colnames(df5)=="YesNoResp"] <- "Matchness"
colnames(df5)[colnames(df5)=="self"] <- "Identity"
colnames(df5)[colnames(df5)=="morality"] <- "Morality"

# rename the condition
df5$Morality[df5$Morality == "Ord"] <- "Neut"
df5$Matchness[df5$Matchness == "Yes"] <- "Match"
df5$Matchness[df5$Matchness == "No"] <- "Mismatch"
df5$Identity[df5$Identity == "self"] <- "Self"
df5$Identity[df5$Identity == "other"] <- "Other"

# change time scale to second
df5$RT <- df5$RT/1000

# remove the practice
df5.P <- df5[is.na(df5$BlockList.Sample),]
df5.T <- df5[complete.cases(df5$BlockList.Sample),]

print(count(df5.T, vars = "Subject"))

# calcuate the overall accuracy (just as in RT analysisi)
df5.T.acc.g <-  ddply(df5.T,.(Subject), summarise,
                      N = length(ACC),
                      countN = sum(ACC),
                      ACC = sum(ACC)/length(ACC))

excld.sub <- df5.T.acc.g$Subject[df5.T.acc.g$ACC < 0.60]

## prepare for hddm

df5.valid <- df5.T[!(df5.T$Subject %in% excld.sub),]  # exclude the subject whose over all accy is lower than 0.5
print(count(df5.valid, vars = "Subject"))             # check the number of each participant for the testing phase

### data for matching task
# separate the match and non-matching trials
# delete the no response trials
df5.match <- df5.valid[df5.valid$Match == 'Match',]
df5.nonmatch <- df5.valid[df5.valid$Match == 'Mismatch',]

df5.match.noResp <- subset(df5.match,is.na(Target.RESP))    #  the trials without response
df5.match.tooFast <- subset(df5.match,RT <= 0.2 & RT != 0) #  the trials less than 200 ms
df5.match.v <- df5.match[df5.match$RT > 0.2,]
ratioExlcd.match <- (nrow(df5.match.noResp) + nrow(df5.match.tooFast))/nrow(df5.match)  # 0.03949
nrow(df5.match.noResp) + nrow(df5.match.tooFast) + nrow(df5.match.v) == nrow(df5.match) # check the number is right
print(count(df5.match.v, vars = "Subject")) # print the trial number for each participants

df5.nonmatch.noResp <- subset(df5.nonmatch,is.na(Target.RESP))  # exclude the trials without response
df5.nonmatch.tooFast <- subset(df5.nonmatch,RT <= 0.2 & RT != 0) # exclude the trials less than 200 ms
df5.nonmatch.v <- subset(df5.nonmatch,RT > 0.2)
ratioExlcd.mismatch <- (nrow(df5.nonmatch.noResp)+nrow(df5.nonmatch.tooFast))/nrow(df5.nonmatch) # 0.05178
nrow(df5.nonmatch.noResp) + nrow(df5.nonmatch.tooFast) + nrow(df5.nonmatch.v) == nrow(df5.nonmatch) # check the number is right
print(count(df5.nonmatch.v, vars = "Subject")) # print the trial number for each participants

# get the neccessary columns and re-name the columes according to HDDM's demand
df5.match.hddm <-  df5.match.v[,c("Subject","Morality","Identity","RT","ACC")]
df5.nonmatch.hddm <- df5.nonmatch.v[,c("Subject","Morality","Identity","RT","ACC")]

colnames(df5.match.hddm)<- c("subj_idx","moral","id","rt","response")
colnames(df5.nonmatch.hddm)<- c("subj_idx","moral","id","rt","response")
df5.match.hddm <- df5.match.hddm[order(df5.match.hddm$subj_idx),] 
df5.nonmatch.hddm <- df5.nonmatch.hddm[order(df5.nonmatch.hddm$subj_idx),] 

# save the files
write.csv(df5.match.hddm,"data_exp5_match_hddm.csv",row.names=FALSE)
write.csv(df5.nonmatch.hddm,"data_exp5_nonmatch_hddm.csv",row.names=FALSE)





### examine the RT between correct and incorrect trials, not finished
df3.match.hddm_check <- summarySE(df3.match.hddm,measurevar = 'rt', groupvar = c('subj_idx','id','moral','response'),na.rm = TRUE)
print(count(df3.match.hddm_check, vars = c("subj_idx",'id','moral','response'))) # print the trial number for each participants
library(ez)
df3.match.rt_check.an <- ezANOVA(df3.match.hddm_check,dv = rt, wid = subj_idx, within = .(id,moral,response), type = 3)
library(reshape2)
df.L2.match.hddm_check_l <- dcast(df.L2.match.hddm_check,subj_idx ~ response,value.var = 'rt')
L2.match.rt_check.t <- t.test(df.L2.match.hddm_check_l$`0`,df.L2.match.hddm_check_l$`1`,paired = T,alternative = 'greater')

### data for categorization task
df.T1 <- df.T
procFailureSub.T <- c("7","8","2027","7035")

df.T1 <- df.T1[!(df.T1$Subject %in% procFailureSub.T),] # exclude participant that failed to finish

# calculate the overall accuracy for categorziation task
df.T1.acc.g <-  ddply(df.T1,.(Subject), summarise,
                      N = length(ACC),
                      countN = sum(ACC),
                      ACC = sum(ACC)/length(ACC))

excld.sub.T <- df.T1.acc.g$Subject[df.T1.acc.g$ACC < 0.5]    # less than 50% accuracy in categorization task

df.T1 <- df.T1[!(df.T1$Subject %in% excld.sub),]        # exclude participant with low overall accuracy
df.T1 <- df.T1[!(df.T1$Subject %in% excld.sub.T),]
print(count(df.T1, vars = "Subject"))

# remove the "importance" data
df.T1.twoTask <- df.T1[!(df.T1$Task == 'importance'),]    # exclude the importance categoirzation task
print(count(df.T1.twoTask, vars = "Subject")) # print the trial number

# remove the trials with no-reponse
df.T1.valid <- df.T1.twoTask[complete.cases(df.T1.twoTask$ResponseKey),] # exclude trials without response
df.T1.noResp <- df.T1.twoTask[is.na(df.T1.twoTask$ResponseKey),] # trials without response
df.T1.tooFast <- df.T1.valid[df.T1.valid$RT <= 0.2,]             # trials responsed too fast
df.T1.valid <- df.T1.valid[df.T1.valid$RT > 0.2,]                # valid trials
ratioExlcd.categ <- (nrow(df.T1.noResp)+nrow(df.T1.tooFast))/nrow(df.T1.twoTask) # 0.0186
nrow(df.T1.noResp)+nrow(df.T1.tooFast) + nrow(df.T1.valid) == nrow(df.T1.twoTask) # check

sum(df.T1.valid$ACC == -1) # check if all the no-response trials were removed
print(count(df.T1.valid, vars = "Subject"))

# extract useful data
df.T1.hddm <- df.T1.valid[,c("Subject","Task","Morality","Identity","RT","ACC")]

# re-name those columan according to HDDM's requirement
colnames(df.T1.hddm)<- c("subj_idx","crit","moral","id","rt","response")
df.T1.hddm$crit[df.T1.hddm$crit == 'moral'] <- 'morality'  # rename the task
df.T1.hddm$crit[df.T1.hddm$crit == 'self'] <- 'identity'   # rename the task
df.T1.hddm.Id <- subset(df.T1.hddm,crit == 'identity')
df.T1.hddm.val <- subset(df.T1.hddm,crit == 'morality')
print(count(df.T1.hddm.val, vars = "subj_idx"))

# write the data to csv
write.csv(df.T1.hddm,"./HDDM/FinalVersion/data_Categ_hddm.csv",row.names=FALSE)
write.csv(df.T1.hddm.Id,"./HDDM/FinalVersion/data_Categ_hddm_ID_task.csv",row.names=FALSE)
write.csv(df.T1.hddm.val,"./HDDM/FinalVersion/data_Categ_hddm_val_task.csv",row.names=FALSE)
