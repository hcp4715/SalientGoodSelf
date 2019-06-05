### about ####
# this code is to Preprocess all the data for exp 5b (ERP exp 2)
# data were collected in 2016,
# Change the levels of moral valence to "good", "bad", "neutral". 


## initializing #### 
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
source('Initial_exp5b.r')
library(mosaic)

## load data ####
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
resDir = curDir

# 23 participants
df5b_d1 <- read.csv("rawdata_erp_exp5b_d1_2016.csv",header = TRUE, sep = ",",
                 stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
  dplyr::filter(!is.na(BlockList.Sample)) %>%                                 # select only form exp
  dplyr::rename(Subject = ï..Subject, ACC = Target.ACC, CRESP = Target.CRESP, # rename the columns
                RESP = Target.RESP, RT = Target.RT, Match = YesNoResp, 
                Morality = morality, Identity = identity) %>%    #
  dplyr::mutate(Morality = ifelse(Morality == "normal", "neutral", Morality), # re-code the data
                Match = ifelse(Match == "Yes", "match", "mismatch"),
                Age = ifelse(Age == 0, NA, Age)) %>%
  dplyr::mutate(Sex = ifelse(Subject == 6207, 'female',Sex)) %>%              # subject 6207 is female but filled male at the first day
  dplyr::mutate(Age = ifelse(Subject == 6217, 25, Age))%>%                    # subject 6217 forgot to fill age
  dplyr::mutate(Morality = derivedFactor("Bad" = (Morality == "bad"),         # re-code valence
                                         "Good" = (Morality == "good"),
                                         "Neutral" = (Morality == "neutral"),
                                         method ="first",  .default = NA)) %>%
  dplyr::mutate(Identity = derivedFactor("Self" = (Identity == "self"),       # re-code identity
                                         "Other" = (Identity == "other"),
                                         method ="first",  .default = NA))

# 22 participants (6209 is missing)
df5b_d2 <- read.csv("rawdata_erp_exp5b_d2_2016.csv",header = TRUE, sep = ",",
                    stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
  dplyr::filter(!is.na(BlockList.Sample)) %>%                                                   # select only form exp
  dplyr::rename(Subject = ï..Subject, ACC = Target.ACC, CRESP = Target.CRESP,                   # rename the columns
                RESP = Target.RESP, RT = Target.RT, Match = YesNoResp, 
                Morality = morality, Identity = identity) %>%    #
  dplyr::mutate(Morality = ifelse(Morality == "normal", "neutral", Morality),                   # re-code the data
                Match = ifelse(Match == "Yes", "match", "mismatch"),
                Age = ifelse(Age == 0, NA, Age)) %>%
  dplyr::mutate(Morality = derivedFactor("Bad" = (Morality == "bad"),         # re-code valence
                                         "Good" = (Morality == "good"),
                                         "Neutral" = (Morality == "neutral"),
                                         method ="first",  .default = NA)) %>%
  dplyr::mutate(Identity = derivedFactor("Self" = (Identity == "self"),       # re-code identity
                                         "Other" = (Identity == "other"),
                                         method ="first",  .default = NA))

        
length(unique(df5b_d1$Subject))

## record from the meta-data:
#  sub 23: added one more block
#  sub 29: added one more block

## Basic information of the data ####
df5b_d1.T.basic     <- df5b_d1 %>%
  dplyr::select(Subject, Age, Handedness, Sex) %>%
  dplyr::distinct(Subject, Age, Sex) %>%
  dplyr::summarise(N = length(Subject),                        # N   = 23
                   N.f = sum(Sex == 'female'),                 # N.f = 9
                   N.m = sum(Sex == 'male'),                   # N.m = 14
                   MeanAge = round(mean(Age,na.rm=TRUE),2),    # age.m = 22.96
                   SDAge   = round(sd(Age,na.rm=TRUE),2))      # age.s = 2.46

df5b_d2.T.basic     <- df5b_d2 %>%
  dplyr::select(Subject, Age, Handedness, Sex) %>%
  dplyr::distinct(Subject, Age, Sex) %>%
  dplyr::summarise(N = length(Subject),                        # N   = 22 
                   N.f = sum(Sex == 'female'),                 # N.f = 9
                   N.m = sum(Sex == 'male'),                   # N.m = 13
                   MeanAge = round(mean(Age,na.rm=TRUE),2),    # age.m = 23.05
                   SDAge   = round(sd(Age,na.rm=TRUE),2))      # age.s = 2.46


# number of participant who didn't finished the experiment
#nQuit <- length(unique(df5b_d1.P$Subject)) - length(unique(df5b_d1.T$Subject))


# caculate the overall accuracy for each subject
df5b_d1.acc.g <-  df5b_d1 %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%
  dplyr::ungroup()

df5b_d2.acc.g <-  df5b_d2 %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%
  dplyr::ungroup()

# exlucde the participants with less than 60% overall accuracy
df5b_d1.excld.sub <- df5b_d1.acc.g$Subject[df5b_d1.acc.g$ACC < 0.6]         # < 60% accuracy, 1

df5b_d2.excld.sub <- df5b_d2.acc.g$Subject[df5b_d2.acc.g$ACC < 0.6]         # < 60% accuracy, Zero

df5b_d1.valid     <- df5b_d1[!(df5b_d1$Subject %in% df5b_d1.excld.sub),]    # exclude the invalid subjects
df5b_d1.V         <- df5b_d1.valid[!(df5b_d1.valid$RT <= 200  & df5b_d1.valid$ACC == 1),]

df5b_d2.valid     <- df5b_d2[!(df5b_d2$Subject %in% df5b_d2.excld.sub),]    # exclude the invalid subjects
df5b_d2.V         <- df5b_d2.valid[!(df5b_d2.valid$RT <= 200  & df5b_d2.valid$ACC == 1),]


# exclude the correct response with less than 200 ms reaction time
df5b_d1.excld.trial   <- df5b_d1.valid[df5b_d1.valid$RT <= 200 & df5b_d1.valid$ACC == 1,]
df5b_d1.excld.trial.r <- nrow(df5b_d1.excld.trial)/nrow(df5b_d1.valid)  # ratio of excluded trials in all trials: 0.00379

df5b_d2.excld.trial   <- df5b_d2.valid[df5b_d2.valid$RT <= 200 & df5b_d2.valid$ACC == 1,]
df5b_d2.excld.trial.r <- nrow(df5b_d2.excld.trial)/nrow(df5b_d2.valid)  # ratio of excluded trials in all trials: 0.0125

# make sure that the number is correct (if neccessary)
#length(unique(df5b_d1.valid$Subject)) + length(df5b_d1.excld.sub) == length(unique(df5b_d1$Subject))

#-----2. Day 1's data -------------------------------------------------------------------------------------------------
###############################
### calculate the Accuracy ####
###############################

df5b_d1.acc_l <-  df5b_d1.V %>%
  dplyr::group_by(Subject,Match, Identity, Morality) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%
  dplyr::ungroup()

#df5b_d1.acc_w <- reshape2::dcast(df5b_d1.acc_l, Subject ~  Match + Identity + Morality,value.var = "ACC")

# rename the column number
#colnames(df5b_d1.acc_w)[2:13] <- paste("ACC", colnames(df5b_d1.acc_w[,2:13]), sep = "_")

### Long to wide
df5b_d1.acc_w <- df5b_d1.acc_l %>%
  tidyr::unite(col = "Cond",c("Match","Identity", "Morality"),
               sep = "_", remove = T) %>%                                 # combine two factors to condition
  dplyr::select(-N, -countN) %>%                                          # remove column N and countN, avoid problems
  tidyr::spread(key = Cond, value = ACC) %>%                              # long to wide
  dplyr::rename_at(vars(-Subject),function(x) paste0("ACC_",x))           # add prefix to certain conditions

        
## d prime ####

# one problem: how to code the trials without response??

df5b_d1.V.dprime_l <- df5b_d1.V %>%
  dplyr::mutate(sdt = derivedFactor("hit" = (ACC ==1 & Match == "match"),    # code as hit
                                    "CR" = (ACC ==1 & Match == "mismatch"),  # code as correct reject
                                    "miss" = (ACC == 0 & Match == "match"),  # code as miss
                                    "FA" = (ACC == 0 & Match == "mismatch"), # code as false alarm
                                    method ="first",  .default = NA)) %>% 
  dplyr::group_by(Subject,Age, Sex, Identity, Morality,sdt) %>%
  dplyr::summarise(N = length(sdt)) %>%                                      # calculate the counts for each 
  dplyr::ungroup() %>%
  tidyr::spread(key = sdt, value = N,fill = 0) %>%                           # long-to-wide format
  dplyr::mutate(hitR = hit/(hit + miss),                                     # hit rate
                FAR  = FA/(FA+CR)) %>%                                       # fa rate
  dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%        # if FA rate is 0, standardize it
  dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
  dplyr::select(Subject, Age,Sex,Identity,Morality, dprime)                  # select relevant columns

### Long to wide
df5b_d1.V.dprime_w <- df5b_d1.V.dprime_l %>%
  tidyr::unite(col = "Cond",c("Identity", "Morality"),sep = "_", remove = T) %>%  # combine two factors to condition
  tidyr::spread(key = Cond, value = dprime) %>%                                   # long to wide
  dplyr::rename_at(vars(-Subject,-Age,-Sex),function(x) paste0("d_",x))           # add prefix to certain conditions

#df5b_d1.V$sdt <- NA
#for (i in 1:nrow(df5b_d1.V)){
#        if (df5b_d1.V$ACC[i] == 1 & df5b_d1.V$Match[i] == "match"){
#                df5b_d1.V$sdt[i] <- "hit"
#        } else if (df5b_d1.V$ACC[i] == 1 & df5b_d1.V$Match[i] == "mismatch"){
#                df5b_d1.V$sdt[i] <- "CR"
#        } else if (df5b_d1.V$ACC[i] == 0 & df5b_d1.V$Match[i] == "match"){
#                df5b_d1.V$sdt[i] <- "miss"
#        } else if (df5b_d1.V$ACC[i] == 0 & df5b_d1.V$Match[i] == "mismatch"){
#                df5b_d1.V$sdt[i] <- "FA"
#        }
#}

# calculate the number of each for each condition
#df5b_d1.V.SDT <-  ddply(df5b_d1.V,.(Subject,Age, Sex, Identity, Morality,sdt), summarise,
#                     N = length(sdt))

# long format to wide to calculate the d prime
#df5b_d1.V.SDT_w  <- dcast(df5b_d1.V.SDT, Subject + Age + Sex + Identity + Morality  ~ sdt,value.var = "N")

# if fa or miss is NA, set it to zero
#df5b_d1.V.SDT_w$miss[is.na(df5b_d1.V.SDT_w$miss)] <- 0
#df5b_d1.V.SDT_w$FA[is.na(df5b_d1.V.SDT_w$FA)] <- 0
#df5b_d1.V.SDT_w$hitR <- df5b_d1.V.SDT_w$hit/(df5b_d1.V.SDT_w$hit + df5b_d1.V.SDT_w$miss)
#df5b_d1.V.SDT_w$faR <- df5b_d1.V.SDT_w$FA/(df5b_d1.V.SDT_w$FA + df5b_d1.V.SDT_w$CR)

# standardized way to deal with the extreme values
#for (i in 1:nrow(df5b_d1.V.SDT_w)){
#        if (df5b_d1.V.SDT_w$hitR[i] == 1){
#                df5b_d1.V.SDT_w$hitR[i] <- 1 - 1/(2*(df5b_d1.V.SDT_w$hit[i] + df5b_d1.V.SDT_w$miss[i]))
#        }
#}

#for (i in 1:nrow(df5b_d1.V.SDT_w)){
#        if (df5b_d1.V.SDT_w$faR[i] == 0){
#                df5b_d1.V.SDT_w$faR[i] <- 1/(2*(df5b_d1.V.SDT_w$FA[i] + df5b_d1.V.SDT_w$CR[i]))
#        }
#}

# calculate the d prime for each condition
#df5b_d1.V.SDT_w$dprime <- mapply(dprime,df5b_d1.V.SDT_w$hitR,df5b_d1.V.SDT_w$faR)
# df5b_d1.V.dprime_l     <- df5b_d1.V.SDT_w[,c("Subject", "Age","Sex","Identity","Morality","dprime")]

# change dprime data from long format to wide
#df5b_d1.V.dprime_w     <- dcast(df5b_d1.V.dprime_l, Subject + Sex + Age ~ Identity + Morality,value.var = "dprime")

# rename the column number
# colnames(df5b_d1.V.dprime_w)[4:6] <- paste("d", colnames(df5b_d1.V.dprime_w[,4:6]), sep = "_")

## RT ####
df5b_d1.V.RT      <- df5b_d1.V %>%
  dplyr::filter(ACC ==1)  # exclued rt data less than 200 ms, and inaccurate data

df5b_d1.V.RT.subj_l <- summarySEwithin(df5b_d1.V.RT,measurevar = 'RT', withinvar = c('Subject','Match',"Identity",'Morality'), idvar = 'Subject',na.rm = TRUE)

## long to wide
#df5b_d1.V.RT.subj_w <- dcast(df5b_d1.V.RT.subj_l, Subject ~ Match + Identity + Morality ,value.var = "RT") 

# rename the columns of RT data
#colnames(df5b_d1.V.RT.subj_w)[2:13] <- paste("RT", colnames(df5b_d1.V.RT.subj_w[,2:13]), sep = "_")

### Long to wide
df5b_d1.V.RT.subj_w <- df5b_d1.V.RT.subj_l %>%
  tidyr::unite(col = "Cond",c("Match","Identity", "Morality"),
               sep = "_", remove = T) %>%                                # combine two factors to condition
  dplyr::select(Subject,Cond,RT) %>%                                     # only select needed columns
  tidyr::spread(key = Cond, value = RT) %>%                              # long to wide
  dplyr::rename_at(vars(-Subject),function(x) paste0("RT_",x))           # add prefix to certain conditions

# merge the dprime and RT data and save
df5b_d1.V.sum_w <- merge(df5b_d1.acc_w,df5b_d1.V.dprime_w,by = "Subject")
df5b_d1.V.sum_w <- merge(df5b_d1.V.sum_w,df5b_d1.V.RT.subj_w,by = 'Subject')
# order the columns
#df5b_d1.V.sum_w <- df5b_d1.V.sum_w[,c("Subject", "Sex","Age", "ACC_Match_Good", "ACC_Match_Neutral", "ACC_Match_Bad", "ACC_Mismatch_Good",
#                                "ACC_Mismatch_Neutral", "ACC_Mismatch_Bad", "d_Good", "d_Neutral", "d_Bad", "RT_Match_Good",
#                                "RT_Match_Neutral", "RT_Match_Bad", "RT_Mismatch_Good", "RT_Mismatch_Neutral","RT_Mismatch_Bad")]
df5b_d1.V.sum_w <- df5b_d1.V.sum_w[,c(1,14,15,2:13,16:33)]

df5b_d1.v.sum_rt_acc_l <- merge(df5b_d1.acc_l,df5b_d1.V.RT.subj_l,by = c("Subject","Match","Identity","Morality"))
df5b_d1.v.sum_rt_acc_l <- df5b_d1.v.sum_rt_acc_l[order(df5b_d1.v.sum_rt_acc_l$Subject),]
df5b_d1.v.sum_rt_acc_l <- df5b_d1.v.sum_rt_acc_l[,c("Subject","Match","Identity","Morality","N.x","countN","ACC","RT")]
colnames(df5b_d1.v.sum_rt_acc_l) <- c("Subject","Match","Identity","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df5b_d1.V.sum_w,'exp5b_d1_behav_wide.csv',row.names = F)
write.csv(df5b_d1.v.sum_rt_acc_l,'exp5b_d1_rt_acc_long.csv',row.names = F)
write.csv(df5b_d1.V.dprime_l,'exp5b_d1_dprime_long.csv',row.names = F)


## plot the data
rtdata <- subset(df5b_d1.v.sum_rt_acc_l,Match == "match")
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp5b_d1', df5b_d1.V.dprime_l,rtdata)

#-----2. Day 2's data -------------------------------------------------------------------------------------------------
###############################
### calculate the Accuracy ####
###############################
df5b_d2.acc_l <-  df5b_d2.V %>%
  dplyr::group_by(Subject,Match, Identity, Morality) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%
  dplyr::ungroup()

### Long to wide
df5b_d2.acc_w <- df5b_d2.acc_l %>%
  tidyr::unite(col = "Cond",c("Match","Identity", "Morality"),
               sep = "_", remove = T) %>%                                 # combine two factors to condition
  dplyr::select(-N, -countN) %>%                                          # remove column N and countN, avoid problems
  tidyr::spread(key = Cond, value = ACC) %>%                              # long to wide
  dplyr::rename_at(vars(-Subject),function(x) paste0("ACC_",x))           # add prefix to certain conditions


### d prime ###
# one problem: how to code the trials without response??

df5b_d2.V.dprime_l <- df5b_d2.V %>%
  dplyr::mutate(sdt = derivedFactor("hit" = (ACC ==1 & Match == "match"),    # code as hit
                                    "CR" = (ACC ==1 & Match == "mismatch"),  # code as correct reject
                                    "miss" = (ACC == 0 & Match == "match"),  # code as miss
                                    "FA" = (ACC == 0 & Match == "mismatch"), # code as false alarm
                                    method ="first",  .default = NA)) %>% 
  dplyr::group_by(Subject,Age, Sex, Identity, Morality,sdt) %>%
  dplyr::summarise(N = length(sdt)) %>%                                      # calculate the counts for each 
  dplyr::ungroup() %>%
  tidyr::spread(key = sdt, value = N,fill = 0) %>%                           # long-to-wide format
  dplyr::mutate(hitR = hit/(hit + miss),                                     # hit rate
                FAR  = FA/(FA+CR)) %>%                                       # fa rate
  dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%        # if FA rate is 0, standardize it
  dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
  dplyr::select(Subject, Age,Sex,Identity,Morality, dprime)                  # select relevant columns

### Long to wide
df5b_d2.V.dprime_w <- df5b_d2.V.dprime_l %>%
  tidyr::unite(col = "Cond",c("Identity", "Morality"),sep = "_", remove = T) %>%  # combine two factors to condition
  tidyr::spread(key = Cond, value = dprime) %>%                                   # long to wide
  dplyr::rename_at(vars(-Subject,-Age,-Sex),function(x) paste0("d_",x))           # add prefix to certain conditions

### RT ####
df5b_d2.V.RT  <- df5b_d2.V %>%
  dplyr::filter(ACC ==1)  # exclued rt data less than 200 ms, and inaccurate data

df5b_d2.V.RT.subj_l <- summarySEwithin(df5b_d2.V.RT,measurevar = 'RT', withinvar = c('Subject','Match',"Identity",'Morality'), idvar = 'Subject',na.rm = TRUE)

## long to wide
#df5b_d1.V.RT.subj_w <- dcast(df5b_d1.V.RT.subj_l, Subject ~ Match + Identity + Morality ,value.var = "RT") 

# rename the columns of RT data
#colnames(df5b_d1.V.RT.subj_w)[2:13] <- paste("RT", colnames(df5b_d1.V.RT.subj_w[,2:13]), sep = "_")

### Long to wide
df5b_d2.V.RT.subj_w <- df5b_d2.V.RT.subj_l %>%
  tidyr::unite(col = "Cond",c("Match","Identity", "Morality"),
               sep = "_", remove = T) %>%                                # combine two factors to condition
  dplyr::select(Subject,Cond,RT) %>%                                     # only select needed columns
  tidyr::spread(key = Cond, value = RT) %>%                              # long to wide
  dplyr::rename_at(vars(-Subject),function(x) paste0("RT_",x))           # add prefix to certain conditions

# merge the dprime and RT data and save
df5b_d2.V.sum_w <- merge(df5b_d2.acc_w,df5b_d2.V.dprime_w,by = "Subject")
df5b_d2.V.sum_w <- merge(df5b_d2.V.sum_w,df5b_d2.V.RT.subj_w,by = 'Subject')
df5b_d2.V.sum_w <- df5b_d2.V.sum_w[,c(1,14,15,2:13,16:33)]

df5b_d2.v.sum_rt_acc_l <- merge(df5b_d2.acc_l,df5b_d2.V.RT.subj_l,by = c("Subject","Match","Identity","Morality"))
df5b_d2.v.sum_rt_acc_l <- df5b_d2.v.sum_rt_acc_l[order(df5b_d2.v.sum_rt_acc_l$Subject),]
df5b_d2.v.sum_rt_acc_l <- df5b_d2.v.sum_rt_acc_l[,c("Subject","Match","Identity","Morality","N.x","countN","ACC","RT")]
colnames(df5b_d2.v.sum_rt_acc_l) <- c("Subject","Match","Identity","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df5b_d2.V.sum_w,'exp5b_d2_behav_wide.csv',row.names = F)

# write the long-format data
write.csv(df5b_d2.v.sum_rt_acc_l,'exp5b_d2_rt_acc_long.csv',row.names = F)
write.csv(df5b_d2.V.dprime_l,'exp5b_d2_dprime_long.csv',row.names = F)


## plot the data
rtdata <- subset(df5b_d2.v.sum_rt_acc_l,Match == "match")
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp5b_d2', df5b_d2.V.dprime_l,rtdata)
