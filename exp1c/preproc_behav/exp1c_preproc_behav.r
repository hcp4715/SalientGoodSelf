## this code is to preprocess the data for exp1b, 
## included these data were colleted at Wenzhou U in 201704

## initializing  #### 
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
source('Initial_exp1c.r')
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
resDir = "D:/HCP_cloud/Exps/P1_Pos_Self/Exp_Behav_Moral_Asso/Results_exp1_5/Data_Analysis/exp1c"

## load data and edite data
df1c_1 <- read.csv("rawdata_behav_exp1c_2014.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1c_1$Subject))
#df1c_2 <- read.csv("rawdata_behav_exp1b_201705.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
#length(unique(df1c_2$Subject))
#df1c   <- rbind(df1c_1,df1c_2)
#rm(df1c_1,df1c_2)

# preprocessing the rawdata
df1c <- df1c_1 %>%
  dplyr::filter(!is.na(TrialList1.Sample)) %>%                    # select formal experimental triasl
  dplyr::select(ï..Subject,Age,Sex,Shape,YesNoResp,CorrectAnswer, # select necessary columns
                 Target1.ACC,Target1.RESP, Target1.RT) %>%
  dplyr::rename(Subject = ï..Subject, ACC = Target1.ACC,          # rename columns
                RT = Target1.RT, Matchness = YesNoResp, Morality = Shape)
  
# renames independent variables (good, neutral, bad)
# df1c$Morality[df1c$Morality == "Good"]   <- "Moral"
df1c$Morality[df1c$Morality == "Normal"] <- "Neutral"
# df1c$Morality[df1c$Morality == "Bad"]    <- "Immoral"
df1c$Morality <- factor(df1c$Morality, levels=c("Good", "Neutral","Bad")) # make the variables in a specified order

df1c$Matchness[df1c$Matchness == "Yes"] <- "Match"
df1c$Matchness[df1c$Matchness == "No"] <- "Mismatch"
df1c$Matchness <- factor(df1c$Matchness, levels=c("Match", "Mismatch"))

## Basic information of the data ####
df1c.T.basic    <- df1c[!duplicated(df1c$Subject), 1:4]
df1c.num.subj    <- nrow(df1c.T.basic)
df1c.numT.female <- sum(df1c.T.basic$Sex == 'female');
df1c.numT.male   <- sum(df1c.T.basic$Sex == 'male');
min(df1c.T.basic$Age) == 0
df1c.ageT.mean   <- round(mean(df1c.T.basic$Age),2);
df1c.ageT.std    <- round(sd(df1c.T.basic$Age),2);

df1c.P <- df1c[is.na(df1c$BlockList.Sample),]            # data from practice
df1c.T <- df1c[complete.cases(df1c$BlockList.Sample),]   # data from test

# exclude the correct trials with less than 200 ms RT
df1c.excld.trials       <- df1c.T[df1c.T$RT <= 200 & df1c.T$ACC == 1,]
df1c.ratio.excld.trials <- nrow(df1c.excld.trials)/nrow(df1c.T) # ratio of excluded trials in all triasl.

# caculate the overall accuracy for each subject
df1c.acc.g    <- ddply(df1c.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
df1c.excld.sub <- df1c.acc.g$Subject[df1c.acc.g$ACC < 0.6]   # 20 participants excluded from analysis, 12 from Tsinghua, 8 from Wenzhou
df1c.valid    <- df1c.T[!(df1c.T$Subject %in% df1c.excld.sub),] # exclude the invalid subjects
length(unique(df1c.valid$Subject)) + length(df1c.excld.sub) == length(unique(df1c$Subject))

# 
df1c.excld.trials2 <- df1c.valid[df1c.valid$RT <= 200 & df1c.valid$ACC == 1,]
df1c.V <- df1c.valid[!(df1c.valid$RT <= 200 & df1c.valid$ACC == 1),]  

## Basic information of the data ####
df1c.num.excld.sub <- length(unique(df1c.excld.sub))
df1c.V.basic <- df1c.V[!duplicated(df1c.V$Subject), 1:4]
df1c.numV.female <- sum(df1c.V.basic$Sex == 'female');
df1c.numV.male <- sum(df1c.V.basic$Sex == 'male');
df1c.ageV.mean <- round(mean(df1c.V.basic$Age),2);
df1c.ageV.std <- round(sd(df1c.V.basic$Age),2);
df1c.ratio.excld.trials2 <- nrow(df1c.excld.trials2)/nrow(df1c.valid)

### ACC ####
df1c.acc  <-  ddply(df1c.V,.(Subject,Matchness, Morality), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))

df1c.acc_w <- dcast(df1c.acc, Subject ~ Matchness + Morality,value.var = "ACC")

# rename the column number
colnames(df1c.acc_w)[2:7] <- paste("ACC", colnames(df1c.acc_w[,2:7]), sep = "_")

# d prime #### 
df1c.V$sdt <- NA
for (i in 1:nrow(df1c.V)){
        if (df1c.V$ACC[i] == 1 & df1c.V$Matchness[i] == "Match"){
                df1c.V$sdt[i] <- "hit"
        } else if (df1c.V$ACC[i] == 1 & df1c.V$Matchness[i] == "Mismatch"){
                df1c.V$sdt[i] <- "CR"
        } else if (df1c.V$ACC[i] == 0 & df1c.V$Matchness[i] == "Match"){
                df1c.V$sdt[i] <- "miss"
        } else if (df1c.V$ACC[i] == 0 & df1c.V$Matchness[i] == "Mismatch"){
                df1c.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df1c.V.SDT <-  ddply(df1c.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))


# long format to wide
df1c.V.SDT_w <- dcast(df1c.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")
df1c.V.SDT_w$miss[is.na(df1c.V.SDT_w$miss)] <- 0
df1c.V.SDT_w$FA[is.na(df1c.V.SDT_w$FA)] <- 0
df1c.V.SDT_w$hitR <- df1c.V.SDT_w$hit/(df1c.V.SDT_w$hit + df1c.V.SDT_w$miss)
df1c.V.SDT_w$faR <- df1c.V.SDT_w$FA/(df1c.V.SDT_w$FA + df1c.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df1c.V.SDT_w)){
        if (df1c.V.SDT_w$hitR[i] == 1){
                df1c.V.SDT_w$hitR[i] <- 1 - 1/(2*(df1c.V.SDT_w$hit[i] + df1c.V.SDT_w$miss[i]))
        }
}

for (i in 1:nrow(df1c.V.SDT_w)){
        if (df1c.V.SDT_w$faR[i] == 0){
                df1c.V.SDT_w$faR[i] <- 1/(2*(df1c.V.SDT_w$FA[i] + df1c.V.SDT_w$CR[i]))
        }
}

# calculate the d prime for each condition
df1c.V.SDT_w$dprime <- mapply(dprime,df1c.V.SDT_w$hitR,df1c.V.SDT_w$faR)
df1c.V.SDT_ww   <- dcast(df1c.V.SDT_w, Subject + Sex + Age ~ Morality ,value.var = "dprime")

df1c.V.SDT_l <- df1c.V.SDT_w[,c(1:4,11)]

# rename the column number
colnames(df1c.V.SDT_ww)[4:6] <- paste("d", colnames(df1c.V.SDT_ww[,4:6]), sep = "_")

## doing the analysis for RT ####
df1c.V.RT <- df1c.V[df1c.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df1c.V.RT.subj <- summarySEwithin(df1c.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'), idvar = 'Subject',na.rm = TRUE)
df1c.V.RT.subj_w <- dcast(df1c.V.RT.subj, Subject ~ Matchness + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df1c.V.RT.subj_w)[2:7] <- paste("RT", colnames(df1c.V.RT.subj_w[,2:7]), sep = "_")

## saving data ####
# merge the dprime and RT data and save
df1c.V.sum_w <- merge(df1c.acc_w,  df1c.V.SDT_ww,by = "Subject")
df1c.V.sum_w <- merge(df1c.V.sum_w,df1c.V.RT.subj_w,by = 'Subject')

# merge the RT and ACC data (long-format)
df1c.v.sum_rt_acc_l <- merge(df1c.acc,df1c.V.RT.subj,by = c("Subject","Matchness","Morality"))
df1c.v.sum_rt_acc_l <- df1c.v.sum_rt_acc_l[order(df1c.v.sum_rt_acc_l$Subject),]
df1c.v.sum_rt_acc_l <- df1c.v.sum_rt_acc_l[,c("Subject","Matchness","Morality","N.x","countN","ACC","RT")]
colnames(df1c.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality","Ntrials","corrtrials","ACC","RT")

# order the columns
df1c.V.sum_w <- df1c.V.sum_w[,c("Subject", "Sex","Age", "ACC_Match_Good", "ACC_Match_Neutral", "ACC_Match_Bad", "ACC_Mismatch_Good",
                                "ACC_Mismatch_Neutral", "ACC_Mismatch_Bad", "d_Good", "d_Neutral", "d_Bad", "RT_Match_Good",
                                "RT_Match_Neutral", "RT_Match_Bad", "RT_Mismatch_Good", "RT_Mismatch_Neutral","RT_Mismatch_Bad")]

# write files
write.csv(df1c.V.sum_w,'exp1b_behav_wide.csv',row.names = F)
write.csv(df1c.V.SDT_l,'exp1b_dprime_long.csv',row.names = F)
write.csv(df1c.v.sum_rt_acc_l,'exp1b_rt_acc_long.csv',row.names = F)

## plot the data
rtdata <- subset(df1c.v.sum_rt_acc_l,Matchness == "Match")
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp1b', df1c.V.SDT_l,rtdata)

