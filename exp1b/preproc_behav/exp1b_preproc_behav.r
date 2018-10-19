## this code is to preprocess the data for exp1b, 
## included these data were colleted at Wenzhou U in 201704

## initializing  #### 
source('Initial_exp1b.r')
curDir = "D:/HCP_cloud/Exps/P1_Pos_Self/Exp_Behav_Moral_Asso/Results_exp1_5/Data_Analysis/exp1b/preproc_behav"
resDir = "D:/HCP_cloud/Exps/P1_Pos_Self/Exp_Behav_Moral_Asso/Results_exp1_5/Data_Analysis/exp1b"


## load data and edite data
df1b_1 <- read.csv("rawdata_behav_exp1b_2014.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1b_1$Subject))
df1b_2 <- read.csv("rawdata_behav_exp1b_201705.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1b_2$Subject))
df1b   <- rbind(df1b_1,df1b_2)
rm(df1b_1,df1b_2)

# rename colnames
colnames(df1b)[colnames(df1b)=="Target.ACC"] <- "ACC"
colnames(df1b)[colnames(df1b)=="Target.RT"]  <- "RT"
colnames(df1b)[colnames(df1b)=="YesNoResp"]  <- "Matchness"
colnames(df1b)[colnames(df1b)=="Shape"]      <- "Morality"

# renames independent variables (good, neutral, bad)
# df1b$Morality[df1b$Morality == "Good"]   <- "Moral"
df1b$Morality[df1b$Morality == "Normal"] <- "Neutral"
# df1b$Morality[df1b$Morality == "Bad"]    <- "Immoral"
df1b$Morality <- factor(df1b$Morality, levels=c("Good", "Neutral","Bad")) # make the variables in a specified order

df1b$Matchness[df1b$Matchness == "Yes"] <- "Match"
df1b$Matchness[df1b$Matchness == "No"] <- "Mismatch"
df1b$Matchness <- factor(df1b$Matchness, levels=c("Match", "Mismatch"))

## Basic information of the data ####
df1b.T.basic    <- df1b[!duplicated(df1b$Subject), 1:4]
df1b.num.subj    <- nrow(df1b.T.basic)
df1b.numT.female <- sum(df1b.T.basic$Sex == 'female');
df1b.numT.male   <- sum(df1b.T.basic$Sex == 'male');
min(df1b.T.basic$Age) == 0
df1b.ageT.mean   <- round(mean(df1b.T.basic$Age),2);
df1b.ageT.std    <- round(sd(df1b.T.basic$Age),2);

df1b.P <- df1b[is.na(df1b$BlockList.Sample),]            # data from practice
df1b.T <- df1b[complete.cases(df1b$BlockList.Sample),]   # data from test

# exclude the correct trials with less than 200 ms RT
df1b.excld.trials       <- df1b.T[df1b.T$RT <= 200 & df1b.T$ACC == 1,]
df1b.ratio.excld.trials <- nrow(df1b.excld.trials)/nrow(df1b.T) # ratio of excluded trials in all triasl.

# caculate the overall accuracy for each subject
df1b.acc.g    <- ddply(df1b.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
df1b.excld.sub <- df1b.acc.g$Subject[df1b.acc.g$ACC < 0.6]   # 20 participants excluded from analysis, 12 from Tsinghua, 8 from Wenzhou
df1b.valid    <- df1b.T[!(df1b.T$Subject %in% df1b.excld.sub),] # exclude the invalid subjects
length(unique(df1b.valid$Subject)) + length(df1b.excld.sub) == length(unique(df1b$Subject))

# 
df1b.excld.trials2 <- df1b.valid[df1b.valid$RT <= 200 & df1b.valid$ACC == 1,]
df1b.V <- df1b.valid[!(df1b.valid$RT <= 200 & df1b.valid$ACC == 1),]  

## Basic information of the data ####
df1b.num.excld.sub <- length(unique(df1b.excld.sub))
df1b.V.basic <- df1b.V[!duplicated(df1b.V$Subject), 1:4]
df1b.numV.female <- sum(df1b.V.basic$Sex == 'female');
df1b.numV.male <- sum(df1b.V.basic$Sex == 'male');
df1b.ageV.mean <- round(mean(df1b.V.basic$Age),2);
df1b.ageV.std <- round(sd(df1b.V.basic$Age),2);
df1b.ratio.excld.trials2 <- nrow(df1b.excld.trials2)/nrow(df1b.valid)

### ACC ####
df1b.acc  <-  ddply(df1b.V,.(Subject,Matchness, Morality), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))

df1b.acc_w <- dcast(df1b.acc, Subject ~ Matchness + Morality,value.var = "ACC")

# rename the column number
colnames(df1b.acc_w)[2:7] <- paste("ACC", colnames(df1b.acc_w[,2:7]), sep = "_")

# d prime #### 
df1b.V$sdt <- NA
for (i in 1:nrow(df1b.V)){
        if (df1b.V$ACC[i] == 1 & df1b.V$Matchness[i] == "Match"){
                df1b.V$sdt[i] <- "hit"
        } else if (df1b.V$ACC[i] == 1 & df1b.V$Matchness[i] == "Mismatch"){
                df1b.V$sdt[i] <- "CR"
        } else if (df1b.V$ACC[i] == 0 & df1b.V$Matchness[i] == "Match"){
                df1b.V$sdt[i] <- "miss"
        } else if (df1b.V$ACC[i] == 0 & df1b.V$Matchness[i] == "Mismatch"){
                df1b.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df1b.V.SDT <-  ddply(df1b.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))


# long format to wide
df1b.V.SDT_w <- dcast(df1b.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")
df1b.V.SDT_w$miss[is.na(df1b.V.SDT_w$miss)] <- 0
df1b.V.SDT_w$FA[is.na(df1b.V.SDT_w$FA)] <- 0
df1b.V.SDT_w$hitR <- df1b.V.SDT_w$hit/(df1b.V.SDT_w$hit + df1b.V.SDT_w$miss)
df1b.V.SDT_w$faR <- df1b.V.SDT_w$FA/(df1b.V.SDT_w$FA + df1b.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df1b.V.SDT_w)){
        if (df1b.V.SDT_w$hitR[i] == 1){
                df1b.V.SDT_w$hitR[i] <- 1 - 1/(2*(df1b.V.SDT_w$hit[i] + df1b.V.SDT_w$miss[i]))
        }
}

for (i in 1:nrow(df1b.V.SDT_w)){
        if (df1b.V.SDT_w$faR[i] == 0){
                df1b.V.SDT_w$faR[i] <- 1/(2*(df1b.V.SDT_w$FA[i] + df1b.V.SDT_w$CR[i]))
        }
}

# calculate the d prime for each condition
df1b.V.SDT_w$dprime <- mapply(dprime,df1b.V.SDT_w$hitR,df1b.V.SDT_w$faR)
df1b.V.SDT_ww   <- dcast(df1b.V.SDT_w, Subject + Sex + Age ~ Morality ,value.var = "dprime")

df1b.V.SDT_l <- df1b.V.SDT_w[,c(1:4,11)]

# rename the column number
colnames(df1b.V.SDT_ww)[4:6] <- paste("d", colnames(df1b.V.SDT_ww[,4:6]), sep = "_")

## doing the analysis for RT ####
df1b.V.RT <- df1b.V[df1b.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df1b.V.RT.subj <- summarySEwithin(df1b.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'), idvar = 'Subject',na.rm = TRUE)
df1b.V.RT.subj_w <- dcast(df1b.V.RT.subj, Subject ~ Matchness + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df1b.V.RT.subj_w)[2:7] <- paste("RT", colnames(df1b.V.RT.subj_w[,2:7]), sep = "_")

## saving data ####
# merge the dprime and RT data and save
df1b.V.sum_w <- merge(df1b.acc_w,  df1b.V.SDT_ww,by = "Subject")
df1b.V.sum_w <- merge(df1b.V.sum_w,df1b.V.RT.subj_w,by = 'Subject')

# merge the RT and ACC data (long-format)
df1b.v.sum_rt_acc_l <- merge(df1b.acc,df1b.V.RT.subj,by = c("Subject","Matchness","Morality"))
df1b.v.sum_rt_acc_l <- df1b.v.sum_rt_acc_l[order(df1b.v.sum_rt_acc_l$Subject),]
df1b.v.sum_rt_acc_l <- df1b.v.sum_rt_acc_l[,c("Subject","Matchness","Morality","N.x","countN","ACC","RT")]
colnames(df1b.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality","Ntrials","corrtrials","ACC","RT")

# order the columns
df1b.V.sum_w <- df1b.V.sum_w[,c("Subject", "Sex","Age", "ACC_Match_Good", "ACC_Match_Neutral", "ACC_Match_Bad", "ACC_Mismatch_Good",
                                "ACC_Mismatch_Neutral", "ACC_Mismatch_Bad", "d_Good", "d_Neutral", "d_Bad", "RT_Match_Good",
                                "RT_Match_Neutral", "RT_Match_Bad", "RT_Mismatch_Good", "RT_Mismatch_Neutral","RT_Mismatch_Bad")]

# write files
write.csv(df1b.V.sum_w,'exp1b_behav_wide.csv',row.names = F)
write.csv(df1b.V.SDT_l,'exp1b_dprime_long.csv',row.names = F)
write.csv(df1b.v.sum_rt_acc_l,'exp1b_rt_acc_long.csv',row.names = F)

## plot the data
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp1b', df1b.V.SDT_l,df1b.v.sum_rt_acc_l)

