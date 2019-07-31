### about ####
# this code is to Preprocess all the data for exp 5a (ERP exp 1)
# data were collected in 2014,
# Change the levels of moral valence to "good", "bad", "neutral". 


## initializing #### 
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
source('Initial_exp5a.r')

## load data ####
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
resDir = curDir

df5a <- read.csv("rawdata_erp_exp5a_2014.csv",header = TRUE, sep = ",",
                 stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
        dplyr::filter(!is.na(BlockList.Sample)) %>%                                                   # select only form exp
        dplyr::rename(Subject = ï..Subject, ACC = Target.ACC, CRESP = Target.CRESP,                   # rename the columns
                      RESP = Target.RESP, RT = Target.RT, Match = YesNoResp, Morality = Shape) %>%    #
        dplyr::mutate(Morality = ifelse(Morality == "Normal", "Neutral", Morality),                   # re-code the data
                      Match = ifelse(Match == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age))
        
length(unique(df5a$Subject))

## record from the meta-data:
#  sub 23: added one more block
#  sub 29: added one more block

## Basic information of the data ####
df5a.T.basic     <- df5a %>%
  dplyr::select(Subject, Age, Handedness, Sex) %>%
  dplyr::distinct(Subject, Age, Sex) %>%
  dplyr::summarise(N = length(Subject),               # N   = 15 
                   N.f = sum(Sex == 'female'),        # N.f = 8
                   N.m = sum(Sex == 'male'),          # N.m = 17
                   MeanAge = round(mean(Age,na.rm=TRUE),2),      # age.m = 22.72
                   SDAge   = round(sd(Age,na.rm=TRUE),2))        # age.s = 2.84

# number of participant who didn't finished the experiment
#nQuit <- length(unique(df5a.P$Subject)) - length(unique(df5a.T$Subject))

# exclude the correct response with less than 200 ms reaction time
df5a.excld.trial   <- df5a[df5a$RT <= 200 & df5a$ACC == 1,]
df5a.excld.trial.r <- nrow(df5a.excld.trial)/nrow(df5a)  # ratio of excluded trials in all trials: 0.00066

# caculate the overall accuracy for each subject
df5a.acc.g <-  df5a %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%
  dplyr::ungroup()


# exlucde the participants with less than 60% overall accuracy
df5a.excld.sub <- df5a.acc.g$Subject[df5a.acc.g$ACC < 0.6]      # < 60% accuracy, Zero

df5a.valid     <- df5a[!(df5a$Subject %in% df5a.excld.sub),]    # exclude the invalid subjects
df5a.V         <- df5a.valid[!(df5a.valid$RT <= 200  & df5a.valid$ACC == 1),]

# make sure that the number is correct (if neccessary)
#length(unique(df5a.valid$Subject)) + length(df5a.excld.sub) == length(unique(df5a$Subject))


## calculate the Accuracy ####
df5a.acc <-  df5a %>%
  dplyr::group_by(Subject,Match, Morality) %>%
  dplyr::summarise(N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC)) %>%
  dplyr::ungroup()

df5a.acc_w <- dcast(df5a.acc, Subject ~ Match + Morality,value.var = "ACC")

# rename the column number
colnames(df5a.acc_w)[2:7] <- paste("ACC", colnames(df5a.acc_w[,2:7]), sep = "_")
        
## d prime ####

# one problem: how to code the trials without response??

df5a.V$sdt <- NA
for (i in 1:nrow(df5a.V)){
        if (df5a.V$ACC[i] == 1 & df5a.V$Match[i] == "Match"){
                df5a.V$sdt[i] <- "hit"
        } else if (df5a.V$ACC[i] == 1 & df5a.V$Match[i] == "Mismatch"){
                df5a.V$sdt[i] <- "CR"
        } else if (df5a.V$ACC[i] == 0 & df5a.V$Match[i] == "Match"){
                df5a.V$sdt[i] <- "miss"
        } else if (df5a.V$ACC[i] == 0 & df5a.V$Match[i] == "Mismatch"){
                df5a.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df5a.V.SDT <-  ddply(df5a.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))

# long format to wide to calculate the d prime
df5a.V.SDT_w  <- dcast(df5a.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")

# if fa or miss is NA, set it to zero
df5a.V.SDT_w$miss[is.na(df5a.V.SDT_w$miss)] <- 0
df5a.V.SDT_w$FA[is.na(df5a.V.SDT_w$FA)] <- 0
df5a.V.SDT_w$hitR <- df5a.V.SDT_w$hit/(df5a.V.SDT_w$hit + df5a.V.SDT_w$miss)
df5a.V.SDT_w$faR <- df5a.V.SDT_w$FA/(df5a.V.SDT_w$FA + df5a.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df5a.V.SDT_w)){
        if (df5a.V.SDT_w$hitR[i] == 1){
                df5a.V.SDT_w$hitR[i] <- 1 - 1/(2*(df5a.V.SDT_w$hit[i] + df5a.V.SDT_w$miss[i]))
        }
}

for (i in 1:nrow(df5a.V.SDT_w)){
        if (df5a.V.SDT_w$faR[i] == 0){
                df5a.V.SDT_w$faR[i] <- 1/(2*(df5a.V.SDT_w$FA[i] + df5a.V.SDT_w$CR[i]))
        }
}

# calculate the d prime for each condition
df5a.V.SDT_w$dprime <- mapply(dprime,df5a.V.SDT_w$hitR,df5a.V.SDT_w$faR)
df5a.V.dprime_l     <- df5a.V.SDT_w[,c("Subject", "Age","Sex","Morality","dprime")]

# change dprime data from long format to wide
df5a.V.dprime_w     <- dcast(df5a.V.dprime_l, Subject + Sex + Age ~ Morality ,value.var = "dprime")

# rename the column number
colnames(df5a.V.dprime_w)[4:6] <- paste("d", colnames(df5a.V.dprime_w[,4:6]), sep = "_")

## RT ####
df5a.V.RT      <- df5a.V[df5a.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df5a.V.RT.subj <- summarySEwithin(df5a.V.RT,measurevar = 'RT', withinvar = c('Subject','Match','Morality'), idvar = 'Subject',na.rm = TRUE)

## long to wide
df5a.V.RT.subj_w <- dcast(df5a.V.RT.subj, Subject ~ Match + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df5a.V.RT.subj_w)[2:7] <- paste("RT", colnames(df5a.V.RT.subj_w[,2:7]), sep = "_")

# merge the dprime and RT data and save
df5a.V.sum_w <- merge(df5a.acc_w,df5a.V.dprime_w,by = "Subject")
df5a.V.sum_w <- merge(df5a.V.sum_w,df5a.V.RT.subj_w,by = 'Subject')
# order the columns
df5a.V.sum_w <- df5a.V.sum_w[,c("Subject", "Sex","Age", "ACC_Match_Good", "ACC_Match_Neutral", "ACC_Match_Bad", "ACC_Mismatch_Good",
                                "ACC_Mismatch_Neutral", "ACC_Mismatch_Bad", "d_Good", "d_Neutral", "d_Bad", "RT_Match_Good",
                                "RT_Match_Neutral", "RT_Match_Bad", "RT_Mismatch_Good", "RT_Mismatch_Neutral","RT_Mismatch_Bad")]

df5a.v.sum_rt_acc_l <- merge(df5a.acc,df5a.V.RT.subj,by = c("Subject","Match","Morality"))
df5a.v.sum_rt_acc_l <- df5a.v.sum_rt_acc_l[order(df5a.v.sum_rt_acc_l$Subject),]
df5a.v.sum_rt_acc_l <- df5a.v.sum_rt_acc_l[,c("Subject","Match","Morality","N.x","countN","ACC","RT")]
colnames(df5a.v.sum_rt_acc_l) <- c("Subject","Match","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df5a.V.sum_w,'exp5a_behav_wide.csv',row.names = F)
write.csv(df5a.v.sum_rt_acc_l,'exp5a_rt_acc_long.csv',row.names = F)
write.csv(df5a.V.dprime_l,'exp5a_dprime_long.csv',row.names = F)


## plot the data
rtdata <- subset(df5a.v.sum_rt_acc_l,Match == "Match")
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp5a', df5a.V.dprime_l,rtdata)
