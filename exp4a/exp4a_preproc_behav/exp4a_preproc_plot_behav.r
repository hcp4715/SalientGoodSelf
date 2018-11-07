## This code is for preparing data of exp4a;
# specifically, it clude three parts
# First, clean the data, and record the basic information of participants
# Second, get the d prime, mean ACC and mean RT of this experiment, and save the in csv file;
#      because we will have a meta-analysis for the effect of moralit, the data the combine self and other condition was also saved.
# Third, plot the results (d prime and RT)

## initializing
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
source('Initial_exp4a.r')

curDir = dirname(rstudioapi::getSourceEditorContext()$path)
resDir = "D:/HCP_cloud/Exps/P1_Pos_Self/Exp_Behav_Moral_Asso/Results_exp1_5/Data_Analysis/exp4a"


## load data and edite data
df4a_1 <- read.csv("rawdata_behav_exp4a_2015.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a_2 <- read.csv("rawdata_behav_exp4a_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
# rename column of later data to keep consistency
colnames(df4a_1)[colnames(df4a_1) == 'morality'] <- 'Morality'
colnames(df4a_1)[colnames(df4a_1) == 'self']     <- 'Identity'

# combine the data
df4a_1$Location <- "Tsinghua"
df4a_2$Location <- "Wenzhou"
df4a <- rbind(df4a_1,df4a_2)

# rename colnames
colnames(df4a)[colnames(df4a)=="Target.ACC"] <- "ACC"
colnames(df4a)[colnames(df4a)=="Target.RT"]  <- "RT"
colnames(df4a)[colnames(df4a)=="YesNoResp"]  <- "Matchness"

# renames independent variables
#df4a$Morality[df4a$Morality == "Good"]    <- "Moral"
df4a$Morality[df4a$Morality == "Normal"]  <- "Neutral"
#df4a$Morality[df4a$Morality == "Bad"]     <- "Immoral"
df4a$Matchness[df4a$Matchness == "Yes"]   <- "Match"
df4a$Matchness[df4a$Matchness == "No"]    <- "Mismatch"
df4a$Identity[df4a$Identity == 'self']    <- 'Self'
df4a$Identity[df4a$Identity == 'other']   <- 'Other'
df4a$Morality  <- factor(df4a$Morality, levels=c("Good", "Neutral","Bad")) # make the variables in a specified order
df4a$Identity  <- factor(df4a$Identity, levels=c("Self", "Other"))
df4a$Matchness  <- factor(df4a$Matchness, levels=c("Match", "Mismatch"))
# there recode are important for real trials (not for practice trials)
#df4a$Morality[df4a$Shape == 'Goodself' | df4a$Shape == 'Goodother']    <- "moral"
#df4a$Morality[df4a$Shape == 'Normalself'| df4a$Shape == 'Normalother'] <- "Neutral"
#df4a$Morality[df4a$Shape == 'Badself' | df4a$Shape == 'Badother']      <- "Immoral"
#df4a$Identity[df4a$Shape == 'Goodself' | df4a$Shape == 'Badself' | df4a$Shape == 'Normalself']    <- "self"
#df4a$Identity[df4a$Shape == 'Goodother'| df4a$Shape == 'Badother'| df4a$Shape == 'Normalother']   <- "other"

#df4a <- df4a[,-which(names(df4a) %in% "Shape")]  # delete column "shape"

df4a.P <- df4a[is.na(df4a$BlockList.Sample),]            # data from practice
df4a.T <- df4a[!(is.na(df4a$BlockList.Sample)),]   # data from test

# number of participant who didn't finished the experiment
nQuit <- length(unique(df4a.P$Subject)) - length(unique(df4a.T$Subject))

# trials that should be excluded from analysis because of less than 200 ms
# note that this should be reconsidered when doinng DDM analysis
# and also the trials without response

excld.trials <- df4a.T[df4a.T$RT <= 200 & df4a.T$ACC == 1,] # correct response with less than 200ms
ratio.excld.trials <- nrow(excld.trials)/nrow(df4a.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df4a.acc.g <-  ddply(df4a.T,.(Subject), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))
excld.sub <- df4a.acc.g$Subject[df4a.acc.g$ACC < 0.6]
df4a.valid <- df4a.T[!(df4a.T$Subject %in% excld.sub),] # exclude the invalid subjects
length(unique(df4a.valid$Subject)) + length(excld.sub) + nQuit == length(unique(df4a$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% excld.sub),]
excld.trials2 <- df4a.valid[df4a.valid$RT <= 200 & df4a.valid$ACC == 1,]
df4a.V <- df4a.valid[!(df4a.valid$RT <= 200 & df4a.valid$ACC == 1),]  

## Basic information of the data ####
df4a.T.basic <- df4a[!duplicated(df4a$Subject), 1:4]
#df4a.T.basic <- df4a.T.basic[,which(names(df4a.T.basic) %in% c("Subject","Age","Handness","Sex"))]
num.subj <- nrow(df4a.T.basic)                      # 64
numT.female <- sum(df4a.T.basic$Sex == 'female');   # 37
numT.male <- sum(df4a.T.basic$Sex == 'male');       # 27
minage <- min(df4a.T.basic$Age)                     # 17
ageT.mean <- round(mean(df4a.T.basic$Age),2);       # 19.7
ageT.std <- round(sd(df4a.T.basic$Age),2);          # 1.22
num.excld.sub <- length(unique(excld.sub))          # 5, Thu,3; WZU, 2
df4a.V.basic <- df4a.V[!duplicated(df4a.V$Subject), 1:4]
numV.female <- sum(df4a.V.basic$Sex == 'female');   # 33
numV.male <- sum(df4a.V.basic$Sex == 'male');       # 26
ageV.mean <- round(mean(df4a.V.basic$Age),2);       # 19.78
ageV.std <- round(sd(df4a.V.basic$Age),2);          # 1.2
ratio.excld.trials2 <- nrow(excld.trials2)/nrow(df4a.valid)  # 0.24%

## ACC data ####
df4a.acc <-  ddply(df4a.V,.(Subject,Matchness, Morality,Identity), summarise,
                  N = length(ACC),
                  countN = sum(ACC),
                  ACC = sum(ACC)/length(ACC))

# ACC for the data without considering Idenity
df4a.acc_moral <-  ddply(df4a.V,.(Subject,Matchness, Morality), summarise,
                        N = length(ACC),
                        countN = sum(ACC),
                        ACC = sum(ACC)/length(ACC))


# to wide-formate
df4a.acc_w <- dcast(df4a.acc, Subject ~ Matchness + Morality + Identity,value.var = "ACC")
df4a.acc_moral_w <- dcast(df4a.acc_moral, Subject ~ Matchness + Morality,value.var = "ACC")

# rename the column number
colnames(df4a.acc_w)[2:13] <- paste("ACC", colnames(df4a.acc_w[,2:13]), sep = "_")
colnames(df4a.acc_moral_w)[2:7] <- paste("ACC", colnames(df4a.acc_moral_w[,2:7]), sep = "_")

#### d prime ####
df4a.V$sdt <- NA

for (i in 1:nrow(df4a.V)){
  if (df4a.V$ACC[i] == 1 & (df4a.V$Matchness[i] == "Match")){
    df4a.V$sdt[i] <- "hit"
  } else if (df4a.V$ACC[i] == 1 & (df4a.V$Matchness[i] == "Mismatch")){
    df4a.V$sdt[i] <- "CR"
  } else if (df4a.V$ACC[i] != 1 & (df4a.V$Matchness[i] == "Match")){
    df4a.V$sdt[i] <- "miss"
  }
  else if (df4a.V$ACC[i] != 1 & (df4a.V$Matchness[i] == "Mismatch")){
    df4a.V$sdt[i] <- "FA"
  }
}

# calculate the number of each for each condition
df4a.V.dprime <-  ddply(df4a.V,.(Location,Subject,Age,Sex,Morality, Identity,sdt), summarise, N = length(sdt))

# long format to wide
df4a.V.dprime_v <- dcast(df4a.V.dprime, Location + Subject + Age + Sex + Morality + Identity ~ sdt,value.var = "N")
df4a.V.dprime_v$miss[is.na(df4a.V.dprime_v$miss)] <- 0
df4a.V.dprime_v$FA[is.na(df4a.V.dprime_v$FA)] <- 0
df4a.V.dprime_v$hitR <- df4a.V.dprime_v$hit/(df4a.V.dprime_v$hit + df4a.V.dprime_v$miss)
df4a.V.dprime_v$faR <- df4a.V.dprime_v$FA/(df4a.V.dprime_v$FA + df4a.V.dprime_v$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df4a.V.dprime_v)){
  if (df4a.V.dprime_v$hitR[i] == 1){
    df4a.V.dprime_v$hitR[i] <- 1 - 1/(2*(df4a.V.dprime_v$hit[i] + df4a.V.dprime_v$miss[i]))
  }
}

for (i in 1:nrow(df4a.V.dprime_v)){
  if (df4a.V.dprime_v$faR[i] == 0){
    df4a.V.dprime_v$faR[i] <- 1/(2*(df4a.V.dprime_v$FA[i] + df4a.V.dprime_v$CR[i]))
  }
}

# calculate the d prime for each condition
df4a.V.dprime_v$dprime <- mapply(dprime,df4a.V.dprime_v$hitR,df4a.V.dprime_v$faR)

# change dprime data from long format to wide
df4a.V.dprime_w <- dcast(df4a.V.dprime_v, Location + Subject + Age + Sex ~ Identity + Morality ,value.var = "dprime")
df4a.V.dprime_l <- df4a.V.dprime_v[,c('Location',"Subject", "Age","Sex","Morality",'Identity',"dprime")]

####### calculate the number of each for each condition ########
# this is the same procedure to calculate the d prime, only consider the moral valence 
df4a.V.d_moral_calc <-  ddply(df4a.V,.(Location,Subject,Age, Sex,Morality,sdt), summarise, N = length(sdt))

# long format to wide
df4a.V.d_moral_calc <- dcast(df4a.V.d_moral_calc, Location + Subject + Age + Sex + Morality ~ sdt,value.var = "N")
df4a.V.d_moral_calc$miss[is.na(df4a.V.d_moral_calc$miss)] <- 0
df4a.V.d_moral_calc$FA[is.na(df4a.V.d_moral_calc$FA)] <- 0
df4a.V.d_moral_calc$hitR <- df4a.V.d_moral_calc$hit/(df4a.V.d_moral_calc$hit + df4a.V.d_moral_calc$miss)
df4a.V.d_moral_calc$faR <- df4a.V.d_moral_calc$FA/(df4a.V.d_moral_calc$FA + df4a.V.d_moral_calc$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df4a.V.d_moral_calc)){
  if (df4a.V.d_moral_calc$hitR[i] == 1){
    df4a.V.d_moral_calc$hitR[i] <- 1 - 1/(2*(df4a.V.d_moral_calc$hit[i] + df4a.V.d_moral_calc$miss[i]))
  }
}

for (i in 1:nrow(df4a.V.d_moral_calc)){
  if (df4a.V.d_moral_calc$faR[i] == 0){
    df4a.V.d_moral_calc$faR[i] <- 1/(2*(df4a.V.d_moral_calc$FA[i] + df4a.V.d_moral_calc$CR[i]))
  }
}

# calculate the d prime for each condition
df4a.V.d_moral_calc$dprime <- mapply(dprime,df4a.V.d_moral_calc$hitR,df4a.V.d_moral_calc$faR)
df4a.V.d_moral_w <- dcast(df4a.V.d_moral_calc, Location + Subject + Age + Sex ~ Morality ,value.var = "dprime")
df4a.V.d_moral_l     <- df4a.V.d_moral_calc[,c("Location","Subject", "Age","Sex","Morality","dprime")]

##################

# rename the column number
colnames(df4a.V.dprime_w)[5:10] <- paste("d", colnames(df4a.V.dprime_w[,5:10]), sep = "_")
colnames(df4a.V.d_moral_w)[5:7] <- paste("d", colnames(df4a.V.d_moral_w[,5:7]), sep = "_")

# check the effect of location for d prime
# df4a.d_anova_site <- ezANOVA(df4a.V.dprime_l,dv = dprime, wid = Subject, within=.(Morality,Identity), between = .(Location), type=3)


# ggsave('dprime_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot

##  RT ####
## doing the analysis for RT ####
df4a.V.RT <- df4a.V[df4a.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df4a.V.RT.subj <- summarySEwithin(df4a.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),idvar = 'Subject', na.rm = TRUE)
df4a.V.RT.subj_moral <- summarySEwithin(df4a.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'),idvar = 'Subject', na.rm = TRUE)

# long to wide
df4a.V.RT.subj_w <- dcast(df4a.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 
df4a.V.RT.subj_moral_w <- dcast(df4a.V.RT.subj_moral, Subject ~ Matchness + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df4a.V.RT.subj_w)[2:13] <- paste("RT", colnames(df4a.V.RT.subj_w[,2:13]), sep = "_")
colnames(df4a.V.RT.subj_moral_w)[2:7] <- paste("RT", colnames(df4a.V.RT.subj_moral_w[,2:7]), sep = "_")

# merge files####
# the dprime and RT data and save
df4a.V.sum_w <- merge(df4a.acc_w,df4a.V.dprime_w,by = "Subject")
df4a.V.sum_w <- merge(df4a.V.sum_w,df4a.V.RT.subj_w,by = 'Subject')
df4a.V.sum_moral_w <- merge(df4a.acc_moral_w,df4a.V.d_moral_w,by = "Subject")
df4a.V.sum_moral_w <- merge(df4a.V.sum_moral_w,df4a.V.RT.subj_moral_w,by = 'Subject')

# order the columns
df4a.V.sum_w <- df4a.V.sum_w[,c(colnames(df4a.V.sum_w)[c(1,14:16,2:13,17:34)])]
df4a.V.sum_moral_w <- df4a.V.sum_moral_w[,c(colnames(df4a.V.sum_moral_w)[c(1,8:10,2:7,11:19)])]

df4a.v.sum_rt_acc_l <- merge(df4a.acc,df4a.V.RT.subj,by = c("Subject","Matchness","Morality",'Identity'))
df4a.v.sum_rt_acc_l <- df4a.v.sum_rt_acc_l[order(df4a.v.sum_rt_acc_l$Subject),]
df4a.v.sum_rt_acc_l <- df4a.v.sum_rt_acc_l[,c("Subject","Matchness","Morality",'Identity',"N.x","countN","ACC","RT")]
colnames(df4a.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality",'Identity',"Ntrials","corrtrials","ACC","RT")

df4a.v.sum_rt_acc_moral_l <- merge(df4a.acc_moral,df4a.V.RT.subj_moral,by = c("Subject","Matchness","Morality"))
df4a.v.sum_rt_acc_moral_l <- df4a.v.sum_rt_acc_moral_l[order(df4a.v.sum_rt_acc_moral_l$Subject),]
df4a.v.sum_rt_acc_moral_l <- df4a.v.sum_rt_acc_moral_l[,c("Subject","Matchness","Morality","N.x","countN","ACC","RT")]
colnames(df4a.v.sum_rt_acc_moral_l) <- c("Subject","Matchness","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df4a.V.sum_w,'exp4a_behav_wide.csv',row.names = F)
write.csv(df4a.v.sum_rt_acc_l,'exp4a_rt_acc_long.csv',row.names = F)
write.csv(df4a.V.dprime_l,'exp4a_dprime_long.csv',row.names = F)

write.csv(df4a.V.sum_moral_w,'exp4a_behav_moral_wide.csv',row.names = F)
write.csv(df4a.v.sum_rt_acc_moral_l,'exp4a_rt_acc_moral_long.csv',row.names = F)
write.csv(df4a.V.d_moral_l,'exp4a_dprime_moral_long.csv',row.names = F)


### plot ####
rtdata <- subset(df4a.v.sum_rt_acc_l,Matchness == "Match")
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp4a', df4a.V.dprime_l,rtdata)
