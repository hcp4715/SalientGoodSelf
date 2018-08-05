## This code is for preparing data of exp3;
# specifically, it clude three parts
# First, clean the data, and record the basic information of participants
# Second, get the d prime, mean ACC and mean RT of this experiment, and save the in csv file;
#      because we will have a meta-analysis for the effect of moralit, the data the combine self and other condition was also saved.
# Third, plot the results (d prime and RT)

## initializing ####
source('Initial.r')

## load data and edite data
df3 <- read.csv("rawdata_behav_exp3_2018.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

# rename colnames
colnames(df3)[colnames(df3)=="Target.ACC"] <- "ACC"
colnames(df3)[colnames(df3)=="Target.RT"]  <- "RT"
colnames(df3)[colnames(df3)=="YesNoResp"]  <- "Matchness"
colnames(df3)[colnames(df3)=="self"]       <- "Identity"
colnames(df3)[colnames(df3)=="Morality"]   <- "Morality"

# renames independent variables
df3$Morality[df3$morality == "moral"]   <- "Moral"
df3$Morality[df3$morality == "normal"]  <- "Neutral"
df3$Morality[df3$morality == "immoral"] <- "Immoral"
df3$Matchness[df3$Matchness == "Yes"]   <- "Match"
df3$Matchness[df3$Matchness == "No"]    <- "Mismatch"

# there recode are important for test trials (not for practice trials)
df3$Morality[df3$Shape == 'Goodself' | df3$Shape == 'Goodother']    <- "Moral"
df3$Morality[df3$Shape == 'Normalself'| df3$Shape == 'Normalother'] <- "Neutral"
df3$Morality[df3$Shape == 'Badself' | df3$Shape == 'Badother']      <- "Immoral"
df3$Identity[df3$Shape == 'Goodself' | df3$Shape == 'Badself' | df3$Shape == 'Normalself']    <- "Self"
df3$Identity[df3$Shape == 'Goodother'| df3$Shape == 'Badother'| df3$Shape == 'Normalother']   <- "Other"
df3$Morality  <- factor(df3$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df3$Identity  <- factor(df3$Identity, levels=c("Self", "Other"))

df3 <- df3[,-which(names(df3) %in% "Shape")]  # delete column "shape"

df3.P <- df3[is.na(df3$BlockList.Sample),]       # data from practice
df3.T <- df3[!(is.na(df3$BlockList.Sample)),]    # data from test

# number of participant who didn't finished the experiment
nQuit <- length(unique(df3.P$Subject)) - length(unique(df3.T$Subject))

# trials that should be excluded from analysis because of less than 200 ms
# note that this should be reconsidered when doinng DDM analysis
# and also the trials without response
excld.trials <- df3.T[df3.T$RT <= 200 & df3.T$ACC == 1,]
ratio.excld.trials <- nrow(excld.trials)/nrow(df3.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df3.acc.g <-  ddply(df3.T,.(Subject), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))
excld.sub <- df3.acc.g$Subject[df3.acc.g$ACC < 0.6]  # find participants whose overall ACC is less than 60%
df3.valid <- df3.T[!(df3.T$Subject %in% excld.sub),] # exclude the invalid participants


# Check the accuracy of the participants number
length(unique(df3.valid$Subject)) + length(excld.sub) + nQuit == length(unique(df3$Subject))

# exclude the invalid trials again for data from valid participants
excld.trials2 <- df3.valid[df3.valid$RT <= 200 & df3.valid$ACC == 1,]
df3.V         <- df3.valid[!(df3.valid$RT <= 200 & df3.valid$ACC == 1),]  

## Basic information of the data ####
df3.T.basic <- df3[!duplicated(df3$Subject), 1:4]  
num.subj    <- nrow(df3.T.basic)                         # total number of participants
numT.female <- sum(df3.T.basic$Sex == 'female');         # number of female
numT.male   <- sum(df3.T.basic$Sex == 'male');           # number of male
mingAge     <- min(df3.T.basic$Age)                      # minimum age (detect or other error input)
ageT.mean   <- round(mean(df3.T.basic$Age),2);           # mean age of all participants
ageT.std    <- round(sd(df3.T.basic$Age),2);             # standard deviation of age from all
num.excld.sub <- length(unique(excld.sub))               # number of participant excluded
df3.V.basic   <- df3.V[!duplicated(df3.V$Subject), 1:4]  # Get basic information for valid data
numV.female   <- sum(df3.V.basic$Sex == 'female');       # number of female
numV.male     <- sum(df3.V.basic$Sex == 'male');         # number of male
ageV.mean     <- round(mean(df3.V.basic$Age),2);         # mean age
ageV.std      <- round(sd(df3.V.basic$Age),2);           # SD of age
ratio.excld.trials2 <- nrow(excld.trials2)/nrow(df3.valid) # excluded trials

## ACC data ####
# ACC for each condition
df3.acc <-  ddply(df3.V,.(Subject,Matchness, Morality,Identity), summarise,
                  N = length(ACC),
                  countN = sum(ACC),
                  ACC = sum(ACC)/length(ACC))

# ACC for the data without considering Idenity
df3.acc_moral <-  ddply(df3.V,.(Subject,Matchness, Morality), summarise,
                  N = length(ACC),
                  countN = sum(ACC),
                  ACC = sum(ACC)/length(ACC))


# transfer to wide-formate for further analysis in JASP
df3.acc_w <- dcast(df3.acc, Subject ~ Matchness + Morality + Identity,value.var = "ACC")
df3.acc_moral_w <- dcast(df3.acc_moral, Subject ~ Matchness + Morality,value.var = "ACC")

# add 'ACC' to column names
colnames(df3.acc_w)[2:13] <- paste("ACC", colnames(df3.acc_w[,2:13]), sep = "_")
colnames(df3.acc_moral_w)[2:7] <- paste("ACC", colnames(df3.acc_moral_w[,2:7]), sep = "_")

## d prime ####

df3.V$sdt <- NA  # new column called sdt, means signal detection theory

# classify each trial to one category.
for (i in 1:nrow(df3.V)){
  if (df3.V$ACC[i] == 1 & (df3.V$Matchness[i] == "Match")){           # matched trials and correct as hit
    df3.V$sdt[i] <- "hit"
  } else if (df3.V$ACC[i] == 1 & (df3.V$Matchness[i] == "Mismatch")){ # mismatched trials and correct as correct rejection
    df3.V$sdt[i] <- "CR"
  } else if (df3.V$ACC[i] != 1 & (df3.V$Matchness[i] == "Match")){    # mathced trials and wrong at miss
    df3.V$sdt[i] <- "miss"
  } else if (df3.V$ACC[i] != 1 & (df3.V$Matchness[i] == "Mismatch")){ # mismatched trials and wrong as false alarms  
    df3.V$sdt[i] <- "FA"
  }
}

# calculate the number of each for each condition
df3.V.SDT <-  ddply(df3.V,.(Subject,Age, Sex,Morality, Identity,sdt), summarise, N = length(sdt))


# long format to wide
df3.V.dprime_calc <- dcast(df3.V.SDT, Subject + Age + Sex + Morality + Identity ~ sdt,value.var = "N")
df3.V.dprime_calc$miss[is.na(df3.V.dprime_calc$miss)] <- 0 # set na to 0
df3.V.dprime_calc$FA[is.na(df3.V.dprime_calc$FA)]     <- 0

# calculate the hit and false alarm rate
df3.V.dprime_calc$hitR <- df3.V.dprime_calc$hit/(df3.V.dprime_calc$hit + df3.V.dprime_calc$miss)
df3.V.dprime_calc$faR <- df3.V.dprime_calc$FA/(df3.V.dprime_calc$FA + df3.V.dprime_calc$CR)

# use the standardized way to deal with the extreme values
for (i in 1:nrow(df3.V.dprime_calc)){
  if (df3.V.dprime_calc$hitR[i] == 1){
    df3.V.dprime_calc$hitR[i] <- 1 - 1/(2*(df3.V.dprime_calc$hit[i] + df3.V.dprime_calc$miss[i]))
  }
}

for (i in 1:nrow(df3.V.dprime_calc)){
  if (df3.V.dprime_calc$faR[i] == 0){
    df3.V.dprime_calc$faR[i] <- 1/(2*(df3.V.dprime_calc$FA[i] + df3.V.dprime_calc$CR[i]))
  }
}

# calculate the d prime for each condition
df3.V.dprime_calc$dprime <- mapply(dprime,df3.V.dprime_calc$hitR,df3.V.dprime_calc$faR)

# change dprime data from long format to wide
df3.V.dprime_w <- dcast(df3.V.dprime_calc, Subject + Age + Sex ~ Identity + Morality ,value.var = "dprime")
df3.V.dprime_l     <- df3.V.dprime_calc[,c("Subject", "Age","Sex","Morality",'Identity',"dprime")]
#df3.V.dprime_w <- df3.V.dprime_ww

####### calculate the number of each for each condition ########
# this is the same procedure to calculate the d prime, only consider the moral valence 
df3.V.d_moral_calc <-  ddply(df3.V,.(Subject,Age, Sex,Morality,sdt), summarise, N = length(sdt))

# long format to wide
df3.V.d_moral_calc <- dcast(df3.V.d_moral_calc, Subject + Age + Sex + Morality ~ sdt,value.var = "N")
df3.V.d_moral_calc$miss[is.na(df3.V.d_moral_calc$miss)] <- 0
df3.V.d_moral_calc$FA[is.na(df3.V.d_moral_calc$FA)] <- 0
df3.V.d_moral_calc$hitR <- df3.V.d_moral_calc$hit/(df3.V.d_moral_calc$hit + df3.V.d_moral_calc$miss)
df3.V.d_moral_calc$faR <- df3.V.d_moral_calc$FA/(df3.V.d_moral_calc$FA + df3.V.d_moral_calc$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df3.V.d_moral_calc)){
  if (df3.V.d_moral_calc$hitR[i] == 1){
    df3.V.d_moral_calc$hitR[i] <- 1 - 1/(2*(df3.V.d_moral_calc$hit[i] + df3.V.d_moral_calc$miss[i]))
  }
}

for (i in 1:nrow(df3.V.d_moral_calc)){
  if (df3.V.d_moral_calc$faR[i] == 0){
    df3.V.d_moral_calc$faR[i] <- 1/(2*(df3.V.d_moral_calc$FA[i] + df3.V.d_moral_calc$CR[i]))
  }
}

# calculate the d prime for each condition
df3.V.d_moral_calc$dprime <- mapply(dprime,df3.V.d_moral_calc$hitR,df3.V.d_moral_calc$faR)
df3.V.d_moral_w <- dcast(df3.V.d_moral_calc, Subject + Age + Sex ~ Morality ,value.var = "dprime")
df3.V.d_moral_l     <- df3.V.d_moral_calc[,c("Subject", "Age","Sex","Morality","dprime")]

##################

# rename the column number
colnames(df3.V.dprime_w)[4:9] <- paste("d", colnames(df3.V.dprime_w[,4:9]), sep = "_")
colnames(df3.V.d_moral_w)[4:6] <- paste("d", colnames(df3.V.d_moral_w[,4:6]), sep = "_")

## RT ####
df3.V.RT <- df3.V[df3.V$ACC ==1,]  # exclued inaccurate data
df3.V.RT.subj <- summarySEwithin(df3.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),idvar = 'Subject', na.rm = TRUE)
df3.V.RT.subj_moral <- summarySEwithin(df3.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'),idvar = 'Subject', na.rm = TRUE)

# long to wide
df3.V.RT.subj_w <- dcast(df3.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 
df3.V.RT.subj_moral_w <- dcast(df3.V.RT.subj_moral, Subject ~ Matchness + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df3.V.RT.subj_w)[2:13] <- paste("RT", colnames(df3.V.RT.subj_w[,2:13]), sep = "_")
colnames(df3.V.RT.subj_moral_w)[2:7] <- paste("RT", colnames(df3.V.RT.subj_moral_w[,2:7]), sep = "_")

# merge files####
# the dprime and RT data and save
df3.V.sum_w <- merge(df3.acc_w,df3.V.dprime_w,by = "Subject")
df3.V.sum_w <- merge(df3.V.sum_w,df3.V.RT.subj_w,by = 'Subject')
df3.V.sum_moral_w <- merge(df3.acc_moral_w,df3.V.d_moral_w,by = "Subject")
df3.V.sum_moral_w <- merge(df3.V.sum_moral_w,df3.V.RT.subj_moral_w,by = 'Subject')
# order the columns
df3.V.sum_w <- df3.V.sum_w[,c(colnames(df3.V.sum_w)[c(1,14,15)],colnames(df3.V.sum_w)[c(2:13,16:33)])]
df3.V.sum_moral_w <- df3.V.sum_moral_w[,c(colnames(df3.V.sum_moral_w)[c(1,8:9)],colnames(df3.V.sum_moral_w)[c(2:7,10:18)])]

# save the long-format data
df3.v.sum_rt_acc_l <- merge(df3.acc,df3.V.RT.subj,by = c("Subject","Matchness","Morality",'Identity'))
df3.v.sum_rt_acc_l <- df3.v.sum_rt_acc_l[order(df3.v.sum_rt_acc_l$Subject),]
df3.v.sum_rt_acc_l <- df3.v.sum_rt_acc_l[,c("Subject","Matchness","Morality",'Identity',"N.x","countN","ACC","RT")]
colnames(df3.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality",'Identity',"Ntrials","corrtrials","ACC","RT")

df3.v.sum_rt_acc_moral_l <- merge(df3.acc_moral,df3.V.RT.subj_moral,by = c("Subject","Matchness","Morality"))
df3.v.sum_rt_acc_moral_l <- df3.v.sum_rt_acc_moral_l[order(df3.v.sum_rt_acc_moral_l$Subject),]
df3.v.sum_rt_acc_moral_l <- df3.v.sum_rt_acc_moral_l[,c("Subject","Matchness","Morality","N.x","countN","ACC","RT")]
colnames(df3.v.sum_rt_acc_moral_l) <- c("Subject","Matchness","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
write.csv(df3.V.sum_w,'exp3_behav_wide.csv',row.names = F)
write.csv(df3.v.sum_rt_acc_l,'exp3_rt_acc_long.csv',row.names = F)
write.csv(df3.V.dprime_l,'exp3_dprime_long.csv',row.names = F)

write.csv(df3.V.sum_moral_w,'exp3_behav_moral_wide.csv',row.names = F)
write.csv(df3.v.sum_rt_acc_moral_l,'exp3_rt_acc_moral_long.csv',row.names = F)
write.csv(df3.V.d_moral_l,'exp3_dprime_moral_long.csv',row.names = F)

## plot ####

## plot and save the results of d'
df3.V.dprime.sum <- summarySE(df3.V.dprime_l,measurevar = 'dprime',groupvars = c('Morality','Identity'))
df3.V.dprime.sum$Morality <- factor(df3.V.dprime.sum$Morality, levels = c('Moral','Neutral','Immoral'))
df3.V.dprime.sum$Identity <- factor(df3.V.dprime.sum$Identity, levels = c('Self','Other'))

# plot the results of dprime, way 1
e3.p_dprime1 <- ggplot(data = df3.V.dprime.sum, aes(x = Identity, y = dprime, group = Morality,shape = Morality, fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(x = 'Self-referential',y = 'd prime') +
  ggtitle("d prime for each condition") +
  coord_cartesian(ylim=c(1,3.5))+
  scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neut. ','Imm. '))+
  apatheme

# plot the results of dprime, way 2
e3.p_dprime2 <- ggplot(data = df3.V.dprime.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                #geom_errorbar(aes(ymin = 1, ymax = 4),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(x = 'Moral valence',y = 'd prime') +
  ggtitle("d prime for each condition") +
  coord_cartesian(ylim=c(1,3.5))+
  scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
  apatheme

## plot RT
df3.V.RT.grand <- summarySE(df3.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)
df3.V.RT.grand.match <- df3.V.RT.grand[df3.V.RT.grand$Matchness == "Match",]

df3.V.RT.grand.match$Morality <- factor(df3.V.RT.grand.match$Morality, levels = c('Moral','Neutral','Immoral'))
df3.V.RT.grand.match$Identity <- factor(df3.V.RT.grand.match$Identity, levels = c('Self','Other'))


e3.p_rt1 <- ggplot(data = df3.V.RT.grand.match, aes(x=Identity,y=RT,group=Morality,shape = Morality,fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Self-referential") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800)) +
  scale_y_continuous(breaks=seq(500,800,50),expand = c(0, 0)) +
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neut. ','Imm.'))+
  ggtitle("RT for each condition") +
  apatheme

e3.p_rt2 <- ggplot(data = df3.V.RT.grand.match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Moral valence") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800))+
  ggtitle("RT for each condition") +
  scale_y_continuous("Reation Times  (ms)",expand = c(0, 0)) + 
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
  apatheme

# ggsave('RT_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot

# save the plot together
tiff(filename = "Figure_exp3_d_RT_1.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e3.p_dprime1,e3.p_rt1,cols = 2)
dev.off()

tiff(filename = "Figure_exp3_d_RT_2.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e3.p_dprime2,e3.p_rt2,cols = 2)
dev.off()


## try the other plot
library(yarrr)
devtools::install_github("mikabr/ggpirate")
pirateplot(formula = dprime ~ Morality + Identity,
           data = df3.V.dprime_l)
