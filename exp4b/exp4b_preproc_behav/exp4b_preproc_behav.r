## this code is to analyze the data for exp4b, included these data were colleted at Wenzhou U in 2017

## initializing ####
source('Initial.r')

## load data####
df4b_1 <- read.csv("rawdata_behav_exp4_2_2015.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b_2 <- read.csv("rawdata_behav_exp4_2_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

# combine the data
df4b_1$Location <- "Tsinghua"
df4b_2$Location <- "Wenzhou"
df4b <- rbind(df4b_1,df4b_2)

# rename colnames
colnames(df4b)[colnames(df4b)=="Target.ACC"] <- "ACC"
colnames(df4b)[colnames(df4b)=="Target.RT"]  <- "RT"
colnames(df4b)[colnames(df4b)=="YesNoResp"]  <- "Matchness"
#colnames(df4b)[colnames(df4b)=="self"]      <- "Identity"
colnames(df4b)[colnames(df4b)=="morality"]   <- "Morality"

# renames independent variables
df4b$Morality[df4b$Morality == "Good"]  <- "Moral"
df4b$Morality[df4b$Morality == "Ord"]   <- "Neutral"
df4b$Morality[df4b$Morality == "Bad"]   <- "Immoral"
df4b$Matchness[df4b$Matchness == "Yes"] <- "Match"
df4b$Matchness[df4b$Matchness == "No"]  <- "Mismatch"
df4b$Identity[df4b$Identity == 'self']  <- 'Self'
df4b$Identity[df4b$Identity == 'other'] <- 'Other'

# df4b$Matchness <- factor(df4b$Matchness, levels=c("Match", "Mismatch")) # not factor before calculating the d-prime
df4b$Morality  <- factor(df4b$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4b$Identity  <- factor(df4b$Identity, levels=c("Self", "Other"))

# there recode are important for real trials (not for practice trials)
#df4b$Morality[df4b$Shape == 'Goodself' | df4b$Shape == 'Goodother']    <- "moral"
#df4b$Morality[df4b$Shape == 'Normalself'| df4b$Shape == 'Normalother'] <- "Neutral"
#df4b$Morality[df4b$Shape == 'Badself' | df4b$Shape == 'Badother']      <- "Immoral"
#df4b$Identity[df4b$Shape == 'Goodself' | df4b$Shape == 'Badself' | df4b$Shape == 'Normalself']    <- "self"
#df4b$Identity[df4b$Shape == 'Goodother'| df4b$Shape == 'Badother'| df4b$Shape == 'Normalother']   <- "other"

#df4b <- df4b[,-which(names(df4b) %in% "Shape")]  # delete column "shape"

df4b.P <- df4b[is.na(df4b$BlockList.Sample),]      # data from practice
df4b.T <- df4b[!(is.na(df4b$BlockList.Sample)),]   # data from test

# number of participant who didn't finished the experiment
nQuit <- length(unique(df4b.P$Subject)) - length(unique(df4b.T$Subject))

# Correct trials that should be excluded from analysis because of less than 200 ms
# note that this should be reconsidered when doinng DDM analysis
# and also the trials without response

excld.trials <- df4b.T[df4b.T$RT <= 200 & df4b.T$ACC == 1,] # correct response with less than 200ms
ratio.excld.trials <- nrow(excld.trials)/nrow(df4b.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df4b.acc.g <-  ddply(df4b.T,.(Subject), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))

excld.sub <- df4b.acc.g$Subject[df4b.acc.g$ACC < 0.6]   # 8 participants were excluded from analysis
df4b.valid <- df4b.T[!(df4b.T$Subject %in% excld.sub),] # exclude the invalid subjects
length(unique(df4b.valid$Subject)) + length(excld.sub) + nQuit == length(unique(df4b$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% excld.sub),]
excld.trials2 <- df4b.valid[df4b.valid$RT <= 200 & df4b.valid$ACC == 1,]
df4b.V <- df4b.valid[!(df4b.valid$RT <= 200 & df4b.valid$ACC == 1),]  

## Basic information of the data ####
df4b.T.basic <- df4b[!duplicated(df4b$Subject), c("Subject","Age","Sex",'Location')]
#df4b.T.basic <- df4b.T.basic[,which(names(df4b.T.basic) %in% c("Subject","Age","Handness","Sex"))]
num.subj           <- nrow(df4b.T.basic)
num.subj_THU       <- nrow(df4b.T.basic[df4b.T.basic$Location == 'Tsinghua',])
num.subj_WZ        <- nrow(df4b.T.basic[df4b.T.basic$Location == 'Wenzhou',])
numT.female        <- sum(df4b.T.basic$Sex == 'female'); # 39 female (26 THU, 13 WZU)
numT.female_THU    <- sum(df4b.T.basic[df4b.T.basic$Location == 'Tsinghua',]$Sex == 'female');
numT.female_WZ     <- sum(df4b.T.basic[df4b.T.basic$Location == 'Wenzhou',]$Sex == 'female');
numT.male          <- sum(df4b.T.basic$Sex == 'male');   # 14 male
ageT.mean          <- round(mean(df4b.T.basic$Age),2);
ageT.mean_THU      <- round(mean(df4b.T.basic[df4b.T.basic$Location == 'Tsinghua',]$Age),2);
ageT.mean_Wenzhou  <- round(mean(df4b.T.basic[df4b.T.basic$Location == 'Wenzhou',]$Age),2);
ageT.std           <- round(sd(df4b.T.basic$Age),2);
ageT.std_THU       <- round(sd(df4b.T.basic[df4b.T.basic$Location == 'Tsinghua',]$Age),2);
ageT.std_WZ        <- round(sd(df4b.T.basic[df4b.T.basic$Location == 'Wenzhou',]$Age),2);
num.excld.sub      <- length(unique(excld.sub))
df4b.V.basic      <- df4b.V[!duplicated(df4b.V$Subject), c("Subject","Age","Sex",'Location')]
numV.female        <- sum(df4b.V.basic$Sex == 'female');
numV.female_THU    <- sum(df4b.V.basic[df4b.V.basic$Location == 'Tsinghua',]$Sex == 'female');
numV.female_WZ     <- sum(df4b.V.basic[df4b.V.basic$Location == 'Wenzhou',]$Sex == 'female');
numV.male          <- sum(df4b.V.basic$Sex == 'male');
ageV.mean          <- round(mean(df4b.V.basic$Age),2);
ageV.mean_THU      <- round(mean(df4b.V.basic[df4b.V.basic$Location == 'Tsinghua',]$Age),2);
ageV.mean_WZ       <- round(mean(df4b.V.basic[df4b.V.basic$Location == 'Wenzhou',]$Age),2);
ageV.std           <- round(sd(df4b.V.basic$Age),2);
ageV.std_THU       <- round(sd(df4b.V.basic[df4b.V.basic$Location == 'Tsinghua',]$Age),2);
ageV.std_WZ        <- round(sd(df4b.V.basic[df4b.V.basic$Location == 'Wenzhou',]$Age),2);
ratio.excld.trials2 <- nrow(excld.trials2)/nrow(df4b.valid)

## ACC data ####
df4b.acc <-  ddply(df4b.V,.(Subject,Matchness, Morality,Identity), summarise,
                  N = length(ACC),
                  countN = sum(ACC),
                  ACC = sum(ACC)/length(ACC))

# ACC for the data without considering Idenity
df4b.acc_moral <-  ddply(df4b.V,.(Subject,Matchness, Morality), summarise,
                         N = length(ACC),
                         countN = sum(ACC),
                         ACC = sum(ACC)/length(ACC))


# to wide-formate
df4b.acc_w <- dcast(df4b.acc, Subject ~ Matchness + Morality + Identity,value.var = "ACC")
df4b.acc_moral_w <- dcast(df4b.acc_moral, Subject ~ Matchness + Morality,value.var = "ACC")

# rename the column number
colnames(df4b.acc_w)[2:13] <- paste("ACC", colnames(df4b.acc_w[,2:13]), sep = "_")
colnames(df4b.acc_moral_w)[2:7] <- paste("ACC", colnames(df4b.acc_moral_w[,2:7]), sep = "_")

## d prime ####
df4b.V$sdt <- NA
for (i in 1:nrow(df4b.V)){
  if        (df4b.V$ACC[i] == 1 & (df4b.V$Matchness[i] == "Match")){
    df4b.V$sdt[i] <- "hit"
  } else if (df4b.V$ACC[i] == 1 & (df4b.V$Matchness[i] == "Mismatch")){
    df4b.V$sdt[i] <- "CR"
  } else if (df4b.V$ACC[i] == 0 & (df4b.V$Matchness[i] == "Mismatch")){
    df4b.V$sdt[i] <- "FA"
  }
    else if (df4b.V$ACC[i] == 0 & (df4b.V$Matchness[i] == "Match")){
    df4b.V$sdt[i] <- "miss"
  }
}

# calculate the number of each for each condition
df4b.V.dprime <-  ddply(df4b.V,.(Subject,Morality, Age, Sex, Identity,Location,sdt), summarise, N = length(sdt))
#df4b.V.dprime <-  ddply(df4b.V,.(Subject,Morality, Identity,sdt), summarise, N = length(sdt))

# long format to wide
#df4b.V.dprime_w <- dcast(df4b.V.dprime, Subject + Morality + Identity ~ sdt,value.var = "N")
df4b.V.dprime <- dcast(df4b.V.dprime, Location + Subject + Age + Sex + Morality + Identity ~ sdt,value.var = "N")
df4b.V.dprime$miss[is.na(df4b.V.dprime$miss)] <- 0
df4b.V.dprime$FA[is.na(df4b.V.dprime$FA)] <- 0
df4b.V.dprime$hitR <- df4b.V.dprime$hit/(df4b.V.dprime$hit + df4b.V.dprime$miss)
df4b.V.dprime$faR <- df4b.V.dprime$FA/(df4b.V.dprime$FA + df4b.V.dprime$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df4b.V.dprime)){
  if (df4b.V.dprime$hitR[i] == 1){
    df4b.V.dprime$hitR[i] <- 1 - 1/(2*(df4b.V.dprime$hit[i] + df4b.V.dprime$miss[i]))
  }
}

for (i in 1:nrow(df4b.V.dprime)){
  if (df4b.V.dprime$faR[i] == 0){
    df4b.V.dprime$faR[i] <- 1/(2*(df4b.V.dprime$FA[i] + df4b.V.dprime$CR[i]))
  }
}

# calculate the d prime for each condition
df4b.V.dprime$dprime <- mapply(dprime,df4b.V.dprime$hitR,df4b.V.dprime$faR)

# long to wide format
df4b.V.dprime_l <- df4b.V.dprime[,c('Subject','Morality','Identity','dprime')]
df4b.V.dprime_w <- dcast(df4b.V.dprime, Location + Subject + Age + Sex ~ Identity + Morality ,value.var = "dprime")

####### calculate the number of each for each condition ########
# this is the same procedure to calculate the d prime, only consider the moral valence 
df4b.V.d_moral_calc <-  ddply(df4b.V,.(Location,Subject,Age, Sex,Morality,sdt), summarise, N = length(sdt))

# long format to wide
df4b.V.d_moral_calc <- dcast(df4b.V.d_moral_calc, Location + Subject + Age + Sex + Morality ~ sdt,value.var = "N")
df4b.V.d_moral_calc$miss[is.na(df4b.V.d_moral_calc$miss)] <- 0
df4b.V.d_moral_calc$FA[is.na(df4b.V.d_moral_calc$FA)] <- 0
df4b.V.d_moral_calc$hitR <- df4b.V.d_moral_calc$hit/(df4b.V.d_moral_calc$hit + df4b.V.d_moral_calc$miss)
df4b.V.d_moral_calc$faR <- df4b.V.d_moral_calc$FA/(df4b.V.d_moral_calc$FA + df4b.V.d_moral_calc$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df4b.V.d_moral_calc)){
  if (df4b.V.d_moral_calc$hitR[i] == 1){
    df4b.V.d_moral_calc$hitR[i] <- 1 - 1/(2*(df4b.V.d_moral_calc$hit[i] + df4b.V.d_moral_calc$miss[i]))
  }
}

for (i in 1:nrow(df4b.V.d_moral_calc)){
  if (df4b.V.d_moral_calc$faR[i] == 0){
    df4b.V.d_moral_calc$faR[i] <- 1/(2*(df4b.V.d_moral_calc$FA[i] + df4b.V.d_moral_calc$CR[i]))
  }
}

# calculate the d prime for each condition
df4b.V.d_moral_calc$dprime <- mapply(dprime,df4b.V.d_moral_calc$hitR,df4b.V.d_moral_calc$faR)
df4b.V.d_moral_w <- dcast(df4b.V.d_moral_calc, Location + Subject + Age + Sex ~ Morality ,value.var = "dprime")
df4b.V.d_moral_l     <- df4b.V.d_moral_calc[,c("Location","Subject", "Age","Sex","Morality","dprime")]

##################

# rename the column number
colnames(df4b.V.dprime_w)[5:10] <- paste("d", colnames(df4b.V.dprime_w[,5:10]), sep = "_")
colnames(df4b.V.d_moral_w)[5:7] <- paste("d", colnames(df4b.V.d_moral_w[,5:7]), sep = "_")

## doing the analysis for RT ####
## plot density of each subject's RT and save them individually
subNo <- unique(df4b.V$Subject)

## RT ####
df4b.V.RT                 <- df4b.V[df4b.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df4b.V.RT.subj            <- summarySEwithin(df4b.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),
                                             idvar = 'Subject', na.rm = TRUE)
df4b.V.RT.subj_moral <- summarySEwithin(df4b.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'),idvar = 'Subject', na.rm = TRUE)

# long to wide
df4b.V.RT.subj_w <- dcast(df4b.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 
df4b.V.RT.subj_moral_w <- dcast(df4b.V.RT.subj_moral, Subject ~ Matchness + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df4b.V.RT.subj_w)[2:13] <- paste("RT", colnames(df4b.V.RT.subj_w[,2:13]), sep = "_")
colnames(df4b.V.RT.subj_moral_w)[2:7] <- paste("RT", colnames(df4b.V.RT.subj_moral_w[,2:7]), sep = "_")

# merge files####
# the dprime and RT data and save
df4b.V.sum_w <- merge(df4b.acc_w,df4b.V.dprime_w,by = "Subject")
df4b.V.sum_w <- merge(df4b.V.sum_w,df4b.V.RT.subj_w,by = 'Subject')

df4b.V.sum_moral_w <- merge(df4b.acc_moral_w,df4b.V.d_moral_w,by = "Subject")
df4b.V.sum_moral_w <- merge(df4b.V.sum_moral_w,df4b.V.RT.subj_moral_w,by = 'Subject')

# order the columns
df4b.V.sum_w <- df4b.V.sum_w[,c(colnames(df4b.V.sum_w)[c(1,14:16,2:13,17:34)])]
df4b.V.sum_moral_w <- df4b.V.sum_moral_w[,c(colnames(df4b.V.sum_moral_w)[c(1,8:10,2:7,11:19)])]

df4b.v.sum_rt_acc_l <- merge(df4b.acc,df4b.V.RT.subj,by = c("Subject","Matchness","Morality",'Identity'))
df4b.v.sum_rt_acc_l <- df4b.v.sum_rt_acc_l[order(df4b.v.sum_rt_acc_l$Subject),]
df4b.v.sum_rt_acc_l <- df4b.v.sum_rt_acc_l[,c("Subject","Matchness","Morality",'Identity',"N.x","countN","ACC","RT")]
colnames(df4b.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality",'Identity',"Ntrials","corrtrials","ACC","RT")

df4b.v.sum_rt_acc_moral_l <- merge(df4b.acc_moral,df4b.V.RT.subj_moral,by = c("Subject","Matchness","Morality"))
df4b.v.sum_rt_acc_moral_l <- df4b.v.sum_rt_acc_moral_l[order(df4b.v.sum_rt_acc_moral_l$Subject),]
df4b.v.sum_rt_acc_moral_l <- df4b.v.sum_rt_acc_moral_l[,c("Subject","Matchness","Morality","N.x","countN","ACC","RT")]
colnames(df4b.v.sum_rt_acc_moral_l) <- c("Subject","Matchness","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df4b.V.sum_w,'exp4b_behav_wide.csv',row.names = F)
write.csv(df4b.v.sum_rt_acc_l,'exp4b_rt_acc_long.csv',row.names = F)
write.csv(df4b.V.dprime_l,'exp4b_dprime_long.csv',row.names = F)

write.csv(df4b.V.sum_moral_w,'exp4b_behav_moral_wide.csv',row.names = F)
write.csv(df4b.v.sum_rt_acc_moral_l,'exp4b_rt_acc_moral_long.csv',row.names = F)
write.csv(df4b.V.d_moral_l,'exp4b_dprime_moral_long.csv',row.names = F)


## plot ####
df4b.V.dprime.sum <- summarySE(df4b.V.dprime,measurevar = 'dprime',groupvars = c('Morality','Identity'))
df4b.V.dprime.sum$Morality  <- factor(df4b.V.dprime.sum$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4b.V.dprime.sum$Identity  <- factor(df4b.V.dprime.sum$Identity, levels=c("Self", "Other"))

# plot the results of dprime, way 1
df4b.p_dprime1 <- ggplot(data = df4b.V.dprime.sum,aes(y = dprime, x = Identity, group = Morality,shape = Morality, fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                #geom_errorbar(aes(ymin = 1, ymax = 4),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(x = 'Identity',y = 'd prime') +
  #ylab(" Reaction times") + 
  #ylim(1, 4) +
  ggtitle("d prime for each condition") +
  coord_cartesian(ylim=c(1,3.5))+
  scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neut.','Imm. '))+
  apatheme
  
# plot the results of dprime, way 2
df4b.p_dprime2 <- ggplot(data = df4b.V.dprime.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
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

# ggsave('dprime_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot

## plot RT ####
df4b.V.RT.grand           <- summarySE(df4b.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)
df4b.V.RT.grand.Match     <- df4b.V.RT.grand[df4b.V.RT.grand$Matchness == "Match",]
df4b.V.RT.grand.Match$Morality  <- factor(df4b.V.RT.grand.Match$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4b.V.RT.grand.Match$Identity  <- factor(df4b.V.RT.grand.Match$Identity, levels=c("Self", "Other"))

df4b.p_rt1 <- ggplot(data = df4b.V.RT.grand.Match, aes(x=Identity,y=RT,group=Morality,shape = Morality,fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Identity") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800)) +
  scale_y_continuous(breaks=seq(500,800,50),expand = c(0, 0)) +
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neut.','Imm. '))+
  ggtitle("RT for each condition") +
  apatheme

df4b.p_rt2 <- ggplot(data = df4b.V.RT.grand.Match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Moral valence") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800))+
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
  ggtitle("RT for each condition") +
  scale_y_continuous("Reation Times  (ms)",expand = c(0, 0)) + 
  apatheme +
  
# ggsave('RT_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot


tiff(filename = "Fig_d_prime_and_RTs_exp4b_1.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(df4b.p_dprime1,df4b.p_rt1,cols = 2)
dev.off()

tiff(filename = "Fig_d_prime_and_RTs_exp4b_2.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(df4b.p_dprime2,df4b.p_rt2,cols = 2)
dev.off()
