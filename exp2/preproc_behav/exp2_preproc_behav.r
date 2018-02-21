## this code is to preprocess the data for exp2


## initializing####
source('Initial.r')

## load and edite data ####
df2 <- read.csv("rawdata_behav_exp2.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df2$Subject))

# rename colnames
colnames(df2)[colnames(df2)=="Target.ACC"] <- "ACC"
colnames(df2)[colnames(df2)=="Target.RT"]  <- "RT"
colnames(df2)[colnames(df2)=="YesNoResp"]  <- "Matchness"
colnames(df2)[colnames(df2)=="Shape"]      <- "Morality"

# renames independent variables
df2$Morality[df2$Morality == "Good"]   <- "Moral"
df2$Morality[df2$Morality == "Normal"] <- "Neutral"
df2$Morality[df2$Morality == "Bad"]    <- "Immoral"
df2$Morality <- factor(df2$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order

df2$Matchness[df2$Matchness == "Yes"] <- "Match"
df2$Matchness[df2$Matchness == "No"] <- "Mismatch"
df2$Matchness <- factor(df2$Matchness, levels=c("Match", "Mismatch"))

## Basic information of the data ####
df2.T.basic    <- df2[!duplicated(df2$Subject), 1:4]
df2.num.subj    <- nrow(df2.T.basic)
df2.numT.female <- sum(df2.T.basic$Sex == 'female');
df2.numT.male   <- sum(df2.T.basic$Sex == 'male');
df2.ageT.mean   <- round(mean(df2.T.basic$Age),2);
df2.ageT.std    <- round(sd(df2.T.basic$Age),2);

# distinguish between practice and formal experimental data
df2.P <- df2[is.na(df2$BlockList.Sample),]            # data from practice
df2.T <- df2[complete.cases(df2$BlockList.Sample),]   # data from test

# subject 45 were excluded because his middle finger is injured when doing the expeirments
df2.excld.sub_extra <- c('45')

# exclude the correct trials with less than 200 ms RT
df2.excld.trials <- df2.T[df2.T$RT <= 200 & df2.T$ACC == 1,]
df2.ratio.excld.trials <- nrow(df2.excld.trials)/nrow(df2.T) # ratio of excluded trials in all triasl.

# caculate the overall accuracy for each subject
df2.acc.g <-  ddply(df2.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
df2.excld.sub <- df2.acc.g$Subject[df2.acc.g$ACC < 0.6]
df2.valid <- df2.T[!(df2.T$Subject %in% df2.excld.sub),]               # exclude the invalid subjects
df2.valid <- df2.valid[!(df2.valid$Subject %in% df2.excld.sub_extra),] # exclude the invalid subjects

length(unique(df2.valid$Subject)) + length(df2.excld.sub) + length(df2.excld.sub_extra)== length(unique(df2$Subject))

# exclude the correct trials within 200ms RT
df2.excld.trials2 <- df2.valid[df2.valid$RT <= 200 & df2.valid$ACC == 1,]
df2.V <- df2.valid[!(df2.valid$RT <= 200 & df2.valid$ACC == 1),]  

## Basic information of the data ####
df2.num.excld.sub <- length(unique(df2.excld.sub))
df2.V.basic <- df2.V[!duplicated(df2.V$Subject), 1:4]
df2.numV.female <- sum(df2.V.basic$Sex == 'female');
df2.numV.male <- sum(df2.V.basic$Sex == 'male');
df2.ageV.mean <- round(mean(df2.V.basic$Age),2);
df2.ageV.std <- round(sd(df2.V.basic$Age),2);
df2.ratio.excld.trials2 <- nrow(df2.excld.trials2)/nrow(df2.valid)

## ACC data ####
df2.acc <-  ddply(df2.V,.(Subject,Matchness, Morality), summarise,
                   N = length(ACC),
                   countN = sum(ACC),
                   ACC = sum(ACC)/length(ACC))

df2.acc_w <- dcast(df2.acc, Subject ~ Matchness + Morality,value.var = "ACC")

# rename the column number
colnames(df2.acc_w)[2:7] <- paste("ACC", colnames(df2.acc_w[,2:7]), sep = "_")


## d prime ####
df2.V$sdt <- NA
for (i in 1:nrow(df2.V)){
        if (df2.V$ACC[i] == 1 & df2.V$Matchness[i] == "Match"){
                df2.V$sdt[i] <- "hit"
        } else if (df2.V$ACC[i] == 1 & df2.V$Matchness[i] == "Mismatch"){
                df2.V$sdt[i] <- "CR"
        } else if (df2.V$ACC[i] == 0 & df2.V$Matchness[i] == "Match"){
                df2.V$sdt[i] <- "miss"
        } else if (df2.V$ACC[i] == 0 & df2.V$Matchness[i] == "Mismatch"){
                df2.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df2.V.dprime <-  ddply(df2.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))
# long format to wide
df2.V.dprime_w <- dcast(df2.V.dprime, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")
df2.V.dprime_w$miss[is.na(df2.V.dprime_w$miss)] <- 0
df2.V.dprime_w$FA[is.na(df2.V.dprime_w$FA)] <- 0
df2.V.dprime_w$hitR <- df2.V.dprime_w$hit/(df2.V.dprime_w$hit + df2.V.dprime_w$miss)
df2.V.dprime_w$faR <- df2.V.dprime_w$FA/(df2.V.dprime_w$FA + df2.V.dprime_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df2.V.dprime_w)){
        if (df2.V.dprime_w$hitR[i] == 1){
                df2.V.dprime_w$hitR[i] <- 1 - 1/(2*(df2.V.dprime_w$hit[i] + df2.V.dprime_w$miss[i]))
        }
}

for (i in 1:nrow(df2.V.dprime_w)){
        if (df2.V.dprime_w$faR[i] == 0){
                df2.V.dprime_w$faR[i] <- 1/(2*(df2.V.dprime_w$FA[i] + df2.V.dprime_w$CR[i]))
        }
}

# calculate the d prime for each condition
df2.V.dprime_w$dprime <- mapply(dprime,df2.V.dprime_w$hitR,df2.V.dprime_w$faR)
df2.V.dprime_l     <- df2.V.dprime_w[,c("Subject", "Age","Sex","Morality","dprime")]

# change dprime data from long format to wide
df2.V.dprime_w     <- dcast(df2.V.dprime_l, Subject + Sex + Age ~ Morality ,value.var = "dprime")

# rename the column number
colnames(df2.V.dprime_w)[4:6] <- paste("d", colnames(df2.V.dprime_w[,4:6]), sep = "_")

## RT data ####
df2.V.RT <- df2.V[df2.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df2.V.RT.subj <- summarySEwithin(df2.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'), idvar = 'Subject',na.rm = TRUE)

# transfer to wide-formate
df2.V.RT.subj_w <- dcast(df2.V.RT.subj, Subject ~ Matchness + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df2.V.RT.subj_w)[2:7] <- paste("RT", colnames(df2.V.RT.subj_w[,2:7]), sep = "_")


# merge files####
# the dprime and RT data and save
df2.V.sum_w <- merge(df2.acc_w,df2.V.dprime_w,by = "Subject")
df2.V.sum_w <- merge(df2.V.sum_w,df2.V.RT.subj_w,by = 'Subject')
# order the columns
df2.V.sum_w <- df2.V.sum_w[,c("Subject", "Sex","Age", "ACC_Match_Moral", "ACC_Match_Neutral", "ACC_Match_Immoral", "ACC_Mismatch_Moral",
                                "ACC_Mismatch_Neutral", "ACC_Mismatch_Immoral", "d_Moral", "d_Neutral", "d_Immoral", "RT_Match_Moral",
                                "RT_Match_Neutral", "RT_Match_Immoral", "RT_Mismatch_Moral", "RT_Mismatch_Neutral","RT_Mismatch_Immoral")]

df2.v.sum_rt_acc_l <- merge(df2.acc,df2.V.RT.subj,by = c("Subject","Matchness","Morality"))
df2.v.sum_rt_acc_l <- df2.v.sum_rt_acc_l[order(df2.v.sum_rt_acc_l$Subject),]
df2.v.sum_rt_acc_l <- df2.v.sum_rt_acc_l[,c("Subject","Matchness","Morality","N.x","countN","ACC","RT")]
colnames(df2.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df2.V.sum_w,'exp2_behav_wide.csv',row.names = F)
write.csv(df2.v.sum_rt_acc_l,'exp2_rt_acc_long.csv',row.names = F)
write.csv(df2.V.dprime_l,'exp2_dprime_long.csv',row.names = F)


## plot d prime #### 
df2.V.dprime.sum <- summarySE(df2.V.dprime_l,measurevar = 'dprime',groupvars = c('Morality'))
df2.V.dprime.sum$Morality <- factor(df2.V.dprime.sum$Morality, levels = c('Moral','Neutral','Immoral'))
df2.p_dprime <- ggplot(data = df2.V.dprime.sum,aes(y = dprime, x = Morality, group = Morality,shape = Morality, fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = 0.6) +         # Thinner lines
  geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                #geom_errorbar(aes(ymin = 1, ymax = 4),
                size = 1,
                width = 0.2,
                lwd = 1,
                position=position_dodge(.6)) +
  labs(x = 'Moral valence',y = 'd prime') +
  #ylab(" Reaction times") + 
  #ylim(1, 4) +
  ggtitle("d prime for each condition") +
  coord_cartesian(ylim=c(1,3.5))+
  scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter. 
  #theme_classic()
  apatheme  +
  theme(axis.text = element_text (size = 20, color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neutral  ',"Immoral"))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

ggsave("df2_2.p_dprime.pdf", df2.p_dprime, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

# plot RT ####
df2.V.RT.sum <- summarySE(df2.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality'),na.rm = TRUE)
df2.V.RT.sum.match <- df2.V.RT.sum[df2.V.RT.sum$Matchness == "Match",]
df2.V.RT.sum.match$Morality <- factor(df2.V.RT.sum.match$Morality, levels = c('Moral','Neutral','Immoral'))

df2.p_rt <- ggplot(data = df2.V.RT.sum.match, aes(x=Morality,y=RT,group=Morality,shape = Morality,fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = 0.6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(y = 'Reaction times (ms)') +
  coord_cartesian(ylim=c(500,800))+
  scale_y_continuous(breaks = seq(500,800,50),expand = c(0, 0)) +
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
  #ylim(0.3, 0.8) +
  ggtitle("RT for each condition") +
  #scale_y_continuous("Reation Times  (ms)",expand = c(0, 0)) + 
  apatheme +
  theme(axis.text = element_text (size = 20,color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral  ",'Neutral  ',"Immoral"))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) 

ggsave("df2_2.p_RT.pdf", df2.p_rt, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")
