## this code is to analyze the data for exp1b, included these data were colleted at Wenzhou U in 201704

## initializing
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
df3$Matchness[df3$Matchness == "Yes"] <- "Match"
df3$Matchness[df3$Matchness == "No"]  <- "Mismatch"
#df3$Matchness <- factor(df3$Matchness, levels=c("match", "nonmatch")) # not factor before calculating the d-prime

# there recode are important for real trials (not for practice trials)
df3$Morality[df3$Shape == 'Goodself' | df3$Shape == 'Goodother']    <- "Moral"
df3$Morality[df3$Shape == 'Normalself'| df3$Shape == 'Normalother'] <- "Neutral"
df3$Morality[df3$Shape == 'Badself' | df3$Shape == 'Badother']      <- "Immoral"
df3$Identity[df3$Shape == 'Goodself' | df3$Shape == 'Badself' | df3$Shape == 'Normalself']    <- "Self"
df3$Identity[df3$Shape == 'Goodother'| df3$Shape == 'Badother'| df3$Shape == 'Normalother']   <- "Other"
df3$Morality  <- factor(df3$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df3$Identity  <- factor(df3$Identity, levels=c("Self", "Other"))

df3 <- df3[,-which(names(df3) %in% "Shape")]  # delete column "shape"

df3.P <- df3[is.na(df3$BlockList.Sample),]            # data from practice
df3.T <- df3[!(is.na(df3$BlockList.Sample)),]   # data from test

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
excld.sub <- df3.acc.g$Subject[df3.acc.g$ACC < 0.6]
df3.valid <- df3.T[!(df3.T$Subject %in% excld.sub),] # exclude the invalid subjects
length(unique(df3.valid$Subject)) + length(excld.sub) + nQuit == length(unique(df3$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% excld.sub),]
excld.trials2 <- df3.valid[df3.valid$RT <= 200 & df3.valid$ACC == 1,]
df3.V <- df3.valid[!(df3.valid$RT <= 200 & df3.valid$ACC == 1),]  

## Basic information of the data ####
df3.T.basic <- df3[!duplicated(df3$Subject), 1:4]
#df3.T.basic <- df3.T.basic[,which(names(df3.T.basic) %in% c("Subject","Age","Handness","Sex"))]
num.subj <- nrow(df3.T.basic)
numT.female <- sum(df3.T.basic$Sex == 'female');
numT.male <- sum(df3.T.basic$Sex == 'male');
mingAge <- min(df3.T.basic$Age)
ageT.mean <- round(mean(df3.T.basic$Age),2);
ageT.std <- round(sd(df3.T.basic$Age),2);
num.excld.sub <- length(unique(excld.sub))
df3.V.basic <- df3.V[!duplicated(df3.V$Subject), 1:4]
numV.female <- sum(df3.V.basic$Sex == 'female');
numV.male <- sum(df3.V.basic$Sex == 'male');
ageV.mean <- round(mean(df3.V.basic$Age),2);
ageV.std <- round(sd(df3.V.basic$Age),2);
ratio.excld.trials2 <- nrow(excld.trials2)/nrow(df3.valid)

## ACC data ####
df3.acc <-  ddply(df3.V,.(Subject,Matchness, Morality,Identity), summarise,
                  N = length(ACC),
                  countN = sum(ACC),
                  ACC = sum(ACC)/length(ACC))

# to wide-formate
df3.acc_w <- dcast(df3.acc, Subject ~ Matchness + Morality + Identity,value.var = "ACC")

# rename the column number
colnames(df3.acc_w)[2:13] <- paste("ACC", colnames(df3.acc_w[,2:13]), sep = "_")


## d prime ####
df3.V$sdt <- NA
#df3.V <- df3.V[!is.na(df3.V$Target.RESP),] ## excluded the non-response trial when analyze the deprime.
for (i in 1:nrow(df3.V)){
  if (df3.V$ACC[i] == 1 & (df3.V$Matchness[i] == "Match")){
    df3.V$sdt[i] <- "hit"
  } else if (df3.V$ACC[i] == 1 & (df3.V$Matchness[i] == "Mismatch")){
    df3.V$sdt[i] <- "CR"
  } else if (df3.V$ACC[i] != 1 & (df3.V$Matchness[i] == "Match")){
    df3.V$sdt[i] <- "miss"
  }
  else if (df3.V$ACC[i] != 1 & (df3.V$Matchness[i] == "Mismatch")){
    df3.V$sdt[i] <- "FA"
  }
}

# calculate the number of each for each condition
df3.V.dprime <-  ddply(df3.V,.(Subject,Age, Sex,Morality, Identity,sdt), summarise, N = length(sdt))


# long format to wide
df3.V.dprime_w <- dcast(df3.V.dprime, Subject + Age + Sex + Morality + Identity ~ sdt,value.var = "N")
df3.V.dprime_w$miss[is.na(df3.V.dprime_w$miss)] <- 0
df3.V.dprime_w$FA[is.na(df3.V.dprime_w$FA)] <- 0
df3.V.dprime_w$hitR <- df3.V.dprime_w$hit/(df3.V.dprime_w$hit + df3.V.dprime_w$miss)
df3.V.dprime_w$faR <- df3.V.dprime_w$FA/(df3.V.dprime_w$FA + df3.V.dprime_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df3.V.dprime_w)){
  if (df3.V.dprime_w$hitR[i] == 1){
    df3.V.dprime_w$hitR[i] <- 1 - 1/(2*(df3.V.dprime_w$hit[i] + df3.V.dprime_w$miss[i]))
  }
}

for (i in 1:nrow(df3.V.dprime_w)){
  if (df3.V.dprime_w$faR[i] == 0){
    df3.V.dprime_w$faR[i] <- 1/(2*(df3.V.dprime_w$FA[i] + df3.V.dprime_w$CR[i]))
  }
}

# calculate the d prime for each condition
df3.V.dprime_w$dprime <- mapply(dprime,df3.V.dprime_w$hitR,df3.V.dprime_w$faR)
df3.V.dprime_w.self <- df3.V.dprime_w[df3.V.dprime_w$Identity == 'self',]
df3.V.dprime_w.other <- df3.V.dprime_w[df3.V.dprime_w$Identity == 'other',]

# change dprime data from long format to wide
df3.V.dprime_ww <- dcast(df3.V.dprime_w, Subject + Age + Sex ~ Identity + Morality ,value.var = "dprime")
df3.V.dprime_l     <- df3.V.dprime_w[,c("Subject", "Age","Sex","Morality",'Identity',"dprime")]
df3.V.dprime_w <- df3.V.dprime_ww

# rename the column number
colnames(df3.V.dprime_w)[4:9] <- paste("d", colnames(df3.V.dprime_w[,4:9]), sep = "_")

## RT ####
df3.V.RT <- df3.V[df3.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df3.V.RT.subj <- summarySEwithin(df3.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),idvar = 'Subject', na.rm = TRUE)

# long to wide
df3.V.RT.subj_w <- dcast(df3.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df3.V.RT.subj_w)[2:13] <- paste("RT", colnames(df3.V.RT.subj_w[,2:13]), sep = "_")


# merge files####
# the dprime and RT data and save
df3.V.sum_w <- merge(df3.acc_w,df3.V.dprime_w,by = "Subject")
df3.V.sum_w <- merge(df3.V.sum_w,df3.V.RT.subj_w,by = 'Subject')
# order the columns
df3.V.sum_w <- df3.V.sum_w[,c(colnames(df3.V.dprime_w)[1:3],colnames(df3.acc_w)[2:13],colnames(df3.V.dprime_w)[4:9],colnames(df3.V.RT.subj_w)[2:13])]

df3.v.sum_rt_acc_l <- merge(df3.acc,df3.V.RT.subj,by = c("Subject","Matchness","Morality",'Identity'))
df3.v.sum_rt_acc_l <- df3.v.sum_rt_acc_l[order(df3.v.sum_rt_acc_l$Subject),]
df3.v.sum_rt_acc_l <- df3.v.sum_rt_acc_l[,c("Subject","Matchness","Morality",'Identity',"N.x","countN","ACC","RT")]
colnames(df3.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality",'Identity',"Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df3.V.sum_w,'exp3_behav_wide.csv',row.names = F)
write.csv(df3.v.sum_rt_acc_l,'exp3_rt_acc_long.csv',row.names = F)
write.csv(df3.V.dprime_l,'exp3_dprime_long.csv',row.names = F)

## plot ####

## plot and save the results of d'
df3.V.dprime.sum <- summarySE(df3.V.dprime_l,measurevar = 'dprime',groupvars = c('Morality','Identity'))
df3.V.dprime.sum$Morality <- factor(df3.V.dprime.sum$Morality, levels = c('Moral','Neutral','Immoral'))
df3.V.dprime.sum$Identity <- factor(df3.V.dprime.sum$Identity, levels = c('Self','Other'))

# plot the results of dprime, way 1
e3.p_dprime1 <- ggplot(data = df3.V.dprime.sum,aes(y = dprime, x = Identity, group = Morality,shape = Morality, fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                #geom_errorbar(aes(ymin = 1, ymax = 4),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(x = 'self-referential',y = 'd prime') +
  #ylab(" Reaction times") + 
  #ylim(1, 4) +
  ggtitle("d prime for each condition") +
  coord_cartesian(ylim=c(1,3.5))+
  scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter. 
  #theme_classic()
  apatheme +
  theme(axis.text = element_text (size = 20, color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good ",'Neutral','Immoral'))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

# plot the results of dprime, way 2
e3.p_dprime2 <- ggplot(data = df3.V.dprime.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                #geom_errorbar(aes(ymin = 1, ymax = 4),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(x = 'Moral valence',y = 'd prime') +
  #ylab(" Reaction times") + 
  #ylim(1, 4) +
  ggtitle("d prime for each condition") +
  coord_cartesian(ylim=c(1,3.5))+
  scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter. 
  #theme_classic()
  apatheme+
  theme(axis.text = element_text (size = 20, color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

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
  scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
  #ylim(0.3, 0.8) +
  ggtitle("RT for each condition") +
  #scale_y_continuous("Reation Times (ms)") + 
  apatheme +  
  theme(axis.text = element_text (size = 20, color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neutral ','Immoral'))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

e3.p_rt2 <- ggplot(data = df3.V.RT.grand.match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Moral valence") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800))+
  scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
  #ylim(0.3, 0.8) +
  ggtitle("RT for each condition") +
  scale_y_continuous("Reation Times  (ms)",expand = c(0, 0)) + 
  apatheme +
  
  theme(axis.text = element_text (size = 20, color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))
# ggsave('RT_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot


tiff(filename = "Figure 3. d prime and RTs of Experiment 3.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e3.p_dprime1,e3.p_rt1,cols = 2)
dev.off()

tiff(filename = "Figure 3.1. d prime and RTs of Experiment 3 (way 2).tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e3.p_dprime2,e3.p_rt2,cols = 2)
dev.off()

