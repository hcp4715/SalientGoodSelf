### about ####
# this code is to Preprocess all the data for exp 1a
# first part of the data were collected in 2014,
# second part of the data were collected in 201704 in Wenzhou U
# Change the levels of moral valence to "good", "bad", "neutral". 20180721


## initializing #### 
source('Initial_exp1a.r')

## load data ####
curDir = "D:/HCP_cloud/Exps/P1_Pos_Self/Exp_Behav_Moral_Asso/Results_exp1_5/Data_Analysis/exp1a/preproc_behav"
resDir = "D:/HCP_cloud/Exps/P1_Pos_Self/Exp_Behav_Moral_Asso/Results_exp1_5/Data_Analysis/exp1a"

df1a_1 <- read.csv("rawdata_behav_exp1a_2014.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1a_1$Subject))

## record from the meta-data:
# One participant's ID changed from 26 to 261, because of duplication.
# participant No 14 finished two sessions of the experiment, only the first session were included in the analysis
# # there are 4 foreign students, we didn't exclude them:
# foreignStdID <- c(24,29,30,33)

df1a_2 <- read.csv("rawdata_behav_exp1a_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1a_2$Subject))
df1a   <- rbind(df1a_1,df1a_2)

rm(df1a_1,df1a_2)

# edit the column names ####
# rename colnames 
colnames(df1a)[colnames(df1a)=="Target.ACC"] <- "ACC"
colnames(df1a)[colnames(df1a)=="Target.RT"]  <- "RT"
colnames(df1a)[colnames(df1a)=="YesNoResp"]  <- "Match"
colnames(df1a)[colnames(df1a)=="Shape"]      <- "Morality"

# renames independent variables
#df1a$Morality[df1a$Morality == "Good"]   <- "Moral"
df1a$Morality[df1a$Morality == "Normal"] <- "Neutral"
# df1a$Morality[df1a$Morality == "Bad"]    <- "Immoral"
df1a$Match[df1a$Match == "Yes"]  <- "Match"
df1a$Match[df1a$Match == "No"]   <- "Mismatch"

## Basic information of the data ####
# if the min age is 0, that age is missing
if (min(df1a$Age) == 0){
        df1a$Age[df1a$Age == 0] <- NA
        
}

df1a.T.basic     <- df1a[!duplicated(df1a$Subject), 1:4]
df1a.num.subj    <- nrow(df1a.T.basic)                         # N = 57
df1a.numT.female <- sum(df1a.T.basic$Sex == 'female');         # N female = 39
df1a.numT.male   <- sum(df1a.T.basic$Sex == 'male');           # N male   = 18
df1a.ageT.mean   <- round(mean(df1a.T.basic$Age,na.rm=TRUE),2);# mean age = 20.75
df1a.ageT.std    <- round(sd(df1a.T.basic$Age,na.rm=TRUE),2);  # sd age   = 2.54

# distinguish between practice and formal data
df1a.P <- df1a[is.na(df1a$BlockList.Sample),]            # data from practice
df1a.T <- df1a[complete.cases(df1a$BlockList.Sample),]   # data from test

# number of participant who didn't finished the experiment
nQuit <- length(unique(df1a.P$Subject)) - length(unique(df1a.T$Subject))

# exclude the correct response with less than 200 ms reaction time
df1a.excld.trial   <- df1a.T[df1a.T$RT <= 200 & df1a.T$ACC == 1,]
df1a.excld.trial.r <- nrow(df1a.excld.trial)/nrow(df1a.T) # ratio of excluded trials in all triasl.

# caculate the overall accuracy for each subject
df1a.acc.g <-  ddply(df1a.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))

# exlucde the participants with less than 60% overall accuracy
df1a.excld.sub <- df1a.acc.g$Subject[df1a.acc.g$ACC < 0.6]
df1a.valid     <- df1a.T[!(df1a.T$Subject %in% df1a.excld.sub),] # exclude the invalid subjects

# make sure that the number is correct (if neccessary)
#length(unique(df1a.valid$Subject)) + length(df1a.excld.sub) == length(unique(df1a$Subject))

# exclude the correct response within 200ms for valid data
df1a.excld.trials2   <- df1a.valid[df1a.valid$RT <= 200 & df1a.valid$ACC == 1,]
df1a.excld.trials2.r <- nrow(df1a.excld.trials2)/nrow(df1a.valid)                # 0.04%
df1a.V               <- df1a.valid[!(df1a.valid$RT <= 200  & df1a.valid$ACC == 1),]

## Basic information of the data ####
df1a.N.excld.sub <- length(unique(df1a.excld.sub))           # excluded 6; 4 from df1a_1; 2 from dfa1_2              
df1a.V.basic     <- df1a.V[!duplicated(df1a.V$Subject), 1:4] # 
df1a.numV.female <- sum(df1a.V.basic$Sex == 'female')        # 34
df1a.numV.male   <- sum(df1a.V.basic$Sex == 'male')          # 17
df1a.ageV.mean   <- round(mean(df1a.V.basic$Age,na.rm=TRUE),2) # 20.72
df1a.ageV.std    <- round(sd(df1a.V.basic$Age,na.rm=TRUE),2)   # 2.44

## calculate the Accuracy ####
df1a.acc  <-  ddply(df1a.V,.(Subject,Match, Morality), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
df1a.acc.sum <- summarySE(df1a.acc,measurevar = 'ACC', groupvars = c('Match','Morality'),na.rm = TRUE)
df1a.acc_w <- dcast(df1a.acc, Subject ~ Match + Morality,value.var = "ACC")

# rename the column number
colnames(df1a.acc_w)[2:7] <- paste("ACC", colnames(df1a.acc_w[,2:7]), sep = "_")
        
## d prime ####

# one problem: how to code the trials without response??

df1a.V$sdt <- NA
for (i in 1:nrow(df1a.V)){
        if (df1a.V$ACC[i] == 1 & df1a.V$Match[i] == "Match"){
                df1a.V$sdt[i] <- "hit"
        } else if (df1a.V$ACC[i] == 1 & df1a.V$Match[i] == "Mismatch"){
                df1a.V$sdt[i] <- "CR"
        } else if (df1a.V$ACC[i] == 0 & df1a.V$Match[i] == "Match"){
                df1a.V$sdt[i] <- "miss"
        } else if (df1a.V$ACC[i] == 0 & df1a.V$Match[i] == "Mismatch"){
                df1a.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df1a.V.SDT <-  ddply(df1a.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))

# long format to wide to calculate the d prime
df1a.V.SDT_w  <- dcast(df1a.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")

# if fa or miss is NA, set it to zero
df1a.V.SDT_w$miss[is.na(df1a.V.SDT_w$miss)] <- 0
df1a.V.SDT_w$FA[is.na(df1a.V.SDT_w$FA)] <- 0
df1a.V.SDT_w$hitR <- df1a.V.SDT_w$hit/(df1a.V.SDT_w$hit + df1a.V.SDT_w$miss)
df1a.V.SDT_w$faR <- df1a.V.SDT_w$FA/(df1a.V.SDT_w$FA + df1a.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df1a.V.SDT_w)){
        if (df1a.V.SDT_w$hitR[i] == 1){
                df1a.V.SDT_w$hitR[i] <- 1 - 1/(2*(df1a.V.SDT_w$hit[i] + df1a.V.SDT_w$miss[i]))
        }
}

for (i in 1:nrow(df1a.V.SDT_w)){
        if (df1a.V.SDT_w$faR[i] == 0){
                df1a.V.SDT_w$faR[i] <- 1/(2*(df1a.V.SDT_w$FA[i] + df1a.V.SDT_w$CR[i]))
        }
}

# calculate the d prime for each condition
df1a.V.SDT_w$dprime <- mapply(dprime,df1a.V.SDT_w$hitR,df1a.V.SDT_w$faR)
df1a.V.dprime_l     <- df1a.V.SDT_w[,c("Subject", "Age","Sex","Morality","dprime")]

# change dprime data from long format to wide
df1a.V.dprime_w     <- dcast(df1a.V.dprime_l, Subject + Sex + Age ~ Morality ,value.var = "dprime")

# rename the column number
colnames(df1a.V.dprime_w)[4:6] <- paste("d", colnames(df1a.V.dprime_w[,4:6]), sep = "_")

## RT ####
df1a.V.RT      <- df1a.V[df1a.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df1a.V.RT.subj <- summarySEwithin(df1a.V.RT,measurevar = 'RT', withinvar = c('Subject','Match','Morality'), idvar = 'Subject',na.rm = TRUE)

## long to wide
df1a.V.RT.subj_w <- dcast(df1a.V.RT.subj, Subject ~ Match + Morality ,value.var = "RT") 

# rename the columns of RT data
colnames(df1a.V.RT.subj_w)[2:7] <- paste("RT", colnames(df1a.V.RT.subj_w[,2:7]), sep = "_")

# merge the dprime and RT data and save
df1a.V.sum_w <- merge(df1a.acc_w,df1a.V.dprime_w,by = "Subject")
df1a.V.sum_w <- merge(df1a.V.sum_w,df1a.V.RT.subj_w,by = 'Subject')
# order the columns
df1a.V.sum_w <- df1a.V.sum_w[,c("Subject", "Sex","Age", "ACC_Match_Good", "ACC_Match_Neutral", "ACC_Match_Bad", "ACC_Mismatch_Good",
                                "ACC_Mismatch_Neutral", "ACC_Mismatch_Bad", "d_Good", "d_Neutral", "d_Bad", "RT_Match_Good",
                                "RT_Match_Neutral", "RT_Match_Bad", "RT_Mismatch_Good", "RT_Mismatch_Neutral","RT_Mismatch_Bad")]

df1a.v.sum_rt_acc_l <- merge(df1a.acc,df1a.V.RT.subj,by = c("Subject","Match","Morality"))
df1a.v.sum_rt_acc_l <- df1a.v.sum_rt_acc_l[order(df1a.v.sum_rt_acc_l$Subject),]
df1a.v.sum_rt_acc_l <- df1a.v.sum_rt_acc_l[,c("Subject","Match","Morality","N.x","countN","ACC","RT")]
colnames(df1a.v.sum_rt_acc_l) <- c("Subject","Match","Morality","Ntrials","corrtrials","ACC","RT")

## write files ####
# write the wide-format data
write.csv(df1a.V.sum_w,'exp1a_behav_wide.csv',row.names = F)
write.csv(df1a.v.sum_rt_acc_l,'exp1a_rt_acc_long.csv',row.names = F)
write.csv(df1a.V.dprime_l,'exp1a_dprime_long.csv',row.names = F)


## plot the data
Mplots(saveDir = resDir, curDir = curDir, expName = 'exp1a', df1a.V.dprime_l,df1a.v.sum_rt_acc_l)

df1a.V.SDT.sum <- summarySE(df1a.V.dprime_l,measurevar = 'dprime',groupvars = c('Morality'))
df1a.V.SDT.sum$Morality <- factor(df1a.V.SDT.sum$Morality,levels = c('Good','Neutral','Bad'))

df1a.p_dprime <- ggplot(data = df1a.V.SDT.sum,aes(y = dprime, x = Morality, group = Morality,shape = Morality, fill = Morality)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = 0.6) +         # Thinner lines
        geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                      size = 1, width = 0.2, lwd = 1,
                      position=position_dodge(.6)) +
        labs(x = 'Moral valence',y = 'd prime')    +
        coord_cartesian(ylim=c(1,3.5))             +
        scale_y_continuous(breaks = seq(1,3.5,0.5),expand = c(0, 0)) +
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good ",'Neut. ',"Bad ")) +
        apatheme

## plot RT
df1a.V.RT.grand <- summarySE(df1a.V.RT.subj,measurevar = 'RT', groupvar = c('Match','Morality'),na.rm = TRUE)
df1a.V.RT.grand.match <- df1a.V.RT.grand[df1a.V.RT.grand$Match == "Match",]
df1a.V.RT.grand.match$Morality <- factor(df1a.V.RT.grand.match$Morality,levels = c('Good','Neutral','Bad'))

df1a.p_rt <- ggplot(data = df1a.V.RT.grand.match, aes(x=Morality,y=RT,group=Morality,shape = Morality,fill = Morality)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = 0.6) +         # Thinner lines
        geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                      size = 1,
                      width = .2,
                      position=position_dodge(.6)) +
        labs(x = 'Moral valence',y = 'Reaction times (ms)') +
        coord_cartesian(ylim=c(500,800))+
        scale_y_continuous(breaks = seq(500,800,50),expand = c(0, 0)) +
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good ",'Neut. ',"Bad ")) +
        apatheme
# ggsave('e1_RT_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot

tiff(filename = "fig_exp1a_old.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(df1a.p_dprime,df1a.p_rt,cols = 2)
dev.off()

# plot the raincloud plot
Ddata1 <- df1a.V.dprime_l %>%
        select(Subject,Morality,dprime) # %>% 
        #filter(Morality == "Good")
Ddata1$Morality <- factor(Ddata1$Morality,levels = c("Good","Neutral","Bad"))
p1 <- ggplot(data = Ddata1, aes(y = dprime, x = Morality,fill = Morality)) +
        geom_flat_violin(position = position_nudge(x = .15, y = 0)) +
        geom_point(aes(y = dprime,color = Morality), position = position_jitter(width = .1), size = 2.5) +
        geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
        guides(fill = FALSE) +guides(color = FALSE)+
        #theme_bw() +
        raincloud_theme+scale_y_continuous(breaks = seq(-1,6,1),limits = c(-1,6))+
        labs(x = '',y = "d prime")
Ddata2 <- df1a.V.RT.subj %>%
        select(Subject,Morality,Match,RT) %>% 
        filter(Match == "Match")
Ddata2$Morality <- factor(Ddata2$Morality,levels = c("Good","Neutral","Bad"))
p2 <- ggplot(data = Ddata2, aes(y = RT, x = Morality,fill = Morality)) +
        geom_flat_violin(position = position_nudge(x = .15, y = 0)) +
        geom_point(aes(y = RT,color = Morality), position = position_jitter(width = .1), size = 2.5) +
        geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
        guides(fill = FALSE) + guides(color = FALSE)+
        #theme_bw() +
        raincloud_theme+scale_y_continuous(breaks = seq(400,900,100),limits = c(400,900))+
        labs(x = '',y = "Reaction times (ms)")


tiff(filename = 'fig_exp1a.tiff', width = 8, height = 6, units = 'in', res = 300)
p_exp1a <- multiplot(p1,p2,cols = 2)
dev.off()

