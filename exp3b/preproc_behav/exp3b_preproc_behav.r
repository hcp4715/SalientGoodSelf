# initializing
source('Initial.r')

# packages
#pkgNeeded <- (c("data.table","plyr","ggplot2", "reshape", 'dplyr','ggthemes','gridExtra',"MBESS", "bootES","metafor","compute.es",'psych','ez'))

#lapply(pkgNeeded,pkgTest)
#rm('pkgNeeded') # remove the variable 'pkgNeeded';


# Preprocessing the behavioral data for experiment 3b
df3b <- read.csv("rawdata_exp3_1.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

# rename colnames
colnames(df3b)[colnames(df3b)=="Target.ACC"]  <- "ACC"
colnames(df3b)[colnames(df3b)=="Target.RT"]   <- "RT"
colnames(df3b)[colnames(df3b)=="Target.RESP"] <- "RESP"
colnames(df3b)[colnames(df3b)=="YesNoResp"]   <- "Matchness"

# renames independent variables
df3b$Morality[df3b$Shape == 'Goodself' | df3b$Shape == 'GoodOther'] <- 'Good'
df3b$Morality[df3b$Shape == 'Badself' | df3b$Shape == 'BadOther']   <- 'Bad'
df3b$Morality[df3b$Shape == 'Neutralself' | df3b$Shape == 'NeutralOther'] <- 'Neutral'
df3b$Block[df3b$otherBlocklList.Sample | df3b$selfBlockList.Sample] <- 'Exp'
df3b$Block[df3b$otherPracList.Sample | df3b$selfPracList.Sample] <- 'Prac'
df3b$Identity[df3b$Identity == 'self'] <- 'Self'
#colnames(df3b)[colnames(df3)=="self"] <- "Identity"
#colnames(df3b)[colnames(df3)=="morality"] <- "Morality"


df3b$Morality <- factor(df3b$Morality, levels=c("Good", "Neutral","Bad")) # make the variables in a specified order
df3b$Identity <- factor(df3b$Identity, levels=c("Self", "Other"))
df3b$Matchness[df3b$Matchness == "Yes"] <- "Match"
df3b$Matchness[df3b$Matchness == "No"] <- "Mismatch"
df3b$Matchness <- factor(df3b$Matchness, levels=c("Match", "Mismatch"))

# df3$Morality[df3$Shape == 'Goodself' |df3$Shape == 'Goodother' ] <- "moral"
# df3$Morality[df3$Shape == 'Badself' | df3$Shape == 'Badother'] <- "immoral"
# df3$Morality[df3$Shape == 'Normalself'| df3$Shape == 'Normalother'] <- "average"
# df3$Identity[df3$Shape == 'Goodself'|df3$Shape == 'Normalself'|df3$Shape == 'Badself'] <- "self"
# df3$Identity[df3$Shape == 'Goodother'|df3$Shape == 'Badother' | df3$Shape == 'Normalother'] <- "other"

df3bP <- df3b[df3b$Block == 'Prac',]  # data from practice
df3bT <- df3b[df3b$Block == 'Exp',]   # data from test

# number of participant who didn't finished the experiment
nQuit <- length(unique(df3bP$Subject)) - length(unique(df3bT$Subject)) # 0

# trials that should be excluded from analysis because of less than 200 ms
# note that this should be reconsidered when doinng DDM analysis
# and also the trials without response
excld.trials <- df3bT[df3bT$RT <= 200 & df3bT$ACC == 1,]
ratio.excld.trials <- nrow(excld.trials)/nrow(df3bT) # ratio of excluded trials in all triasl.

# caculate the overall accuracy for each subject
df3b.acc.g <-  ddply(df3bT,.(Subject), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))
excld.sub <- df3b.acc.g$Subject[df3b.acc.g$ACC < 0.6]
excld.sub2 <- c(31003)
df3b.valid <- df3bT[!(df3bT$Subject %in% excld.sub),] # exclude the invalid subjects
df3b.valid <- df3b.valid[!(df3b.valid$Subject %in% excld.sub2),] # exclude the invalid subjects

length(unique(df3b.valid$Subject)) + length(excld.sub) + 1 + nQuit == length(unique(df3b$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% excld.sub),]
excld.trials2 <- df3b.valid[df3b.valid$RT <= 200 & df3b.valid$ACC ==1 ,]
df3b.V <- df3b.valid[!(df3b.valid$RT <= 200 & df3b.valid$ACC ==1),]  

## Basic information of the data ####
df3bT.basic <- df3b[!duplicated(df3b$Subject), 1:5]
df3bT.basic$Age[df3bT.basic$Age == 0] <- NA
num.subj.NAage <- sum(is.na(df3bT.basic$Age))
#df3bT.basic <- df3bT.basic[,which(names(df3bT.basic) %in% c("Subject","Age","Handness","Sex"))]
num.subj <- nrow(df3bT.basic)
numT.female <- sum(df3bT.basic$Sex == 'female');
numT.male <- sum(df3bT.basic$Sex == 'male');
ageT.mean <- round(mean(df3bT.basic$Age,na.rm = T),2);
ageT.std <- round(sd(df3bT.basic$Age,na.rm = T),2);
num.excld.sub <- length(unique(excld.sub))
df3b.V.basic <- df3b.V[!duplicated(df3b.V$Subject), 1:5]
df3b.V.basic$Age[df3b.V.basic$Age == 0] <- NA
numV.subj.NAage <- sum(is.na(df3b.V.basic$Age))
numV.female <- sum(df3b.V.basic$Sex == 'female');
numV.male <- sum(df3b.V.basic$Sex == 'male');
ageV.mean <- round(mean(df3b.V.basic$Age,na.rm = T),2);
ageV.std <- round(sd(df3b.V.basic$Age,na.rm = T),2);
ratio.excld.trials2 <- nrow(excld.trials2)/nrow(df3b.valid)

## check the block order
df3b.acc.g_id <- df3bT[!duplicated(df3bT$Subject), 1:8]

## save the valid data for future analysis
df3b.V <- df3b.V[,c('Subject','Age','Sex','Matchness','Morality','Identity','ACC','RT')]
write.csv(df3b.V,'cleanData_exp3_1.csv',row.names = F)

### ACC ####
df3b.acc  <-  ddply(df3b.V,.(Subject,Matchness, Morality,Identity), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))

df3b.acc_w <- dcast(df3b.acc, Subject ~ Matchness + Morality + Identity,value.var = "ACC")

# rename the column number
colnames(df3b.acc_w)[2:13] <- paste("ACC", colnames(df3b.acc_w[,2:13]), sep = "_")

# d prime #### 
df3b.V$sdt <- NA
for (i in 1:nrow(df3b.V)){
  if (df3b.V$ACC[i] == 1 & df3b.V$Matchness[i] == "Match"){
    df3b.V$sdt[i] <- "hit"
  } else if (df3b.V$ACC[i] == 1 & df3b.V$Matchness[i] == "Mismatch"){
    df3b.V$sdt[i] <- "CR"
  } else if (df3b.V$ACC[i] == 0 & df3b.V$Matchness[i] == "Match"){
    df3b.V$sdt[i] <- "miss"
  } else if (df3b.V$ACC[i] == 0 & df3b.V$Matchness[i] == "Mismatch"){
    df3b.V$sdt[i] <- "FA"
  }
}

# calculate the number of each for each condition
df3b.V.SDT <-  ddply(df3b.V,.(Subject,Age, Sex, Morality,Identity,sdt), summarise,
                     N = length(sdt))


# long format to wide
df3b.V.SDT_w <- dcast(df3b.V.SDT, Subject + Age + Sex+ Morality + Identity ~ sdt,value.var = "N")
df3b.V.SDT_w$miss[is.na(df3b.V.SDT_w$miss)] <- 0
df3b.V.SDT_w$FA[is.na(df3b.V.SDT_w$FA)] <- 0
df3b.V.SDT_w$hitR <- df3b.V.SDT_w$hit/(df3b.V.SDT_w$hit + df3b.V.SDT_w$miss)
df3b.V.SDT_w$faR <- df3b.V.SDT_w$FA/(df3b.V.SDT_w$FA + df3b.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df3b.V.SDT_w)){
  if (df3b.V.SDT_w$hitR[i] == 1){
    df3b.V.SDT_w$hitR[i] <- 1 - 1/(2*(df3b.V.SDT_w$hit[i] + df3b.V.SDT_w$miss[i]))
  }
}

for (i in 1:nrow(df3b.V.SDT_w)){
  if (df3b.V.SDT_w$faR[i] == 0){
    df3b.V.SDT_w$faR[i] <- 1/(2*(df3b.V.SDT_w$FA[i] + df3b.V.SDT_w$CR[i]))
  }
}

# calculate the d prime for each condition
df3b.V.SDT_w$dprime <- mapply(dprime,df3b.V.SDT_w$hitR,df3b.V.SDT_w$faR)
df3b.V.SDT_ww   <- dcast(df3b.V.SDT_w, Subject + Sex + Age ~ Morality + Identity ,value.var = "dprime")

df3b.V.SDT_l <- df3b.V.SDT_w[,c(1:5,12)]

# rename the column number
colnames(df3b.V.SDT_ww)[4:9] <- paste("d", colnames(df3b.V.SDT_ww[,4:9]), sep = "_")

## doing the analysis for RT ####
df3b.V.RT <- df3b.V[df3b.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df3b.V.RT.subj <- summarySEwithin(df3b.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'), idvar = 'Subject',na.rm = TRUE)
df3b.V.RT.subj_w <- dcast(df3b.V.RT.subj, Subject ~ Matchness + Morality + Identity ,value.var = "RT") 

# rename the columns of RT data
colnames(df3b.V.RT.subj_w)[2:13] <- paste("RT", colnames(df3b.V.RT.subj_w[,2:13]), sep = "_")

## saving data ####
# merge the dprime and RT data and save
df3b.V.sum_w <- merge(df3b.acc_w,  df3b.V.SDT_ww,by = "Subject")
df3b.V.sum_w <- merge(df3b.V.sum_w,df3b.V.RT.subj_w,by = 'Subject')

# merge the RT and ACC data (long-format)
df3b.v.sum_rt_acc_l <- merge(df3b.acc,df3b.V.RT.subj,by = c("Subject","Matchness","Morality","Identity"))
df3b.v.sum_rt_acc_l <- df3b.v.sum_rt_acc_l[order(df3b.v.sum_rt_acc_l$Subject),]
df3b.v.sum_rt_acc_l <- df3b.v.sum_rt_acc_l[,c("Subject","Matchness","Morality",'Identity',"N.x","countN","ACC","RT")]
colnames(df3b.v.sum_rt_acc_l) <- c("Subject","Matchness","Morality",'Identity',"Ntrials","corrtrials","ACC","RT")

# order the columns
df3b.V.sum_w <- df3b.V.sum_w[,c(1,14,15,2:13,16:33)]

# write files
write.csv(df3b.V.sum_w,'exp3b_behav_wide.csv',row.names = F)
write.csv(df3b.V.SDT_l,'exp3b_dprime_long.csv',row.names = F)
write.csv(df3b.v.sum_rt_acc_l,'exp3b_rt_acc_long.csv',row.names = F)


## plot d prime ####
Ddata1 <- df3b.V.SDT_l %>%
  select(Subject,Morality,Identity,dprime) %>% 
  filter(Identity == "Self")
Ddata1$Morality <- factor(Ddata1$Morality,levels = c("Good","Neutral","Bad"))
p1 <- ggplot(data = Ddata1, aes(y = dprime, x = Morality,fill = Morality)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  geom_point(aes(y = dprime,color = Morality), position = position_jitter(width = .1), size = 1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +guides(color = FALSE)+
  #theme_bw() +
  raincloud_theme+scale_y_continuous(breaks = seq(-1,5,1),limits = c(-1,5))+labs(x = "Self-referential",y = "dprime")
Ddata2 <- df3b.V.SDT_l %>%
  select(Subject,Morality,Identity,dprime) %>% 
  filter(Identity == "Other")
Ddata2$Morality <- factor(Ddata2$Morality,levels = c("Good","Neutral","Bad"))
p2 <- ggplot(data = Ddata2, aes(y = dprime, x = Morality,fill = Morality)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  geom_point(aes(y = dprime,color = Morality), position = position_jitter(width = .1), size = 1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +guides(color = FALSE)+
  # theme_bw() +
  raincloud_theme+scale_y_continuous(limits = c(-1,5),breaks = NULL)+labs(x = "Other-referential",y = "")

tiff('exp3b_dprime.tiff', width = 9, height = 6, units = 'in', res = 300)
p_dprime_match <- multiplot(p1,p2,cols = 2)
dev.off()

## plot RT ####
RTdata1 <- df3b.v.sum_rt_acc_l %>%
  select(Subject,Matchness,Morality,Identity,RT) %>% 
  filter(Identity == "Self" & Matchness == 'Match')
RTdata1$Morality <- factor(RTdata1$Morality,levels = c("Good","Neutral","Bad"))
p1_rt <- ggplot(data = RTdata1, aes(y = RT, x = Morality,fill = Morality)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  geom_point(aes(y = RT,color = Morality), position = position_jitter(width = .1), size = 1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +guides(color = FALSE)+
  #theme_bw() +
  raincloud_theme+scale_y_continuous(breaks = seq(300,1300,100),limits = c(300,1300))+labs(x = "Self-referential",y = "Reaction times (ms)")
RTdata2 <- df3b.v.sum_rt_acc_l %>%
  select(Subject,Matchness,Morality,Identity,RT) %>% 
  filter(Identity == "Other" & Matchness == 'Match')
RTdata2$Morality <- factor(RTdata2$Morality,levels = c("Good","Neutral","Bad"))
p2_rt <- ggplot(data = RTdata2, aes(y = RT, x = Morality,fill = Morality)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0)) +
  geom_point(aes(y = RT,color = Morality), position = position_jitter(width = .1), size = 1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  guides(fill = FALSE) +guides(color = FALSE)+
  # theme_bw() +
  raincloud_theme+scale_y_continuous(limits = c(300,1300),breaks = NULL)+labs(x = "Other-referential",y = "")

tiff('exp3b_RT.tiff', width = 9, height = 6, units = 'in', res = 300)
p_RT_match <- multiplot(p1_rt,p2_rt,cols = 2)
dev.off()
