## this code is to analyze the data for exp1b, included these data were colleted at Wenzhou U in 201704

## initializing
source('Initial.r')

## load data and edite data
df4_1a <- read.csv("rawdata_behav_exp4_1_2014.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df4_1b <- read.csv("rawdata_behav_exp4_1_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
# rename column of later data to keep consistency
colnames(df4_1b)[colnames(df4_1b) == 'Morality'] <- 'morality'
colnames(df4_1b)[colnames(df4_1b) == 'Identity'] <- 'self'

# combine the data
df4_1a$Location <- "Tsinghua"
df4_1b$Location <- "Wenzhou"
df4_1 <- rbind(df4_1a,df4_1b)

# analysis new data separately first
#df4_1 <- read.csv("rawdata_behav_exp4_1_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

# rename colnames
colnames(df4_1)[colnames(df4_1)=="Target.ACC"] <- "ACC"
colnames(df4_1)[colnames(df4_1)=="Target.RT"]  <- "RT"
colnames(df4_1)[colnames(df4_1)=="YesNoResp"]  <- "Matchness"
colnames(df4_1)[colnames(df4_1)=="self"]       <- "Identity"
colnames(df4_1)[colnames(df4_1)=="morality"]   <- "Morality"

# renames independent variables
df4_1$Morality[df4_1$Morality == "Good"]   <- "Moral"
df4_1$Morality[df4_1$Morality == "Normal"]  <- "Neutral"
df4_1$Morality[df4_1$Morality == "Bad"] <- "Immoral"
df4_1$Matchness[df4_1$Matchness == "Yes"] <- "Match"
df4_1$Matchness[df4_1$Matchness == "No"]  <- "Non-Match"
df4_1$Identity[df4_1$Identity == 'self'] <- 'Self'
df4_1$Identity[df4_1$Identity == 'other'] <- 'Other'

#df4_1$Matchness <- factor(df4_1$Matchness, levels=c("Match", "Non-match")) # not factor before calculating the d-prime

# there recode are important for real trials (not for practice trials)
#df4_1$Morality[df4_1$Shape == 'Goodself' | df4_1$Shape == 'Goodother']    <- "moral"
#df4_1$Morality[df4_1$Shape == 'Normalself'| df4_1$Shape == 'Normalother'] <- "Neutral"
#df4_1$Morality[df4_1$Shape == 'Badself' | df4_1$Shape == 'Badother']      <- "Immoral"
#df4_1$Identity[df4_1$Shape == 'Goodself' | df4_1$Shape == 'Badself' | df4_1$Shape == 'Normalself']    <- "self"
#df4_1$Identity[df4_1$Shape == 'Goodother'| df4_1$Shape == 'Badother'| df4_1$Shape == 'Normalother']   <- "other"

#df4_1 <- df4_1[,-which(names(df4_1) %in% "Shape")]  # delete column "shape"

df4_1.P <- df4_1[is.na(df4_1$BlockList.Sample),]            # data from practice
df4_1.T <- df4_1[!(is.na(df4_1$BlockList.Sample)),]   # data from test

# number of participant who didn't finished the experiment
nQuit <- length(unique(df4_1.P$Subject)) - length(unique(df4_1.T$Subject))

# trials that should be excluded from analysis because of less than 200 ms
# note that this should be reconsidered when doinng DDM analysis
# and also the trials without response

excld.trials <- df4_1.T[df4_1.T$RT <= 200 & df4_1.T$ACC == 1,] # correct response with less than 200ms
ratio.excld.trials <- nrow(excld.trials)/nrow(df4_1.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df4_1.acc.g <-  ddply(df4_1.T,.(Subject), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))
excld.sub <- df4_1.acc.g$Subject[df4_1.acc.g$ACC < 0.6]
df4_1.valid <- df4_1.T[!(df4_1.T$Subject %in% excld.sub),] # exclude the invalid subjects
length(unique(df4_1.valid$Subject)) + length(excld.sub) + nQuit == length(unique(df4_1$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% excld.sub),]
excld.trials2 <- df4_1.valid[df4_1.valid$RT <= 200 & df4_1.valid$ACC == 1,]
df4_1.V <- df4_1.valid[!(df4_1.valid$RT <= 200 & df4_1.valid$ACC == 1),]  

## Basic information of the data ####
df4_1.T.basic <- df4_1[!duplicated(df4_1$Subject), 1:4]
#df4_1.T.basic <- df4_1.T.basic[,which(names(df4_1.T.basic) %in% c("Subject","Age","Handness","Sex"))]
num.subj <- nrow(df4_1.T.basic)
numT.female <- sum(df4_1.T.basic$Sex == 'female');
numT.male <- sum(df4_1.T.basic$Sex == 'male');
ageT.mean <- round(mean(df4_1.T.basic$Age),2);
ageT.std <- round(sd(df4_1.T.basic$Age),2);
num.excld.sub <- length(unique(excld.sub))
df4_1.V.basic <- df4_1.V[!duplicated(df4_1.V$Subject), 1:4]
numV.female <- sum(df4_1.V.basic$Sex == 'female');
numV.male <- sum(df4_1.V.basic$Sex == 'male');
ageV.mean <- round(mean(df4_1.V.basic$Age),2);
ageV.std <- round(sd(df4_1.V.basic$Age),2);
ratio.excld.trials2 <- nrow(excld.trials2)/nrow(df4_1.valid)

#### analyze the d prime ####
df4_1.V$sdt <- NA
df4_1.V_sdt <- df4_1.V[!is.na(df4_1.V$Target.RESP),] ## excluded the non-response trial when analyze the deprime.
df4_1.V_miss <- df4_1.V[is.na(df4_1.V$Target.RESP),] ## the non-response trials
df4_1.V_miss$sdt <- "miss"                           ## non-response trials regarded as miss
for (i in 1:nrow(df4_1.V_sdt)){
  if (df4_1.V_sdt$Target.RESP[i] == df4_1.V_sdt$Target.CRESP[i] & (df4_1.V_sdt$Matchness[i] == "Non-Match")){
    df4_1.V_sdt$sdt[i] <- "CR"
  } else if (df4_1.V_sdt$Target.RESP[i] != df4_1.V_sdt$Target.CRESP[i] & (df4_1.V_sdt$Matchness[i] == "Match")){
    df4_1.V_sdt$sdt[i] <- "miss"
  } else if (df4_1.V_sdt$Target.RESP[i] != df4_1.V_sdt$Target.CRESP[i] & (df4_1.V_sdt$Matchness[i] == "Non-Match")){
    df4_1.V_sdt$sdt[i] <- "FA"
  }
  else if (df4_1.V_sdt$Target.RESP[i] == df4_1.V_sdt$Target.CRESP[i] & (df4_1.V_sdt$Matchness[i] == "Match")){
    df4_1.V_sdt$sdt[i] <- "hit"
  }
}

# combine the two
df4_1.V <- rbind(df4_1.V_sdt,df4_1.V_miss) 

# calculate the number of each for each condition
df4_1.V.SDT <-  ddply(df4_1.V,.(Subject,Morality, Identity,Location,sdt), summarise, N = length(sdt))

# long format to wide
df4_1.V.SDT_w <- dcast(df4_1.V.SDT, Subject + Morality + Identity + Location ~ sdt,value.var = "N")
df4_1.V.SDT_w$miss[is.na(df4_1.V.SDT_w$miss)] <- 0
df4_1.V.SDT_w$FA[is.na(df4_1.V.SDT_w$FA)] <- 0
df4_1.V.SDT_w$hitR <- df4_1.V.SDT_w$hit/(df4_1.V.SDT_w$hit + df4_1.V.SDT_w$miss)
df4_1.V.SDT_w$faR <- df4_1.V.SDT_w$FA/(df4_1.V.SDT_w$FA + df4_1.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df4_1.V.SDT_w)){
  if (df4_1.V.SDT_w$hitR[i] == 1){
    df4_1.V.SDT_w$hitR[i] <- 1 - 1/(2*(df4_1.V.SDT_w$hit[i] + df4_1.V.SDT_w$miss[i]))
  }
}

for (i in 1:nrow(df4_1.V.SDT_w)){
  if (df4_1.V.SDT_w$faR[i] == 0){
    df4_1.V.SDT_w$faR[i] <- 1/(2*(df4_1.V.SDT_w$FA[i] + df4_1.V.SDT_w$CR[i]))
  }
}

# calculate the d prime for each condition
df4_1.V.SDT_w$dprime <- mapply(dprime,df4_1.V.SDT_w$hitR,df4_1.V.SDT_w$faR)
df4_1.V.SDT_w.self <- df4_1.V.SDT_w[df4_1.V.SDT_w$Identity == 'Self',]
df4_1.V.SDT_w.other <- df4_1.V.SDT_w[df4_1.V.SDT_w$Identity == 'Other',]

# check the effect of location for d prime
e4_1.d_anova_site <- ezANOVA(df4_1.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity), between = .(Location), type=3)

# anova for d prime with 2*2 design
e4_1.d_anova <- ezANOVA(df4_1.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity), type=3)
e4_1.d_anova.self <- ezANOVA(df4_1.V.SDT_w.self, dv = dprime, wid = Subject, within=.(Morality), type=3)
e4_1.d_anova.other <- ezANOVA(df4_1.V.SDT_w.other, dv = dprime, wid = Subject, within=.(Morality), type=3)
#print(d_anova1)

# change dprime data from long format to wide
df4_1.V.SDT_ww <- dcast(df4_1.V.SDT_w, Subject ~ Identity + Morality ,value.var = "dprime")
df4_1.V.SDT_ww_r <- df4_1.V.SDT_ww

# t-test
# df4_1.V.SDT_w$condition <- paste(df4_1.V.SDT_w$Morality,df4_1.V.SDT_w$Identity,'_')
# pairwise.t.test(df4_1.V.SDT_w$dprime,df4_1.V.SDT_w$conidtion,p.adj = "bonf")

# moral self vs Immoral self
e4_1.d.t.mrl_imm_slf <- t.test(df4_1.V.SDT_ww$Self_Moral,df4_1.V.SDT_ww$Self_Immoral,paired = TRUE)
df4_1.V.SDT_ww$mrl_imm_slf <- df4_1.V.SDT_ww$Self_Moral - df4_1.V.SDT_ww$Self_Immoral
e4_1.d.t.mrl_imm_slf.CI <- bootES(df4_1.V.SDT_ww$mrl_imm_slf,R = 20000, effect.type = "cohens.d")

e4_1.d.tvalue.mrl_imm_slf <- round(as.numeric(e4_1.d.t.mrl_imm_slf[[1]]),3)
e4_1.d.df4_1.mrl_imm_slf <- as.numeric(e4_1.d.t.mrl_imm_slf[[2]])
e4_1.d.pvalue.mrl_imm_slf.adj <- p.adjust(as.numeric(e4_1.d.t.mrl_imm_slf[[3]],"bonferroni",3))
e4_1.d.cohens.mrl_imm_slf <- round(e4_1.d.t.mrl_imm_slf.CI[[1]],4) 
e4_1.d.CI.L.mrl_imm_slf <- round(e4_1.d.t.mrl_imm_slf.CI[[12]][1],4)
e4_1.d.CI.H.mrl_imm_slf <- round(e4_1.d.t.mrl_imm_slf.CI[[12]][2],4)

# moral self vs Average self
e4_1.d.t.mrl_ave_slf <- t.test(df4_1.V.SDT_ww$Self_Moral,df4_1.V.SDT_ww$Self_Neutral,paired = TRUE)
df4_1.V.SDT_ww$mrl_ave_slf <- df4_1.V.SDT_ww$Self_Moral - df4_1.V.SDT_ww$Self_Neutral
e4_1.d.t.mrl_ave_slf.CI <- bootES(df4_1.V.SDT_ww$mrl_ave_slf,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.mrl_ave_slf  <- round(as.numeric(e4_1.d.t.mrl_ave_slf[[1]]),3)
e4_1.d.df4_1.mrl_ave_slf  <- as.numeric(e4_1.d.t.mrl_ave_slf[[2]])
e4_1.d.pvalue.mrl_ave_slf.adj <- p.adjust(as.numeric(e4_1.d.t.mrl_ave_slf[[3]],"bonferroni",3))
e4_1.d.cohens.mrl_ave_slf <- round(e4_1.d.t.mrl_ave_slf.CI[[1]],4) 
e4_1.d.CI.L.mrl_ave_slf <- round(e4_1.d.t.mrl_ave_slf.CI[[12]][1],4)
e4_1.d.CI.H.mrl_ave_slf <- round(e4_1.d.t.mrl_ave_slf.CI[[12]][2],4)

# Immoral self vs. Average self
e4_1.d.t.imm_ave_slf <- t.test(df4_1.V.SDT_ww$Self_Immoral,df4_1.V.SDT_ww$Self_Neutral,paired = TRUE)
df4_1.V.SDT_ww$imm_ave_slf <- df4_1.V.SDT_ww$Self_Immoral - df4_1.V.SDT_ww$Self_Neutral
e4_1.d.t.imm_ave_slf.CI <- bootES(df4_1.V.SDT_ww$imm_ave_slf,R = 20000, effect.type = "cohens.d")

e4_1.d.tvalue.imm_ave_slf  <- round(as.numeric(e4_1.d.t.imm_ave_slf[[1]]),3)
e4_1.d.df4_1.imm_ave_slf  <- as.numeric(e4_1.d.t.imm_ave_slf[[2]])
e4_1.d.pvalue.imm_ave_slf.adj <- p.adjust(as.numeric(e4_1.d.t.imm_ave_slf[[3]],"bonferroni",3))
e4_1.d.cohens.imm_ave_slf <- round(e4_1.d.t.imm_ave_slf.CI[[1]],4) 
e4_1.d.CI.L.imm_ave_slf <- round(e4_1.d.t.imm_ave_slf.CI[[12]][1],4)
e4_1.d.CI.H.imm_ave_slf <- round(e4_1.d.t.imm_ave_slf.CI[[12]][2],4)

# moral other vs. Immoral other
e4_1.d.t.mrl_imm_oth <- t.test(df4_1.V.SDT_ww$Other_Moral,df4_1.V.SDT_ww$Other_Immoral,paired = TRUE)
df4_1.V.SDT_ww$mrl_imm_oth <- df4_1.V.SDT_ww$Other_Moral - df4_1.V.SDT_ww$Other_Immoral
e4_1.d.t.mrl_imm_oth.CI <- bootES(df4_1.V.SDT_ww$mrl_imm_oth,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.mrl_imm_oth  <- round(as.numeric(e4_1.d.t.mrl_imm_oth[[1]]),3)
e4_1.d.df4_1.mrl_imm_oth  <- as.numeric(e4_1.d.t.mrl_imm_oth[[2]])
e4_1.d.pvalue.mrl_imm_oth.adj <- p.adjust(as.numeric(e4_1.d.t.mrl_imm_oth[[3]],"bonferroni",3))
e4_1.d.cohens.mrl_imm_oth <- round(e4_1.d.t.mrl_imm_oth.CI[[1]],4) 
e4_1.d.CI.L.mrl_imm_oth <- round(e4_1.d.t.mrl_imm_oth.CI[[12]][1],4)
e4_1.d.CI.H.mrl_imm_oth <- round(e4_1.d.t.mrl_imm_oth.CI[[12]][2],4)

# moral other vs. Neutral other
e4_1.d.t.mrl_ave_oth <- t.test(df4_1.V.SDT_ww$Other_Moral,df4_1.V.SDT_ww$Other_Neutral,paired = TRUE)
df4_1.V.SDT_ww$mrl_ave_oth <- df4_1.V.SDT_ww$Other_Moral - df4_1.V.SDT_ww$Other_Neutral
e4_1.d.t.mrl_ave_oth.CI <- bootES(df4_1.V.SDT_ww$mrl_ave_oth,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.mrl_ave_oth  <- round(as.numeric(e4_1.d.t.mrl_ave_oth[[1]]),3)
e4_1.d.df4_1.mrl_ave_oth  <- as.numeric(e4_1.d.t.mrl_ave_oth[[2]])
e4_1.d.pvalue.mrl_ave_oth.adj <- p.adjust(as.numeric(e4_1.d.t.mrl_ave_oth[[3]],"bonferroni",3))
e4_1.d.cohens.mrl_ave_oth <- round(e4_1.d.t.mrl_ave_oth.CI[[1]],4) 
e4_1.d.CI.L.mrl_ave_oth <- round(e4_1.d.t.mrl_ave_oth.CI[[12]][1],4)
e4_1.d.CI.H.mrl_ave_oth <- round(e4_1.d.t.mrl_ave_oth.CI[[12]][2],4)

# Immoral other vs. Average other
e4_1.d.t.imm_ave_oth <- t.test(df4_1.V.SDT_ww$Other_Immoral,df4_1.V.SDT_ww$Other_Neutral,paired = TRUE)
df4_1.V.SDT_ww$imm_ave_oth <- df4_1.V.SDT_ww$Other_Immoral - df4_1.V.SDT_ww$Other_Neutral
e4_1.d.t.imm_ave_oth.CI <- bootES(df4_1.V.SDT_ww$imm_ave_oth,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.imm_ave_oth  <- round(as.numeric(e4_1.d.t.imm_ave_oth[[1]]),3)
e4_1.d.df4_1.imm_ave_oth  <- as.numeric(e4_1.d.t.imm_ave_oth[[2]])
e4_1.d.pvalue.imm_ave_oth.adj <- p.adjust(as.numeric(e4_1.d.t.imm_ave_oth[[3]],"bonferroni",3))
e4_1.d.cohens.imm_ave_oth <- round(e4_1.d.t.imm_ave_oth.CI[[1]],4) 
e4_1.d.CI.L.imm_ave_oth <- round(e4_1.d.t.imm_ave_oth.CI[[12]][1],4)
e4_1.d.CI.H.imm_ave_oth <- round(e4_1.d.t.imm_ave_oth.CI[[12]][2],4)

# moral self vs moral other
e4_1.d.t.slf_oth_mrl <- t.test(df4_1.V.SDT_ww$Self_Moral,df4_1.V.SDT_ww$Other_Moral,paired = TRUE)
df4_1.V.SDT_ww$slf_oth_mrl <- df4_1.V.SDT_ww$Self_Moral - df4_1.V.SDT_ww$Other_Moral
e4_1.d.t.slf_oth_mrl.CI <- bootES(df4_1.V.SDT_ww$slf_oth_mrl,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.slf_oth_mrl  <- round(as.numeric(e4_1.d.t.slf_oth_mrl[[1]]),3)
e4_1.d.df4_1.slf_oth_mrl  <- as.numeric(e4_1.d.t.slf_oth_mrl[[2]])
e4_1.d.pvalue.slf_oth_mrl.adj <- p.adjust(as.numeric(e4_1.d.t.slf_oth_mrl[[3]],"bonferroni",3))
e4_1.d.cohens.slf_oth_mrl <- round(e4_1.d.t.slf_oth_mrl.CI[[1]],4) 
e4_1.d.CI.L.slf_oth_mrl <- round(e4_1.d.t.slf_oth_mrl.CI[[12]][1],4)
e4_1.d.CI.H.slf_oth_mrl <- round(e4_1.d.t.slf_oth_mrl.CI[[12]][2],4)

# Average self vs. Average other
e4_1.d.t.slf_oth_ave <- t.test(df4_1.V.SDT_ww$Self_Neutral,df4_1.V.SDT_ww$Other_Neutral,paired = TRUE)
df4_1.V.SDT_ww$slf_oth_ave <- df4_1.V.SDT_ww$Self_Neutral - df4_1.V.SDT_ww$Other_Neutral
e4_1.d.t.slf_oth_ave.CI <- bootES(df4_1.V.SDT_ww$slf_oth_ave,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.slf_oth_ave  <- round(as.numeric(e4_1.d.t.slf_oth_ave[[1]]),3)
e4_1.d.df4_1.slf_oth_ave  <- as.numeric(e4_1.d.t.slf_oth_ave[[2]])
e4_1.d.pvalue.slf_oth_ave.adj <- p.adjust(as.numeric(e4_1.d.t.slf_oth_ave[[3]],"bonferroni",3))
e4_1.d.cohens.slf_oth_ave <- round(e4_1.d.t.slf_oth_ave.CI[[1]],4) 
e4_1.d.CI.L.slf_oth_ave <- round(e4_1.d.t.slf_oth_ave.CI[[12]][1],4)
e4_1.d.CI.H.slf_oth_ave <- round(e4_1.d.t.slf_oth_ave.CI[[12]][2],4)

# Immoral self vs. Immoral other
e4_1.d.t.slf_oth_imm <- t.test(df4_1.V.SDT_ww$Self_Immoral,df4_1.V.SDT_ww$Other_Immoral,paired = TRUE)
df4_1.V.SDT_ww$slf_oth_imm <- df4_1.V.SDT_ww$Self_Immoral - df4_1.V.SDT_ww$Other_Immoral
e4_1.d.t.slf_oth_imm.CI <- bootES(df4_1.V.SDT_ww$slf_oth_imm,R = 10000, effect.type = "cohens.d")

e4_1.d.tvalue.slf_oth_imm  <- round(as.numeric(e4_1.d.t.slf_oth_imm[[1]]),3)
e4_1.d.df4_1.slf_oth_imm <- as.numeric(e4_1.d.t.slf_oth_imm[[2]])
e4_1.d.pvalue.slf_oth_imm.adj <- p.adjust(as.numeric(e4_1.d.t.slf_oth_imm[[3]],"bonferroni",3))
e4_1.d.cohens.slf_oth_imm <- round(e4_1.d.t.slf_oth_imm.CI[[1]],4) 
e4_1.d.CI.L.slf_oth_imm <- round(e4_1.d.t.slf_oth_imm.CI[[12]][1],4)
e4_1.d.CI.H.slf_oth_imm <- round(e4_1.d.t.slf_oth_imm.CI[[12]][2],4)

## plot and save the results of d'
df4_1.V.SDT.sum <- summarySE(df4_1.V.SDT_w,measurevar = 'dprime',groupvars = c('Morality','Identity'))
df4_1.V.SDT.sum$Morality  <- factor(df4_1.V.SDT.sum$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4_1.V.SDT.sum$Identity  <- factor(df4_1.V.SDT.sum$Identity, levels=c("Self", "Other"))

e4_1.d.mean.moralself <- round(df4_1.V.SDT.sum$dprime[df4_1.V.SDT.sum$Morality == 'Moral' & df4_1.V.SDT.sum$Identity == 'self'],3)
e4_1.d.sd.moralself <- round(df4_1.V.SDT.sum$sd[df4_1.V.SDT.sum$Morality == 'Moral' & df4_1.V.SDT.sum$Identity == 'self'],3)
e4_1.d.mean.immoralself <- round(df4_1.V.SDT.sum$dprime[df4_1.V.SDT.sum$Morality == 'Immoral' & df4_1.V.SDT.sum$Identity == 'self'],3)
e4_1.d.sd.immoralself <- round(df4_1.V.SDT.sum$sd[df4_1.V.SDT.sum$Morality == 'Immoral' & df4_1.V.SDT.sum$Identity == 'self'],3)
e4_1.d.mean.aveself <- round(df4_1.V.SDT.sum$dprime[df4_1.V.SDT.sum$Morality == 'Neutral' & df4_1.V.SDT.sum$Identity == 'self'],3)
e4_1.d.sd.aveself <- round(df4_1.V.SDT.sum$sd[df4_1.V.SDT.sum$Morality == 'Neutral' & df4_1.V.SDT.sum$Identity == 'self'],3)

e4_1.d.mean.moralother <- round(df4_1.V.SDT.sum$dprime[df4_1.V.SDT.sum$Morality == 'Moral' & df4_1.V.SDT.sum$Identity == 'other'],3)
e4_1.d.sd.moralother <- round(df4_1.V.SDT.sum$sd[df4_1.V.SDT.sum$Morality == 'Moral' & df4_1.V.SDT.sum$Identity == 'other'],3)
e4_1.d.mean.immoralother <- round(df4_1.V.SDT.sum$dprime[df4_1.V.SDT.sum$Morality == 'Immoral' & df4_1.V.SDT.sum$Identity == 'other'],3)
e4_1.d.sd.immoralother <- round(df4_1.V.SDT.sum$sd[df4_1.V.SDT.sum$Morality == 'Immoral' & df4_1.V.SDT.sum$Identity == 'other'],3)
e4_1.d.mean.aveother <- round(df4_1.V.SDT.sum$dprime[df4_1.V.SDT.sum$Morality == 'Neutral' & df4_1.V.SDT.sum$Identity == 'other'],3)
e4_1.d.sd.aveother <- round(df4_1.V.SDT.sum$sd[df4_1.V.SDT.sum$Morality == 'Neutral' & df4_1.V.SDT.sum$Identity == 'other'],3)


# effect size and variance (need to revised further)
e2.d.es.mrl_imm <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.im, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.im, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.mrl_imm)

e2.d.es.mrl_ave <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.av, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.mrl_ave)

e2.d.es.imm_ave <- d.sgpp(m.1 = e2.d.mean.im, m.2 = e2.d.mean.av, sd.1=e2.d.sd.im,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.imm_ave)


# plot the results of dprime, way 1
e4_1.p_dprime1 <- ggplot(data = df4_1.V.SDT.sum,aes(y = dprime, x = Identity, group = Morality,shape = Morality, fill = Morality)) +
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
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter. 
  #theme_classic()
  apatheme +
  theme(axis.text = element_text (size = 20, color = 'black')) + 
  theme(axis.title = element_text (size = 20)) + 
  theme(plot.title = element_text(size = 20)) +
  theme(legend.text = element_text(size =20)) +
  theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
  theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neut.','Imm. '))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

# plot the results of dprime, way 2
e4_1.p_dprime2 <- ggplot(data = df4_1.V.SDT.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
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

# ggsave('dprime_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot

## doing the analysis for RT ####
## plot density of each subject's RT and save them individually
subNo <- unique(df4_1.V$Subject)

## doing the analysis for RT ####
df4_1.V.RT <- df4_1.V[df4_1.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df4_1.V.RT.subj <- summarySEwithin(df4_1.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),idvar = 'Subject', na.rm = TRUE)
df4_1.V.RT.grand <- summarySE(df4_1.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)
df4_1.V.RT.grand$Morality  <- factor(df4_1.V.RT.grand$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4_1.V.RT.grand$Identity  <- factor(df4_1.V.RT.grand$Identity, levels=c("Self", "Other"))

df4_1.V.RT_match <- df4_1.V.RT[df4_1.V.RT$Matchness == "Match",]
df4_1.V.RT_nonmatch <- df4_1.V.RT[df4_1.V.RT$Matchness == "Non-Match",]
df4_1.V.RT_match.self <- df4_1.V.RT_match[df4_1.V.RT_match$Identity == 'Self',]
df4_1.V.RT_match.other <- df4_1.V.RT_match[df4_1.V.RT_match$Identity == 'Other',]


## check the effect of site
e4_1.rt_anova_site <- ezANOVA(df4_1.V.RT,dv = RT, wid = Subject, within=.(Matchness,Morality,Identity),within_full=.(Matchness,Identity,Morality),
                                    between = .(Location), type=3)
# analyze the data
e4_1.rt_anova <- ezANOVA(df4_1.V.RT,dv = RT, wid = Subject, within=.(Matchness,Morality,Identity),within_full=.(Matchness,Morality,Identity), type=3)
e4_1.rt_anova.Match <- ezANOVA(df4_1.V.RT_match,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)
e4_1.rt_anova.nonmatch <- ezANOVA(df4_1.V.RT_nonmatch,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)

e4_1.rt_anova.Match.self <- ezANOVA(df4_1.V.RT_match.self,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
e4_1.rt_anova.Match.other <- ezANOVA(df4_1.V.RT_match.other,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
## t-test 
df4_1.V.RT.subj_w <- dcast(df4_1.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 

e4_1.rt.t.m.mrl_imm_slf <- t.test(df4_1.V.RT.subj_w$Match_Self_Moral,df4_1.V.RT.subj_w$Match_Self_Immoral,paired = TRUE)
df4_1.V.RT.subj_w$m.mrl_imm_slf <- df4_1.V.RT.subj_w$Match_Self_Moral - df4_1.V.RT.subj_w$Match_Self_Immoral
e4_1.rt.t.m.mrl_imm_slf.CI <- bootES(df4_1.V.RT.subj_w$m.mrl_imm_slf, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.mrl_imm_slf  <- round(as.numeric(e4_1.rt.t.m.mrl_imm_slf [[1]]),3)
e4_1.rt.df4_1.mrl_imm_slf  <- as.numeric(e4_1.rt.t.m.mrl_imm_slf [[2]])
e4_1.rt.pvalue.mrl_imm_slf.adj <- p.adjust(as.numeric(e4_1.rt.t.m.mrl_imm_slf [[3]],"bonferroni",3))
e4_1.rt.cohens.mrl_imm_slf <- round(e4_1.rt.t.m.mrl_imm_slf.CI[[1]],4) 
e4_1.rt.CI.L.mrl_imm_slf <- round(e4_1.rt.t.m.mrl_imm_slf.CI[[12]][1],4)
e4_1.rt.CI.H.mrl_imm_slf <- round(e4_1.rt.t.m.mrl_imm_slf.CI[[12]][2],4)


e4_1.rt.t.m.mrl_ave_slf <- t.test(df4_1.V.RT.subj_w$Match_Self_Moral,df4_1.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4_1.V.RT.subj_w$m.mrl_ave_slf <- df4_1.V.RT.subj_w$Match_Self_Moral - df4_1.V.RT.subj_w$Match_Self_Neutral
e4_1.rt.t.m.mrl_ave_slf.CI <- bootES(df4_1.V.RT.subj_w$m.mrl_ave_slf, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.mrl_ave_slf  <- round(as.numeric(e4_1.rt.t.m.mrl_ave_slf [[1]]),3)
e4_1.rt.df4_1.mrl_ave_slf  <- as.numeric(e4_1.rt.t.m.mrl_ave_slf[[2]])
e4_1.rt.pvalue.mrl_ave_slf.adj <- p.adjust(as.numeric(e4_1.rt.t.m.mrl_ave_slf[[3]],"bonferroni",3))
e4_1.rt.cohens.mrl_ave_slf <- round(e4_1.rt.t.m.mrl_ave_slf.CI[[1]],4) 
e4_1.rt.CI.L.mrl_ave_slf <- round(e4_1.rt.t.m.mrl_ave_slf.CI[[12]][1],4)
e4_1.rt.CI.H.mrl_ave_slf <- round(e4_1.rt.t.m.mrl_ave_slf.CI[[12]][2],4)


e4_1.rt.t.m.imm_ave_slf <- t.test(df4_1.V.RT.subj_w$Match_Self_Immoral,df4_1.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4_1.V.RT.subj_w$m.imm_ave_slf <- df4_1.V.RT.subj_w$Match_Self_Immoral - df4_1.V.RT.subj_w$Match_Self_Neutral
e4_1.rt.t.m.imm_ave_slf.CI <- bootES(df4_1.V.RT.subj_w$m.imm_ave_slf, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.imm_ave_slf  <- round(as.numeric(e4_1.rt.t.m.imm_ave_slf[[1]]),3)
e4_1.rt.df4_1.imm_ave_slf  <- as.numeric(e4_1.rt.t.m.imm_ave_slf[[2]])
e4_1.rt.pvalue.imm_ave_slf.adj <- p.adjust(as.numeric(e4_1.rt.t.m.imm_ave_slf[[3]],"bonferroni",3))
e4_1.rt.cohens.imm_ave_slf <- round(e4_1.rt.t.m.imm_ave_slf.CI [[1]],4) 
e4_1.rt.CI.L.imm_ave_slf <- round(e4_1.rt.t.m.imm_ave_slf.CI[[12]][1],4)
e4_1.rt.CI.H.imm_ave_slf <- round(e4_1.rt.t.m.imm_ave_slf.CI[[12]][2],4)

e4_1.rt.t.m.mrl_imm_oth <- t.test(df4_1.V.RT.subj_w$Match_Self_Moral,df4_1.V.RT.subj_w$Match_Self_Immoral,paired = TRUE)
df4_1.V.RT.subj_w$m.mrl_imm_oth <- df4_1.V.RT.subj_w$Match_Self_Moral - df4_1.V.RT.subj_w$Match_Self_Immoral
e4_1.rt.t.m.mrl_imm_oth.CI <- bootES(df4_1.V.RT.subj_w$m.mrl_imm_oth, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.mrl_imm_oth  <- round(as.numeric(e4_1.rt.t.m.mrl_imm_oth [[1]]),3)
e4_1.rt.df4_1.mrl_imm_oth  <- as.numeric(e4_1.rt.t.m.mrl_imm_oth [[2]])
e4_1.rt.pvalue.mrl_imm_oth.adj <- p.adjust(as.numeric(e4_1.rt.t.m.mrl_imm_oth [[3]],"bonferroni",3))
e4_1.rt.cohens.mrl_imm_oth <- round(e4_1.rt.t.m.mrl_imm_oth.CI [[1]],4) 
e4_1.rt.CI.L.mrl_imm_oth <- round(e4_1.rt.t.m.mrl_imm_oth.CI [[12]][1],4)
e4_1.rt.CI.H.mrl_imm_oth <- round(e4_1.rt.t.m.mrl_imm_oth.CI [[12]][2],4)


e4_1.rt.t.m.mrl_ave_oth <- t.test(df4_1.V.RT.subj_w$Match_Self_Moral,df4_1.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4_1.V.RT.subj_w$m.mrl_ave_oth <- df4_1.V.RT.subj_w$Match_Self_Moral - df4_1.V.RT.subj_w$Match_Self_Neutral
e4_1.rt.t.m.mrl_ave_oth.CI <- bootES(df4_1.V.RT.subj_w$m.mrl_ave_oth, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.mrl_ave_oth  <- round(as.numeric(e4_1.rt.t.m.mrl_ave_oth [[1]]),3)
e4_1.rt.df4_1.mrl_ave_oth  <- as.numeric(e4_1.rt.t.m.mrl_ave_oth[[2]])
e4_1.rt.pvalue.mrl_ave_oth.adj <- p.adjust(as.numeric(e4_1.rt.t.m.mrl_ave_oth[[3]],"bonferroni",3))
e4_1.rt.cohens.mrl_ave_oth <- round(e4_1.rt.t.m.mrl_ave_oth.CI [[1]],4) 
e4_1.rt.CI.L.mrl_ave_oth <- round(e4_1.rt.t.m.mrl_ave_oth.CI [[12]][1],4)
e4_1.rt.CI.H.mrl_ave_oth <- round(e4_1.rt.t.m.mrl_ave_oth.CI [[12]][2],4)


e4_1.rt.t.m.imm_ave_oth <- t.test(df4_1.V.RT.subj_w$Match_Self_Immoral,df4_1.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4_1.V.RT.subj_w$m.imm_ave_oth <- df4_1.V.RT.subj_w$Match_Self_Immoral - df4_1.V.RT.subj_w$Match_Self_Neutral
e4_1.rt.t.m.imm_ave_oth.CI <- bootES(df4_1.V.RT.subj_w$m.imm_ave_oth, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.imm_ave_oth  <- round(as.numeric(e4_1.rt.t.m.imm_ave_oth[[1]]),3)
e4_1.rt.df4_1.imm_ave_oth  <- as.numeric(e4_1.rt.t.m.imm_ave_oth[[2]])
e4_1.rt.pvalue.imm_ave_oth.adj <- p.adjust(as.numeric(e4_1.rt.t.m.imm_ave_oth[[3]],"bonferroni",3))
e4_1.rt.cohens.imm_ave_oth <- round(e4_1.rt.t.m.imm_ave_oth.CI[[1]],4) 
e4_1.rt.CI.L.imm_ave_oth <- round(e4_1.rt.t.m.imm_ave_oth.CI[[12]][1],4)
e4_1.rt.CI.H.imm_ave_oth <- round(e4_1.rt.t.m.imm_ave_oth.CI[[12]][2],4)


e4_1.rt.t.m.slf_oth_mrl <- t.test(df4_1.V.RT.subj_w$Match_Self_Moral,df4_1.V.RT.subj_w$Match_Other_Moral,paired = TRUE)
df4_1.V.RT.subj_w$m.slf_oth_mrl <- df4_1.V.RT.subj_w$Match_Self_Moral - df4_1.V.RT.subj_w$Match_Other_Moral
e4_1.rt.t.m.slf_oth_mrl.CI <- bootES(df4_1.V.RT.subj_w$m.slf_oth_mrl, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.slf_oth_mrl  <- round(as.numeric(e4_1.rt.t.m.slf_oth_mrl[[1]]),3)
e4_1.rt.df4_1.slf_oth_mrl  <- as.numeric(e4_1.rt.t.m.slf_oth_mrl[[2]])
e4_1.rt.pvalue.slf_oth_mrl.adj <- p.adjust(as.numeric(e4_1.rt.t.m.slf_oth_mrl[[3]],"bonferroni",3))
e4_1.rt.cohens.slf_oth_mrl <- round(e4_1.rt.t.m.slf_oth_mrl.CI[[1]],4) 
e4_1.rt.CI.L.slf_oth_mrl <- round(e4_1.rt.t.m.slf_oth_mrl.CI[[12]][1],4)
e4_1.rt.CI.H.slf_oth_mrl <- round(e4_1.rt.t.m.slf_oth_mrl.CI[[12]][2],4)

e4_1.rt.t.m.slf_oth_imm <- t.test(df4_1.V.RT.subj_w$Match_Self_Immoral,df4_1.V.RT.subj_w$Match_Other_Immoral,paired = TRUE)
df4_1.V.RT.subj_w$m.slf_oth_imm <- df4_1.V.RT.subj_w$Match_Self_Immoral - df4_1.V.RT.subj_w$Match_Other_Immoral
e4_1.rt.t.m.slf_oth_imm.CI <- bootES(df4_1.V.RT.subj_w$m.slf_oth_imm, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.slf_oth_imm  <- round(as.numeric(e4_1.rt.t.m.slf_oth_imm[[1]]),3)
e4_1.rt.df4_1.slf_oth_imm  <- as.numeric(e4_1.rt.t.m.slf_oth_imm[[2]])
e4_1.rt.pvalue.slf_oth_imm.adj <- p.adjust(as.numeric(e4_1.rt.t.m.slf_oth_imm[[3]],"bonferroni",3))
e4_1.rt.cohens.slf_oth_imm <- round(e4_1.rt.t.m.slf_oth_imm.CI[[1]],4) 
e4_1.rt.CI.L.slf_oth_imm <- round(e4_1.rt.t.m.slf_oth_imm.CI[[12]][1],4)
e4_1.rt.CI.H.slf_oth_imm <- round(e4_1.rt.t.m.slf_oth_imm.CI[[12]][2],4)


e4_1.rt.t.m.slf_oth_ave <- t.test(df4_1.V.RT.subj_w$Match_Self_Neutral,df4_1.V.RT.subj_w$Match_Other_Neutral,paired = TRUE)
df4_1.V.RT.subj_w$m.slf_oth_ave <- df4_1.V.RT.subj_w$Match_Self_Neutral - df4_1.V.RT.subj_w$Match_Other_Neutral
e4_1.rt.t.m.slf_oth_ave.CI <- bootES(df4_1.V.RT.subj_w$m.slf_oth_ave, R = 5000,effect.type = "cohens.d")

e4_1.rt.tvalue.slf_oth_ave  <- round(as.numeric(e4_1.rt.t.m.slf_oth_ave[[1]]),3)
e4_1.rt.df4_1.slf_oth_ave  <- as.numeric(e4_1.rt.t.m.slf_oth_ave[[2]])
e4_1.rt.pvalue.slf_oth_ave.adj <- p.adjust(as.numeric(e4_1.rt.t.m.slf_oth_ave[[3]],"bonferroni",3))
e4_1.rt.cohens.slf_oth_ave <- round(e4_1.rt.t.m.slf_oth_ave.CI[[1]],4) 
e4_1.rt.CI.L.slf_oth_ave <- round(e4_1.rt.t.m.slf_oth_ave.CI[[12]][1],4)
e4_1.rt.CI.H.slf_oth_ave <- round(e4_1.rt.t.m.slf_oth_ave.CI[[12]][2],4)

df4_1.V.RT.grand.Match <- df4_1.V.RT.grand[df4_1.V.RT.grand$Matchness == "Match",]
df4_1.V.RT.grand.Match$Morality  <- factor(df4_1.V.RT.grand.Match$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4_1.V.RT.grand.Match$Identity  <- factor(df4_1.V.RT.grand.Match$Identity, levels=c("Self", "Other"))


e4_1.rt.mean.moralself <- round(df4_1.V.RT.grand.Match$RT[df4_1.V.RT.grand.Match$Morality == 'Moral' & df4_1.V.RT.grand.Match$Identity == 'self'],0)
e4_1.rt.sd.moralself <- round(df4_1.V.RT.grand.Match$sd[df4_1.V.RT.grand.Match$Morality == 'Moral' & df4_1.V.RT.grand.Match$Identity == 'self'],0)
e4_1.rt.mean.immoralself <- round(df4_1.V.RT.grand.Match$RT[df4_1.V.RT.grand.Match$Morality == 'Immoral' & df4_1.V.RT.grand.Match$Identity == 'self'],0)
e4_1.rt.sd.immoralself <- round(df4_1.V.RT.grand.Match$sd[df4_1.V.RT.grand.Match$Morality == 'Immoral' & df4_1.V.RT.grand.Match$Identity == 'self'],0)
e4_1.rt.mean.aveself <- round(df4_1.V.RT.grand.Match$RT[df4_1.V.RT.grand.Match$Morality == 'Neutral' & df4_1.V.RT.grand.Match$Identity == 'self'],0)
e4_1.rt.sd.aveself <- round(df4_1.V.RT.grand.Match$sd[df4_1.V.RT.grand.Match$Morality == 'Neutral' & df4_1.V.RT.grand.Match$Identity == 'self'],0)

e4_1.rt.mean.moralother <- round(df4_1.V.RT.grand.Match$RT[df4_1.V.RT.grand.Match$Morality == 'Moral' & df4_1.V.RT.grand.Match$Identity == 'other'],0)
e4_1.rt.sd.moralother <- round(df4_1.V.RT.grand.Match$sd[df4_1.V.RT.grand.Match$Morality == 'Moral' & df4_1.V.RT.grand.Match$Identity == 'other'],0)
e4_1.rt.mean.immoralother <- round(df4_1.V.RT.grand.Match$RT[df4_1.V.RT.grand.Match$Morality == 'Immoral' & df4_1.V.RT.grand.Match$Identity == 'other'],0)
e4_1.rt.sd.immoralother <- round(df4_1.V.RT.grand.Match$sd[df4_1.V.RT.grand.Match$Morality == 'Immoral' & df4_1.V.RT.grand.Match$Identity == 'other'],0)
e4_1.rt.mean.aveother <- round(df4_1.V.RT.grand.Match$RT[df4_1.V.RT.grand.Match$Morality == 'Neutral' & df4_1.V.RT.grand.Match$Identity == 'other'],0)
e4_1.rt.sd.aveother <- round(df4_1.V.RT.grand.Match$sd[df4_1.V.RT.grand.Match$Morality == 'Neutral' & df4_1.V.RT.grand.Match$Identity == 'other'],0)

e4_1.p_rt1 <- ggplot(data = df4_1.V.RT.grand.Match, aes(x=Identity,y=RT,group=Morality,shape = Morality,fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Identity") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800)) +
  scale_y_continuous(breaks=seq(500,800,50),expand = c(0, 0)) +
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
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
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Moral ",'Neut.','Imm. '))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))


e4_1.p_rt2 <- ggplot(data = df4_1.V.RT.grand.Match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Moral valence") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800))+
  #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
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
multiplot(e4_1.p_dprime1,e4_1.p_rt1,cols = 2)
dev.off()

tiff(filename = "Figure 3.1. d prime and RTs of Experiment 3 (way 2).tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e4_1.p_dprime2,e4_1.p_rt2,cols = 2)
dev.off()