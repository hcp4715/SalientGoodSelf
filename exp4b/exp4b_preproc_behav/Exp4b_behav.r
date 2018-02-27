## this code is to analyze the data for exp1b, included these data were colleted at Wenzhou U in 201704

## initializing
source('Initial.r')

## load data and edite data
df4b_1 <- read.csv("rawdata_behav_exp4_2_2015.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b_2 <- read.csv("rawdata_behav_exp4_2_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
# rename column of later data to keep consistency
#colnames(df4b_2)[colnames(df4b_2) == 'Morality'] <- 'morality'
#colnames(df4b_2)[colnames(df4b_2) == 'Identity'] <- 'self'

# combine the data
df4b_1$Location <- "Tsinghua"
df4b_2$Location <- "Wenzhou"
df4b <- rbind(df4b_1,df4b_2)

# analysis new data separately first
#df4b <- read.csv("rawdata_behav_exp4_1_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

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
df4b$Matchness[df4b$Matchness == "No"]  <- "Non-Match"
df4b$Identity[df4b$Identity == 'self']  <- 'Self'
df4b$Identity[df4b$Identity == 'other'] <- 'Other'

#df4b$Matchness <- factor(df4b$Matchness, levels=c("Match", "Non-match")) # not factor before calculating the d-prime

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

# trials that should be excluded from analysis because of less than 200 ms
# note that this should be reconsidered when doinng DDM analysis
# and also the trials without response

excld.trials <- df4b.T[df4b.T$RT <= 200 & df4b.T$ACC == 1,] # correct response with less than 200ms
ratio.excld.trials <- nrow(excld.trials)/nrow(df4b.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df4b.acc.g <-  ddply(df4b.T,.(Subject), summarise,
                    N = length(ACC),
                    countN = sum(ACC),
                    ACC = sum(ACC)/length(ACC))
excld.sub <- df4b.acc.g$Subject[df4b.acc.g$ACC < 0.6]
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
numT.female        <- sum(df4b.T.basic$Sex == 'female');
numT.female_THU    <- sum(df4b.T.basic[df4b.T.basic$Location == 'Tsinghua',]$Sex == 'female');
numT.female_WZ     <- sum(df4b.T.basic[df4b.T.basic$Location == 'Wenzhou',]$Sex == 'female');
numT.male          <- sum(df4b.T.basic$Sex == 'male');
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

#### analyze the d prime ####
df4b.V$sdt <- NA
df4b.V_sdt <- df4b.V[!is.na(df4b.V$Target.RESP),] ## excluded the non-response trial when analyze the deprime.
df4b.V_miss <- df4b.V[is.na(df4b.V$Target.RESP),] ## the non-response trials
df4b.V_miss$sdt <- "miss"                           ## non-response trials regarded as miss
for (i in 1:nrow(df4b.V_sdt)){
  if (df4b.V_sdt$Target.RESP[i] == df4b.V_sdt$Target.CRESP[i] & (df4b.V_sdt$Matchness[i] == "Non-Match")){
    df4b.V_sdt$sdt[i] <- "CR"
  } else if (df4b.V_sdt$Target.RESP[i] != df4b.V_sdt$Target.CRESP[i] & (df4b.V_sdt$Matchness[i] == "Match")){
    df4b.V_sdt$sdt[i] <- "miss"
  } else if (df4b.V_sdt$Target.RESP[i] != df4b.V_sdt$Target.CRESP[i] & (df4b.V_sdt$Matchness[i] == "Non-Match")){
    df4b.V_sdt$sdt[i] <- "FA"
  }
  else if (df4b.V_sdt$Target.RESP[i] == df4b.V_sdt$Target.CRESP[i] & (df4b.V_sdt$Matchness[i] == "Match")){
    df4b.V_sdt$sdt[i] <- "hit"
  }
}

# combine the two
df4b.V <- rbind(df4b.V_sdt,df4b.V_miss) 

# calculate the number of each for each condition
df4b.V.SDT <-  ddply(df4b.V,.(Subject,Morality, Identity,Location,sdt), summarise, N = length(sdt))
#df4b.V.SDT <-  ddply(df4b.V,.(Subject,Morality, Identity,sdt), summarise, N = length(sdt))

# long format to wide
#df4b.V.SDT_w <- dcast(df4b.V.SDT, Subject + Morality + Identity ~ sdt,value.var = "N")
df4b.V.SDT_w <- dcast(df4b.V.SDT, Subject + Morality + Identity + Location ~ sdt,value.var = "N")
df4b.V.SDT_w$miss[is.na(df4b.V.SDT_w$miss)] <- 0
df4b.V.SDT_w$FA[is.na(df4b.V.SDT_w$FA)] <- 0
df4b.V.SDT_w$hitR <- df4b.V.SDT_w$hit/(df4b.V.SDT_w$hit + df4b.V.SDT_w$miss)
df4b.V.SDT_w$faR <- df4b.V.SDT_w$FA/(df4b.V.SDT_w$FA + df4b.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df4b.V.SDT_w)){
  if (df4b.V.SDT_w$hitR[i] == 1){
    df4b.V.SDT_w$hitR[i] <- 1 - 1/(2*(df4b.V.SDT_w$hit[i] + df4b.V.SDT_w$miss[i]))
  }
}

for (i in 1:nrow(df4b.V.SDT_w)){
  if (df4b.V.SDT_w$faR[i] == 0){
    df4b.V.SDT_w$faR[i] <- 1/(2*(df4b.V.SDT_w$FA[i] + df4b.V.SDT_w$CR[i]))
  }
}

# calculate the d prime for each condition
df4b.V.SDT_w$dprime <- mapply(dprime,df4b.V.SDT_w$hitR,df4b.V.SDT_w$faR)
df4b.V.SDT_w.self <- df4b.V.SDT_w[df4b.V.SDT_w$Identity == 'Self',]
df4b.V.SDT_w.other <- df4b.V.SDT_w[df4b.V.SDT_w$Identity == 'Other',]

# check the effect of location for d prime
df4b.d_anova_site <- ezANOVA(df4b.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity), between = .(Location), type=3)

# anova for d prime with 2*2 design
df4b.d_anova <- ezANOVA(df4b.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity), type=3)
df4b.d_anova.self <- ezANOVA(df4b.V.SDT_w.self, dv = dprime, wid = Subject, within=.(Morality), type=3)
df4b.d_anova.other <- ezANOVA(df4b.V.SDT_w.other, dv = dprime, wid = Subject, within=.(Morality), type=3)
#print(d_anova1)

# change dprime data from long format to wide
df4b.V.SDT_ww <- dcast(df4b.V.SDT_w, Subject ~ Identity + Morality ,value.var = "dprime")
df4b.V.SDT_ww_r <- df4b.V.SDT_ww

# t-test
# df4b.V.SDT_w$condition <- paste(df4b.V.SDT_w$Morality,df4b.V.SDT_w$Identity,'_')
# pairwise.t.test(df4b.V.SDT_w$dprime,df4b.V.SDT_w$conidtion,p.adj = "bonf")

# moral self vs Immoral self
df4b.d.t.mrl_imm_slf <- t.test(df4b.V.SDT_ww$Self_Moral,df4b.V.SDT_ww$Self_Immoral,paired = TRUE)
df4b.V.SDT_ww$mrl_imm_slf <- df4b.V.SDT_ww$Self_Moral - df4b.V.SDT_ww$Self_Immoral
df4b.d.t.mrl_imm_slf.CI <- bootES(df4b.V.SDT_ww$mrl_imm_slf,R = 20000, effect.type = "cohens.d")

df4b.d.tvalue.mrl_imm_slf <- round(as.numeric(df4b.d.t.mrl_imm_slf[[1]]),3)
df4b.d.df4b.mrl_imm_slf <- as.numeric(df4b.d.t.mrl_imm_slf[[2]])
df4b.d.pvalue.mrl_imm_slf.adj <- p.adjust(as.numeric(df4b.d.t.mrl_imm_slf[[3]],"bonferroni",3))
df4b.d.cohens.mrl_imm_slf <- round(df4b.d.t.mrl_imm_slf.CI[[1]],4) 
df4b.d.CI.L.mrl_imm_slf <- round(df4b.d.t.mrl_imm_slf.CI[[12]][1],4)
df4b.d.CI.H.mrl_imm_slf <- round(df4b.d.t.mrl_imm_slf.CI[[12]][2],4)

# moral self vs Average self
df4b.d.t.mrl_ave_slf <- t.test(df4b.V.SDT_ww$Self_Moral,df4b.V.SDT_ww$Self_Neutral,paired = TRUE)
df4b.V.SDT_ww$mrl_ave_slf <- df4b.V.SDT_ww$Self_Moral - df4b.V.SDT_ww$Self_Neutral
df4b.d.t.mrl_ave_slf.CI <- bootES(df4b.V.SDT_ww$mrl_ave_slf,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.mrl_ave_slf  <- round(as.numeric(df4b.d.t.mrl_ave_slf[[1]]),3)
df4b.d.df4b.mrl_ave_slf  <- as.numeric(df4b.d.t.mrl_ave_slf[[2]])
df4b.d.pvalue.mrl_ave_slf.adj <- p.adjust(as.numeric(df4b.d.t.mrl_ave_slf[[3]],"bonferroni",3))
df4b.d.cohens.mrl_ave_slf <- round(df4b.d.t.mrl_ave_slf.CI[[1]],4) 
df4b.d.CI.L.mrl_ave_slf <- round(df4b.d.t.mrl_ave_slf.CI[[12]][1],4)
df4b.d.CI.H.mrl_ave_slf <- round(df4b.d.t.mrl_ave_slf.CI[[12]][2],4)

# Immoral self vs. Average self
df4b.d.t.imm_ave_slf <- t.test(df4b.V.SDT_ww$Self_Immoral,df4b.V.SDT_ww$Self_Neutral,paired = TRUE)
df4b.V.SDT_ww$imm_ave_slf <- df4b.V.SDT_ww$Self_Immoral - df4b.V.SDT_ww$Self_Neutral
df4b.d.t.imm_ave_slf.CI <- bootES(df4b.V.SDT_ww$imm_ave_slf,R = 20000, effect.type = "cohens.d")

df4b.d.tvalue.imm_ave_slf  <- round(as.numeric(df4b.d.t.imm_ave_slf[[1]]),3)
df4b.d.df4b.imm_ave_slf  <- as.numeric(df4b.d.t.imm_ave_slf[[2]])
df4b.d.pvalue.imm_ave_slf.adj <- p.adjust(as.numeric(df4b.d.t.imm_ave_slf[[3]],"bonferroni",3))
df4b.d.cohens.imm_ave_slf <- round(df4b.d.t.imm_ave_slf.CI[[1]],4) 
df4b.d.CI.L.imm_ave_slf <- round(df4b.d.t.imm_ave_slf.CI[[12]][1],4)
df4b.d.CI.H.imm_ave_slf <- round(df4b.d.t.imm_ave_slf.CI[[12]][2],4)

# moral other vs. Immoral other
df4b.d.t.mrl_imm_oth <- t.test(df4b.V.SDT_ww$Other_Moral,df4b.V.SDT_ww$Other_Immoral,paired = TRUE)
df4b.V.SDT_ww$mrl_imm_oth <- df4b.V.SDT_ww$Other_Moral - df4b.V.SDT_ww$Other_Immoral
df4b.d.t.mrl_imm_oth.CI <- bootES(df4b.V.SDT_ww$mrl_imm_oth,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.mrl_imm_oth  <- round(as.numeric(df4b.d.t.mrl_imm_oth[[1]]),3)
df4b.d.df4b.mrl_imm_oth  <- as.numeric(df4b.d.t.mrl_imm_oth[[2]])
df4b.d.pvalue.mrl_imm_oth.adj <- p.adjust(as.numeric(df4b.d.t.mrl_imm_oth[[3]],"bonferroni",3))
df4b.d.cohens.mrl_imm_oth <- round(df4b.d.t.mrl_imm_oth.CI[[1]],4) 
df4b.d.CI.L.mrl_imm_oth <- round(df4b.d.t.mrl_imm_oth.CI[[12]][1],4)
df4b.d.CI.H.mrl_imm_oth <- round(df4b.d.t.mrl_imm_oth.CI[[12]][2],4)

# moral other vs. Neutral other
df4b.d.t.mrl_ave_oth <- t.test(df4b.V.SDT_ww$Other_Moral,df4b.V.SDT_ww$Other_Neutral,paired = TRUE)
df4b.V.SDT_ww$mrl_ave_oth <- df4b.V.SDT_ww$Other_Moral - df4b.V.SDT_ww$Other_Neutral
df4b.d.t.mrl_ave_oth.CI <- bootES(df4b.V.SDT_ww$mrl_ave_oth,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.mrl_ave_oth  <- round(as.numeric(df4b.d.t.mrl_ave_oth[[1]]),3)
df4b.d.df4b.mrl_ave_oth  <- as.numeric(df4b.d.t.mrl_ave_oth[[2]])
df4b.d.pvalue.mrl_ave_oth.adj <- p.adjust(as.numeric(df4b.d.t.mrl_ave_oth[[3]],"bonferroni",3))
df4b.d.cohens.mrl_ave_oth <- round(df4b.d.t.mrl_ave_oth.CI[[1]],4) 
df4b.d.CI.L.mrl_ave_oth <- round(df4b.d.t.mrl_ave_oth.CI[[12]][1],4)
df4b.d.CI.H.mrl_ave_oth <- round(df4b.d.t.mrl_ave_oth.CI[[12]][2],4)

# Immoral other vs. Average other
df4b.d.t.imm_ave_oth <- t.test(df4b.V.SDT_ww$Other_Immoral,df4b.V.SDT_ww$Other_Neutral,paired = TRUE)
df4b.V.SDT_ww$imm_ave_oth <- df4b.V.SDT_ww$Other_Immoral - df4b.V.SDT_ww$Other_Neutral
df4b.d.t.imm_ave_oth.CI <- bootES(df4b.V.SDT_ww$imm_ave_oth,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.imm_ave_oth  <- round(as.numeric(df4b.d.t.imm_ave_oth[[1]]),3)
df4b.d.df4b.imm_ave_oth  <- as.numeric(df4b.d.t.imm_ave_oth[[2]])
df4b.d.pvalue.imm_ave_oth.adj <- p.adjust(as.numeric(df4b.d.t.imm_ave_oth[[3]],"bonferroni",3))
df4b.d.cohens.imm_ave_oth <- round(df4b.d.t.imm_ave_oth.CI[[1]],4) 
df4b.d.CI.L.imm_ave_oth <- round(df4b.d.t.imm_ave_oth.CI[[12]][1],4)
df4b.d.CI.H.imm_ave_oth <- round(df4b.d.t.imm_ave_oth.CI[[12]][2],4)

# moral self vs moral other
df4b.d.t.slf_oth_mrl <- t.test(df4b.V.SDT_ww$Self_Moral,df4b.V.SDT_ww$Other_Moral,paired = TRUE)
df4b.V.SDT_ww$slf_oth_mrl <- df4b.V.SDT_ww$Self_Moral - df4b.V.SDT_ww$Other_Moral
df4b.d.t.slf_oth_mrl.CI <- bootES(df4b.V.SDT_ww$slf_oth_mrl,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.slf_oth_mrl  <- round(as.numeric(df4b.d.t.slf_oth_mrl[[1]]),3)
df4b.d.df4b.slf_oth_mrl  <- as.numeric(df4b.d.t.slf_oth_mrl[[2]])
df4b.d.pvalue.slf_oth_mrl.adj <- p.adjust(as.numeric(df4b.d.t.slf_oth_mrl[[3]],"bonferroni",3))
df4b.d.cohens.slf_oth_mrl <- round(df4b.d.t.slf_oth_mrl.CI[[1]],4) 
df4b.d.CI.L.slf_oth_mrl <- round(df4b.d.t.slf_oth_mrl.CI[[12]][1],4)
df4b.d.CI.H.slf_oth_mrl <- round(df4b.d.t.slf_oth_mrl.CI[[12]][2],4)

# Average self vs. Average other
df4b.d.t.slf_oth_ave <- t.test(df4b.V.SDT_ww$Self_Neutral,df4b.V.SDT_ww$Other_Neutral,paired = TRUE)
df4b.V.SDT_ww$slf_oth_ave <- df4b.V.SDT_ww$Self_Neutral - df4b.V.SDT_ww$Other_Neutral
df4b.d.t.slf_oth_ave.CI <- bootES(df4b.V.SDT_ww$slf_oth_ave,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.slf_oth_ave  <- round(as.numeric(df4b.d.t.slf_oth_ave[[1]]),3)
df4b.d.df4b.slf_oth_ave  <- as.numeric(df4b.d.t.slf_oth_ave[[2]])
df4b.d.pvalue.slf_oth_ave.adj <- p.adjust(as.numeric(df4b.d.t.slf_oth_ave[[3]],"bonferroni",3))
df4b.d.cohens.slf_oth_ave <- round(df4b.d.t.slf_oth_ave.CI[[1]],4) 
df4b.d.CI.L.slf_oth_ave <- round(df4b.d.t.slf_oth_ave.CI[[12]][1],4)
df4b.d.CI.H.slf_oth_ave <- round(df4b.d.t.slf_oth_ave.CI[[12]][2],4)

# Immoral self vs. Immoral other
df4b.d.t.slf_oth_imm <- t.test(df4b.V.SDT_ww$Self_Immoral,df4b.V.SDT_ww$Other_Immoral,paired = TRUE)
df4b.V.SDT_ww$slf_oth_imm <- df4b.V.SDT_ww$Self_Immoral - df4b.V.SDT_ww$Other_Immoral
df4b.d.t.slf_oth_imm.CI <- bootES(df4b.V.SDT_ww$slf_oth_imm,R = 10000, effect.type = "cohens.d")

df4b.d.tvalue.slf_oth_imm  <- round(as.numeric(df4b.d.t.slf_oth_imm[[1]]),3)
df4b.d.df4b.slf_oth_imm <- as.numeric(df4b.d.t.slf_oth_imm[[2]])
df4b.d.pvalue.slf_oth_imm.adj <- p.adjust(as.numeric(df4b.d.t.slf_oth_imm[[3]],"bonferroni",3))
df4b.d.cohens.slf_oth_imm <- round(df4b.d.t.slf_oth_imm.CI[[1]],4) 
df4b.d.CI.L.slf_oth_imm <- round(df4b.d.t.slf_oth_imm.CI[[12]][1],4)
df4b.d.CI.H.slf_oth_imm <- round(df4b.d.t.slf_oth_imm.CI[[12]][2],4)

## plot and save the results of d'
df4b.V.SDT.sum <- summarySE(df4b.V.SDT_w,measurevar = 'dprime',groupvars = c('Morality','Identity'))
df4b.V.SDT.sum$Morality  <- factor(df4b.V.SDT.sum$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4b.V.SDT.sum$Identity  <- factor(df4b.V.SDT.sum$Identity, levels=c("Self", "Other"))

df4b.d.mean.moralself <- round(df4b.V.SDT.sum$dprime[df4b.V.SDT.sum$Morality == 'Moral' & df4b.V.SDT.sum$Identity == 'self'],3)
df4b.d.sd.moralself <- round(df4b.V.SDT.sum$sd[df4b.V.SDT.sum$Morality == 'Moral' & df4b.V.SDT.sum$Identity == 'self'],3)
df4b.d.mean.immoralself <- round(df4b.V.SDT.sum$dprime[df4b.V.SDT.sum$Morality == 'Immoral' & df4b.V.SDT.sum$Identity == 'self'],3)
df4b.d.sd.immoralself <- round(df4b.V.SDT.sum$sd[df4b.V.SDT.sum$Morality == 'Immoral' & df4b.V.SDT.sum$Identity == 'self'],3)
df4b.d.mean.aveself <- round(df4b.V.SDT.sum$dprime[df4b.V.SDT.sum$Morality == 'Neutral' & df4b.V.SDT.sum$Identity == 'self'],3)
df4b.d.sd.aveself <- round(df4b.V.SDT.sum$sd[df4b.V.SDT.sum$Morality == 'Neutral' & df4b.V.SDT.sum$Identity == 'self'],3)

df4b.d.mean.moralother <- round(df4b.V.SDT.sum$dprime[df4b.V.SDT.sum$Morality == 'Moral' & df4b.V.SDT.sum$Identity == 'other'],3)
df4b.d.sd.moralother <- round(df4b.V.SDT.sum$sd[df4b.V.SDT.sum$Morality == 'Moral' & df4b.V.SDT.sum$Identity == 'other'],3)
df4b.d.mean.immoralother <- round(df4b.V.SDT.sum$dprime[df4b.V.SDT.sum$Morality == 'Immoral' & df4b.V.SDT.sum$Identity == 'other'],3)
df4b.d.sd.immoralother <- round(df4b.V.SDT.sum$sd[df4b.V.SDT.sum$Morality == 'Immoral' & df4b.V.SDT.sum$Identity == 'other'],3)
df4b.d.mean.aveother <- round(df4b.V.SDT.sum$dprime[df4b.V.SDT.sum$Morality == 'Neutral' & df4b.V.SDT.sum$Identity == 'other'],3)
df4b.d.sd.aveother <- round(df4b.V.SDT.sum$sd[df4b.V.SDT.sum$Morality == 'Neutral' & df4b.V.SDT.sum$Identity == 'other'],3)


# effect size and variance (need to revised further)
e2.d.es.mrl_imm <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.im, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.im, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.mrl_imm)

e2.d.es.mrl_ave <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.av, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.mrl_ave)

e2.d.es.imm_ave <- d.sgpp(m.1 = e2.d.mean.im, m.2 = e2.d.mean.av, sd.1=e2.d.sd.im,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.imm_ave)


# plot the results of dprime, way 1
df4b.p_dprime1 <- ggplot(data = df4b.V.SDT.sum,aes(y = dprime, x = Identity, group = Morality,shape = Morality, fill = Morality)) +
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
df4b.p_dprime2 <- ggplot(data = df4b.V.SDT.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
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
subNo <- unique(df4b.V$Subject)

## doing the analysis for RT ####
df4b.V.RT                 <- df4b.V[df4b.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df4b.V.RT.subj            <- summarySEwithin(df4b.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),
                                              idvar = 'Subject', na.rm = TRUE)
df4b.V.RT.grand           <- summarySE(df4b.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)
df4b.V.RT.grand$Morality  <- factor(df4b.V.RT.grand$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4b.V.RT.grand$Identity  <- factor(df4b.V.RT.grand$Identity, levels=c("Self", "Other"))

df4b.V.RT_match       <- df4b.V.RT[df4b.V.RT$Matchness == "Match",]
df4b.V.RT_nonmatch    <- df4b.V.RT[df4b.V.RT$Matchness == "Non-Match",]
df4b.V.RT_match.self  <- df4b.V.RT_match[df4b.V.RT_match$Identity == 'Self',]
df4b.V.RT_match.other <- df4b.V.RT_match[df4b.V.RT_match$Identity == 'Other',]


## check the effect of site
df4b.rt_anova_site <- ezANOVA(df4b.V.RT,dv = RT, wid = Subject, within=.(Matchness,Morality,Identity),within_full=.(Matchness,Identity,Morality),
                                    between = .(Location), type=3)
# analyze the data
df4b.rt_anova <- ezANOVA(df4b.V.RT,dv = RT, wid = Subject, within=.(Matchness,Morality,Identity),within_full=.(Matchness,Morality,Identity), type=3)
df4b.rt_anova.Match <- ezANOVA(df4b.V.RT_match,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)
df4b.rt_anova.nonmatch <- ezANOVA(df4b.V.RT_nonmatch,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)

df4b.rt_anova.Match.self <- ezANOVA(df4b.V.RT_match.self,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
df4b.rt_anova.Match.other <- ezANOVA(df4b.V.RT_match.other,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
## t-test 
df4b.V.RT.subj_w <- dcast(df4b.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 

df4b.rt.t.m.mrl_imm_slf <- t.test(df4b.V.RT.subj_w$Match_Self_Moral,df4b.V.RT.subj_w$Match_Self_Immoral,paired = TRUE)
df4b.V.RT.subj_w$m.mrl_imm_slf <- df4b.V.RT.subj_w$Match_Self_Moral - df4b.V.RT.subj_w$Match_Self_Immoral
df4b.rt.t.m.mrl_imm_slf.CI <- bootES(df4b.V.RT.subj_w$m.mrl_imm_slf, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.mrl_imm_slf  <- round(as.numeric(df4b.rt.t.m.mrl_imm_slf [[1]]),3)
df4b.rt.df4b.mrl_imm_slf  <- as.numeric(df4b.rt.t.m.mrl_imm_slf [[2]])
df4b.rt.pvalue.mrl_imm_slf.adj <- p.adjust(as.numeric(df4b.rt.t.m.mrl_imm_slf [[3]],"bonferroni",3))
df4b.rt.cohens.mrl_imm_slf <- round(df4b.rt.t.m.mrl_imm_slf.CI[[1]],4) 
df4b.rt.CI.L.mrl_imm_slf <- round(df4b.rt.t.m.mrl_imm_slf.CI[[12]][1],4)
df4b.rt.CI.H.mrl_imm_slf <- round(df4b.rt.t.m.mrl_imm_slf.CI[[12]][2],4)


df4b.rt.t.m.mrl_ave_slf <- t.test(df4b.V.RT.subj_w$Match_Self_Moral,df4b.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4b.V.RT.subj_w$m.mrl_ave_slf <- df4b.V.RT.subj_w$Match_Self_Moral - df4b.V.RT.subj_w$Match_Self_Neutral
df4b.rt.t.m.mrl_ave_slf.CI <- bootES(df4b.V.RT.subj_w$m.mrl_ave_slf, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.mrl_ave_slf  <- round(as.numeric(df4b.rt.t.m.mrl_ave_slf [[1]]),3)
df4b.rt.df4b.mrl_ave_slf  <- as.numeric(df4b.rt.t.m.mrl_ave_slf[[2]])
df4b.rt.pvalue.mrl_ave_slf.adj <- p.adjust(as.numeric(df4b.rt.t.m.mrl_ave_slf[[3]],"bonferroni",3))
df4b.rt.cohens.mrl_ave_slf <- round(df4b.rt.t.m.mrl_ave_slf.CI[[1]],4) 
df4b.rt.CI.L.mrl_ave_slf <- round(df4b.rt.t.m.mrl_ave_slf.CI[[12]][1],4)
df4b.rt.CI.H.mrl_ave_slf <- round(df4b.rt.t.m.mrl_ave_slf.CI[[12]][2],4)


df4b.rt.t.m.imm_ave_slf <- t.test(df4b.V.RT.subj_w$Match_Self_Immoral,df4b.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4b.V.RT.subj_w$m.imm_ave_slf <- df4b.V.RT.subj_w$Match_Self_Immoral - df4b.V.RT.subj_w$Match_Self_Neutral
df4b.rt.t.m.imm_ave_slf.CI <- bootES(df4b.V.RT.subj_w$m.imm_ave_slf, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.imm_ave_slf  <- round(as.numeric(df4b.rt.t.m.imm_ave_slf[[1]]),3)
df4b.rt.df4b.imm_ave_slf  <- as.numeric(df4b.rt.t.m.imm_ave_slf[[2]])
df4b.rt.pvalue.imm_ave_slf.adj <- p.adjust(as.numeric(df4b.rt.t.m.imm_ave_slf[[3]],"bonferroni",3))
df4b.rt.cohens.imm_ave_slf <- round(df4b.rt.t.m.imm_ave_slf.CI [[1]],4) 
df4b.rt.CI.L.imm_ave_slf <- round(df4b.rt.t.m.imm_ave_slf.CI[[12]][1],4)
df4b.rt.CI.H.imm_ave_slf <- round(df4b.rt.t.m.imm_ave_slf.CI[[12]][2],4)

df4b.rt.t.m.mrl_imm_oth <- t.test(df4b.V.RT.subj_w$Match_Self_Moral,df4b.V.RT.subj_w$Match_Self_Immoral,paired = TRUE)
df4b.V.RT.subj_w$m.mrl_imm_oth <- df4b.V.RT.subj_w$Match_Self_Moral - df4b.V.RT.subj_w$Match_Self_Immoral
df4b.rt.t.m.mrl_imm_oth.CI <- bootES(df4b.V.RT.subj_w$m.mrl_imm_oth, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.mrl_imm_oth  <- round(as.numeric(df4b.rt.t.m.mrl_imm_oth [[1]]),3)
df4b.rt.df4b.mrl_imm_oth  <- as.numeric(df4b.rt.t.m.mrl_imm_oth [[2]])
df4b.rt.pvalue.mrl_imm_oth.adj <- p.adjust(as.numeric(df4b.rt.t.m.mrl_imm_oth [[3]],"bonferroni",3))
df4b.rt.cohens.mrl_imm_oth <- round(df4b.rt.t.m.mrl_imm_oth.CI [[1]],4) 
df4b.rt.CI.L.mrl_imm_oth <- round(df4b.rt.t.m.mrl_imm_oth.CI [[12]][1],4)
df4b.rt.CI.H.mrl_imm_oth <- round(df4b.rt.t.m.mrl_imm_oth.CI [[12]][2],4)


df4b.rt.t.m.mrl_ave_oth <- t.test(df4b.V.RT.subj_w$Match_Self_Moral,df4b.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4b.V.RT.subj_w$m.mrl_ave_oth <- df4b.V.RT.subj_w$Match_Self_Moral - df4b.V.RT.subj_w$Match_Self_Neutral
df4b.rt.t.m.mrl_ave_oth.CI <- bootES(df4b.V.RT.subj_w$m.mrl_ave_oth, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.mrl_ave_oth  <- round(as.numeric(df4b.rt.t.m.mrl_ave_oth [[1]]),3)
df4b.rt.df4b.mrl_ave_oth  <- as.numeric(df4b.rt.t.m.mrl_ave_oth[[2]])
df4b.rt.pvalue.mrl_ave_oth.adj <- p.adjust(as.numeric(df4b.rt.t.m.mrl_ave_oth[[3]],"bonferroni",3))
df4b.rt.cohens.mrl_ave_oth <- round(df4b.rt.t.m.mrl_ave_oth.CI [[1]],4) 
df4b.rt.CI.L.mrl_ave_oth <- round(df4b.rt.t.m.mrl_ave_oth.CI [[12]][1],4)
df4b.rt.CI.H.mrl_ave_oth <- round(df4b.rt.t.m.mrl_ave_oth.CI [[12]][2],4)


df4b.rt.t.m.imm_ave_oth <- t.test(df4b.V.RT.subj_w$Match_Self_Immoral,df4b.V.RT.subj_w$Match_Self_Neutral,paired = TRUE)
df4b.V.RT.subj_w$m.imm_ave_oth <- df4b.V.RT.subj_w$Match_Self_Immoral - df4b.V.RT.subj_w$Match_Self_Neutral
df4b.rt.t.m.imm_ave_oth.CI <- bootES(df4b.V.RT.subj_w$m.imm_ave_oth, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.imm_ave_oth  <- round(as.numeric(df4b.rt.t.m.imm_ave_oth[[1]]),3)
df4b.rt.df4b.imm_ave_oth  <- as.numeric(df4b.rt.t.m.imm_ave_oth[[2]])
df4b.rt.pvalue.imm_ave_oth.adj <- p.adjust(as.numeric(df4b.rt.t.m.imm_ave_oth[[3]],"bonferroni",3))
df4b.rt.cohens.imm_ave_oth <- round(df4b.rt.t.m.imm_ave_oth.CI[[1]],4) 
df4b.rt.CI.L.imm_ave_oth <- round(df4b.rt.t.m.imm_ave_oth.CI[[12]][1],4)
df4b.rt.CI.H.imm_ave_oth <- round(df4b.rt.t.m.imm_ave_oth.CI[[12]][2],4)


df4b.rt.t.m.slf_oth_mrl <- t.test(df4b.V.RT.subj_w$Match_Self_Moral,df4b.V.RT.subj_w$Match_Other_Moral,paired = TRUE)
df4b.V.RT.subj_w$m.slf_oth_mrl <- df4b.V.RT.subj_w$Match_Self_Moral - df4b.V.RT.subj_w$Match_Other_Moral
df4b.rt.t.m.slf_oth_mrl.CI <- bootES(df4b.V.RT.subj_w$m.slf_oth_mrl, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.slf_oth_mrl  <- round(as.numeric(df4b.rt.t.m.slf_oth_mrl[[1]]),3)
df4b.rt.df4b.slf_oth_mrl  <- as.numeric(df4b.rt.t.m.slf_oth_mrl[[2]])
df4b.rt.pvalue.slf_oth_mrl.adj <- p.adjust(as.numeric(df4b.rt.t.m.slf_oth_mrl[[3]],"bonferroni",3))
df4b.rt.cohens.slf_oth_mrl <- round(df4b.rt.t.m.slf_oth_mrl.CI[[1]],4) 
df4b.rt.CI.L.slf_oth_mrl <- round(df4b.rt.t.m.slf_oth_mrl.CI[[12]][1],4)
df4b.rt.CI.H.slf_oth_mrl <- round(df4b.rt.t.m.slf_oth_mrl.CI[[12]][2],4)

df4b.rt.t.m.slf_oth_imm <- t.test(df4b.V.RT.subj_w$Match_Self_Immoral,df4b.V.RT.subj_w$Match_Other_Immoral,paired = TRUE)
df4b.V.RT.subj_w$m.slf_oth_imm <- df4b.V.RT.subj_w$Match_Self_Immoral - df4b.V.RT.subj_w$Match_Other_Immoral
df4b.rt.t.m.slf_oth_imm.CI <- bootES(df4b.V.RT.subj_w$m.slf_oth_imm, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.slf_oth_imm  <- round(as.numeric(df4b.rt.t.m.slf_oth_imm[[1]]),3)
df4b.rt.df4b.slf_oth_imm  <- as.numeric(df4b.rt.t.m.slf_oth_imm[[2]])
df4b.rt.pvalue.slf_oth_imm.adj <- p.adjust(as.numeric(df4b.rt.t.m.slf_oth_imm[[3]],"bonferroni",3))
df4b.rt.cohens.slf_oth_imm <- round(df4b.rt.t.m.slf_oth_imm.CI[[1]],4) 
df4b.rt.CI.L.slf_oth_imm <- round(df4b.rt.t.m.slf_oth_imm.CI[[12]][1],4)
df4b.rt.CI.H.slf_oth_imm <- round(df4b.rt.t.m.slf_oth_imm.CI[[12]][2],4)


df4b.rt.t.m.slf_oth_ave <- t.test(df4b.V.RT.subj_w$Match_Self_Neutral,df4b.V.RT.subj_w$Match_Other_Neutral,paired = TRUE)
df4b.V.RT.subj_w$m.slf_oth_ave <- df4b.V.RT.subj_w$Match_Self_Neutral - df4b.V.RT.subj_w$Match_Other_Neutral
df4b.rt.t.m.slf_oth_ave.CI <- bootES(df4b.V.RT.subj_w$m.slf_oth_ave, R = 5000,effect.type = "cohens.d")

df4b.rt.tvalue.slf_oth_ave  <- round(as.numeric(df4b.rt.t.m.slf_oth_ave[[1]]),3)
df4b.rt.df4b.slf_oth_ave  <- as.numeric(df4b.rt.t.m.slf_oth_ave[[2]])
df4b.rt.pvalue.slf_oth_ave.adj <- p.adjust(as.numeric(df4b.rt.t.m.slf_oth_ave[[3]],"bonferroni",3))
df4b.rt.cohens.slf_oth_ave <- round(df4b.rt.t.m.slf_oth_ave.CI[[1]],4) 
df4b.rt.CI.L.slf_oth_ave <- round(df4b.rt.t.m.slf_oth_ave.CI[[12]][1],4)
df4b.rt.CI.H.slf_oth_ave <- round(df4b.rt.t.m.slf_oth_ave.CI[[12]][2],4)

df4b.V.RT.grand.Match <- df4b.V.RT.grand[df4b.V.RT.grand$Matchness == "Match",]
df4b.V.RT.grand.Match$Morality  <- factor(df4b.V.RT.grand.Match$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order
df4b.V.RT.grand.Match$Identity  <- factor(df4b.V.RT.grand.Match$Identity, levels=c("Self", "Other"))


df4b.rt.mean.moralself <- round(df4b.V.RT.grand.Match$RT[df4b.V.RT.grand.Match$Morality == 'Moral' & df4b.V.RT.grand.Match$Identity == 'self'],0)
df4b.rt.sd.moralself <- round(df4b.V.RT.grand.Match$sd[df4b.V.RT.grand.Match$Morality == 'Moral' & df4b.V.RT.grand.Match$Identity == 'self'],0)
df4b.rt.mean.immoralself <- round(df4b.V.RT.grand.Match$RT[df4b.V.RT.grand.Match$Morality == 'Immoral' & df4b.V.RT.grand.Match$Identity == 'self'],0)
df4b.rt.sd.immoralself <- round(df4b.V.RT.grand.Match$sd[df4b.V.RT.grand.Match$Morality == 'Immoral' & df4b.V.RT.grand.Match$Identity == 'self'],0)
df4b.rt.mean.aveself <- round(df4b.V.RT.grand.Match$RT[df4b.V.RT.grand.Match$Morality == 'Neutral' & df4b.V.RT.grand.Match$Identity == 'self'],0)
df4b.rt.sd.aveself <- round(df4b.V.RT.grand.Match$sd[df4b.V.RT.grand.Match$Morality == 'Neutral' & df4b.V.RT.grand.Match$Identity == 'self'],0)

df4b.rt.mean.moralother <- round(df4b.V.RT.grand.Match$RT[df4b.V.RT.grand.Match$Morality == 'Moral' & df4b.V.RT.grand.Match$Identity == 'other'],0)
df4b.rt.sd.moralother <- round(df4b.V.RT.grand.Match$sd[df4b.V.RT.grand.Match$Morality == 'Moral' & df4b.V.RT.grand.Match$Identity == 'other'],0)
df4b.rt.mean.immoralother <- round(df4b.V.RT.grand.Match$RT[df4b.V.RT.grand.Match$Morality == 'Immoral' & df4b.V.RT.grand.Match$Identity == 'other'],0)
df4b.rt.sd.immoralother <- round(df4b.V.RT.grand.Match$sd[df4b.V.RT.grand.Match$Morality == 'Immoral' & df4b.V.RT.grand.Match$Identity == 'other'],0)
df4b.rt.mean.aveother <- round(df4b.V.RT.grand.Match$RT[df4b.V.RT.grand.Match$Morality == 'Neutral' & df4b.V.RT.grand.Match$Identity == 'other'],0)
df4b.rt.sd.aveother <- round(df4b.V.RT.grand.Match$sd[df4b.V.RT.grand.Match$Morality == 'Neutral' & df4b.V.RT.grand.Match$Identity == 'other'],0)

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


df4b.p_rt2 <- ggplot(data = df4b.V.RT.grand.Match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
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


tiff(filename = "Fig_d_prime and RTs of Experiment 4.2.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(df4b.p_dprime1,df4b.p_rt1,cols = 2)
dev.off()

tiff(filename = "Figure_d prime and RTs of Experiment 4.2 (way 2).tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(df4b.p_dprime2,df4b.p_rt2,cols = 2)
dev.off()
