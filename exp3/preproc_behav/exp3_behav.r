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
df3$Morality[df3$Morality == "moral"]   <- "Moral"
df3$Morality[df3$Morality == "normal"]  <- "Neutral"
df3$Morality[df3$Morality == "immoral"] <- "Immoral"
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


#### analyze the d prime ####
df3.V$sdt <- NA
df3.V <- df3.V[!is.na(df3.V$Target.RESP),] ## excluded the non-response trial when analyze the deprime.
for (i in 1:nrow(df3.V)){
  if (df3.V$Target.RESP[i] == df3.V$Target.CRESP[i] & (df3.V$Matchness[i] == "nonmatch")){
    df3.V$sdt[i] <- "CR"
  } else if (df3.V$Target.RESP[i] != df3.V$Target.CRESP[i] & (df3.V$Matchness[i] == "match")){
    df3.V$sdt[i] <- "miss"
  } else if (df3.V$Target.RESP[i] != df3.V$Target.CRESP[i] & (df3.V$Matchness[i] == "nonmatch")){
    df3.V$sdt[i] <- "FA"
  }
  else if (df3.V$Target.RESP[i] == df3.V$Target.CRESP[i] & (df3.V$Matchness[i] == "match")){
    df3.V$sdt[i] <- "hit"
  }
}

# calculate the number of each for each condition
df3.V.SDT <-  ddply(df3.V,.(Subject,Morality, Identity,sdt), summarise, N = length(sdt))


# long format to wide
df3.V.SDT_w <- dcast(df3.V.SDT, Subject + Morality + Identity ~ sdt,value.var = "N")
df3.V.SDT_w$miss[is.na(df3.V.SDT_w$miss)] <- 0
df3.V.SDT_w$FA[is.na(df3.V.SDT_w$FA)] <- 0
df3.V.SDT_w$hitR <- df3.V.SDT_w$hit/(df3.V.SDT_w$hit + df3.V.SDT_w$miss)
df3.V.SDT_w$faR <- df3.V.SDT_w$FA/(df3.V.SDT_w$FA + df3.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df3.V.SDT_w)){
  if (df3.V.SDT_w$hitR[i] == 1){
    df3.V.SDT_w$hitR[i] <- 1 - 1/(2*(df3.V.SDT_w$hit[i] + df3.V.SDT_w$miss[i]))
  }
}

for (i in 1:nrow(df3.V.SDT_w)){
  if (df3.V.SDT_w$faR[i] == 0){
    df3.V.SDT_w$faR[i] <- 1/(2*(df3.V.SDT_w$FA[i] + df3.V.SDT_w$CR[i]))
  }
}

# calculate the d prime for each condition
df3.V.SDT_w$dprime <- mapply(dprime,df3.V.SDT_w$hitR,df3.V.SDT_w$faR)
df3.V.SDT_w.self <- df3.V.SDT_w[df3.V.SDT_w$Identity == 'self',]
df3.V.SDT_w.other <- df3.V.SDT_w[df3.V.SDT_w$Identity == 'other',]

# anova for d prime with 2*2 design
e3.d_anova <- ezANOVA(df3.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity), type=3)
e3.d_anova.self <- ezANOVA(df3.V.SDT_w.self, dv = dprime, wid = Subject, within=.(Morality), type=3)
e3.d_anova.other <- ezANOVA(df3.V.SDT_w.other, dv = dprime, wid = Subject, within=.(Morality), type=3)
#print(d_anova1)

# change dprime data from long format to wide
df3.V.SDT_ww <- dcast(df3.V.SDT_w, Subject ~ Identity + Morality ,value.var = "dprime")
df3.V.SDT_ww_r <- df3.V.SDT_ww

# t-test
# df3.V.SDT_w$condition <- paste(df3.V.SDT_w$Morality,df3.V.SDT_w$Identity,'_')
# pairwise.t.test(df3.V.SDT_w$dprime,df3.V.SDT_w$conidtion,p.adj = "bonf")

# Moral self vs immoral self
e3.d.t.mrl_imm_slf <- t.test(df3.V.SDT_ww$self_Moral,df3.V.SDT_ww$self_immoral,paired = TRUE)
df3.V.SDT_ww$mrl_imm_slf <- df3.V.SDT_ww$self_Moral - df3.V.SDT_ww$self_immoral
e3.d.t.mrl_imm_slf.CI <- bootES(df3.V.SDT_ww$mrl_imm_slf,R = 20000, effect.type = "cohens.d")

e3.d.tvalue.mrl_imm_slf <- round(as.numeric(e3.d.t.mrl_imm_slf[[1]]),3)
e3.d.df3.mrl_imm_slf <- as.numeric(e3.d.t.mrl_imm_slf[[2]])
e3.d.pvalue.mrl_imm_slf.adj <- p.adjust(as.numeric(e3.d.t.mrl_imm_slf[[3]],"bonferroni",3))
e3.d.cohens.mrl_imm_slf <- round(e3.d.t.mrl_imm_slf.CI[[1]],4) 
e3.d.CI.L.mrl_imm_slf <- round(e3.d.t.mrl_imm_slf.CI[[12]][1],4)
e3.d.CI.H.mrl_imm_slf <- round(e3.d.t.mrl_imm_slf.CI[[12]][2],4)

# Moral self vs Average self
e3.d.t.mrl_ave_slf <- t.test(df3.V.SDT_ww$self_Moral,df3.V.SDT_ww$self_average,paired = TRUE)
df3.V.SDT_ww$mrl_ave_slf <- df3.V.SDT_ww$self_Moral - df3.V.SDT_ww$self_average
e3.d.t.mrl_ave_slf.CI <- bootES(df3.V.SDT_ww$mrl_ave_slf,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.mrl_ave_slf  <- round(as.numeric(e3.d.t.mrl_ave_slf[[1]]),3)
e3.d.df3.mrl_ave_slf  <- as.numeric(e3.d.t.mrl_ave_slf[[2]])
e3.d.pvalue.mrl_ave_slf.adj <- p.adjust(as.numeric(e3.d.t.mrl_ave_slf[[3]],"bonferroni",3))
e3.d.cohens.mrl_ave_slf <- round(e3.d.t.mrl_ave_slf.CI[[1]],4) 
e3.d.CI.L.mrl_ave_slf <- round(e3.d.t.mrl_ave_slf.CI[[12]][1],4)
e3.d.CI.H.mrl_ave_slf <- round(e3.d.t.mrl_ave_slf.CI[[12]][2],4)

# immoral self vs. Average self
e3.d.t.imm_ave_slf <- t.test(df3.V.SDT_ww$self_immoral,df3.V.SDT_ww$self_average,paired = TRUE)
df3.V.SDT_ww$imm_ave_slf <- df3.V.SDT_ww$self_immoral - df3.V.SDT_ww$self_average
e3.d.t.imm_ave_slf.CI <- bootES(df3.V.SDT_ww$imm_ave_slf,R = 20000, effect.type = "cohens.d")

e3.d.tvalue.imm_ave_slf  <- round(as.numeric(e3.d.t.imm_ave_slf[[1]]),3)
e3.d.df3.imm_ave_slf  <- as.numeric(e3.d.t.imm_ave_slf[[2]])
e3.d.pvalue.imm_ave_slf.adj <- p.adjust(as.numeric(e3.d.t.imm_ave_slf[[3]],"bonferroni",3))
e3.d.cohens.imm_ave_slf <- round(e3.d.t.imm_ave_slf.CI[[1]],4) 
e3.d.CI.L.imm_ave_slf <- round(e3.d.t.imm_ave_slf.CI[[12]][1],4)
e3.d.CI.H.imm_ave_slf <- round(e3.d.t.imm_ave_slf.CI[[12]][2],4)

# Moral other vs. immoral other
e3.d.t.mrl_imm_oth <- t.test(df3.V.SDT_ww$other_Moral,df3.V.SDT_ww$other_immoral,paired = TRUE)
df3.V.SDT_ww$mrl_imm_oth <- df3.V.SDT_ww$other_Moral - df3.V.SDT_ww$other_immoral
e3.d.t.mrl_imm_oth.CI <- bootES(df3.V.SDT_ww$mrl_imm_oth,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.mrl_imm_oth  <- round(as.numeric(e3.d.t.mrl_imm_oth[[1]]),3)
e3.d.df3.mrl_imm_oth  <- as.numeric(e3.d.t.mrl_imm_oth[[2]])
e3.d.pvalue.mrl_imm_oth.adj <- p.adjust(as.numeric(e3.d.t.mrl_imm_oth[[3]],"bonferroni",3))
e3.d.cohens.mrl_imm_oth <- round(e3.d.t.mrl_imm_oth.CI[[1]],4) 
e3.d.CI.L.mrl_imm_oth <- round(e3.d.t.mrl_imm_oth.CI[[12]][1],4)
e3.d.CI.H.mrl_imm_oth <- round(e3.d.t.mrl_imm_oth.CI[[12]][2],4)

# Moral other vs. average other
e3.d.t.mrl_ave_oth <- t.test(df3.V.SDT_ww$other_Moral,df3.V.SDT_ww$other_average,paired = TRUE)
df3.V.SDT_ww$mrl_ave_oth <- df3.V.SDT_ww$other_Moral - df3.V.SDT_ww$other_average
e3.d.t.mrl_ave_oth.CI <- bootES(df3.V.SDT_ww$mrl_ave_oth,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.mrl_ave_oth  <- round(as.numeric(e3.d.t.mrl_ave_oth[[1]]),3)
e3.d.df3.mrl_ave_oth  <- as.numeric(e3.d.t.mrl_ave_oth[[2]])
e3.d.pvalue.mrl_ave_oth.adj <- p.adjust(as.numeric(e3.d.t.mrl_ave_oth[[3]],"bonferroni",3))
e3.d.cohens.mrl_ave_oth <- round(e3.d.t.mrl_ave_oth.CI[[1]],4) 
e3.d.CI.L.mrl_ave_oth <- round(e3.d.t.mrl_ave_oth.CI[[12]][1],4)
e3.d.CI.H.mrl_ave_oth <- round(e3.d.t.mrl_ave_oth.CI[[12]][2],4)

# immoral other vs. Average other
e3.d.t.imm_ave_oth <- t.test(df3.V.SDT_ww$other_immoral,df3.V.SDT_ww$other_average,paired = TRUE)
df3.V.SDT_ww$imm_ave_oth <- df3.V.SDT_ww$other_immoral - df3.V.SDT_ww$other_average
e3.d.t.imm_ave_oth.CI <- bootES(df3.V.SDT_ww$imm_ave_oth,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.imm_ave_oth  <- round(as.numeric(e3.d.t.imm_ave_oth[[1]]),3)
e3.d.df3.imm_ave_oth  <- as.numeric(e3.d.t.imm_ave_oth[[2]])
e3.d.pvalue.imm_ave_oth.adj <- p.adjust(as.numeric(e3.d.t.imm_ave_oth[[3]],"bonferroni",3))
e3.d.cohens.imm_ave_oth <- round(e3.d.t.imm_ave_oth.CI[[1]],4) 
e3.d.CI.L.imm_ave_oth <- round(e3.d.t.imm_ave_oth.CI[[12]][1],4)
e3.d.CI.H.imm_ave_oth <- round(e3.d.t.imm_ave_oth.CI[[12]][2],4)

# Moral self vs Moral other
e3.d.t.slf_oth_mrl <- t.test(df3.V.SDT_ww$self_Moral,df3.V.SDT_ww$other_Moral,paired = TRUE)
df3.V.SDT_ww$slf_oth_mrl <- df3.V.SDT_ww$self_Moral - df3.V.SDT_ww$other_Moral
e3.d.t.slf_oth_mrl.CI <- bootES(df3.V.SDT_ww$slf_oth_mrl,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.slf_oth_mrl  <- round(as.numeric(e3.d.t.slf_oth_mrl[[1]]),3)
e3.d.df3.slf_oth_mrl  <- as.numeric(e3.d.t.slf_oth_mrl[[2]])
e3.d.pvalue.slf_oth_mrl.adj <- p.adjust(as.numeric(e3.d.t.slf_oth_mrl[[3]],"bonferroni",3))
e3.d.cohens.slf_oth_mrl <- round(e3.d.t.slf_oth_mrl.CI[[1]],4) 
e3.d.CI.L.slf_oth_mrl <- round(e3.d.t.slf_oth_mrl.CI[[12]][1],4)
e3.d.CI.H.slf_oth_mrl <- round(e3.d.t.slf_oth_mrl.CI[[12]][2],4)

# Average self vs. Average other
e3.d.t.slf_oth_ave <- t.test(df3.V.SDT_ww$self_average,df3.V.SDT_ww$other_average,paired = TRUE)
df3.V.SDT_ww$slf_oth_ave <- df3.V.SDT_ww$self_average - df3.V.SDT_ww$other_average
e3.d.t.slf_oth_ave.CI <- bootES(df3.V.SDT_ww$slf_oth_ave,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.slf_oth_ave  <- round(as.numeric(e3.d.t.slf_oth_ave[[1]]),3)
e3.d.df3.slf_oth_ave  <- as.numeric(e3.d.t.slf_oth_ave[[2]])
e3.d.pvalue.slf_oth_ave.adj <- p.adjust(as.numeric(e3.d.t.slf_oth_ave[[3]],"bonferroni",3))
e3.d.cohens.slf_oth_ave <- round(e3.d.t.slf_oth_ave.CI[[1]],4) 
e3.d.CI.L.slf_oth_ave <- round(e3.d.t.slf_oth_ave.CI[[12]][1],4)
e3.d.CI.H.slf_oth_ave <- round(e3.d.t.slf_oth_ave.CI[[12]][2],4)

# immoral self vs. immoral other
e3.d.t.slf_oth_imm <- t.test(df3.V.SDT_ww$self_immoral,df3.V.SDT_ww$other_immoral,paired = TRUE)
df3.V.SDT_ww$slf_oth_imm <- df3.V.SDT_ww$self_immoral - df3.V.SDT_ww$other_immoral
e3.d.t.slf_oth_imm.CI <- bootES(df3.V.SDT_ww$slf_oth_imm,R = 10000, effect.type = "cohens.d")

e3.d.tvalue.slf_oth_imm  <- round(as.numeric(e3.d.t.slf_oth_imm[[1]]),3)
e3.d.df3.slf_oth_imm <- as.numeric(e3.d.t.slf_oth_imm[[2]])
e3.d.pvalue.slf_oth_imm.adj <- p.adjust(as.numeric(e3.d.t.slf_oth_imm[[3]],"bonferroni",3))
e3.d.cohens.slf_oth_imm <- round(e3.d.t.slf_oth_imm.CI[[1]],4) 
e3.d.CI.L.slf_oth_imm <- round(e3.d.t.slf_oth_imm.CI[[12]][1],4)
e3.d.CI.H.slf_oth_imm <- round(e3.d.t.slf_oth_imm.CI[[12]][2],4)

## plot and save the results of d'
df3.V.SDT.sum <- summarySE(df3.V.SDT_w,measurevar = 'dprime',groupvars = c('Morality','Identity'))
e3.d.mean.Moralself <- round(df3.V.SDT.sum$dprime[df3.V.SDT.sum$Morality == 'Moral' & df3.V.SDT.sum$Identity == 'self'],3)
e3.d.sd.Moralself <- round(df3.V.SDT.sum$sd[df3.V.SDT.sum$Morality == 'Moral' & df3.V.SDT.sum$Identity == 'self'],3)
e3.d.mean.immoralself <- round(df3.V.SDT.sum$dprime[df3.V.SDT.sum$Morality == 'immoral' & df3.V.SDT.sum$Identity == 'self'],3)
e3.d.sd.immoralself <- round(df3.V.SDT.sum$sd[df3.V.SDT.sum$Morality == 'immoral' & df3.V.SDT.sum$Identity == 'self'],3)
e3.d.mean.aveself <- round(df3.V.SDT.sum$dprime[df3.V.SDT.sum$Morality == 'average' & df3.V.SDT.sum$Identity == 'self'],3)
e3.d.sd.aveself <- round(df3.V.SDT.sum$sd[df3.V.SDT.sum$Morality == 'average' & df3.V.SDT.sum$Identity == 'self'],3)

e3.d.mean.Moralother <- round(df3.V.SDT.sum$dprime[df3.V.SDT.sum$Morality == 'Moral' & df3.V.SDT.sum$Identity == 'other'],3)
e3.d.sd.Moralother <- round(df3.V.SDT.sum$sd[df3.V.SDT.sum$Morality == 'Moral' & df3.V.SDT.sum$Identity == 'other'],3)
e3.d.mean.immoralother <- round(df3.V.SDT.sum$dprime[df3.V.SDT.sum$Morality == 'immoral' & df3.V.SDT.sum$Identity == 'other'],3)
e3.d.sd.immoralother <- round(df3.V.SDT.sum$sd[df3.V.SDT.sum$Morality == 'immoral' & df3.V.SDT.sum$Identity == 'other'],3)
e3.d.mean.aveother <- round(df3.V.SDT.sum$dprime[df3.V.SDT.sum$Morality == 'average' & df3.V.SDT.sum$Identity == 'other'],3)
e3.d.sd.aveother <- round(df3.V.SDT.sum$sd[df3.V.SDT.sum$Morality == 'average' & df3.V.SDT.sum$Identity == 'other'],3)


# effect size and variance (need to revised further)
e2.d.es.mrl_imm <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.im, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.im, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.mrl_imm)

e2.d.es.mrl_ave <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.av, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.mrl_ave)

e2.d.es.imm_ave <- d.sgpp(m.1 = e2.d.mean.im, m.2 = e2.d.mean.av, sd.1=e2.d.sd.im,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$Moral),r=e2.d.cor.imm_ave)

# plot the results of dprime, way 1
e3.p_dprime1 <- ggplot(data = df3.V.SDT.sum,aes(y = dprime, x = Identity, group = Morality,shape = Morality, fill = Morality)) +
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
  scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good   ",'Neutral','Bad   '))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

# plot the results of dprime, way 2
e3.p_dprime2 <- ggplot(data = df3.V.SDT.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
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

# ggsave('dprime_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot

## doing the analysis for RT ####
## plot density of each subject's RT and save them individually
subNo <- unique(df3.V$Subject)

## doing the analysis for RT ####
df3.V.RT <- df3.V[df3.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df3.V.RT.subj <- summarySEwithin(df3.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),idvar = 'Subject', na.rm = TRUE)
df3.V.RT.grand <- summarySE(df3.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)
df3.V.RT_match <- df3.V.RT[df3.V.RT$Matchness == "match",]
df3.V.RT_nonmatch <- df3.V.RT[df3.V.RT$Matchness == "nonmatch",]
df3.V.RT_match.self <- df3.V.RT_match[df3.V.RT_match$Identity == 'self',]
df3.V.RT_match.other <- df3.V.RT_match[df3.V.RT_match$Identity == 'other',]

e3.rt_anova <- ezANOVA(df3.V.RT,dv = RT, wid = Subject, within=.(Matchness,Morality,Identity),within_full=.(Matchness,Morality,Identity), type=3)
e3.rt_anova.match <- ezANOVA(df3.V.RT_match,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)
e3.rt_anova.nonmatch <- ezANOVA(df3.V.RT_nonmatch,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)

e3.rt_anova.match.self <- ezANOVA(df3.V.RT_match.self,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
e3.rt_anova.match.other <- ezANOVA(df3.V.RT_match.other,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
## t-test 
df3.V.RT.subj_w <- dcast(df3.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT") 

e3.rt.t.m.mrl_imm_slf <- t.test(df3.V.RT.subj_w$match_self_Moral,df3.V.RT.subj_w$match_self_immoral,paired = TRUE)
df3.V.RT.subj_w$m.mrl_imm_slf <- df3.V.RT.subj_w$match_self_Moral - df3.V.RT.subj_w$match_self_immoral
e3.rt.t.m.mrl_imm_slf.CI <- bootES(df3.V.RT.subj_w$m.mrl_imm_slf, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.mrl_imm_slf  <- round(as.numeric(e3.rt.t.m.mrl_imm_slf [[1]]),3)
e3.rt.df3.mrl_imm_slf  <- as.numeric(e3.rt.t.m.mrl_imm_slf [[2]])
e3.rt.pvalue.mrl_imm_slf.adj <- p.adjust(as.numeric(e3.rt.t.m.mrl_imm_slf [[3]],"bonferroni",3))
e3.rt.cohens.mrl_imm_slf <- round(e3.rt.t.m.mrl_imm_slf.CI[[1]],4) 
e3.rt.CI.L.mrl_imm_slf <- round(e3.rt.t.m.mrl_imm_slf.CI[[12]][1],4)
e3.rt.CI.H.mrl_imm_slf <- round(e3.rt.t.m.mrl_imm_slf.CI[[12]][2],4)


e3.rt.t.m.mrl_ave_slf <- t.test(df3.V.RT.subj_w$match_self_Moral,df3.V.RT.subj_w$match_self_average,paired = TRUE)
df3.V.RT.subj_w$m.mrl_ave_slf <- df3.V.RT.subj_w$match_self_Moral - df3.V.RT.subj_w$match_self_average
e3.rt.t.m.mrl_ave_slf.CI <- bootES(df3.V.RT.subj_w$m.mrl_ave_slf, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.mrl_ave_slf  <- round(as.numeric(e3.rt.t.m.mrl_ave_slf [[1]]),3)
e3.rt.df3.mrl_ave_slf  <- as.numeric(e3.rt.t.m.mrl_ave_slf[[2]])
e3.rt.pvalue.mrl_ave_slf.adj <- p.adjust(as.numeric(e3.rt.t.m.mrl_ave_slf[[3]],"bonferroni",3))
e3.rt.cohens.mrl_ave_slf <- round(e3.rt.t.m.mrl_ave_slf.CI[[1]],4) 
e3.rt.CI.L.mrl_ave_slf <- round(e3.rt.t.m.mrl_ave_slf.CI[[12]][1],4)
e3.rt.CI.H.mrl_ave_slf <- round(e3.rt.t.m.mrl_ave_slf.CI[[12]][2],4)


e3.rt.t.m.imm_ave_slf <- t.test(df3.V.RT.subj_w$match_self_immoral,df3.V.RT.subj_w$match_self_average,paired = TRUE)
df3.V.RT.subj_w$m.imm_ave_slf <- df3.V.RT.subj_w$match_self_immoral - df3.V.RT.subj_w$match_self_average
e3.rt.t.m.imm_ave_slf.CI <- bootES(df3.V.RT.subj_w$m.imm_ave_slf, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.imm_ave_slf  <- round(as.numeric(e3.rt.t.m.imm_ave_slf[[1]]),3)
e3.rt.df3.imm_ave_slf  <- as.numeric(e3.rt.t.m.imm_ave_slf[[2]])
e3.rt.pvalue.imm_ave_slf.adj <- p.adjust(as.numeric(e3.rt.t.m.imm_ave_slf[[3]],"bonferroni",3))
e3.rt.cohens.imm_ave_slf <- round(e3.rt.t.m.imm_ave_slf.CI [[1]],4) 
e3.rt.CI.L.imm_ave_slf <- round(e3.rt.t.m.imm_ave_slf.CI[[12]][1],4)
e3.rt.CI.H.imm_ave_slf <- round(e3.rt.t.m.imm_ave_slf.CI[[12]][2],4)

e3.rt.t.m.mrl_imm_oth <- t.test(df3.V.RT.subj_w$match_self_Moral,df3.V.RT.subj_w$match_self_immoral,paired = TRUE)
df3.V.RT.subj_w$m.mrl_imm_oth <- df3.V.RT.subj_w$match_self_Moral - df3.V.RT.subj_w$match_self_immoral
e3.rt.t.m.mrl_imm_oth.CI <- bootES(df3.V.RT.subj_w$m.mrl_imm_oth, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.mrl_imm_oth  <- round(as.numeric(e3.rt.t.m.mrl_imm_oth [[1]]),3)
e3.rt.df3.mrl_imm_oth  <- as.numeric(e3.rt.t.m.mrl_imm_oth [[2]])
e3.rt.pvalue.mrl_imm_oth.adj <- p.adjust(as.numeric(e3.rt.t.m.mrl_imm_oth [[3]],"bonferroni",3))
e3.rt.cohens.mrl_imm_oth <- round(e3.rt.t.m.mrl_imm_oth.CI [[1]],4) 
e3.rt.CI.L.mrl_imm_oth <- round(e3.rt.t.m.mrl_imm_oth.CI [[12]][1],4)
e3.rt.CI.H.mrl_imm_oth <- round(e3.rt.t.m.mrl_imm_oth.CI [[12]][2],4)


e3.rt.t.m.mrl_ave_oth <- t.test(df3.V.RT.subj_w$match_self_Moral,df3.V.RT.subj_w$match_self_average,paired = TRUE)
df3.V.RT.subj_w$m.mrl_ave_oth <- df3.V.RT.subj_w$match_self_Moral - df3.V.RT.subj_w$match_self_average
e3.rt.t.m.mrl_ave_oth.CI <- bootES(df3.V.RT.subj_w$m.mrl_ave_oth, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.mrl_ave_oth  <- round(as.numeric(e3.rt.t.m.mrl_ave_oth [[1]]),3)
e3.rt.df3.mrl_ave_oth  <- as.numeric(e3.rt.t.m.mrl_ave_oth[[2]])
e3.rt.pvalue.mrl_ave_oth.adj <- p.adjust(as.numeric(e3.rt.t.m.mrl_ave_oth[[3]],"bonferroni",3))
e3.rt.cohens.mrl_ave_oth <- round(e3.rt.t.m.mrl_ave_oth.CI [[1]],4) 
e3.rt.CI.L.mrl_ave_oth <- round(e3.rt.t.m.mrl_ave_oth.CI [[12]][1],4)
e3.rt.CI.H.mrl_ave_oth <- round(e3.rt.t.m.mrl_ave_oth.CI [[12]][2],4)


e3.rt.t.m.imm_ave_oth <- t.test(df3.V.RT.subj_w$match_self_immoral,df3.V.RT.subj_w$match_self_average,paired = TRUE)
df3.V.RT.subj_w$m.imm_ave_oth <- df3.V.RT.subj_w$match_self_immoral - df3.V.RT.subj_w$match_self_average
e3.rt.t.m.imm_ave_oth.CI <- bootES(df3.V.RT.subj_w$m.imm_ave_oth, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.imm_ave_oth  <- round(as.numeric(e3.rt.t.m.imm_ave_oth[[1]]),3)
e3.rt.df3.imm_ave_oth  <- as.numeric(e3.rt.t.m.imm_ave_oth[[2]])
e3.rt.pvalue.imm_ave_oth.adj <- p.adjust(as.numeric(e3.rt.t.m.imm_ave_oth[[3]],"bonferroni",3))
e3.rt.cohens.imm_ave_oth <- round(e3.rt.t.m.imm_ave_oth.CI[[1]],4) 
e3.rt.CI.L.imm_ave_oth <- round(e3.rt.t.m.imm_ave_oth.CI[[12]][1],4)
e3.rt.CI.H.imm_ave_oth <- round(e3.rt.t.m.imm_ave_oth.CI[[12]][2],4)


e3.rt.t.m.slf_oth_mrl <- t.test(df3.V.RT.subj_w$match_self_moral,df3.V.RT.subj_w$match_other_moral,paired = TRUE)
df3.V.RT.subj_w$m.slf_oth_mrl <- df3.V.RT.subj_w$match_self_moral - df3.V.RT.subj_w$match_other_moral
e3.rt.t.m.slf_oth_mrl.CI <- bootES(df3.V.RT.subj_w$m.slf_oth_mrl, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.slf_oth_mrl  <- round(as.numeric(e3.rt.t.m.slf_oth_mrl[[1]]),3)
e3.rt.df3.slf_oth_mrl  <- as.numeric(e3.rt.t.m.slf_oth_mrl[[2]])
e3.rt.pvalue.slf_oth_mrl.adj <- p.adjust(as.numeric(e3.rt.t.m.slf_oth_mrl[[3]],"bonferroni",3))
e3.rt.cohens.slf_oth_mrl <- round(e3.rt.t.m.slf_oth_mrl.CI[[1]],4) 
e3.rt.CI.L.slf_oth_mrl <- round(e3.rt.t.m.slf_oth_mrl.CI[[12]][1],4)
e3.rt.CI.H.slf_oth_mrl <- round(e3.rt.t.m.slf_oth_mrl.CI[[12]][2],4)

e3.rt.t.m.slf_oth_imm <- t.test(df3.V.RT.subj_w$match_self_immoral,df3.V.RT.subj_w$match_other_immoral,paired = TRUE)
df3.V.RT.subj_w$m.slf_oth_imm <- df3.V.RT.subj_w$match_self_immoral - df3.V.RT.subj_w$match_other_immoral
e3.rt.t.m.slf_oth_imm.CI <- bootES(df3.V.RT.subj_w$m.slf_oth_imm, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.slf_oth_imm  <- round(as.numeric(e3.rt.t.m.slf_oth_imm[[1]]),3)
e3.rt.df3.slf_oth_imm  <- as.numeric(e3.rt.t.m.slf_oth_imm[[2]])
e3.rt.pvalue.slf_oth_imm.adj <- p.adjust(as.numeric(e3.rt.t.m.slf_oth_imm[[3]],"bonferroni",3))
e3.rt.cohens.slf_oth_imm <- round(e3.rt.t.m.slf_oth_imm.CI[[1]],4) 
e3.rt.CI.L.slf_oth_imm <- round(e3.rt.t.m.slf_oth_imm.CI[[12]][1],4)
e3.rt.CI.H.slf_oth_imm <- round(e3.rt.t.m.slf_oth_imm.CI[[12]][2],4)


e3.rt.t.m.slf_oth_ave <- t.test(df3.V.RT.subj_w$match_self_average,df3.V.RT.subj_w$match_other_average,paired = TRUE)
df3.V.RT.subj_w$m.slf_oth_ave <- df3.V.RT.subj_w$match_self_average - df3.V.RT.subj_w$match_other_average
e3.rt.t.m.slf_oth_ave.CI <- bootES(df3.V.RT.subj_w$m.slf_oth_ave, R = 5000,effect.type = "cohens.d")

e3.rt.tvalue.slf_oth_ave  <- round(as.numeric(e3.rt.t.m.slf_oth_ave[[1]]),3)
e3.rt.df3.slf_oth_ave  <- as.numeric(e3.rt.t.m.slf_oth_ave[[2]])
e3.rt.pvalue.slf_oth_ave.adj <- p.adjust(as.numeric(e3.rt.t.m.slf_oth_ave[[3]],"bonferroni",3))
e3.rt.cohens.slf_oth_ave <- round(e3.rt.t.m.slf_oth_ave.CI[[1]],4) 
e3.rt.CI.L.slf_oth_ave <- round(e3.rt.t.m.slf_oth_ave.CI[[12]][1],4)
e3.rt.CI.H.slf_oth_ave <- round(e3.rt.t.m.slf_oth_ave.CI[[12]][2],4)

df3.V.RT.grand.match <- df3.V.RT.grand[df3.V.RT.grand$Matchness == "match",]

e3.rt.mean.moralself <- round(df3.V.RT.grand.match$RT[df3.V.RT.grand.match$Morality == 'Moral' & df3.V.RT.grand.match$Identity == 'self'],0)
e3.rt.sd.Moralself <- round(df3.V.RT.grand.match$sd[df3.V.RT.grand.match$Morality == 'Moral' & df3.V.RT.grand.match$Identity == 'self'],0)
e3.rt.mean.immoralself <- round(df3.V.RT.grand.match$RT[df3.V.RT.grand.match$Morality == 'immoral' & df3.V.RT.grand.match$Identity == 'self'],0)
e3.rt.sd.immoralself <- round(df3.V.RT.grand.match$sd[df3.V.RT.grand.match$Morality == 'immoral' & df3.V.RT.grand.match$Identity == 'self'],0)
e3.rt.mean.aveself <- round(df3.V.RT.grand.match$RT[df3.V.RT.grand.match$Morality == 'average' & df3.V.RT.grand.match$Identity == 'self'],0)
e3.rt.sd.aveself <- round(df3.V.RT.grand.match$sd[df3.V.RT.grand.match$Morality == 'average' & df3.V.RT.grand.match$Identity == 'self'],0)

e3.rt.mean.moralother <- round(df3.V.RT.grand.match$RT[df3.V.RT.grand.match$Morality == 'Moral' & df3.V.RT.grand.match$Identity == 'other'],0)
e3.rt.sd.moralother <- round(df3.V.RT.grand.match$sd[df3.V.RT.grand.match$Morality == 'Moral' & df3.V.RT.grand.match$Identity == 'other'],0)
e3.rt.mean.immoralother <- round(df3.V.RT.grand.match$RT[df3.V.RT.grand.match$Morality == 'immoral' & df3.V.RT.grand.match$Identity == 'other'],0)
e3.rt.sd.immoralother <- round(df3.V.RT.grand.match$sd[df3.V.RT.grand.match$Morality == 'immoral' & df3.V.RT.grand.match$Identity == 'other'],0)
e3.rt.mean.aveother <- round(df3.V.RT.grand.match$RT[df3.V.RT.grand.match$Morality == 'average' & df3.V.RT.grand.match$Identity == 'other'],0)
e3.rt.sd.aveother <- round(df3.V.RT.grand.match$sd[df3.V.RT.grand.match$Morality == 'average' & df3.V.RT.grand.match$Identity == 'other'],0)

e3.p_rt1 <- ggplot(data = df3.V.RT.grand.match, aes(x=Identity,y=RT,group=Morality,shape = Morality,fill = Morality)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = .3,
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
  apatheme

e3.p_rt2 <- ggplot(data = df3.V.RT.grand.match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
  geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                size = .3,
                width = .2,
                position=position_dodge(.6)) +
  xlab("Moral valence") +
  ylab(" Reaction times (ms)") + 
  coord_cartesian(ylim=c(500,800))+
  scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
  #ylim(0.3, 0.8) +
  ggtitle("RT for each condition") +
  scale_y_continuous("Reation Times  (ms)",expand = c(0, 0)) + 
  apatheme
# ggsave('RT_mean_plot.png', width=4, height=6, unit='in', dpi=300)  # save the plot


tiff(filename = "Figure 3. d prime and RTs of Experiment 3.tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e3.p_dprime1,e3.p_rt1,cols = 2)
dev.off()

tiff(filename = "Figure 3.1. d prime and RTs of Experiment 3 (way 2).tiff", width = 8, height = 6, units = 'in', res = 300)
multiplot(e3.p_dprime2,e3.p_rt2,cols = 2)
dev.off()

