## this code is to analyze the data for exp1b, included these data were colleted at Wenzhou U in 201704

## initializing
source('Initial.r')

## load data and edite data
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
e2.num.subj    <- nrow(df2.T.basic)
e2.numT.female <- sum(df2.T.basic$Sex == 'female');
e2.numT.male   <- sum(df2.T.basic$Sex == 'male');
e2.ageT.mean   <- round(mean(df2.T.basic$Age),2);
e2.ageT.std    <- round(sd(df2.T.basic$Age),2);

# data from foreign students were excluded because their may not familar with Chinese Character
#foreignStdID <- c(24,29,30,33)
#nforeign <- length(foreignStdID)
#df2 <- df2[!(df2$Subject %in% foreignStdID),]

df2.P <- df2[is.na(df2$BlockList.Sample),]            # data from practice
df2.T <- df2[complete.cases(df2$BlockList.Sample),]   # data from test

# subject 45 were excluded because his middle finger is injured when doing the expeirments
e2.excld.sub_extra <- c('45')

e2.excld.trials <- df2.T[df2.T$RT <= 200,]
e2.ratio.excld.trials <- nrow(e2.excld.trials)/nrow(df2.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df2.acc.g <-  ddply(df2.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
e2.excld.sub <- df2.acc.g$Subject[df2.acc.g$ACC < 0.6]
df2.valid <- df2.T[!(df2.T$Subject %in% e2.excld.sub),] # exclude the invalid subjects
df2.valid <- df2.valid[!(df2.valid$Subject %in% e2.excld.sub_extra),] # exclude the invalid subjects
length(unique(df2.valid$Subject)) + length(e2.excld.sub) == length(unique(df2$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% e2.excld.sub),]
e2.excld.trials2 <- df2.valid[df2.valid$RT <= 200,]
df2.V <- df2.valid[!(df2.valid$RT <= 200),]  

## Basic information of the data ####
e2.num.excld.sub <- length(unique(e2.excld.sub))
df2.V.basic <- df2.V[!duplicated(df2.V$Subject), 1:4]
e2.numV.female <- sum(df2.V.basic$Sex == 'female');
e2.numV.male <- sum(df2.V.basic$Sex == 'male');
e2.ageV.mean <- round(mean(df2.V.basic$Age),2);
e2.ageV.std <- round(sd(df2.V.basic$Age),2);
e2.ratio.excld.trials2 <- nrow(e2.excld.trials2)/nrow(df2.valid)

# start to analyze the d prime
df2.V$sdt <- NA
for (i in 1:nrow(df2.V)){
        if (df2.V$Target.RESP[i] == df2.V$Target.CRESP[i] & df2.V$Matchness[i] == "Match"){
                df2.V$sdt[i] <- "hit"
        } else if (df2.V$Target.RESP[i] == df2.V$Target.CRESP[i] & df2.V$Matchness[i] == "Mismatch"){
                df2.V$sdt[i] <- "CR"
        } else if (df2.V$Target.RESP[i] != df2.V$Target.CRESP[i] & df2.V$Matchness[i] == "Match"){
                df2.V$sdt[i] <- "miss"
        } else if (df2.V$Target.RESP[i] != df2.V$Target.CRESP[i] & df2.V$Matchness[i] == "Mismatch"){
                df2.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df2.V.SDT <-  ddply(df2.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))


# long format to wide
df2.V.SDT_w <- dcast(df2.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")
df2.V.SDT_w$miss[is.na(df2.V.SDT_w$miss)] <- 0
df2.V.SDT_w$FA[is.na(df2.V.SDT_w$FA)] <- 0
df2.V.SDT_w$hitR <- df2.V.SDT_w$hit/(df2.V.SDT_w$hit + df2.V.SDT_w$miss)
df2.V.SDT_w$faR <- df2.V.SDT_w$FA/(df2.V.SDT_w$FA + df2.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df2.V.SDT_w)){
        if (df2.V.SDT_w$hitR[i] == 1){
                df2.V.SDT_w$hitR[i] <- 1 - 1/(2*(df2.V.SDT_w$hit[i] + df2.V.SDT_w$miss[i]))
        }
}

for (i in 1:nrow(df2.V.SDT_w)){
        if (df2.V.SDT_w$faR[i] == 0){
                df2.V.SDT_w$faR[i] <- 1/(2*(df2.V.SDT_w$FA[i] + df2.V.SDT_w$CR[i]))
        }
}

# calculate the d prime for each condition
df2.V.SDT_w$dprime <- mapply(dprime,df2.V.SDT_w$hitR,df2.V.SDT_w$faR)

# anova for d prime with 2*2 design
e2.d_anova1 <- ezANOVA(df2.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality), type=3)
print(e2.d_anova1)

# change dprime data from long format to wide
df2.V.SDT_ww   <- dcast(df2.V.SDT_w, Subject + Sex + Age ~ Morality ,value.var = "dprime")
df2.V.SDT_ww_r <- df2.V.SDT_ww

# t-test
# good vs bad
e2.d.t.mrl_imm <- t.test(df2.V.SDT_ww$Moral,df2.V.SDT_ww$Immoral,paired = TRUE)

df2.V.SDT_ww$mrl_imm <- df2.V.SDT_ww$Moral - df2.V.SDT_ww$Immoral
e2.d.mrl_imm.CI <- bootES(df2.V.SDT_ww$mrl_imm,R = 20000, effect.type = "cohens.d")

e2.d.tvalue.mrl_imm <- round(as.numeric(e2.d.t.mrl_imm[[1]]),3)
e2.d.df2.d.mrl_imm <- as.numeric(e2.d.t.mrl_imm[[2]])
e2.d.pvalue.mrl_imm.adj <- p.adjust(as.numeric(e2.d.t.mrl_imm[[3]],"bonferroni",3))
e2.d.cohens.mrl_imm <- round(e2.d.mrl_imm.CI[[1]],4) 
e2.d.CI.L.mrl_imm <- round(e2.d.mrl_imm.CI[[12]][1],4)
e2.d.CI.H.mrl_imm <- round(e2.d.mrl_imm.CI[[12]][2],4)

# Good vs Normal
e2.d.t.mrl_ave <- t.test(df2.V.SDT_ww$Moral,df2.V.SDT_ww$Neutral,paired = TRUE)
e2.d.cor.mrl_ave <- cor(df2.V.SDT_ww$Moral,df2.V.SDT_ww$Neutral)
df2.V.SDT_ww$mrl_ave <- df2.V.SDT_ww$Moral - df2.V.SDT_ww$Neutral
e2.d.mrl_ave.CI <- bootES(df2.V.SDT_ww$mrl_ave,R = 20000, effect.type = "cohens.d")

e2.d.tvalue.mrl_ave  <- round(as.numeric(e2.d.t.mrl_ave[[1]]),3)
e2.d.df2.mrl_ave  <- as.numeric(e2.d.t.mrl_ave[[2]])
e2.d.pvalue.mrl_ave.adj <- p.adjust(as.numeric(e2.d.t.mrl_ave[[3]],"bonferroni",3))
e2.d.cohens.mrl_ave <- round(e2.d.mrl_ave.CI[[1]],4) 
e2.d.CI.L.mrl_ave <- round(e2.d.mrl_ave.CI[[12]][1],4)
e2.d.CI.H.mrl_ave <- round(e2.d.mrl_ave.CI[[12]][2],4)

# Bad vs. Average
e2.d.t.imm_ave <- t.test(df2.V.SDT_ww$Immoral,df2.V.SDT_ww$Neutral,paired = TRUE)
e2.d.cor.imm_ave <- cor(df2.V.SDT_ww$Immoral,df2.V.SDT_ww$Neutral)
df2.V.SDT_ww$imm_ave <- df2.V.SDT_ww$Immoral - df2.V.SDT_ww$Neutral
e2.d.imm_ave.CI <- bootES(df2.V.SDT_ww$imm_ave,R = 20000, effect.type = "cohens.d")

e2.d.tvalue.imm_ave <- round(as.numeric(e2.d.t.imm_ave[[1]]),3)
e2.d.df2.imm_ave <- as.numeric(e2.d.t.imm_ave[[2]])
e2.d.pvalue.imm_ave.adj <- p.adjust(as.numeric(e2.d.t.imm_ave[[3]],"bonferroni",3))
e2.d.cohens.imm_ave <- round(e2.d.imm_ave.CI[[1]],4) 
e2.d.CI.L.imm_ave <- round(e2.d.imm_ave.CI[[12]][1],4)
e2.d.CI.H.imm_ave <- round(e2.d.imm_ave.CI[[12]][2],4)

## plot and save the results of d'
df2.V.SDT.sum <- summarySE(df2.V.SDT_w,measurevar = 'dprime',groupvars = c('Morality'))
e2.d.mean.ml  <- round(df2.V.SDT.sum$dprime[df2.V.SDT.sum$Morality == 'Moral'],2)
e2.d.sd.ml    <- round(df2.V.SDT.sum$sd[df2.V.SDT.sum$Morality == 'Moral'],2)
e2.d.mean.im  <- round(df2.V.SDT.sum$dprime[df2.V.SDT.sum$Morality == 'Immoral'],2)
e2.d.sd.im    <- round(df2.V.SDT.sum$sd[df2.V.SDT.sum$Morality == 'Immoral'],2)
e2.d.mean.av  <- round(df2.V.SDT.sum$dprime[df2.V.SDT.sum$Morality == 'Neutral'],2)
e2.d.sd.av    <- round(df2.V.SDT.sum$sd[df2.V.SDT.sum$Morality == 'Neutral'],2)

# calculate the effect size for meta
# correlation between
e2.d.cor.mrl_imm <- cor(df2.V.SDT_ww$Moral,df2.V.SDT_ww$Immoral)
e2.d.cor.mrl_ave <- cor(df2.V.SDT_ww$Moral,df2.V.SDT_ww$Neutral)
e2.d.cor.imm_ave <- cor(df2.V.SDT_ww$Immoral,df2.V.SDT_ww$Neutral)

# effect size and variance
e2.d.es.mrl_imm <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.im, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.im, n=length(df2.V.SDT_ww$moral),r=e2.d.cor.mrl_imm)

e2.d.es.mrl_ave <- d.sgpp(m.1 = e2.d.mean.ml, m.2 = e2.d.mean.av, sd.1=e2.d.sd.ml,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$moral),r=e2.d.cor.mrl_ave)

e2.d.es.imm_ave <- d.sgpp(m.1 = e2.d.mean.im, m.2 = e2.d.mean.av, sd.1=e2.d.sd.im,sd.2=e2.d.sd.av, n=length(df2.V.SDT_ww$moral),r=e2.d.cor.imm_ave)


e2.p_dprime <- ggplot(data = df2.V.SDT.sum,aes(y = dprime, x = Morality, group = Morality,shape = Morality, fill = Morality)) +
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

ggsave("e2_2.p_dprime.pdf", e2.p_dprime, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

## doing the analysis for RT ####
df2.V.RT <- df2.V[df2.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df2.V.RT.subj <- summarySEwithin(df2.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'), idvar = 'Subject',na.rm = TRUE)
e2.rt_anova <- ezANOVA(df2.V.RT,dv = RT, wid = Subject, within=.(Morality,Matchness),within_full=.(Morality,Matchness), type=3)

df2.V.RT.grand <- summarySE(df2.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality'),na.rm = TRUE)
df2.V.RT_match <- df2.V.RT[df2.V.RT$Matchness == "Match",]
df2.V.RT_mismatch <- df2.V.RT[df2.V.RT$Matchness == "Mismatch",]

e2.rt_anova.match <- ezANOVA(df2.V.RT_match,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
e2.rt_anova.mismatch <- ezANOVA(df2.V.RT_mismatch,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)

## t-test for matched trials
df2.V.RT.subj_w <- dcast(df2.V.RT.subj, Subject ~ Matchness + Morality ,value.var = "RT") 
df2.V.RT.subj_w_r <-df2.V.RT.subj_w

# moral vs. immoral
e2.rt.t.m.mrl_imm <- t.test(df2.V.RT.subj_w$Match_Moral,df2.V.RT.subj_w$Match_Immoral,paired = TRUE)
df2.V.RT.subj_w$m.mrl_imm <- df2.V.RT.subj_w$Match_Moral - df2.V.RT.subj_w$Match_Immoral
e2.rt.t.m.mrl_imm.CI <- bootES(df2.V.RT.subj_w$m.mrl_imm, R = 20000,effect.type = "cohens.d")

e2.rt.tvalue.mrl_imm  <- round(as.numeric(e2.rt.t.m.mrl_imm [[1]]),3)
e2.rt.df2.mrl_imm  <- as.numeric(e2.rt.t.m.mrl_imm [[2]])
e2.rt.pvalue.mrl_imm.adj <- p.adjust(as.numeric(e2.rt.t.m.mrl_imm [[3]],"bonferroni",3))
e2.rt.cohens.mrl_imm <- round(e2.rt.t.m.mrl_imm.CI[[1]],4) 
e2.rt.CI.L.mrl_imm <- round(e2.rt.t.m.mrl_imm.CI[[12]][1],4)
e2.rt.CI.H.mrl_imm <- round(e2.rt.t.m.mrl_imm.CI[[12]][2],4)

# moral vs. average
e2.rt.t.m.mrl_ave <- t.test(df2.V.RT.subj_w$Match_Moral,df2.V.RT.subj_w$Match_Neutral,paired = TRUE)
df2.V.RT.subj_w$m.mrl_ave <- df2.V.RT.subj_w$Match_Moral - df2.V.RT.subj_w$Match_Neutral
e2.rt.t.m.mrl_ave.CI <- bootES(df2.V.RT.subj_w$m.mrl_ave, R = 20000,effect.type = "cohens.d")

e2.rt.tvalue.mrl_ave <- round(as.numeric(e2.rt.t.m.mrl_ave [[1]]),3)
e2.rt.df2.mrl_ave <- as.numeric(e2.rt.t.m.mrl_ave[[2]])
e2.rt.pvalue.mrl_ave.adj <- p.adjust(as.numeric(e2.rt.t.m.mrl_ave[[3]],"bonferroni",3))
e2.rt.cohens.mrl_ave <- round(e2.rt.t.m.mrl_ave.CI[[1]],4) 
e2.rt.CI.L.mrl_ave <- round(e2.rt.t.m.mrl_ave.CI[[12]][1],4)
e2.rt.CI.H.mrl_ave <- round(e2.rt.t.m.mrl_ave.CI[[12]][2],4)

# immoral vs. average
e2.rt.t.m.imm_ave <- t.test(df2.V.RT.subj_w$Match_Immoral,df2.V.RT.subj_w$Match_Neutral,paired = TRUE)
df2.V.RT.subj_w$m.imm_ave <- df2.V.RT.subj_w$Match_Immoral - df2.V.RT.subj_w$Match_Neutral
e2.rt.t.m.imm_ave.CI <- bootES(df2.V.RT.subj_w$m.imm_ave, R = 20000,effect.type = "cohens.d")

e2.rt.tvalue.imm_ave <- round(as.numeric(e2.rt.t.m.imm_ave[[1]]),3)
e2.rt.df2.imm_ave  <- as.numeric(e2.rt.t.m.imm_ave[[2]])
e2.rt.pvalue.imm_ave.adj <- p.adjust(as.numeric(e2.rt.t.m.imm_ave[[3]],"bonferroni",3))
e2.rt.cohens.imm_ave <- round(e2.rt.t.m.imm_ave.CI [[1]],4) 
e2.rt.CI.L.imm_ave <- round(e2.rt.t.m.imm_ave.CI[[12]][1],4)
e2.rt.CI.H.imm_ave <- round(e2.rt.t.m.imm_ave.CI[[12]][2],4)

df2.V.RT.grand.match <- df2.V.RT.grand[df2.V.RT.grand$Matchness == "match",]
e2.rt.mean.ml <- round(df2.V.RT.grand.match$RT[df2.V.RT.grand.match$Morality == 'moral'],0)
e2.rt.sd.ml <- round(df2.V.RT.grand.match$sd[df2.V.RT.grand.match$Morality == 'moral'],0)
e2.rt.mean.im <- round(df2.V.RT.grand.match$RT[df2.V.RT.grand.match$Morality == 'immoral'],0)
e2.rt.sd.im <- round(df2.V.RT.grand.match$sd[df2.V.RT.grand.match$Morality == 'immoral'],0)
e2.rt.mean.av <- round(df2.V.RT.grand.match$RT[df2.V.RT.grand.match$Morality == 'average'],0)
e2.rt.sd.av <- round(df2.V.RT.grand.match$sd[df2.V.RT.grand.match$Morality == 'average'],0)

# calculate the effect size for rt
# correlation between
e2.rt.cor.mrl_imm <- cor(df2.V.RT.subj_w$Match_Moral,df2.V.RT.subj_w$Match_Immoral)
e2.rt.cor.mrl_ave <- cor(df2.V.RT.subj_w$Match_Moral,df2.V.RT.subj_w$Match_Neutral)
e2.rt.cor.imm_ave <- cor(df2.V.RT.subj_w$Match_Immoral,df2.V.RT.subj_w$Match_Neutral)

# effect size and variance
e2.rt.es.mrl_imm <- d.sgpp(m.1 = e2.rt.mean.ml, m.2 = e2.rt.mean.im, sd.1=e2.rt.sd.ml,sd.2=e2.rt.sd.im, n=length(df2.V.RT.subj_w$Match_Moral),r=e2.rt.cor.mrl_imm)

e2.rt.es.mrl_ave <- d.sgpp(m.1 = e2.rt.mean.ml, m.2 = e2.rt.mean.av, sd.1=e2.rt.sd.ml,sd.2=e2.rt.sd.av, n=length(df2.V.RT.subj_w$Match_Moral),r=e2.rt.cor.mrl_ave)

e2.rt.es.imm_ave <- d.sgpp(m.1 = e2.rt.mean.im, m.2 = e2.rt.mean.av, sd.1=e2.rt.sd.im,sd.2=e2.rt.sd.av, n=length(df2.V.RT.subj_w$Match_Moral),r=e2.rt.cor.imm_ave)

# plot
e2.p_rt <- ggplot(data = df2.V.RT.grand.match, aes(x=Morality,y=RT,group=Morality,shape = Morality,fill = Morality)) +
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

ggsave("e2_2.p_RT.pdf", e2.p_rt, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

