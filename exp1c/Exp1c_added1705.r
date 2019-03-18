## this code is to analyze the data for exp1b, included these data were colleted at Wenzhou U in 201704

## initializing
source('Initial.r')

## load data and edite data
df1b_1 <- read.csv("rawdata_behav_exp1b_2014.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1b_1$Subject))
df1b_2 <- read.csv("rawdata_behav_exp1b_201705.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1b_2$Subject))
df1b   <- rbind(df1b_1,df1b_2)

# rename colnames
colnames(df1b)[colnames(df1b)=="Target.ACC"] <- "ACC"
colnames(df1b)[colnames(df1b)=="Target.RT"]  <- "RT"
colnames(df1b)[colnames(df1b)=="YesNoResp"]  <- "Matchness"
colnames(df1b)[colnames(df1b)=="Shape"]      <- "Morality"

# renames independent variables
df1b$Morality[df1b$Morality == "Good"]   <- "Moral"
df1b$Morality[df1b$Morality == "Normal"] <- "Neutral"
df1b$Morality[df1b$Morality == "Bad"]    <- "Immoral"
df1b$Morality <- factor(df1b$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order

df1b$Matchness[df1b$Matchness == "Yes"] <- "Match"
df1b$Matchness[df1b$Matchness == "No"] <- "Mismatch"
df1b$Matchness <- factor(df1b$Matchness, levels=c("Match", "Mismatch"))

## Basic information of the data ####
df1b.T.basic    <- df1b[!duplicated(df1b$Subject), 1:4]
e1b.num.subj    <- nrow(df1b.T.basic)
e1b.numT.female <- sum(df1b.T.basic$Sex == 'female');
e1b.numT.male   <- sum(df1b.T.basic$Sex == 'male');
e1b.ageT.mean   <- round(mean(df1b.T.basic$Age),2);
e1b.ageT.std    <- round(sd(df1b.T.basic$Age),2);

# data from foreign students were excluded because their may not familar with Chinese Character
#foreignStdID <- c(24,29,30,33)
#nforeign <- length(foreignStdID)
#df1b <- df1b[!(df1b$Subject %in% foreignStdID),]

df1b.P <- df1b[is.na(df1b$BlockList.Sample),]            # data from practice
df1b.T <- df1b[complete.cases(df1b$BlockList.Sample),]   # data from test

e1b.excld.trials <- df1b.T[df1b.T$RT <= 200,]
e1b.ratio.excld.trials <- nrow(e1b.excld.trials)/nrow(df1b.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df1b.acc.g <-  ddply(df1b.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
e1b.excld.sub <- df1b.acc.g$Subject[df1b.acc.g$ACC < 0.6]
df1b.valid <- df1b.T[!(df1b.T$Subject %in% e1b.excld.sub),] # exclude the invalid subjects
length(unique(df1b.valid$Subject)) + length(e1b.excld.sub) == length(unique(df1b$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% e1b.excld.sub),]
e1b.excld.trials2 <- df1b.valid[df1b.valid$RT <= 200,]
df1b.V <- df1b.valid[!(df1b.valid$RT <= 200),]  

## Basic information of the data ####
e1b.num.excld.sub <- length(unique(e1b.excld.sub))
df1b.V.basic <- df1b.V[!duplicated(df1b.V$Subject), 1:4]
e1b.numV.female <- sum(df1b.V.basic$Sex == 'female');
e1b.numV.male <- sum(df1b.V.basic$Sex == 'male');
e1b.ageV.mean <- round(mean(df1b.V.basic$Age),2);
e1b.ageV.std <- round(sd(df1b.V.basic$Age),2);
e1b.ratio.excld.trials2 <- nrow(e1b.excld.trials2)/nrow(df1b.valid)

# start to analyze the d prime
df1b.V$sdt <- NA
for (i in 1:nrow(df1b.V)){
        if (df1b.V$Target.RESP[i] == df1b.V$Target.CRESP[i] & df1b.V$Matchness[i] == "Match"){
                df1b.V$sdt[i] <- "hit"
        } else if (df1b.V$Target.RESP[i] == df1b.V$Target.CRESP[i] & df1b.V$Matchness[i] == "Mismatch"){
                df1b.V$sdt[i] <- "CR"
        } else if (df1b.V$Target.RESP[i] != df1b.V$Target.CRESP[i] & df1b.V$Matchness[i] == "Match"){
                df1b.V$sdt[i] <- "miss"
        } else if (df1b.V$Target.RESP[i] != df1b.V$Target.CRESP[i] & df1b.V$Matchness[i] == "Mismatch"){
                df1b.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df1b.V.SDT <-  ddply(df1b.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))


# long format to wide
df1b.V.SDT_w <- dcast(df1b.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")
df1b.V.SDT_w$miss[is.na(df1b.V.SDT_w$miss)] <- 0
df1b.V.SDT_w$FA[is.na(df1b.V.SDT_w$FA)] <- 0
df1b.V.SDT_w$hitR <- df1b.V.SDT_w$hit/(df1b.V.SDT_w$hit + df1b.V.SDT_w$miss)
df1b.V.SDT_w$faR <- df1b.V.SDT_w$FA/(df1b.V.SDT_w$FA + df1b.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df1b.V.SDT_w)){
        if (df1b.V.SDT_w$hitR[i] == 1){
                df1b.V.SDT_w$hitR[i] <- 1 - 1/(2*(df1b.V.SDT_w$hit[i] + df1b.V.SDT_w$miss[i]))
        }
}

for (i in 1:nrow(df1b.V.SDT_w)){
        if (df1b.V.SDT_w$faR[i] == 0){
                df1b.V.SDT_w$faR[i] <- 1/(2*(df1b.V.SDT_w$FA[i] + df1b.V.SDT_w$CR[i]))
        }
}

# calculate the d prime for each condition
df1b.V.SDT_w$dprime <- mapply(dprime,df1b.V.SDT_w$hitR,df1b.V.SDT_w$faR)

# anova for d prime with 2*2 design
e1b.d_anova1 <- ezANOVA(df1b.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality), type=3)
print(e1b.d_anova1)

# change dprime data from long format to wide
df1b.V.SDT_ww   <- dcast(df1b.V.SDT_w, Subject + Sex + Age ~ Morality ,value.var = "dprime")
df1b.V.SDT_ww_r <- df1b.V.SDT_ww

# t-test
# good vs bad
e1b.d.t.mrl_imm <- t.test(df1b.V.SDT_ww$Moral,df1b.V.SDT_ww$Immoral,paired = TRUE)

df1b.V.SDT_ww$mrl_imm <- df1b.V.SDT_ww$Moral - df1b.V.SDT_ww$Immoral
e1b.d.mrl_imm.CI <- bootES(df1b.V.SDT_ww$mrl_imm,R = 20000, effect.type = "cohens.d")

e1b.d.tvalue.mrl_imm <- round(as.numeric(e1b.d.t.mrl_imm[[1]]),3)
e1b.d.df1b.d.mrl_imm <- as.numeric(e1b.d.t.mrl_imm[[2]])
e1b.d.pvalue.mrl_imm.adj <- p.adjust(as.numeric(e1b.d.t.mrl_imm[[3]],"bonferroni",3))
e1b.d.cohens.mrl_imm <- round(e1b.d.mrl_imm.CI[[1]],4) 
e1b.d.CI.L.mrl_imm <- round(e1b.d.mrl_imm.CI[[12]][1],4)
e1b.d.CI.H.mrl_imm <- round(e1b.d.mrl_imm.CI[[12]][2],4)

# Good vs Normal
e1b.d.t.mrl_ave <- t.test(df1b.V.SDT_ww$Moral,df1b.V.SDT_ww$Neutral,paired = TRUE)
e1b.d.cor.mrl_ave <- cor(df1b.V.SDT_ww$Moral,df1b.V.SDT_ww$Neutral)
df1b.V.SDT_ww$mrl_ave <- df1b.V.SDT_ww$Moral - df1b.V.SDT_ww$Neutral
e1b.d.mrl_ave.CI <- bootES(df1b.V.SDT_ww$mrl_ave,R = 20000, effect.type = "cohens.d")

e1b.d.tvalue.mrl_ave  <- round(as.numeric(e1b.d.t.mrl_ave[[1]]),3)
e1b.d.df1b.mrl_ave  <- as.numeric(e1b.d.t.mrl_ave[[2]])
e1b.d.pvalue.mrl_ave.adj <- p.adjust(as.numeric(e1b.d.t.mrl_ave[[3]],"bonferroni",3))
e1b.d.cohens.mrl_ave <- round(e1b.d.mrl_ave.CI[[1]],4) 
e1b.d.CI.L.mrl_ave <- round(e1b.d.mrl_ave.CI[[12]][1],4)
e1b.d.CI.H.mrl_ave <- round(e1b.d.mrl_ave.CI[[12]][2],4)

# Bad vs. Average
e1b.d.t.imm_ave <- t.test(df1b.V.SDT_ww$Immoral,df1b.V.SDT_ww$Neutral,paired = TRUE)
e1b.d.cor.imm_ave <- cor(df1b.V.SDT_ww$Immoral,df1b.V.SDT_ww$Neutral)
df1b.V.SDT_ww$imm_ave <- df1b.V.SDT_ww$Immoral - df1b.V.SDT_ww$Neutral
e1b.d.imm_ave.CI <- bootES(df1b.V.SDT_ww$imm_ave,R = 20000, effect.type = "cohens.d")

e1b.d.tvalue.imm_ave <- round(as.numeric(e1b.d.t.imm_ave[[1]]),3)
e1b.d.df1b.imm_ave <- as.numeric(e1b.d.t.imm_ave[[2]])
e1b.d.pvalue.imm_ave.adj <- p.adjust(as.numeric(e1b.d.t.imm_ave[[3]],"bonferroni",3))
e1b.d.cohens.imm_ave <- round(e1b.d.imm_ave.CI[[1]],4) 
e1b.d.CI.L.imm_ave <- round(e1b.d.imm_ave.CI[[12]][1],4)
e1b.d.CI.H.imm_ave <- round(e1b.d.imm_ave.CI[[12]][2],4)

## plot and save the results of d'
df1b.V.SDT.sum <- summarySE(df1b.V.SDT_w,measurevar = 'dprime',groupvars = c('Morality'))
e1b.d.mean.ml  <- round(df1b.V.SDT.sum$dprime[df1b.V.SDT.sum$Morality == 'Moral'],2)
e1b.d.sd.ml    <- round(df1b.V.SDT.sum$sd[df1b.V.SDT.sum$Morality == 'Moral'],2)
e1b.d.mean.im  <- round(df1b.V.SDT.sum$dprime[df1b.V.SDT.sum$Morality == 'Immoral'],2)
e1b.d.sd.im    <- round(df1b.V.SDT.sum$sd[df1b.V.SDT.sum$Morality == 'Immoral'],2)
e1b.d.mean.av  <- round(df1b.V.SDT.sum$dprime[df1b.V.SDT.sum$Morality == 'Neutral'],2)
e1b.d.sd.av    <- round(df1b.V.SDT.sum$sd[df1b.V.SDT.sum$Morality == 'Neutral'],2)

# calculate the effect size for meta
# correlation between
e1b.d.cor.mrl_imm <- cor(df1b.V.SDT_ww$Moral,df1b.V.SDT_ww$Immoral)
e1b.d.cor.mrl_ave <- cor(df1b.V.SDT_ww$Moral,df1b.V.SDT_ww$Neutral)
e1b.d.cor.imm_ave <- cor(df1b.V.SDT_ww$Immoral,df1b.V.SDT_ww$Neutral)

# effect size and variance
e1b.d.es.mrl_imm <- d.sgpp(m.1 = e1b.d.mean.ml, m.2 = e1b.d.mean.im, sd.1=e1b.d.sd.ml,sd.2=e1b.d.sd.im, n=length(df1b.V.SDT_ww$moral),r=e1b.d.cor.mrl_imm)

e1b.d.es.mrl_ave <- d.sgpp(m.1 = e1b.d.mean.ml, m.2 = e1b.d.mean.av, sd.1=e1b.d.sd.ml,sd.2=e1b.d.sd.av, n=length(df1b.V.SDT_ww$moral),r=e1b.d.cor.mrl_ave)

e1b.d.es.imm_ave <- d.sgpp(m.1 = e1b.d.mean.im, m.2 = e1b.d.mean.av, sd.1=e1b.d.sd.im,sd.2=e1b.d.sd.av, n=length(df1b.V.SDT_ww$moral),r=e1b.d.cor.imm_ave)


e1b.p_dprime <- ggplot(data = df1b.V.SDT.sum,aes(y = dprime, x = Morality, group = Morality,shape = Morality, fill = Morality)) +
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

ggsave("e1b_2.p_dprime.pdf", e1b.p_dprime, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

## doing the analysis for RT ####
df1b.V.RT <- df1b.V[df1b.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df1b.V.RT.subj <- summarySEwithin(df1b.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'), idvar = 'Subject',na.rm = TRUE)
e1b.rt_anova <- ezANOVA(df1b.V.RT,dv = RT, wid = Subject, within=.(Morality,Matchness),within_full=.(Morality,Matchness), type=3)

df1b.V.RT.grand <- summarySE(df1b.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality'),na.rm = TRUE)
df1b.V.RT_match <- df1b.V.RT[df1b.V.RT$Matchness == "Match",]
df1b.V.RT_mismatch <- df1b.V.RT[df1b.V.RT$Matchness == "Mismatch",]

e1b.rt_anova.match <- ezANOVA(df1b.V.RT_match,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
e1b.rt_anova.mismatch <- ezANOVA(df1b.V.RT_mismatch,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)

## t-test for matched trials
df1b.V.RT.subj_w <- dcast(df1b.V.RT.subj, Subject ~ Matchness + Morality ,value.var = "RT") 
df1b.V.RT.subj_w_r <-df1b.V.RT.subj_w

# moral vs. immoral
e1b.rt.t.m.mrl_imm <- t.test(df1b.V.RT.subj_w$Match_Moral,df1b.V.RT.subj_w$Match_Immoral,paired = TRUE)
df1b.V.RT.subj_w$m.mrl_imm <- df1b.V.RT.subj_w$Match_Moral - df1b.V.RT.subj_w$Match_Immoral
e1b.rt.t.m.mrl_imm.CI <- bootES(df1b.V.RT.subj_w$m.mrl_imm, R = 20000,effect.type = "cohens.d")

e1b.rt.tvalue.mrl_imm  <- round(as.numeric(e1b.rt.t.m.mrl_imm [[1]]),3)
e1b.rt.df1b.mrl_imm  <- as.numeric(e1b.rt.t.m.mrl_imm [[2]])
e1b.rt.pvalue.mrl_imm.adj <- p.adjust(as.numeric(e1b.rt.t.m.mrl_imm [[3]],"bonferroni",3))
e1b.rt.cohens.mrl_imm <- round(e1b.rt.t.m.mrl_imm.CI[[1]],4) 
e1b.rt.CI.L.mrl_imm <- round(e1b.rt.t.m.mrl_imm.CI[[12]][1],4)
e1b.rt.CI.H.mrl_imm <- round(e1b.rt.t.m.mrl_imm.CI[[12]][2],4)

# moral vs. average
e1b.rt.t.m.mrl_ave <- t.test(df1b.V.RT.subj_w$Match_Moral,df1b.V.RT.subj_w$Match_Neutral,paired = TRUE)
df1b.V.RT.subj_w$m.mrl_ave <- df1b.V.RT.subj_w$Match_Moral - df1b.V.RT.subj_w$Match_Neutral
e1b.rt.t.m.mrl_ave.CI <- bootES(df1b.V.RT.subj_w$m.mrl_ave, R = 20000,effect.type = "cohens.d")

e1b.rt.tvalue.mrl_ave <- round(as.numeric(e1b.rt.t.m.mrl_ave [[1]]),3)
e1b.rt.df1b.mrl_ave <- as.numeric(e1b.rt.t.m.mrl_ave[[2]])
e1b.rt.pvalue.mrl_ave.adj <- p.adjust(as.numeric(e1b.rt.t.m.mrl_ave[[3]],"bonferroni",3))
e1b.rt.cohens.mrl_ave <- round(e1b.rt.t.m.mrl_ave.CI[[1]],4) 
e1b.rt.CI.L.mrl_ave <- round(e1b.rt.t.m.mrl_ave.CI[[12]][1],4)
e1b.rt.CI.H.mrl_ave <- round(e1b.rt.t.m.mrl_ave.CI[[12]][2],4)

# immoral vs. average
e1b.rt.t.m.imm_ave <- t.test(df1b.V.RT.subj_w$Match_Immoral,df1b.V.RT.subj_w$Match_Neutral,paired = TRUE)
df1b.V.RT.subj_w$m.imm_ave <- df1b.V.RT.subj_w$Match_Immoral - df1b.V.RT.subj_w$Match_Neutral
e1b.rt.t.m.imm_ave.CI <- bootES(df1b.V.RT.subj_w$m.imm_ave, R = 20000,effect.type = "cohens.d")

e1b.rt.tvalue.imm_ave <- round(as.numeric(e1b.rt.t.m.imm_ave[[1]]),3)
e1b.rt.df1b.imm_ave  <- as.numeric(e1b.rt.t.m.imm_ave[[2]])
e1b.rt.pvalue.imm_ave.adj <- p.adjust(as.numeric(e1b.rt.t.m.imm_ave[[3]],"bonferroni",3))
e1b.rt.cohens.imm_ave <- round(e1b.rt.t.m.imm_ave.CI [[1]],4) 
e1b.rt.CI.L.imm_ave <- round(e1b.rt.t.m.imm_ave.CI[[12]][1],4)
e1b.rt.CI.H.imm_ave <- round(e1b.rt.t.m.imm_ave.CI[[12]][2],4)

df1b.V.RT.grand.match <- df1b.V.RT.grand[df1b.V.RT.grand$Matchness == "match",]
e1b.rt.mean.ml <- round(df1b.V.RT.grand.match$RT[df1b.V.RT.grand.match$Morality == 'moral'],0)
e1b.rt.sd.ml <- round(df1b.V.RT.grand.match$sd[df1b.V.RT.grand.match$Morality == 'moral'],0)
e1b.rt.mean.im <- round(df1b.V.RT.grand.match$RT[df1b.V.RT.grand.match$Morality == 'immoral'],0)
e1b.rt.sd.im <- round(df1b.V.RT.grand.match$sd[df1b.V.RT.grand.match$Morality == 'immoral'],0)
e1b.rt.mean.av <- round(df1b.V.RT.grand.match$RT[df1b.V.RT.grand.match$Morality == 'average'],0)
e1b.rt.sd.av <- round(df1b.V.RT.grand.match$sd[df1b.V.RT.grand.match$Morality == 'average'],0)

# calculate the effect size for rt
# correlation between
e1b.rt.cor.mrl_imm <- cor(df1b.V.RT.subj_w$Match_Moral,df1b.V.RT.subj_w$Match_Immoral)
e1b.rt.cor.mrl_ave <- cor(df1b.V.RT.subj_w$Match_Moral,df1b.V.RT.subj_w$Match_Neutral)
e1b.rt.cor.imm_ave <- cor(df1b.V.RT.subj_w$Match_Immoral,df1b.V.RT.subj_w$Match_Neutral)

# effect size and variance
e1b.rt.es.mrl_imm <- d.sgpp(m.1 = e1b.rt.mean.ml, m.2 = e1b.rt.mean.im, sd.1=e1b.rt.sd.ml,sd.2=e1b.rt.sd.im, n=length(df1b.V.RT.subj_w$Match_Moral),r=e1b.rt.cor.mrl_imm)

e1b.rt.es.mrl_ave <- d.sgpp(m.1 = e1b.rt.mean.ml, m.2 = e1b.rt.mean.av, sd.1=e1b.rt.sd.ml,sd.2=e1b.rt.sd.av, n=length(df1b.V.RT.subj_w$Match_Moral),r=e1b.rt.cor.mrl_ave)

e1b.rt.es.imm_ave <- d.sgpp(m.1 = e1b.rt.mean.im, m.2 = e1b.rt.mean.av, sd.1=e1b.rt.sd.im,sd.2=e1b.rt.sd.av, n=length(df1b.V.RT.subj_w$Match_Moral),r=e1b.rt.cor.imm_ave)

# plot
e1b.p_rt <- ggplot(data = df1b.V.RT.grand.match, aes(x=Morality,y=RT,group=Morality,shape = Morality,fill = Morality)) +
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

ggsave("e1b_2.p_RT.pdf", e1b.p_rt, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

