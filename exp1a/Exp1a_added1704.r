## this code is to analyze the added data for exp 1, these data were colleted at Wenzhou U in 201704

## initializing
source('Initial.r')

## load data and edite data
df1a_1 <- read.csv("rawdata_behav_exp1a_2014.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1a_1$Subject))
df1a_2 <- read.csv("rawdata_behav_exp1a_2017.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
length(unique(df1a_2$Subject))
df1a   <- rbind(df1a_1,df1a_2)

# rename colnames
colnames(df1a)[colnames(df1a)=="Target.ACC"] <- "ACC"
colnames(df1a)[colnames(df1a)=="Target.RT"]  <- "RT"
colnames(df1a)[colnames(df1a)=="YesNoResp"]  <- "Matchness"
colnames(df1a)[colnames(df1a)=="Shape"]      <- "Morality"

# renames independent variables
df1a$Morality[df1a$Morality == "Good"]   <- "Moral"
df1a$Morality[df1a$Morality == "Normal"] <- "Neutral"
df1a$Morality[df1a$Morality == "Bad"]    <- "Immoral"
df1a$Morality <- factor(df1a$Morality, levels=c("Moral", "Neutral","Immoral")) # make the variables in a specified order

df1a$Matchness[df1a$Matchness == "Yes"] <- "Match"
df1a$Matchness[df1a$Matchness == "No"] <- "Mismatch"
df1a$Matchness <- factor(df1a$Matchness, levels=c("Match", "Mismatch"))

## Basic information of the data ####
df1a.T.basic <- df1a[!duplicated(df1a$Subject), 1:4]
e1a.num.subj <- nrow(df1a.T.basic)
e1a.numT.female <- sum(df1a.T.basic$Sex == 'female');
e1a.numT.male <- sum(df1a.T.basic$Sex == 'male');
e1a.ageT.mean <- round(mean(df1a.T.basic$Age),2);
e1a.ageT.std <- round(sd(df1a.T.basic$Age),2);

# data from foreign students were excluded because their may not familar with Chinese Character
#foreignStdID <- c(24,29,30,33)
#nforeign <- length(foreignStdID)
#df1a <- df1a[!(df1a$Subject %in% foreignStdID),]

df1a.P <- df1a[is.na(df1a$BlockList.Sample),]            # data from practice
df1a.T <- df1a[complete.cases(df1a$BlockList.Sample),]   # data from test

e1a.excld.trials <- df1a.T[df1a.T$RT <= 200,]
e1a.ratio.excld.trials <- nrow(e1a.excld.trials)/nrow(df1a.T) # ratio of excluded trials in all triasl.
# caculate the overall accuracy for each subject
df1a.acc.g <-  ddply(df1a.T,.(Subject), summarise,
                     N = length(ACC),
                     countN = sum(ACC),
                     ACC = sum(ACC)/length(ACC))
e1a.excld.sub <- df1a.acc.g$Subject[df1a.acc.g$ACC < 0.6]
df1a.valid <- df1a.T[!(df1a.T$Subject %in% e1a.excld.sub),] # exclude the invalid subjects
length(unique(df1a.valid$Subject)) + length(e1a.excld.sub) == length(unique(df1a$Subject))
# excld.trials3 <- excld.trials[!(excld.trials$Subject %in% e1a.excld.sub),]
e1a.excld.trials2 <- df1a.valid[df1a.valid$RT <= 200,]
df1a.V <- df1a.valid[!(df1a.valid$RT <= 200),]  

## Basic information of the data ####
e1a.num.excld.sub <- length(unique(e1a.excld.sub))
df1a.V.basic <- df1a.V[!duplicated(df1a.V$Subject), 1:4]
e1a.numV.female <- sum(df1a.V.basic$Sex == 'female');
e1a.numV.male <- sum(df1a.V.basic$Sex == 'male');
e1a.ageV.mean <- round(mean(df1a.V.basic$Age),2);
e1a.ageV.std <- round(sd(df1a.V.basic$Age),2);
e1a.ratio.excld.trials2 <- nrow(e1a.excld.trials2)/nrow(df1a.valid)

# start to analyze the d prime
df1a.V$sdt <- NA
for (i in 1:nrow(df1a.V)){
        if (df1a.V$Target.RESP[i] == df1a.V$Target.CRESP[i] & df1a.V$Matchness[i] == "Match"){
                df1a.V$sdt[i] <- "hit"
        } else if (df1a.V$Target.RESP[i] == df1a.V$Target.CRESP[i] & df1a.V$Matchness[i] == "Mismatch"){
                df1a.V$sdt[i] <- "CR"
        } else if (df1a.V$Target.RESP[i] != df1a.V$Target.CRESP[i] & df1a.V$Matchness[i] == "Match"){
                df1a.V$sdt[i] <- "miss"
        } else if (df1a.V$Target.RESP[i] != df1a.V$Target.CRESP[i] & df1a.V$Matchness[i] == "Mismatch"){
                df1a.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df1a.V.SDT <-  ddply(df1a.V,.(Subject,Age, Sex, Morality,sdt), summarise,
                     N = length(sdt))


# long format to wide
df1a.V.SDT_w <- dcast(df1a.V.SDT, Subject + Age + Sex+ Morality  ~ sdt,value.var = "N")
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

# anova for d prime with 2*2 design
e1a.d_anova1 <- ezANOVA(df1a.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality), type=3)
print(e1a.d_anova1)

# change dprime data from long format to wide
df1a.V.SDT_ww   <- dcast(df1a.V.SDT_w, Subject + Sex + Age ~ Morality ,value.var = "dprime")
df1a.V.SDT_ww_r <- df1a.V.SDT_ww
colnames(df1a.V.SDT_ww_r)[4:6] <- paste("d", colnames(df1a.V.SDT_ww_r[,4:6]), sep = "_")

# t-test
# good vs bad
e1a.d.t.mrl_imm <- t.test(df1a.V.SDT_ww$Moral,df1a.V.SDT_ww$Immoral,paired = TRUE)

df1a.V.SDT_ww$mrl_imm <- df1a.V.SDT_ww$Moral - df1a.V.SDT_ww$Immoral
e1a.d.mrl_imm.CI <- bootES(df1a.V.SDT_ww$mrl_imm,R = 20000, effect.type = "cohens.d")

e1a.d.tvalue.mrl_imm <- round(as.numeric(e1a.d.t.mrl_imm[[1]]),3)
e1a.d.df1a.d.mrl_imm <- as.numeric(e1a.d.t.mrl_imm[[2]])
e1a.d.pvalue.mrl_imm.adj <- p.adjust(as.numeric(e1a.d.t.mrl_imm[[3]],"bonferroni",3))
e1a.d.cohens.mrl_imm <- round(e1a.d.mrl_imm.CI[[1]],4) 
e1a.d.CI.L.mrl_imm <- round(e1a.d.mrl_imm.CI[[12]][1],4)
e1a.d.CI.H.mrl_imm <- round(e1a.d.mrl_imm.CI[[12]][2],4)

# Good vs Normal
e1a.d.t.mrl_ave <- t.test(df1a.V.SDT_ww$Moral,df1a.V.SDT_ww$Neutral,paired = TRUE)
e1a.d.cor.mrl_ave <- cor(df1a.V.SDT_ww$Moral,df1a.V.SDT_ww$Neutral)
df1a.V.SDT_ww$mrl_ave <- df1a.V.SDT_ww$Moral - df1a.V.SDT_ww$Neutral
e1a.d.mrl_ave.CI <- bootES(df1a.V.SDT_ww$mrl_ave,R = 20000, effect.type = "cohens.d")

e1a.d.tvalue.mrl_ave  <- round(as.numeric(e1a.d.t.mrl_ave[[1]]),3)
e1a.d.df1a.mrl_ave  <- as.numeric(e1a.d.t.mrl_ave[[2]])
e1a.d.pvalue.mrl_ave.adj <- p.adjust(as.numeric(e1a.d.t.mrl_ave[[3]],"bonferroni",3))
e1a.d.cohens.mrl_ave <- round(e1a.d.mrl_ave.CI[[1]],4) 
e1a.d.CI.L.mrl_ave <- round(e1a.d.mrl_ave.CI[[12]][1],4)
e1a.d.CI.H.mrl_ave <- round(e1a.d.mrl_ave.CI[[12]][2],4)

# Bad vs. Average
e1a.d.t.imm_ave <- t.test(df1a.V.SDT_ww$Immoral,df1a.V.SDT_ww$Neutral,paired = TRUE)
e1a.d.cor.imm_ave <- cor(df1a.V.SDT_ww$Immoral,df1a.V.SDT_ww$Neutral)
df1a.V.SDT_ww$imm_ave <- df1a.V.SDT_ww$Immoral - df1a.V.SDT_ww$Neutral
e1a.d.imm_ave.CI <- bootES(df1a.V.SDT_ww$imm_ave,R = 20000, effect.type = "cohens.d")

e1a.d.tvalue.imm_ave <- round(as.numeric(e1a.d.t.imm_ave[[1]]),3)
e1a.d.df1a.imm_ave <- as.numeric(e1a.d.t.imm_ave[[2]])
e1a.d.pvalue.imm_ave.adj <- p.adjust(as.numeric(e1a.d.t.imm_ave[[3]],"bonferroni",3))
e1a.d.cohens.imm_ave <- round(e1a.d.imm_ave.CI[[1]],4) 
e1a.d.CI.L.imm_ave <- round(e1a.d.imm_ave.CI[[12]][1],4)
e1a.d.CI.H.imm_ave <- round(e1a.d.imm_ave.CI[[12]][2],4)

## plot and save the results of d'
df1a.V.SDT.sum <- summarySE(df1a.V.SDT_w,measurevar = 'dprime',groupvars = c('Morality'))
e1a.d.mean.ml  <- round(df1a.V.SDT.sum$dprime[df1a.V.SDT.sum$Morality == 'Moral'],2)
e1a.d.sd.ml    <- round(df1a.V.SDT.sum$sd[df1a.V.SDT.sum$Morality == 'Moral'],2)
e1a.d.mean.im  <- round(df1a.V.SDT.sum$dprime[df1a.V.SDT.sum$Morality == 'Immoral'],2)
e1a.d.sd.im    <- round(df1a.V.SDT.sum$sd[df1a.V.SDT.sum$Morality == 'Immoral'],2)
e1a.d.mean.av  <- round(df1a.V.SDT.sum$dprime[df1a.V.SDT.sum$Morality == 'Neutral'],2)
e1a.d.sd.av    <- round(df1a.V.SDT.sum$sd[df1a.V.SDT.sum$Morality == 'Neutral'],2)

# calculate the effect size for meta
# correlation between
e1a.d.cor.mrl_imm <- cor(df1a.V.SDT_ww$Moral,df1a.V.SDT_ww$Immoral)
e1a.d.cor.mrl_ave <- cor(df1a.V.SDT_ww$Moral,df1a.V.SDT_ww$Neutral)
e1a.d.cor.imm_ave <- cor(df1a.V.SDT_ww$Immoral,df1a.V.SDT_ww$Neutral)

# effect size and variance
e1a.d.es.mrl_imm <- d.sgpp(m.1 = e1a.d.mean.ml, m.2 = e1a.d.mean.im, sd.1=e1a.d.sd.ml,sd.2=e1a.d.sd.im, n=length(df1a.V.SDT_ww$moral),r=e1a.d.cor.mrl_imm)

e1a.d.es.mrl_ave <- d.sgpp(m.1 = e1a.d.mean.ml, m.2 = e1a.d.mean.av, sd.1=e1a.d.sd.ml,sd.2=e1a.d.sd.av, n=length(df1a.V.SDT_ww$moral),r=e1a.d.cor.mrl_ave)

e1a.d.es.imm_ave <- d.sgpp(m.1 = e1a.d.mean.im, m.2 = e1a.d.mean.av, sd.1=e1a.d.sd.im,sd.2=e1a.d.sd.av, n=length(df1a.V.SDT_ww$moral),r=e1a.d.cor.imm_ave)


e1a.p_dprime <- ggplot(data = df1a.V.SDT.sum,aes(y = dprime, x = Morality, group = Morality,shape = Morality, fill = Morality)) +
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

ggsave("e1a_2.p_dprime.pdf", e1a.p_dprime, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

## doing the analysis for RT ####
df1a.V.RT <- df1a.V[df1a.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df1a.V.RT.subj <- summarySEwithin(df1a.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality'), idvar = 'Subject',na.rm = TRUE)
e1a.rt_anova <- ezANOVA(df1a.V.RT,dv = RT, wid = Subject, within=.(Morality,Matchness),within_full=.(Morality,Matchness), type=3)

df1a.V.RT.grand <- summarySE(df1a.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality'),na.rm = TRUE)
df1a.V.RT_match <- df1a.V.RT[df1a.V.RT$Matchness == "Match",]
df1a.V.RT_mismatch <- df1a.V.RT[df1a.V.RT$Matchness == "Mismatch",]

e1a.rt_anova.match <- ezANOVA(df1a.V.RT_match,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
e1a.rt_anova.mismatch <- ezANOVA(df1a.V.RT_mismatch,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)

## t-test for matched trials
df1a.V.RT.subj_w <- dcast(df1a.V.RT.subj, Subject ~ Matchness + Morality ,value.var = "RT") 
df1a.V.RT.subj_w_r <-df1a.V.RT.subj_w

# rename the columns of RT data
colnames(df1a.V.RT.subj_w_r)[2:7] <- paste("RT", colnames(df1a.V.RT.subj_w_r[,2:7]), sep = "_")

# merge the dprime and RT data and save
df1a.V.sum_w <- merge(df1a.V.SDT_ww_r,df1a.V.RT.subj_w_r,by="Subject")
write.csv(df1a.V.sum_w,'Data_exp1a_RT_dprime.csv',row.names = F)

# moral vs. immoral
e1a.rt.t.m.mrl_imm <- t.test(df1a.V.RT.subj_w$Match_Moral,df1a.V.RT.subj_w$Match_Immoral,paired = TRUE)
df1a.V.RT.subj_w$m.mrl_imm <- df1a.V.RT.subj_w$Match_Moral - df1a.V.RT.subj_w$Match_Immoral
e1a.rt.t.m.mrl_imm.CI <- bootES(df1a.V.RT.subj_w$m.mrl_imm, R = 20000,effect.type = "cohens.d")

e1a.rt.tvalue.mrl_imm  <- round(as.numeric(e1a.rt.t.m.mrl_imm [[1]]),3)
e1a.rt.df1a.mrl_imm  <- as.numeric(e1a.rt.t.m.mrl_imm [[2]])
e1a.rt.pvalue.mrl_imm.adj <- p.adjust(as.numeric(e1a.rt.t.m.mrl_imm [[3]],"bonferroni",3))
e1a.rt.cohens.mrl_imm <- round(e1a.rt.t.m.mrl_imm.CI[[1]],4) 
e1a.rt.CI.L.mrl_imm <- round(e1a.rt.t.m.mrl_imm.CI[[12]][1],4)
e1a.rt.CI.H.mrl_imm <- round(e1a.rt.t.m.mrl_imm.CI[[12]][2],4)

# moral vs. average
e1a.rt.t.m.mrl_ave <- t.test(df1a.V.RT.subj_w$Match_Moral,df1a.V.RT.subj_w$Match_Neutral,paired = TRUE)
df1a.V.RT.subj_w$m.mrl_ave <- df1a.V.RT.subj_w$Match_Moral - df1a.V.RT.subj_w$Match_Neutral
e1a.rt.t.m.mrl_ave.CI <- bootES(df1a.V.RT.subj_w$m.mrl_ave, R = 20000,effect.type = "cohens.d")

e1a.rt.tvalue.mrl_ave <- round(as.numeric(e1a.rt.t.m.mrl_ave [[1]]),3)
e1a.rt.df1a.mrl_ave <- as.numeric(e1a.rt.t.m.mrl_ave[[2]])
e1a.rt.pvalue.mrl_ave.adj <- p.adjust(as.numeric(e1a.rt.t.m.mrl_ave[[3]],"bonferroni",3))
e1a.rt.cohens.mrl_ave <- round(e1a.rt.t.m.mrl_ave.CI[[1]],4) 
e1a.rt.CI.L.mrl_ave <- round(e1a.rt.t.m.mrl_ave.CI[[12]][1],4)
e1a.rt.CI.H.mrl_ave <- round(e1a.rt.t.m.mrl_ave.CI[[12]][2],4)

# immoral vs. average
e1a.rt.t.m.imm_ave <- t.test(df1a.V.RT.subj_w$Match_Immoral,df1a.V.RT.subj_w$Match_Neutral,paired = TRUE)
df1a.V.RT.subj_w$m.imm_ave <- df1a.V.RT.subj_w$Match_Immoral - df1a.V.RT.subj_w$Match_Neutral
e1a.rt.t.m.imm_ave.CI <- bootES(df1a.V.RT.subj_w$m.imm_ave, R = 20000,effect.type = "cohens.d")

e1a.rt.tvalue.imm_ave <- round(as.numeric(e1a.rt.t.m.imm_ave[[1]]),3)
e1a.rt.df1a.imm_ave  <- as.numeric(e1a.rt.t.m.imm_ave[[2]])
e1a.rt.pvalue.imm_ave.adj <- p.adjust(as.numeric(e1a.rt.t.m.imm_ave[[3]],"bonferroni",3))
e1a.rt.cohens.imm_ave <- round(e1a.rt.t.m.imm_ave.CI [[1]],4) 
e1a.rt.CI.L.imm_ave <- round(e1a.rt.t.m.imm_ave.CI[[12]][1],4)
e1a.rt.CI.H.imm_ave <- round(e1a.rt.t.m.imm_ave.CI[[12]][2],4)

df1a.V.RT.grand.match <- df1a.V.RT.grand[df1a.V.RT.grand$Matchness == "match",]
e1a.rt.mean.ml <- round(df1a.V.RT.grand.match$RT[df1a.V.RT.grand.match$Morality == 'moral'],0)
e1a.rt.sd.ml <- round(df1a.V.RT.grand.match$sd[df1a.V.RT.grand.match$Morality == 'moral'],0)
e1a.rt.mean.im <- round(df1a.V.RT.grand.match$RT[df1a.V.RT.grand.match$Morality == 'immoral'],0)
e1a.rt.sd.im <- round(df1a.V.RT.grand.match$sd[df1a.V.RT.grand.match$Morality == 'immoral'],0)
e1a.rt.mean.av <- round(df1a.V.RT.grand.match$RT[df1a.V.RT.grand.match$Morality == 'average'],0)
e1a.rt.sd.av <- round(df1a.V.RT.grand.match$sd[df1a.V.RT.grand.match$Morality == 'average'],0)

# calculate the effect size for rt
# correlation between
e1a.rt.cor.mrl_imm <- cor(df1a.V.RT.subj_w$Match_Moral,df1a.V.RT.subj_w$Match_Immoral)
e1a.rt.cor.mrl_ave <- cor(df1a.V.RT.subj_w$Match_Moral,df1a.V.RT.subj_w$Match_Neutral)
e1a.rt.cor.imm_ave <- cor(df1a.V.RT.subj_w$Match_Immoral,df1a.V.RT.subj_w$Match_Neutral)

# effect size and variance
e1a.rt.es.mrl_imm <- d.sgpp(m.1 = e1a.rt.mean.ml, m.2 = e1a.rt.mean.im, sd.1=e1a.rt.sd.ml,sd.2=e1a.rt.sd.im, n=length(df1a.V.RT.subj_w$Match_Moral),r=e1a.rt.cor.mrl_imm)

e1a.rt.es.mrl_ave <- d.sgpp(m.1 = e1a.rt.mean.ml, m.2 = e1a.rt.mean.av, sd.1=e1a.rt.sd.ml,sd.2=e1a.rt.sd.av, n=length(df1a.V.RT.subj_w$Match_Moral),r=e1a.rt.cor.mrl_ave)

e1a.rt.es.imm_ave <- d.sgpp(m.1 = e1a.rt.mean.im, m.2 = e1a.rt.mean.av, sd.1=e1a.rt.sd.im,sd.2=e1a.rt.sd.av, n=length(df1a.V.RT.subj_w$Match_Moral),r=e1a.rt.cor.imm_ave)

# plot
e1a.p_rt <- ggplot(data = df1a.V.RT.grand.match, aes(x=Morality,y=RT,group=Morality,shape = Morality,fill = Morality)) +
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

ggsave("e1a_2.p_RT.pdf", e1a.p_rt, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

