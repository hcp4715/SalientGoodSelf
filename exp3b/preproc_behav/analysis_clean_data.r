## analysis for experiment 3.1

# initializing
source('Initial.r')

# packages
pkgNeeded <- (c("data.table","plyr","ggplot2", "reshape", 'dplyr','ggthemes','gridExtra',"MBESS", "bootES","metafor","compute.es",'psych','ez'))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

# load valid data:
df3_1.V <- read.csv("cleanData_exp3_1.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

# added the presenting order
df3_1.V$PreOrder[df3_1.V$Subject%%2 == 1] <-1 
df3_1.V$PreOrder[df3_1.V$Subject%%2 == 0] <-2 


# rank the factors
df3_1.V$Matchness <- factor(df3_1.V$Matchness, levels=c("Match", "Mismatch"))
df3_1.V$Morality <- factor(df3_1.V$Morality, levels=c("Good", "Neutral","Bad")) # make the variables in a specified order
df3_1.V$Identity <- factor(df3_1.V$Identity, levels=c("Self", "Other"))


df3_1.V$sdt <- NA
for (i in 1:nrow(df3_1.V)){
        if (is.na(df3_1.V$RESP[i]) & df3_1.V$Matchness[i] == "Match"){
                df3_1.V$sdt[i] <- "miss"
        }
          else if ((is.na(df3_1.V$RESP[i]) & df3_1.V$Matchness[i] == "Mismatch")){
                df3_1.V$sdt[i] <- NA
        }
          else if (df3_1.V$RESP[i] == df3_1.V$CorrectAnswer[i] & df3_1.V$Matchness[i] == "Match"){
                df3_1.V$sdt[i] <- "hit"
        } else if (df3_1.V$RESP[i] == df3_1.V$CorrectAnswer[i] & df3_1.V$Matchness[i] == "Mismatch"){
                df3_1.V$sdt[i] <- "CR"
        } else if (df3_1.V$RESP[i] != df3_1.V$CorrectAnswer[i] & df3_1.V$Matchness[i] == "Match"){
                df3_1.V$sdt[i] <- "miss"
        } else if (df3_1.V$RESP[i] != df3_1.V$CorrectAnswer[i] & df3_1.V$Matchness[i] == "Mismatch"){
                df3_1.V$sdt[i] <- "FA"
        }
}

# calculate the number of each for each condition
df3_1.V.SDT <-  ddply(df3_1.V,.(Subject,Sex, Age, Morality, Identity,sdt), summarise, N = length(sdt))
df3_1.V.SDT <- df3_1.V.SDT[complete.cases(df3_1.V.SDT$sdt),]


# long format to wide
df3_1.V.SDT_w <- dcast(df3_1.V.SDT, Subject + Sex + Age + Morality + Identity ~ sdt,value.var = "N")

excld.sub <- df3_1.V.SDT_w$Subject[is.na(df3_1.V.SDT_w$hit)]             # exclude one participant who's hit rate is 0 for one condition
df3_1.V.SDT_w <- df3_1.V.SDT_w[!(df3_1.V.SDT_w$Subject %in% excld.sub),] 
df3_1.V <- df3_1.V[!(df3_1.V$Subject %in% excld.sub),] 


df3_1.V.SDT_w$miss[is.na(df3_1.V.SDT_w$miss)] <- 0
df3_1.V.SDT_w$FA[is.na(df3_1.V.SDT_w$FA)] <- 0
df3_1.V.SDT_w$hitR <- df3_1.V.SDT_w$hit/(df3_1.V.SDT_w$hit + df3_1.V.SDT_w$miss)
df3_1.V.SDT_w$faR <- df3_1.V.SDT_w$FA/(df3_1.V.SDT_w$FA + df3_1.V.SDT_w$CR)

# standardized way to deal with the extreme values
for (i in 1:nrow(df3_1.V.SDT_w)){
        if (df3_1.V.SDT_w$hitR[i] == 1){
                df3_1.V.SDT_w$hitR[i] <- 1 - 1/(2*60)
        }
}

for (i in 1:nrow(df3_1.V.SDT_w)){
        if (df3_1.V.SDT_w$faR[i] == 0){
                df3_1.V.SDT_w$faR[i] <- 1/(2*60)
        }
}


# calculate the d prime for each condition
df3_1.V.SDT_w$dprime <- mapply(dprime,df3_1.V.SDT_w$hitR,df3_1.V.SDT_w$faR)

# added the presenting order
df3_1.V.SDT_w$PreOrder[df3_1.V.SDT_w$Subject%%2 == 1] <-1 
df3_1.V.SDT_w$PreOrder[df3_1.V.SDT_w$Subject%%2 == 0] <-2 

df3_1.V.sdt_w.order1 <- df3_1.V.SDT_w[df3_1.V.SDT_w$PreOrder == 1,]
df3_1.V.sdt_w.order2 <- df3_1.V.SDT_w[df3_1.V.SDT_w$PreOrder == 2,]

df3_1.V.SDT_w.self <- df3_1.V.SDT_w[df3_1.V.SDT_w$Identity == 'Self',]
df3_1.V.SDT_w.other <- df3_1.V.SDT_w[df3_1.V.SDT_w$Identity == 'Other',]

# anova for d prime with 2*2 design
e3.d_anova <- ezANOVA(df3_1.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity),between = .(PreOrder), type=3)
e3.d_anova_ord1 <- ezANOVA(df3_1.V.SDT_w,dv = dprime, wid = Subject, within=.(Morality,Identity), type=3)

e3.d_anova.self <- ezANOVA(df3_1.V.SDT_w.self, dv = dprime, wid = Subject, within=.(Morality), type=3)
e3.d_anova.other <- ezANOVA(df3_1.V.SDT_w.other, dv = dprime, wid = Subject, within=.(Morality), type=3)
#print(d_anova1)


df3_1.V.SDT.sum <- summarySE(df3_1.V.SDT_w, measurevar = "dprime",groupvars = c('Morality','Identity'))
df3_1.V.SDT.sum$Morality <- factor(df3_1.V.SDT.sum$Morality, levels=c("Good", "Neutral","Bad"))
df3_1.V.SDT.sum$Identity <- factor(df3_1.V.SDT.sum$Identity, levels=c("Self", "Other"))

df3_1.V.SDT.sum_ord1 <- summarySE(df3_1.V.sdt_w.order1, measurevar = "dprime",groupvars = c('Morality','Identity'))
df3_1.V.SDT.sum_ord2 <- summarySE(df3_1.V.sdt_w.order2, measurevar = "dprime",groupvars = c('Morality','Identity'))

e3_1.p_dprime1 <- ggplot(data = df3_1.V.SDT.sum,aes(y = dprime, x = Identity, group = Morality,shape = Morality, fill = Morality)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
        geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                      #geom_errorbar(aes(ymin = 1, ymax = 4),
                      size = 1.5,
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
        apatheme  +
        theme(axis.text = element_text (color="black",size = 20)) + 
        theme(axis.title = element_text (color="black",size = 20)) + 
        theme(plot.title = element_text(color="black",size = 20)) +
        theme(legend.text = element_text(color="black",size =20)) +
        theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
        theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good   ",'Neutral   ',"Bad"))+
        theme(axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1)) 

ggsave("e3_1.p_dprime1.pdf", e3_1.p_dprime1, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")


e3_1.p_dprime2 <- ggplot(data = df3_1.V.SDT.sum,aes(y = dprime, x = Morality, group = Identity,shape = Identity, fill = Identity)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
        geom_errorbar(aes(ymin = dprime - se, ymax = dprime + se),
                      #geom_errorbar(aes(ymin = 1, ymax = 4),
                      size = 1.5,
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
        apatheme  +
        theme(axis.text = element_text (color="black",size = 20)) + 
        theme(axis.title = element_text (color="black",size = 20)) + 
        theme(plot.title = element_text(color="black",size = 20)) +
        theme(legend.text = element_text(color="black",size =20)) +
        theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
        theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self   ",'Other'))+
        theme(axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1)) 

ggsave("e3_1.p_dprime2.pdf", e3_1.p_dprime2, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")



## analysis for the RT times
df3_1.V.RT <- df3_1.V[df3_1.V$ACC ==1,]  # exclued rt data less than 200 ms, and inaccurate data
df3_1.V.RT$condition <- paste(df3_1.V.RT$Matchness,df3_1.V.RT$Identity,df3_1.V.RT$Morality,sep = '_')
df3_1.V.RT.subj <- summarySEwithin(df3_1.V.RT,measurevar = 'RT', withinvar = c('Subject','Matchness','Morality','Identity'),idvar = c('Subject','PreOrder'), na.rm = TRUE)

# added the presenting order
df3_1.V.RT.subj$Subject <- as.numeric(as.character(df3_1.V.RT.subj$Subject))
df3_1.V.RT.subj$PreOrder[df3_1.V.RT.subj$Subject%%2 == 1] <-1 
df3_1.V.RT.subj$PreOrder[df3_1.V.RT.subj$Subject%%2 == 0] <-2 
df3_1.V.RT.subj.ord1 <- df3_1.V.RT.subj[df3_1.V.RT.subj$PreOrder ==1,]
df3_1.V.RT.subj.ord2 <- df3_1.V.RT.subj[df3_1.V.RT.subj$PreOrder ==2,]

df3_1.V.RT.grand <- summarySE(df3_1.V.RT.subj,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)

df3_1.V.RT.grand.ord1 <- summarySE(df3_1.V.RT.subj.ord1,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)
df3_1.V.RT.grand.ord2 <- summarySE(df3_1.V.RT.subj.ord2,measurevar = 'RT', groupvar = c('Matchness','Morality','Identity'),na.rm = TRUE)


df3_1.V.RT.grand$Morality <- factor(df3_1.V.RT.grand$Morality, levels=c("Good", "Neutral","Bad")) # make the variables in a specified order
df3_1.V.RT.grand$Identity <- factor(df3_1.V.RT.grand$Identity, levels=c("Self", "Other"))
df3_1.V.RT.grand$Matchness <- factor(df3_1.V.RT.grand$Matchness, levels=c("Match", "Mismatch"))

df3_1.V.RT_match <- df3_1.V.RT[df3_1.V.RT$Matchness == "Match",]
df3_1.V.RT_mismatch <- df3_1.V.RT[df3_1.V.RT$Matchness == "Mismatch",]
df3_1.V.RT_match.self <- df3_1.V.RT_match[df3_1.V.RT_match$Identity == 'Self',]
df3_1.V.RT_match.other <- df3_1.V.RT_match[df3_1.V.RT_match$Identity == 'Other',]

df3_1.V.RT.subj_w <- dcast(df3_1.V.RT.subj, Subject ~ Matchness + Identity + Morality ,value.var = "RT")
df3_1.V.RT.subj_w_r <- df3_1.V.RT.subj_w 

# combine the self and other condition 
df3_1.V.RT.subj_w.moral <- dcast(df3_1.V.RT.subj, Subject ~ Matchness + Morality, mean, value.var = "RT") 

# self condition
df3_1.V.RT.subj_w.moralself <- df3_1.V.RT.subj[df3_1.V.RT.subj$Identity == 'Self',]
df3_1.V.RT.subj_w.moralself <- dcast(df3_1.V.RT.subj_w.moralself,Subject ~ Matchness + Morality, value.var = 'RT')

# other condition
df3_1.V.RT.subj_w.moralother <- df3_1.V.RT.subj[df3_1.V.RT.subj$Identity == 'Other',]
df3_1.V.RT.subj_w.moralother <- dcast(df3_1.V.RT.subj_w.moralother,Subject ~ Matchness + Morality, value.var = 'RT')

e3.rt_anova <- ezANOVA(df3_1.V.RT,dv = RT, wid = Subject, within=.(Matchness,Morality,Identity),within_full=.(Matchness,Identity,Morality), type=3)
e3.rt_anova.match <- ezANOVA(df3_1.V.RT_match,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)
e3.rt_anova.mismatch <- ezANOVA(df3_1.V.RT_mismatch,dv = RT, wid = Subject, within=.(Morality,Identity),within_full=.(Identity,Morality), type=3)

e3.rt_anova.match.self <- ezANOVA(df3_1.V.RT_match.self,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)
e3.rt_anova.match.other <- ezANOVA(df3_1.V.RT_match.other,dv = RT, wid = Subject, within=.(Morality),within_full=.(Morality), type=3)

# pairwise t test
pairwise.t.test(df3_1.V.RT$RT,df3_1.V.RT$condition,p.adjust.method = 'bonferroni')

df3_1.V.RT.grand.match <- df3_1.V.RT.grand[df3_1.V.RT.grand$Matchness == "Match",]
df3_1.V.RT.grand.mismatch <- df3_1.V.RT.grand[df3_1.V.RT.grand$Matchness == "Mismatch",]

## plot
e3_1.p_rt_M1 <- ggplot(data = df3_1.V.RT.grand.match, aes(x=Identity,y=RT,group=Morality,shape = Morality,fill = Morality)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
        geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                      size = 1,
                      width = .25,
                      position=position_dodge(.6)) +
        xlab("Identity") +
        ylab(" Reaction times (ms)") + 
        coord_cartesian(ylim=c(650,1000)) +
        scale_y_continuous(breaks=seq(650,1000,50),expand = c(0, 0)) +
        #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
        #ylim(0.3, 0.8) +
        ggtitle("RT for Match trials") +
        #scale_y_continuous("Reation Times (ms)") + 
        apatheme  +
        theme(axis.text = element_text(color="black",size = 20)) + 
        theme(axis.title = element_text(color="black",size = 20)) + 
        theme(plot.title = element_text(color="black",size = 20)) +
        theme(legend.text = element_text(color="black",size =20)) +
        theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
        theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good  ",'Neutral  ',"Bad"))+
        theme(axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1)) 

ggsave("e3_1.p_rt_M1.pdf", e3_1.p_rt_M1, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

e3_1.p_rt_M2 <- ggplot(data = df3_1.V.RT.grand.match, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
        geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                      size = 1,
                      width = .25,
                      position=position_dodge(.6)) +
        xlab("Moral valence") +
        ylab(" Reaction times (ms)") + 
        coord_cartesian(ylim=c(650,1000)) +
        scale_y_continuous(breaks=seq(650,1000,50),expand = c(0, 0)) +
        #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
        #ylim(0.3, 0.8) +
        ggtitle("RT for Match trials") +
        #scale_y_continuous("Reation Times (ms)") + 
        apatheme  +
        theme(axis.text = element_text(color="black",size = 20)) + 
        theme(axis.title = element_text(color="black",size = 20)) + 
        theme(plot.title = element_text(color="black",size = 20)) +
        theme(legend.text = element_text(color="black",size =20)) +
        theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
        theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
        theme(axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1)) 

ggsave("e3_1.p_rt_M2.pdf", e3_1.p_rt_M2, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

e3_1.p_rt_NM1 <- ggplot(data = df3_1.V.RT.grand.mismatch, aes(x=Identity,y=RT,group=Morality,shape = Morality,fill = Morality)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
        geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                      size = 1,
                      width = .25,
                      position=position_dodge(.6)) +
        xlab("Identity") +
        ylab(" Reaction times (ms)") + 
        coord_cartesian(ylim=c(650,1000)) +
        scale_y_continuous(breaks=seq(650,1000,50),expand = c(0, 0)) +
        #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
        #ylim(0.3, 0.8) +
        ggtitle("RT for Mismatch trials") +
        #scale_y_continuous("Reation Times (ms)") + 
        apatheme  +
        theme(axis.text = element_text(color="black",size = 20)) + 
        theme(axis.title = element_text(color="black",size = 20)) + 
        theme(plot.title = element_text(color="black",size = 20)) +
        theme(legend.text = element_text(color="black",size =20)) +
        theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
        theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Good  ",'Neutral  ',"Bad"))+
        theme(axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1)) 

ggsave("e3_1.p_rt_NM1.pdf", e3_1.p_rt_NM1, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")

e3_1.p_rt_NM2 <- ggplot(data = df3_1.V.RT.grand.mismatch, aes(x=Morality,y=RT,group=Identity,shape = Identity,fill = Identity)) +
        geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3, width = .6) +         # Thinner lines
        geom_errorbar(aes(ymin = RT-se, ymax = RT + se),
                      size = 1,
                      width = .25,
                      position=position_dodge(.6)) +
        xlab("Moral valence") +
        ylab(" Reaction times (ms)") + 
        coord_cartesian(ylim=c(650,1000)) +
        scale_y_continuous(breaks=seq(650,1000,50),expand = c(0, 0)) +
        #scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter.
        #ylim(0.3, 0.8) +
        ggtitle("RT for Mismatch trials") +
        #scale_y_continuous("Reation Times (ms)") + 
        apatheme  +
        theme(axis.text = element_text(color="black",size = 20)) + 
        theme(axis.title = element_text(color="black",size = 20)) + 
        theme(plot.title = element_text(color="black",size = 20)) +
        theme(legend.text = element_text(color="black",size =20)) +
        theme(axis.title.y = element_text(margin=margin(0,20,0,0))) +  # increase the space between title and y axis
        theme(axis.title.x = element_text(margin=margin(20,0,0,0))) +   # increase the sapce betwen title and x axis
        scale_fill_manual(values=c("grey20",'grey50', "grey80"),labels=c("Self  ",'Other'))+
        theme(axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1)) 

ggsave("e3_1.p_rt_NM2.pdf", e3_1.p_rt_NM2, scale = 1,height = 6, width = 4, dpi = 300, family = "Times")
