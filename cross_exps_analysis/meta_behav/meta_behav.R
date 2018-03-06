# this script is for meta-anlysis:
# using long-format data

# initiazling
source('initial.r')

## calculate the effect size and standard error of effect size
## exp1a ####
df1a_d <- read.csv('exp1a_dprime_long.csv',header = T, sep = ',',
                 stringsAsFactors=FALSE,na.strings=c("","NA"))
df1a.d.sum <- summarySE(df1a_d,measurevar = 'dprime',groupvars = c('Morality'))
df1a.d.sum$Exp <- 'exp1a'

# effect size and variance
df1a.cor.mrl_imm <- cor(x = df1a_d$dprime[df1a_d$Morality == "Moral"],df1a_d$dprime[df1a_d$Morality == "Immoral"])
df1a.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[2], m.2  = df1a.d.sum$dprime[1], 
                           sd.1 = df1a.d.sum$sd[2],     sd.2 = df1a.d.sum$sd[1], 
                           n    = df1a.d.sum$N[1],
                           r    = df1a.cor.mrl_imm))
df1a.d.es.mrl_imm$Exp <- 'exp1a'
df1a.d.es.mrl_imm$cond <- 'mrl_imm'

df1a.cor.mrl_neut <- cor(x = df1a_d$dprime[df1a_d$Morality == "Moral"],df1a_d$dprime[df1a_d$Morality == "Neutral"])
df1a.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[2], m.2  = df1a.d.sum$dprime[3], 
                                       sd.1 = df1a.d.sum$sd[2],     sd.2 = df1a.d.sum$sd[3], 
                                       n    = df1a.d.sum$N[1],
                                       r    = df1a.cor.mrl_neut))
df1a.d.es.mrl_neut$Exp <- 'exp1a'
df1a.d.es.mrl_neut$cond <- 'mrl_neut'

df1a.cor.neut_imm <- cor(x = df1a_d$dprime[df1a_d$Morality == "Neutral"],df1a_d$dprime[df1a_d$Morality == "Immoral"])
df1a.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[3], m.2  = df1a.d.sum$dprime[1], 
                                        sd.1 = df1a.d.sum$sd[3],    sd.2 = df1a.d.sum$sd[1], 
                                        n    = df1a.d.sum$N[1],
                                        r    = df1a.cor.neut_imm))
df1a.d.es.neut_imm$Exp <- 'exp1a'
df1a.d.es.neut_imm$cond <- 'neut_imm'

df1a.d.es <- rbind(df1a.d.es.mrl_neut,df1a.d.es.mrl_imm,df1a.d.es.neut_imm)

## exp1b ####
df1b_d <- read.csv('exp1b_dprime_long.csv',header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b.d.sum <- summarySE(df1b_d,measurevar = 'dprime',groupvars = c('Morality'))
df1b.d.sum$Exp <- 'exp1b'

# effect size and variance
df1b.cor.mrl_imm <- cor(x = df1b_d$dprime[df1b_d$Morality == "Moral"],df1b_d$dprime[df1b_d$Morality == "Immoral"])
df1b.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df1b.d.sum$dprime[2], m.2  = df1b.d.sum$dprime[1], 
                                       sd.1 = df1b.d.sum$sd[2],     sd.2 = df1b.d.sum$sd[1], 
                                       n    = df1b.d.sum$N[1],
                                       r    = df1b.cor.mrl_imm))
df1b.d.es.mrl_imm$Exp <- 'exp1b'
df1b.d.es.mrl_imm$cond <- 'mrl_imm'

df1b.cor.mrl_neut <- cor(x = df1b_d$dprime[df1b_d$Morality == "Moral"],df1b_d$dprime[df1b_d$Morality == "Neutral"])
df1b.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df1b.d.sum$dprime[2], m.2  = df1b.d.sum$dprime[3], 
                                        sd.1 = df1b.d.sum$sd[2],     sd.2 = df1b.d.sum$sd[3], 
                                        n    = df1b.d.sum$N[1],
                                        r    = df1b.cor.mrl_neut))
df1b.d.es.mrl_neut$Exp <- 'exp1b'
df1b.d.es.mrl_neut$cond <- 'mrl_neut'

df1b.cor.neut_imm <- cor(x = df1b_d$dprime[df1b_d$Morality == "Neutral"],df1b_d$dprime[df1b_d$Morality == "Immoral"])
df1b.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df1b.d.sum$dprime[3], m.2  = df1b.d.sum$dprime[1], 
                                        sd.1 = df1b.d.sum$sd[3],    sd.2 = df1b.d.sum$sd[1], 
                                        n    = df1b.d.sum$N[1],
                                        r    = df1b.cor.neut_imm))
df1b.d.es.neut_imm$Exp <- 'exp1b'
df1b.d.es.neut_imm$cond <- 'neut_imm'

df1b.d.es <- rbind(df1b.d.es.mrl_neut,df1b.d.es.mrl_imm,df1b.d.es.neut_imm)


## exp2 ####
df2_d <- read.csv('exp2_dprime_long.csv',header = T, sep = ',',
                  stringsAsFactors=FALSE,na.strings=c("","NA"))
df2.d.sum <- summarySE(df2_d,measurevar = 'dprime',groupvars = c('Morality'))
df2.d.sum$Exp <- 'exp2'

# effect size and variance
df2.cor.mrl_imm <- cor(x = df2_d$dprime[df2_d$Morality == "Moral"],df2_d$dprime[df2_d$Morality == "Immoral"])
df2.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df2.d.sum$dprime[2], m.2  = df2.d.sum$dprime[1], 
                                       sd.1 = df2.d.sum$sd[2],     sd.2 = df2.d.sum$sd[1], 
                                       n    = df2.d.sum$N[1],
                                       r    = df2.cor.mrl_imm))
df2.d.es.mrl_imm$Exp <- 'exp2'
df2.d.es.mrl_imm$cond <- 'mrl_imm'

df2.cor.mrl_neut <- cor(x = df2_d$dprime[df2_d$Morality == "Moral"],df2_d$dprime[df2_d$Morality == "Neutral"])
df2.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df2.d.sum$dprime[2], m.2  = df2.d.sum$dprime[3], 
                                        sd.1 = df2.d.sum$sd[2],     sd.2 = df2.d.sum$sd[3], 
                                        n    = df2.d.sum$N[1],
                                        r    = df2.cor.mrl_neut))
df2.d.es.mrl_neut$Exp <- 'exp2'
df2.d.es.mrl_neut$cond <- 'mrl_neut'

df2.cor.neut_imm <- cor(x = df2_d$dprime[df2_d$Morality == "Neutral"],df2_d$dprime[df2_d$Morality == "Immoral"])
df2.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df2.d.sum$dprime[3], m.2  = df2.d.sum$dprime[1], 
                                        sd.1 = df2.d.sum$sd[3],    sd.2 = df2.d.sum$sd[1], 
                                        n    = df2.d.sum$N[1],
                                        r    = df2.cor.neut_imm))
df2.d.es.neut_imm$Exp <- 'exp2'
df2.d.es.neut_imm$cond <- 'neut_imm'

df2.d.es <- rbind(df2.d.es.mrl_neut,df2.d.es.mrl_imm,df2.d.es.neut_imm)

## exp3 ####
df3_d <- read.csv('exp3_dprime_long.csv',header = T, sep = ',',
                  stringsAsFactors=FALSE,na.strings=c("","NA"))
df3.d.sum <- summarySE(df3_d,measurevar = 'dprime',groupvars = c('Morality'))
df3.d.sum$Exp <- 'exp3'

# effect size and variance
df3.cor.mrl_imm <- cor(x = df3_d$dprime[df3_d$Morality == "Moral"],df3_d$dprime[df3_d$Morality == "Immoral"])
df3.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df3.d.sum$dprime[2], m.2  = df3.d.sum$dprime[1], 
                                       sd.1 = df3.d.sum$sd[2],     sd.2 = df3.d.sum$sd[1], 
                                       n    = df3.d.sum$N[1],
                                       r    = df3.cor.mrl_imm))
df3.d.es.mrl_imm$Exp <- 'exp3'
df3.d.es.mrl_imm$cond <- 'mrl_imm'

df3.cor.mrl_neut <- cor(x = df3_d$dprime[df3_d$Morality == "Moral"],df3_d$dprime[df3_d$Morality == "Neutral"])
df3.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df3.d.sum$dprime[2], m.2  = df3.d.sum$dprime[3], 
                                        sd.1 = df3.d.sum$sd[2],     sd.2 = df3.d.sum$sd[3], 
                                        n    = df3.d.sum$N[1],
                                        r    = df3.cor.mrl_neut))
df3.d.es.mrl_neut$Exp <- 'exp3'
df3.d.es.mrl_neut$cond <- 'mrl_neut'

df3.cor.neut_imm <- cor(x = df3_d$dprime[df3_d$Morality == "Neutral"],df3_d$dprime[df3_d$Morality == "Immoral"])
df3.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df3.d.sum$dprime[3], m.2  = df3.d.sum$dprime[1], 
                                        sd.1 = df3.d.sum$sd[3],    sd.2 = df3.d.sum$sd[1], 
                                        n    = df3.d.sum$N[1],
                                        r    = df3.cor.neut_imm))
df3.d.es.neut_imm$Exp <- 'exp3'
df3.d.es.neut_imm$cond <- 'neut_imm'

df3.d.es <- rbind(df3.d.es.mrl_neut,df3.d.es.mrl_imm,df3.d.es.neut_imm)


## exp 4a ####
df4a_d <- read.csv('exp4a_dprime_long.csv',header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a.d.sum <- summarySE(df4a_d,measurevar = 'dprime',groupvars = c('Morality'))
df4a.d.sum$Exp <- 'exp4a'

# effect size and variance
df4a.cor.mrl_imm <- cor(x = df4a_d$dprime[df4a_d$Morality == "Moral"],df4a_d$dprime[df4a_d$Morality == "Immoral"])
df4a.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df4a.d.sum$dprime[2], m.2  = df4a.d.sum$dprime[1], 
                                       sd.1 = df4a.d.sum$sd[2],     sd.2 = df4a.d.sum$sd[1], 
                                       n    = df4a.d.sum$N[1],
                                       r    = df4a.cor.mrl_imm))
df4a.d.es.mrl_imm$Exp <- 'exp4a'
df4a.d.es.mrl_imm$cond <- 'mrl_imm'

df4a.cor.mrl_neut <- cor(x = df4a_d$dprime[df4a_d$Morality == "Moral"],df4a_d$dprime[df4a_d$Morality == "Neutral"])
df4a.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df4a.d.sum$dprime[2], m.2  = df4a.d.sum$dprime[3], 
                                        sd.1 = df4a.d.sum$sd[2],     sd.2 = df4a.d.sum$sd[3], 
                                        n    = df4a.d.sum$N[1],
                                        r    = df4a.cor.mrl_neut))
df4a.d.es.mrl_neut$Exp <- 'exp4a'
df4a.d.es.mrl_neut$cond <- 'mrl_neut'

df4a.cor.neut_imm <- cor(x = df4a_d$dprime[df4a_d$Morality == "Neutral"],df4a_d$dprime[df4a_d$Morality == "Immoral"])
df4a.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df4a.d.sum$dprime[3], m.2  = df4a.d.sum$dprime[1], 
                                        sd.1 = df4a.d.sum$sd[3],    sd.2 = df4a.d.sum$sd[1], 
                                        n    = df4a.d.sum$N[1],
                                        r    = df4a.cor.neut_imm))
df4a.d.es.neut_imm$Exp <- 'exp4a'
df4a.d.es.neut_imm$cond <- 'neut_imm'

df4a.d.es <- rbind(df4a.d.es.mrl_neut,df4a.d.es.mrl_imm,df4a.d.es.neut_imm)

## exp4b###
df4b_d <- read.csv('exp4b_dprime_long.csv',header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b.d.sum <- summarySE(df4b_d,measurevar = 'dprime',groupvars = c('Morality'))
df4b.d.sum$Exp <- 'exp4b'

# effect size and variance
df4b.cor.mrl_imm <- cor(x = df4b_d$dprime[df4b_d$Morality == "Moral"],df4b_d$dprime[df4b_d$Morality == "Immoral"])
df4b.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df4b.d.sum$dprime[2], m.2  = df4b.d.sum$dprime[1], 
                                       sd.1 = df4b.d.sum$sd[2],     sd.2 = df4b.d.sum$sd[1], 
                                       n    = df4b.d.sum$N[1],
                                       r    = df4b.cor.mrl_imm))
df4b.d.es.mrl_imm$Exp <- 'exp4b'
df4b.d.es.mrl_imm$cond <- 'mrl_imm'

df4b.cor.mrl_neut <- cor(x = df4b_d$dprime[df4b_d$Morality == "Moral"],df4b_d$dprime[df4b_d$Morality == "Neutral"])
df4b.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df4b.d.sum$dprime[2], m.2  = df4b.d.sum$dprime[3], 
                                        sd.1 = df4b.d.sum$sd[2],     sd.2 = df4b.d.sum$sd[3], 
                                        n    = df4b.d.sum$N[1],
                                        r    = df4b.cor.mrl_neut))
df4b.d.es.mrl_neut$Exp <- 'exp4b'
df4b.d.es.mrl_neut$cond <- 'mrl_neut'

df4b.cor.neut_imm <- cor(x = df4b_d$dprime[df4b_d$Morality == "Neutral"],df4b_d$dprime[df4b_d$Morality == "Immoral"])
df4b.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df4b.d.sum$dprime[3], m.2  = df4b.d.sum$dprime[1], 
                                        sd.1 = df4b.d.sum$sd[3],    sd.2 = df4b.d.sum$sd[1], 
                                        n    = df4b.d.sum$N[1],
                                        r    = df4b.cor.neut_imm))
df4b.d.es.neut_imm$Exp <- 'exp4b'
df4b.d.es.neut_imm$cond <- 'neut_imm'

df4b.d.es <- rbind(df4b.d.es.mrl_neut,df4b.d.es.mrl_imm,df4b.d.es.neut_imm)

df.d.es <- rbind(df1a.d.es, df1b.d.es, df2.d.es, df3.d.es, df4a.d.es, df4b.d.es)
write.csv(df.d.es[df.d.es$cond == 'mrl_neut',],'effect_size_dprime_Moral_Neutral.csv',row.names = F)
write.csv(df.d.es[df.d.es$cond == 'mrl_imm',], 'effect_size_dprime_Moral_Immoral.csv',row.names = F)
write.csv(df.d.es[df.d.es$cond == 'neut_imm',], 'effect_size_dprime_Neut_Immoral.csv',row.names = F)

# meta-analysis for d prime ####
# moral vs. neutral
M_D_mrl_neut <- rma(yi = df.d.es$d[df.d.es$cond == 'mrl_neut'], 
                    vi = df.d.es$var.d[df.d.es$cond == 'mrl_neut'],
                    slab = df.d.es$Exp[df.d.es$cond == 'mrl_neut'])
forest(M_D_mrl_neut)

M_D_mrl_imm <- rma(yi = df.d.es$d[df.d.es$cond == 'mrl_imm'], 
                   vi = df.d.es$var.d[df.d.es$cond == 'mrl_imm'],
                   slab = df.d.es$Exp[df.d.es$cond == 'mrl_imm'])
forest(M_D_mrl_imm)

M_D_neut_imm <- rma(yi = df.d.es$d[df.d.es$cond == 'neut_imm'], 
                    vi = df.d.es$var.d[df.d.es$cond == 'neut_imm'],
                    slab = df.d.es$Exp[df.d.es$cond == 'neut_imm'])
forest(M_D_neut_imm)




## for RT ####
df1a_rt <- read.csv('exp1a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df1a_rt_m <-df1a_rt[df1a_rt$Matchness == 'Match',]
df1a.rt.sum <- summarySE(df1a_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df1a.rt.sum$Exp <- 'study 1a'
df1a.rt.sum <- df1a.rt.sum[,c('Exp','Morality','RT','sd')]
df1a.rt.sum_w <- dcast(melt(df1a.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df1a.rt.sum_w$N <- nrow(df1a_rt_m)/3

df1b_rt <- read.csv('exp1b_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b_rt_m <-df1b_rt[df1b_rt$Matchness == 'Match',]
df1b.rt.sum <- summarySE(df1b_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df1b.rt.sum$Exp <- 'study 1b'
df1b.rt.sum <- df1b.rt.sum[,c('Exp','Morality','RT','sd')]
df1b.rt.sum_w <- dcast(melt(df1b.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df1b.rt.sum_w$N <- nrow(df1b_rt_m)/3

df2_rt <- read.csv('exp2_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df2_rt_m <-df2_rt[df2_rt$Matchness == 'Match',]
df2.rt.sum <- summarySE(df2_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df2.rt.sum$Exp <- 'study 2'
df2.rt.sum <- df2.rt.sum[,c('Exp','Morality','RT','sd')]
df2.rt.sum_w <- dcast(melt(df2.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df2.rt.sum_w$N <- nrow(df2_rt_m)/3

df3_rt <- read.csv('exp3_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df3 <- df3[,colnames(df1a)]
df3_rt_m <-df3_rt[df3_rt$Matchness == 'Match',]
df3.rt.sum <- summarySE(df3_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df3.rt.sum$Exp <- 'study 3'
df3.rt.sum <- df3.rt.sum[,c('Exp','Morality','RT','sd')]
df3.rt.sum_w <- dcast(melt(df3.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df3.rt.sum_w$N <- nrow(df3_rt_m)/6

# study 4a
df4a_rt <- read.csv('exp4a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a <- df4a[,2:33]
colnames(df4a)[colnames(df4a) == 'SessionID'] <- 'session'
df4a <- df4a[,colnames(df1a)]
df4a_rt_m <-df4a_rt[df4a_rt$Matchness == 'Match',]
df4a.rt.sum <- summarySE(df4a_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df4a.rt.sum$Exp <- 'study 4a'
df4a.rt.sum <- df4a.rt.sum[,c('Exp','Morality','RT','sd')]
df4a.rt.sum_w <- dcast(melt(df4a.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df4a.rt.sum_w$N <- nrow(df4a_rt_m)/6

# study 4b
df4b_rt <- read.csv('exp4b_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b_rt_m <-df4b_rt[df4b_rt$Matchness == 'Match',]
df4b.rt.sum <- summarySE(df4b_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df4b.rt.sum$Exp <- 'study 4b'
df4b.rt.sum <- df4b.rt.sum[,c('Exp','Morality','RT','sd')]
df4b.rt.sum_w <- dcast(melt(df4b.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df4b.rt.sum_w$N <- nrow(df4b_rt_m)/6

df.rt.sum_w <- rbind(df1a.rt.sum_w,df1b.rt.sum_w,df2.rt.sum_w,df3.rt.sum_w,df4a.rt.sum_w,df4b.rt.sum_w)

# RT: Moral vs. Neutral
df.rt.sum_w_mrl_neut <- df.rt.sum_w[,c(1,3,4,6:8)]
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Moral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Moral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 2'] <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Moral"], df2_rt_m$RT[df2_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 3'] <- cor(df3_rt_m$RT[df3_rt_m$Morality == "Moral"], df3_rt_m$RT[df3_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 4a'] <- cor(df4a_rt_m$RT[df4a_rt_m$Morality == "Moral"], df4a_rt_m$RT[df4a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 4b'] <- cor(df4b_rt_m$RT[df4b_rt_m$Morality == "Moral"], df4b_rt_m$RT[df4b_rt_m$Morality == "Neutral"])
for (i in 1:nrow(df.rt.sum_w_mrl_neut)){
  tmp <- d.sgpp(m.1 = df.rt.sum_w_mrl_neut[i,2], m.2  = df.rt.sum_w_mrl_neut[i,3], 
                sd.1 = df.rt.sum_w_mrl_neut[i,4], sd.2 = df.rt.sum_w_mrl_neut[i,5], 
                n    = df.rt.sum_w_mrl_neut[i,6],
                r    = df.rt.sum_w_mrl_neut[i,7])
  df.rt.sum_w_mrl_neut$yi[i] = tmp[1]
  df.rt.sum_w_mrl_neut$vi[i] = tmp[2]
}
M_RT_mrl_neut <- rma(yi = df.rt.sum_w_mrl_neut$yi, 
                    vi = df.rt.sum_w_mrl_neut$vi,
                    slab = df.rt.sum_w_mrl_neut$Exp)
forest(M_RT_mrl_neut)

# RT: Moral vs. Immoral
df.rt.sum_w_mrl_imm <- df.rt.sum_w[,c(1,3,2,6,5,8)]
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Moral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Moral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 2'] <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Moral"], df2_rt_m$RT[df2_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 3'] <- cor(df3_rt_m$RT[df3_rt_m$Morality == "Moral"], df3_rt_m$RT[df3_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 4a'] <- cor(df4a_rt_m$RT[df4a_rt_m$Morality == "Moral"], df4a_rt_m$RT[df4a_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 4b'] <- cor(df4b_rt_m$RT[df4b_rt_m$Morality == "Moral"], df4b_rt_m$RT[df4b_rt_m$Morality == "Immoral"])
for (i in 1:nrow(df.rt.sum_w_mrl_imm)){
  tmp <- d.sgpp(m.1 = df.rt.sum_w_mrl_imm[i,2], m.2  = df.rt.sum_w_mrl_imm[i,3], 
                sd.1 = df.rt.sum_w_mrl_imm[i,4], sd.2 = df.rt.sum_w_mrl_imm[i,5], 
                n    = df.rt.sum_w_mrl_imm[i,6],
                r    = df.rt.sum_w_mrl_imm[i,7])
  df.rt.sum_w_mrl_imm$yi[i] = tmp[1]
  df.rt.sum_w_mrl_imm$vi[i] = tmp[2]
}
M_RT_mrl_imm <- rma(yi = df.rt.sum_w_mrl_imm$yi, 
                     vi = df.rt.sum_w_mrl_imm$vi,
                     slab = df.rt.sum_w_mrl_imm$Exp)
forest(M_RT_mrl_imm)

# RT: Neutral v. Immoral
df.rt.sum_w_neut_imm <- df.rt.sum_w[,c(1,4,2,7,5,8)]
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Neutral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Neutral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 2'] <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Neutral"], df2_rt_m$RT[df2_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 3'] <- cor(df3_rt_m$RT[df3_rt_m$Morality == "Neutral"], df3_rt_m$RT[df3_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 4a'] <- cor(df4a_rt_m$RT[df4a_rt_m$Morality == "Neutral"], df4a_rt_m$RT[df4a_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 4b'] <- cor(df4b_rt_m$RT[df4b_rt_m$Morality == "Neutral"], df4b_rt_m$RT[df4b_rt_m$Morality == "Immoral"])
for (i in 1:nrow(df.rt.sum_w_neut_imm)){
  tmp <- d.sgpp(m.1 = df.rt.sum_w_neut_imm[i,2], m.2  = df.rt.sum_w_neut_imm[i,3], 
                sd.1 = df.rt.sum_w_neut_imm[i,4], sd.2 = df.rt.sum_w_neut_imm[i,5], 
                n    = df.rt.sum_w_neut_imm[i,6],
                r    = df.rt.sum_w_neut_imm[i,7])
  df.rt.sum_w_neut_imm$yi[i] = tmp[1]
  df.rt.sum_w_neut_imm$vi[i] = tmp[2]
}
M_RT_neut_imm <- rma(yi = df.rt.sum_w_neut_imm$yi, 
                    vi = df.rt.sum_w_neut_imm$vi,
                    slab = df.rt.sum_w_neut_imm$Exp)
forest(M_RT_neut_imm)
