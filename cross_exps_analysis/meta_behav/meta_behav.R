# this script is for meta-anlysis:
# using long-format data

# initiazling
source('initial.r')

########## calculate the effect size and standard error of effect size ####
## load data
df1a_d <- read.csv('exp1a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df1a_d_1 <- df1a_d[,c('Subject','Morality','dprime')]
df1a_d_1$exp <- 'exp1a'

df1b_d <- read.csv('exp1b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b_d_1 <- df1b_d[,c('Subject','Morality','dprime')]
df1b_d_1$exp <- 'exp1b'

df2_d  <- read.csv('exp2_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df2_d_1 <- df2_d[,c('Subject','Morality','dprime')]
df2_d_1$exp <- 'exp2'

df3a_d <- read.csv('exp3a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df3a_d_1 <- df3a_d[,c('Subject','Morality','dprime')]
df3a_d_1$exp <- 'exp3a'
df3a_d_2 <- df3a_d[,c('Subject','Identity','Morality','dprime')]
df3a_d_2$exp <- 'exp3a'

df3b_d <- read.csv('exp3b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df3b_d_1 <- df3b_d[,c('Subject','Morality','dprime')]
df3b_d_1$exp <- 'exp3b'
df3b_d_2 <- df3b_d[,c('Subject','Identity','Morality','dprime')]
df3b_d_2$exp <- 'exp3b'

df4a_d <- read.csv('exp4a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a_d_1 <- df4a_d[,c('Subject','Morality','dprime')]
df4a_d_1$exp <- 'exp4a'
df4a_d_2 <- df4a_d[,c('Subject','Identity','Morality','dprime')]
df4a_d_2$exp <- 'exp4a'

df4b_d <- read.csv('exp4b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b_d_1 <- df4b_d[,c('Subject','Morality','dprime')]
df4b_d_1$exp <- 'exp4b'
df4b_d_2 <- df4b_d[,c('Subject','Identity','Morality','dprime')]
df4b_d_2$exp <- 'exp4b'

# combine dataset
df.d_1 <- rbind(df1a_d_1,df1b_d_1,df2_d_1,df3a_d_1,df3b_d_1,df4a_d_1,df4b_d_1) # main effect of moral valence
df.d_1$Morality[df.d_1$Morality == 'Good'] <- 'Moral'
df.d_1$Morality[df.d_1$Morality == 'Bad']  <- 'Immoral'

df.d_2 <- rbind(df3a_d_2,df3b_d_2,df4a_d_2,df4b_d_2)                           # both effect of valence and id
df.d_2$Morality[df.d_2$Morality == 'Good'] <- 'Moral'
df.d_2$Morality[df.d_2$Morality == 'Bad']  <- 'Immoral'


rm(df1a_d,df1a_d_1,df1b_d,df1b_d_1,df2_d,df2_d_1,df3a_d,df3a_d_1,df3a_d_2,df3b_d,df3b_d_1,df3b_d_2,df4a_d,df4a_d_1,df4a_d_2,
   df4b_d,df4b_d_1,df4b_d_2)

# calculate the mean and sd for each condition
df.d_1.sum <- summarySE(df.d_1,measurevar = 'dprime',groupvars = c('exp','Morality'))
df.d_2.sum <- summarySE(df.d_2,measurevar = 'dprime',groupvars = c('exp','Identity','Morality'))


#### calculate the effect size and variation of them ########
#1#  ## calculate the correlations
df3a.cor <- setNames(data.frame(matrix(ncol = 4, nrow = 6)), c("Exp", "Identity", "condition",'r'))
df3a.cor$Exp <- 'exp3a'
df3a.cor$Identity <- c('Self','Self','Self','Other','Other','Other')
df3a.cor$condition <- c('Good-Bad','Good-Neut','Neut-Bad','Good-Bad','Good-Neut','Neut-Bad')

for (expName in unique(df.d_1$exp)){
  # if correlation dataset doesn't exist, create it
  if (!exists("df.d_1.cor")){
    df.d_1.cor <-setNames(data.frame(matrix(ncol = 3, nrow = 3)), c("exp", "condition",'r'))
    df.d_1.cor$exp <- expName
    df.d_1.cor$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
    
    tmp.df <- subset(df.d_1,exp == expName)
    moralVal = unique(tmp.df$Morality)
    tmp.cor <- sapply(1:length(moralVal),function(i)
      sapply(1:length(moralVal),function(j)
        cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
    
    df.d_1.cor$r[df.d_1.cor$exp == expName & df.d_1.cor$condition == 'Good-Bad']  <- tmp.cor[3,1]
    df.d_1.cor$r[df.d_1.cor$exp == expName & df.d_1.cor$condition == 'Good-Neut'] <- tmp.cor[2,1]
    df.d_1.cor$r[df.d_1.cor$exp == expName & df.d_1.cor$condition == 'Neut-Bad']  <- tmp.cor[3,2]
  }
  
  # if the correlation dataset  does exist, append to it
  else if (exists("df.d_1.cor")){
    tmp <-setNames(data.frame(matrix(ncol = 3, nrow = 3)), c("exp", "condition",'r'))
    tmp$exp <- expName
    tmp$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
    df.d_1.cor <- rbind(df.d_1.cor,tmp)
                        
    tmp.df <- subset(df.d_1,exp == expName)
    moralVal = unique(tmp.df$Morality)
    tmp.cor <- sapply(1:length(moralVal),function(i)
      sapply(1:length(moralVal),function(j)
        cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
    
    df.d_1.cor$r[df.d_1.cor$exp == expName & df.d_1.cor$condition == 'Good-Bad']  <- tmp.cor[3,1]
    df.d_1.cor$r[df.d_1.cor$exp == expName & df.d_1.cor$condition == 'Good-Neut'] <- tmp.cor[2,1]
    df.d_1.cor$r[df.d_1.cor$exp == expName & df.d_1.cor$condition == 'Neut-Bad']  <- tmp.cor[3,2]
  }
}

# re-arrange the column
df3a.d.es <- df3a.cor 
for (id in c('Self','Other')){
  for (cond in c('Good-Bad','Good-Neut','Neut-Bad')){
    if (cond == 'Good-Bad'){
      df3a.d.es$N[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$N[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$sd.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
      
      df3a.d.es$sd.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
    } else if (cond == 'Good-Neut'){
      df3a.d.es$N[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$N[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$sd.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$sd.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
    } else {
      df3a.d.es$N[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$N[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$m.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$sd.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$m.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
      
      df3a.d.es$sd.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
    }
    
  }
}

# calculate the d and variation of d
df3a.d.es[,c('d','var.d')] <- data.frame(d.sgpp(m.1 = df3a.d.es$m.1,m.2 = df3a.d.es$m.2,
                                                sd.1 = df3a.d.es$sd.1, sd.2 = df3a.d.es$sd.2,
                                                n = df3a.d.es$N,
                                                r = df3a.d.es$r))




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
df1a.d.es.mrl_imm$cond <- 'Good-Bad'

df1a.cor.mrl_neut <- cor(x = df1a_d$dprime[df1a_d$Morality == "Moral"],df1a_d$dprime[df1a_d$Morality == "Neutral"])
df1a.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[2], m.2  = df1a.d.sum$dprime[3], 
                                       sd.1 = df1a.d.sum$sd[2],     sd.2 = df1a.d.sum$sd[3], 
                                       n    = df1a.d.sum$N[1],
                                       r    = df1a.cor.mrl_neut))
df1a.d.es.mrl_neut$Exp <- 'exp1a'
df1a.d.es.mrl_neut$cond <- 'Good-Neut'

df1a.cor.neut_imm <- cor(x = df1a_d$dprime[df1a_d$Morality == "Neutral"],df1a_d$dprime[df1a_d$Morality == "Immoral"])
df1a.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[3], m.2  = df1a.d.sum$dprime[1], 
                                        sd.1 = df1a.d.sum$sd[3],    sd.2 = df1a.d.sum$sd[1], 
                                        n    = df1a.d.sum$N[1],
                                        r    = df1a.cor.neut_imm))
df1a.d.es.neut_imm$Exp <- 'exp1a'
df1a.d.es.neut_imm$cond <- 'Neut-Bad'

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
df1b.d.es.mrl_imm$cond <- 'Good-Bad'

df1b.cor.mrl_neut <- cor(x = df1b_d$dprime[df1b_d$Morality == "Moral"],df1b_d$dprime[df1b_d$Morality == "Neutral"])
df1b.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df1b.d.sum$dprime[2], m.2  = df1b.d.sum$dprime[3], 
                                        sd.1 = df1b.d.sum$sd[2],     sd.2 = df1b.d.sum$sd[3], 
                                        n    = df1b.d.sum$N[1],
                                        r    = df1b.cor.mrl_neut))
df1b.d.es.mrl_neut$Exp <- 'exp1b'
df1b.d.es.mrl_neut$cond <- 'Good-Neut'

df1b.cor.neut_imm <- cor(x = df1b_d$dprime[df1b_d$Morality == "Neutral"],df1b_d$dprime[df1b_d$Morality == "Immoral"])
df1b.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df1b.d.sum$dprime[3], m.2  = df1b.d.sum$dprime[1], 
                                        sd.1 = df1b.d.sum$sd[3],    sd.2 = df1b.d.sum$sd[1], 
                                        n    = df1b.d.sum$N[1],
                                        r    = df1b.cor.neut_imm))
df1b.d.es.neut_imm$Exp <- 'exp1b'
df1b.d.es.neut_imm$cond <- 'Neut-Bad'

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
df2.d.es.mrl_imm$cond <- 'Good-Bad'

df2.cor.mrl_neut <- cor(x = df2_d$dprime[df2_d$Morality == "Moral"],df2_d$dprime[df2_d$Morality == "Neutral"])
df2.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df2.d.sum$dprime[2], m.2  = df2.d.sum$dprime[3], 
                                        sd.1 = df2.d.sum$sd[2],     sd.2 = df2.d.sum$sd[3], 
                                        n    = df2.d.sum$N[1],
                                        r    = df2.cor.mrl_neut))
df2.d.es.mrl_neut$Exp <- 'exp2'
df2.d.es.mrl_neut$cond <- 'Good-Neut'

df2.cor.neut_imm <- cor(x = df2_d$dprime[df2_d$Morality == "Neutral"],df2_d$dprime[df2_d$Morality == "Immoral"])
df2.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df2.d.sum$dprime[3], m.2  = df2.d.sum$dprime[1], 
                                        sd.1 = df2.d.sum$sd[3],    sd.2 = df2.d.sum$sd[1], 
                                        n    = df2.d.sum$N[1],
                                        r    = df2.cor.neut_imm))
df2.d.es.neut_imm$Exp <- 'exp2'
df2.d.es.neut_imm$cond <- 'Neut-Bad'

df2.d.es <- rbind(df2.d.es.mrl_neut,df2.d.es.mrl_imm,df2.d.es.neut_imm)

## exp3a ####
df3a_d <- read.csv('exp3a_dprime_long.csv',header = T, sep = ',',
                  stringsAsFactors=FALSE,na.strings=c("","NA"))
df3a.d.sum <- summarySE(df3a_d,measurevar = 'dprime',groupvars = c('Morality'))
df3a.d.sum$Exp <- 'exp3a'

# effect size and variance
df3a.cor.mrl_imm <- cor(x = df3a_d$dprime[df3a_d$Morality == "Moral"],df3a_d$dprime[df3a_d$Morality == "Immoral"])
df3a.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df3a.d.sum$dprime[2], m.2  = df3a.d.sum$dprime[1], 
                                       sd.1 = df3a.d.sum$sd[2],     sd.2 = df3a.d.sum$sd[1], 
                                       n    = df3a.d.sum$N[1],
                                       r    = df3a.cor.mrl_imm))
df3a.d.es.mrl_imm$Exp <- 'exp3a'
df3a.d.es.mrl_imm$cond <- 'Good-Bad'

df3a.cor.mrl_neut <- cor(x = df3a_d$dprime[df3a_d$Morality == "Moral"],df3a_d$dprime[df3a_d$Morality == "Neutral"])
df3a.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df3a.d.sum$dprime[2], m.2  = df3a.d.sum$dprime[3], 
                                        sd.1 = df3a.d.sum$sd[2],     sd.2 = df3a.d.sum$sd[3], 
                                        n    = df3a.d.sum$N[1],
                                        r    = df3a.cor.mrl_neut))
df3a.d.es.mrl_neut$Exp <- 'exp3a'
df3a.d.es.mrl_neut$cond <- 'Good-Neut'

df3a.cor.neut_imm <- cor(x = df3a_d$dprime[df3a_d$Morality == "Neutral"],df3a_d$dprime[df3a_d$Morality == "Immoral"])
df3a.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df3a.d.sum$dprime[3], m.2  = df3a.d.sum$dprime[1], 
                                        sd.1 = df3a.d.sum$sd[3],    sd.2 = df3a.d.sum$sd[1], 
                                        n    = df3a.d.sum$N[1],
                                        r    = df3a.cor.neut_imm))
df3a.d.es.neut_imm$Exp <- 'exp3a'
df3a.d.es.neut_imm$cond <- 'Neut-Bad'

df3a.d.es <- rbind(df3a.d.es.mrl_neut,df3a.d.es.mrl_imm,df3a.d.es.neut_imm)

# calculate the effect of self and other conditions separately
df3a.d.sum_slf <- summarySE(df3a_d,measurevar = 'dprime',groupvars = c('Morality','Identity'))
df3a.d.sum_slf$Exp <- 'exp3a'

# calculate the correlations
df3a.cor <- setNames(data.frame(matrix(ncol = 4, nrow = 6)), c("Exp", "Identity", "condition",'r'))
df3a.cor$Exp <- 'exp3a'
df3a.cor$Identity <- c('Self','Self','Self','Other','Other','Other')
df3a.cor$condition <- c('Good-Bad','Good-Neut','Neut-Bad','Good-Bad','Good-Neut','Neut-Bad')
for (id in c('Self','Other')){
  tmp.df <- subset(df3a_d,Identity == id)
  moralVal = unique(tmp.df$Morality)
  tmp.cor <- sapply(1:length(moralVal),function(i)
    sapply(1:length(moralVal),function(j)
      cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
  df3a.cor$r[df3a.cor$Identity == id & df3a.cor$condition == 'Good-Bad']  <- tmp.cor[3,1]
  df3a.cor$r[df3a.cor$Identity == id & df3a.cor$condition == 'Good-Neut'] <- tmp.cor[2,1]
  df3a.cor$r[df3a.cor$Identity == id & df3a.cor$condition == 'Neut-Bad']  <- tmp.cor[3,2]
}
# re-arrange the column
df3a.d.es <- df3a.cor 
for (id in c('Self','Other')){
  for (cond in c('Good-Bad','Good-Neut','Neut-Bad')){
    if (cond == 'Good-Bad'){
      df3a.d.es$N[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$N[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$sd.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
      
      df3a.d.es$sd.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
    } else if (cond == 'Good-Neut'){
      df3a.d.es$N[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$N[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$sd.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Moral']
      
      df3a.d.es$m.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$sd.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
    } else {
      df3a.d.es$N[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$N[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$m.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$sd.1[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Neutral']
      
      df3a.d.es$m.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$dprime[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
      
      df3a.d.es$sd.2[df3a.d.es$Identity == id & df3a.d.es$condition == cond] <- 
        df3a.d.sum_slf$sd[df3a.d.sum_slf$Identity == id & df3a.d.sum_slf$Morality =='Immoral']
    }
    
  }
}

# calculate the d and variation of d
df3a.d.es[,c('d','var.d')] <- data.frame(d.sgpp(m.1 = df3a.d.es$m.1,m.2 = df3a.d.es$m.2,
                         sd.1 = df3a.d.es$sd.1, sd.2 = df3a.d.es$sd.2,
                         n = df3a.d.es$N,
                         r = df3a.d.es$r))


## exp3b ####
df3b_d <- read.csv('exp3b_dprime_long.csv',header = T, sep = ',',
                  stringsAsFactors=FALSE,na.strings=c("","NA"))
df3b.d.sum <- summarySE(df3b_d,measurevar = 'dprime',groupvars = c('Morality'))
df3b.d.sum$Exp <- 'exp3b'

# effect size and variance
df3b.cor.mrl_imm <- cor(x = df3b_d$dprime[df3b_d$Morality == "Good"],df3b_d$dprime[df3b_d$Morality == "Bad"])
df3b.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df3b.d.sum$dprime[2], m.2  = df3b.d.sum$dprime[1], 
                                      sd.1 = df3b.d.sum$sd[2],     sd.2 = df3b.d.sum$sd[1], 
                                      n    = df3b.d.sum$N[1],
                                      r    = df3b.cor.mrl_imm))
df3b.d.es.mrl_imm$Exp <- 'exp3b'
df3b.d.es.mrl_imm$cond <- 'Good-Bad'

df3b.cor.mrl_neut <- cor(x = df3b_d$dprime[df3b_d$Morality == "Good"],df3b_d$dprime[df3b_d$Morality == "Neutral"])
df3b.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df3b.d.sum$dprime[2], m.2  = df3b.d.sum$dprime[3], 
                                       sd.1 = df3b.d.sum$sd[2],     sd.2 = df3b.d.sum$sd[3], 
                                       n    = df3b.d.sum$N[1],
                                       r    = df3b.cor.mrl_neut))
df3b.d.es.mrl_neut$Exp <- 'exp3b'
df3b.d.es.mrl_neut$cond <- 'Good-Neut'

df3b.cor.neut_imm <- cor(x = df3b_d$dprime[df3b_d$Morality == "Neutral"],df3b_d$dprime[df3b_d$Morality == "Bad"])
df3b.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df3b.d.sum$dprime[3], m.2  = df3b.d.sum$dprime[1], 
                                       sd.1 = df3b.d.sum$sd[3],    sd.2 = df3b.d.sum$sd[1], 
                                       n    = df3b.d.sum$N[1],
                                       r    = df3b.cor.neut_imm))
df3b.d.es.neut_imm$Exp <- 'exp3b'
df3b.d.es.neut_imm$cond <- 'Neut-Bad'

df3b.d.es <- rbind(df3b.d.es.mrl_neut,df3b.d.es.mrl_imm,df3b.d.es.neut_imm)

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
df4a.d.es.mrl_imm$cond <- 'Good-Bad'

df4a.cor.mrl_neut <- cor(x = df4a_d$dprime[df4a_d$Morality == "Moral"],df4a_d$dprime[df4a_d$Morality == "Neutral"])
df4a.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df4a.d.sum$dprime[2], m.2  = df4a.d.sum$dprime[3], 
                                        sd.1 = df4a.d.sum$sd[2],     sd.2 = df4a.d.sum$sd[3], 
                                        n    = df4a.d.sum$N[1],
                                        r    = df4a.cor.mrl_neut))
df4a.d.es.mrl_neut$Exp <- 'exp4a'
df4a.d.es.mrl_neut$cond <- 'Good-Neut'

df4a.cor.neut_imm <- cor(x = df4a_d$dprime[df4a_d$Morality == "Neutral"],df4a_d$dprime[df4a_d$Morality == "Immoral"])
df4a.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df4a.d.sum$dprime[3], m.2  = df4a.d.sum$dprime[1], 
                                        sd.1 = df4a.d.sum$sd[3],    sd.2 = df4a.d.sum$sd[1], 
                                        n    = df4a.d.sum$N[1],
                                        r    = df4a.cor.neut_imm))
df4a.d.es.neut_imm$Exp <- 'exp4a'
df4a.d.es.neut_imm$cond <- 'Neut-Bad'

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
df4b.d.es.mrl_imm$cond <- 'Good-Bad'

df4b.cor.mrl_neut <- cor(x = df4b_d$dprime[df4b_d$Morality == "Moral"],df4b_d$dprime[df4b_d$Morality == "Neutral"])
df4b.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df4b.d.sum$dprime[2], m.2  = df4b.d.sum$dprime[3], 
                                        sd.1 = df4b.d.sum$sd[2],     sd.2 = df4b.d.sum$sd[3], 
                                        n    = df4b.d.sum$N[1],
                                        r    = df4b.cor.mrl_neut))
df4b.d.es.mrl_neut$Exp <- 'exp4b'
df4b.d.es.mrl_neut$cond <- 'Good-Neut'

df4b.cor.neut_imm <- cor(x = df4b_d$dprime[df4b_d$Morality == "Neutral"],df4b_d$dprime[df4b_d$Morality == "Immoral"])
df4b.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df4b.d.sum$dprime[3], m.2  = df4b.d.sum$dprime[1], 
                                        sd.1 = df4b.d.sum$sd[3],    sd.2 = df4b.d.sum$sd[1], 
                                        n    = df4b.d.sum$N[1],
                                        r    = df4b.cor.neut_imm))
df4b.d.es.neut_imm$Exp <- 'exp4b'
df4b.d.es.neut_imm$cond <- 'Neut-Bad'

df4b.d.es <- rbind(df4b.d.es.mrl_neut,df4b.d.es.mrl_imm,df4b.d.es.neut_imm)

df.d.es <- rbind(df1a.d.es, df1b.d.es, df2.d.es, df3a.d.es, df3b.d.es, df4a.d.es, df4b.d.es)
write.csv(df.d.es[df.d.es$cond == 'Good-Neut',],'effect_size_dprime_Moral_Neutral.csv',row.names = F)
write.csv(df.d.es[df.d.es$cond == 'Good-Bad',], 'effect_size_dprime_Moral_Immoral.csv',row.names = F)
write.csv(df.d.es[df.d.es$cond == 'Neut-Bad',], 'effect_size_dprime_Neut_Immoral.csv',row.names = F)

# meta-analysis for d prime ####
# moral vs. neutral
M_D_mrl_neut <- rma(yi = df.d.es$d[df.d.es$cond == 'Good-Neut'], 
                    vi = df.d.es$var.d[df.d.es$cond == 'Good-Neut'],
                    slab = df.d.es$Exp[df.d.es$cond == 'Good-Neut'])
forest(M_D_mrl_neut)

M_D_mrl_imm <- rma(yi = df.d.es$d[df.d.es$cond == 'Good-Bad'], 
                   vi = df.d.es$var.d[df.d.es$cond == 'Good-Bad'],
                   slab = df.d.es$Exp[df.d.es$cond == 'Good-Bad'])
forest(M_D_mrl_imm)

M_D_neut_imm <- rma(yi = df.d.es$d[df.d.es$cond == 'Neut-Bad'], 
                    vi = df.d.es$var.d[df.d.es$cond == 'Neut-Bad'],
                    slab = df.d.es$Exp[df.d.es$cond == 'Neut-Bad'])
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

df3a_rt <- read.csv('exp3a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
#df3a <- df3a[,colnames(df1a)]
df3a_rt_m <-df3a_rt[df3a_rt$Matchness == 'Match',]
df3a.rt.sum <- summarySE(df3a_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df3a.rt.sum$Exp <- 'study 3a'
df3a.rt.sum <- df3a.rt.sum[,c('Exp','Morality','RT','sd')]
df3a.rt.sum_w <- dcast(melt(df3a.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df3a.rt.sum_w$N <- nrow(df3a_rt_m)/6

df3b_rt <- read.csv('exp3b_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
#df3b <- df3b[,colnames(df1a)]
df3b_rt_m <-df3b_rt[df3b_rt$Matchness == 'Match',]
df3b.rt.sum <- summarySE(df3b_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df3b.rt.sum$Exp <- 'study 3b'
df3b.rt.sum <- df3b.rt.sum[,c('Exp','Morality','RT','sd')]
df3b.rt.sum_w <- dcast(melt(df3b.rt.sum, id.vars=c("Exp",'Morality')), Exp ~ variable + Morality)
df3b.rt.sum_w$N <- nrow(df3b_rt_m)/6

# study 4a
df4a_rt <- read.csv('exp4a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
#df4a <- df4a[,2:33]
#colnames(df4a)[colnames(df4a) == 'SessionID'] <- 'session'
#df4a <- df4a[,colnames(df1a)]
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
 
colnames(df1a.rt.sum_w) <- colnames(df3b.rt.sum_w)
colnames(df1b.rt.sum_w) <- colnames(df3b.rt.sum_w)
colnames(df2.rt.sum_w) <- colnames(df3b.rt.sum_w)
colnames(df3a.rt.sum_w) <- colnames(df3b.rt.sum_w)
colnames(df4a.rt.sum_w) <- colnames(df3b.rt.sum_w)
colnames(df4b.rt.sum_w) <- colnames(df3b.rt.sum_w)

df.rt.sum_w <- rbind(df1a.rt.sum_w,df1b.rt.sum_w,df2.rt.sum_w,df3a.rt.sum_w,df3b.rt.sum_w,df4a.rt.sum_w,df4b.rt.sum_w)

# RT: Moral vs. Neutral
df.rt.sum_w_mrl_neut <- df.rt.sum_w[,c(1,3,4,6:8)]
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Moral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Moral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 2']  <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Moral"], df2_rt_m$RT[df2_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 3a'] <- cor(df3a_rt_m$RT[df3a_rt_m$Morality == "Moral"], df3a_rt_m$RT[df3a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$Exp == 'study 3b'] <- cor(df3b_rt_m$RT[df3b_rt_m$Morality == "Good"], df3b_rt_m$RT[df3b_rt_m$Morality == "Neutral"])
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
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 3a'] <- cor(df3a_rt_m$RT[df3a_rt_m$Morality == "Moral"], df3a_rt_m$RT[df3a_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$Exp == 'study 3b'] <- cor(df3b_rt_m$RT[df3b_rt_m$Morality == "Good"], df3b_rt_m$RT[df3b_rt_m$Morality == "Bad"])
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
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 3a'] <- cor(df3a_rt_m$RT[df3a_rt_m$Morality == "Neutral"], df3a_rt_m$RT[df3a_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$Exp == 'study 3b'] <- cor(df3b_rt_m$RT[df3b_rt_m$Morality == "Neutral"], df3b_rt_m$RT[df3b_rt_m$Morality == "Bad"])
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
