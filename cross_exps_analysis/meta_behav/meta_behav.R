# this script is for meta-anlysis:
# using long-format data
#
# Author       Date               Log of change
# ==========   ===============    =======================
# hcp          2019-04-02         tidyverse, for loop, unsolved: N for exp3 & 4

# initiazling
source('initial.r')

########## calculate the effect size and standard error of effect size ####
## load data
fileNames <- c('exp1a_dprime_long.csv',
               'exp1b_dprime_long.csv',
               'exp1c_dprime_long.csv',
               'exp2_dprime_long.csv',
               'exp3a_dprime_long.csv',
               'exp3b_dprime_long.csv',
               'exp4a_dprime_long.csv',
               'exp4b_dprime_long.csv')

for (i in 1:length(fileNames)) {
  # when start the loop, check whether exist the variable alredy
  if (i == 1){
    if (exists('df.d_1')){  # if exist, remove the variable first
      rm(df.d_1)     
    }
  }
  
  curFile = fileNames[i];                 # get the current file name
  expName = sub("\\_.*", "", curFile)     # get the current experiment name
  
  if (!exists('df.d_1')){                 # if the the first
    df.d_1 <- read.csv(curFile, header = T, sep = ',', 
                       stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
        dplyr::select(Subject,Morality,dprime) %>%   # select three columns
        dplyr::mutate(exp = expName,                 # add experimental name
                      Morality = recode(Morality, Moral = "Good", Immoral = "Bad"))  # recode morality
  } else {
      tmp <- read.csv(curFile, header = T, sep = ',', 
                       stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
      dplyr::select(Subject,Morality,dprime) %>%
      dplyr::mutate(exp = expName,
                    Morality = recode(Morality, Moral = "Good", Immoral = "Bad"))
      df.d_1 <- rbind(df.d_1,tmp)
  }
  
  # if the i >=4, we need more  conditions  
  if (i == 5){
    df.d_2 <- read.csv(curFile, header = T, sep = ',', 
                       stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
      dplyr::select(Subject, Identity, Morality,dprime) %>%   # select three columns
      dplyr::mutate(exp = expName,                 # add experimental name
                    Morality = recode(Morality, Moral = "Good", Immoral = "Bad"))  # recode morality
  } else if (i > 5){
    tmp2 <- read.csv(curFile, header = T, sep = ',', 
                       stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
      dplyr::select(Subject, Identity, Morality,dprime) %>%   # select three columns
      dplyr::mutate(exp = expName,                 # add experimental name
                    Morality = recode(Morality, Moral = "Good", Immoral = "Bad"))  # recode morality
    df.d_2 <- rbind(df.d_2,tmp2)
  }
}

#df1a_d_1 <- read.csv('exp1a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp1a')

#df1a_d_1 <- df1a_d[,c('Subject','Morality','dprime')]
#df1a_d_1$exp <- 'exp1a'

#df1b_d_1 <- read.csv('exp1b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp1b')
#df1b_d_1 <- df1b_d[,c('Subject','Morality','dprime')]
#df1b_d_1$exp <- 'exp1b'

#df1c_d_1 <- read.csv('exp1c_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp1c')
#df1c_d_1 <- df1c_d[,c('Subject','Morality','dprime')]
#df1c_d_1$exp <- 'exp1c'

#df2_d_1  <- read.csv('exp2_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp2')
#df2_d_1 <- df2_d[,c('Subject','Morality','dprime')]
#df2_d_1$exp <- 'exp2'

#df3a_d_1 <- read.csv('exp3a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp3a')
#df3a_d_1 <- df3a_d[,c('Subject','Morality','dprime')]
#df3a_d_1$exp <- 'exp3a'
#df3a_d_2 <- read.csv('exp3a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject, Identity, Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp3a')
#df3a_d[,c('Subject','Identity','Morality','dprime')]
#df3a_d_2$exp <- 'exp3a'

#df3b_d_1 <- read.csv('exp3b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp3b')

#df3b_d_1 <- df3b_d[,c('Subject','Morality','dprime')]
#df3b_d_1$exp <- 'exp3b'
#df3b_d_2 <- read.csv('exp3b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject, Identity, Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp3b')

#df3b_d[,c('Subject','Identity','Morality','dprime')]
#df3b_d_2$exp <- 'exp3b'

#df4a_d_1 <- read.csv('exp4a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp4a')

#df4a_d_1 <- df4a_d[,c('Subject','Morality','dprime')]
#df4a_d_1$exp <- 'exp4a'
#df4a_d_2 <- read.csv('exp4a_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject, Identity, Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp4a')

#  df4a_d[,c('Subject','Identity','Morality','dprime')]
#df4a_d_2$exp <- 'exp4a'

#df4b_d_1 <- read.csv('exp4b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject,Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp4b')

#df4b_d_1 <- df4b_d[,c('Subject','Morality','dprime')]
#df4b_d_1$exp <- 'exp4b'

#df4b_d_2 <- read.csv('exp4b_dprime_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
#  dplyr::select(Subject, Identity, Morality,dprime) %>%
#  dplyr::mutate(exp = 'exp4b') 

#df4b_d[,c('Subject','Identity','Morality','dprime')]
#df4b_d_2$exp <- 'exp4b'

# combine dataset
#df.d_1 <- rbind(df1a_d_1,df1b_d_1,df1c_d_1,df2_d_1,df3a_d_1,df3b_d_1,df4a_d_1,df4b_d_1) %>% # main effect of moral valence
#  dplyr::mutate(Morality = recode(Morality, Moral = "Good", Immoral = "Bad"))

#df.d_1$Morality[df.d_1$Morality == 'Moral'] <- 'Good'
#df.d_1$Morality[df.d_1$Morality == 'Immoral']  <- 'Bad'

#df.d_2 <- rbind(df3a_d_2,df3b_d_2,df4a_d_2,df4b_d_2) %>%                        # both effect of valence and id
#  dplyr::mutate(Morality = recode(Morality, Moral = "Good", Immoral = "Bad"))
#df.d_2$Morality[df.d_2$Morality == 'Moral'] <- 'Good'
#df.d_2$Morality[df.d_2$Morality == 'Immoral']  <- 'Bad'

#rm(df1a_d,df1a_d_1,df1b_d,df1b_d_1,df2_d,df2_d_1,df3a_d,df3a_d_1,df3a_d_2,df3b_d,df3b_d_1,df3b_d_2,df4a_d,df4a_d_1,df4a_d_2,
#   df4b_d,df4b_d_1,df4b_d_2)

##### calculate the mean and sd for each condition ####
## for main effect of moral valence:
df.d_1.sum <- df.d_1 %>%
  dplyr::group_by(exp, Morality) %>%
  dplyr::summarise(N = length(dprime),
                   mean = mean(dprime),
                   sd   = sd(dprime),
                   se   = sd(dprime)/sqrt(length(dprime)-1))
  #summarySE(df.d_1,measurevar = 'dprime',groupvars = c('exp','Morality'))

## separate the self vs. other conditions
df.d_2.sum <- df.d_2 %>%
  dplyr::group_by(exp, Identity, Morality) %>%
  dplyr::summarise(mean = mean(dprime),
                   sd   = sd(dprime),
                   se   = sd(dprime)/sqrt(length(dprime)-1))
  
  #summarySE(df.d_2,measurevar = 'dprime',groupvars = c('exp','Identity','Morality'))


#### calculate the effect size and variation of them ########
#################################################################################################
#####   calculate the effect size for effect of moral valence 
#################################################################################################

# delete df.d_1.cor if this variable already exist.
if (exists('df.d_1.es')){
  rm(df.d_1.es)
}

# create the correlation pairs between different conditions:
for (expName in unique(df.d_1$exp)){
  # if correlation dataset doesn't exist, create it
  if (!exists("df.d_1.es")){
    df.d_1.es <-setNames(data.frame(matrix(ncol = 3, nrow = 3)), c("exp", "condition",'r'))
    df.d_1.es$exp <- expName
    df.d_1.es$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
    
    tmp.df   <- subset(df.d_1,exp == expName)
    moralVal <- c("Bad", "Good", "Neutral") # order: immoral, moral, neutral
    tmp.cor  <- sapply(1:length(moralVal),function(i)
      sapply(1:length(moralVal),function(j)
        cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
    
    df.d_1.es$r[df.d_1.es$exp == expName & df.d_1.es$condition == 'Good-Bad']  <- tmp.cor[2,1] # row two (moral) column one (immoral)
    df.d_1.es$r[df.d_1.es$exp == expName & df.d_1.es$condition == 'Good-Neut'] <- tmp.cor[3,2] # row three (neutral) column two (moral)
    df.d_1.es$r[df.d_1.es$exp == expName & df.d_1.es$condition == 'Neut-Bad']  <- tmp.cor[3,1] # row three (neutral) column one (immoral)
  }
  
  # if the correlation dataset  does exist, append to it
  else if (exists("df.d_1.es")){
    tmp <-setNames(data.frame(matrix(ncol = 3, nrow = 3)), c("exp", "condition",'r'))
    tmp$exp <- expName
    tmp$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
    df.d_1.es <- rbind(df.d_1.es,tmp)
                        
    tmp.df <- subset(df.d_1,exp == expName)
    moralVal =  c("Bad", "Good", "Neutral")
    tmp.cor <- sapply(1:length(moralVal),function(i)
      sapply(1:length(moralVal),function(j)
        cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
    
    df.d_1.es$r[df.d_1.es$exp == expName & df.d_1.es$condition == 'Good-Bad']  <- tmp.cor[2,1]
    df.d_1.es$r[df.d_1.es$exp == expName & df.d_1.es$condition == 'Good-Neut'] <- tmp.cor[3,2]
    df.d_1.es$r[df.d_1.es$exp == expName & df.d_1.es$condition == 'Neut-Bad']  <- tmp.cor[3,1]
  }
}

# using for loop to get the mean and sd for each condition
for (expName in unique(df.d_1$exp)){
  for (cond in c('Good-Bad','Good-Neut','Neut-Bad')){
      if (cond == 'Good-Bad'){  # if the contrast is good vs. bad
        # chose the smaple size of moral good for the current experiment (exp = expName) and current condition
        df.d_1.es$N[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$N[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Good']
        # chose the mean of moral good for the current experiment (exp = expName) and current condition
        df.d_1.es$m.1[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$mean[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Good']
        # chose the sd of moral good for the current experiment (exp = expName) and current condition
        df.d_1.es$sd.1[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$sd[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Good']
        # chose the mean of moral bad for the current experiment (exp = expName) and current condition
        df.d_1.es$m.2[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$mean[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Bad']
        # chose the sd of moral bad for the current experiment (exp = expName) and current condition
        df.d_1.es$sd.2[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$sd[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Bad']
      } else if (cond == 'Good-Neut'){
        df.d_1.es$N[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$N[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Good']
      
        df.d_1.es$m.1[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
            df.d_1.sum$mean[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Good']
      
        df.d_1.es$sd.1[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
           df.d_1.sum$sd[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Good']
      
        df.d_1.es$m.2[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
           df.d_1.sum$mean[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Neutral']
        
        df.d_1.es$sd.2[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
           df.d_1.sum$sd[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Neutral']
      } else {
        df.d_1.es$N[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
          df.d_1.sum$N[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Neutral']
      
        df.d_1.es$m.1[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
          df.d_1.sum$mean[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Neutral']
      
        df.d_1.es$sd.1[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
          df.d_1.sum$sd[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Neutral']
      
        df.d_1.es$m.2[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
          df.d_1.sum$mean[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Bad']
      
        df.d_1.es$sd.2[df.d_1.es$exp == expName & df.d_1.es$condition == cond] <- 
          df.d_1.sum$sd[df.d_1.sum$exp == expName & df.d_1.sum$Morality =='Bad']
      }
  }
}

# calculate the d and variation of d
df.d_1.es[,c('d','var.d')] <- data.frame(d.sgpp(m.1 = df.d_1.es$m.1,m.2 = df.d_1.es$m.2,
                                                sd.1 = df.d_1.es$sd.1, sd.2 = df.d_1.es$sd.2,
                                                n = df.d_1.es$N,
                                                r = df.d_1.es$r))

#################################################################################################
#####   calculate the effect size for effect of moral valence under self and other conditions
#################################################################################################

## calculate the correlation pairs #### 
# delete df.d_1.cor if this variable already exist.
if (exists('df.d_2.es')){
  rm(df.d_2.es)
}
# create the correlation pairs between different conditions:
for (expName in unique(df.d_2$exp)){
  for (id in unique(df.d_2$Identity)){
    # if correlation dataset doesn't exist, create it
    if (!exists("df.d_2.es")){
      df.d_2.es <-setNames(data.frame(matrix(ncol = 4, nrow = 3)), c("exp", "Identity", "condition",'r'))
      df.d_2.es$exp <- expName
      df.d_2.es$Identity <- id
      df.d_2.es$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
      
      tmp.df <- subset(df.d_2, exp == expName & Identity == id)
      moralVal = c("Bad", "Good", "Neutral") # order: immoral, moral, neutral
      tmp.cor <- sapply(1:length(moralVal),function(i)
        sapply(1:length(moralVal),function(j)
          cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
      df.d_2.es$r[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == 'Good-Bad']  <- tmp.cor[2,1] # row two (moral) column one (immoral)
      df.d_2.es$r[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == 'Good-Neut'] <- tmp.cor[3,2] # row three (neutral) column two (moral)
      df.d_2.es$r[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == 'Neut-Bad']  <- tmp.cor[3,1] # row three (neutral) column one (immoral)
    }
    
    # if the correlation dataset  does exist, append to it
    else if (exists("df.d_2.es")){
      tmp <- setNames(data.frame(matrix(ncol = 4, nrow = 3)), c("exp", "Identity", "condition",'r'))
      tmp$exp <- expName
      tmp$Identity <- id
      tmp$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
      df.d_2.es <- rbind(df.d_2.es,tmp)
      
      tmp.df <- subset(df.d_2,exp == expName & Identity == id)
      moralVal =  c("Bad", "Good", "Neutral")
      tmp.cor <- sapply(1:length(moralVal),function(i)
        sapply(1:length(moralVal),function(j)
          cor(x = tmp.df$dprime[tmp.df$Morality == moralVal[i]], y = tmp.df$dprime[tmp.df$Morality == moralVal[j]])))
      df.d_2.es$r[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == 'Good-Bad']  <- tmp.cor[2,1]
      df.d_2.es$r[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == 'Good-Neut'] <- tmp.cor[3,2]
      df.d_2.es$r[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == 'Neut-Bad']  <- tmp.cor[3,1]
    }
  }
}

# using for loop to get the mean and sd for each condition
for (expName in unique(df.d_2$exp)){
  for (id in unique(df.d_2$Identity)){
    for (cond in c('Good-Bad','Good-Neut','Neut-Bad')){
      if (cond == 'Good-Bad'){  # if the contrast is good vs. bad
        # chose the current experiment (exp = expName) and current condition
        
        df.d_2.es$N[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$N[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Good']
        
        df.d_2.es$m.1[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$dprime[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Good']
        
        df.d_2.es$sd.1[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$sd[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Good']
        
        df.d_2.es$m.2[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$dprime[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Bad']
        
        df.d_2.es$sd.2[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$sd[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Bad']
      } else if (cond == 'Good-Neut'){
        df.d_2.es$N[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$N[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Good']
        
        df.d_2.es$m.1[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$dprime[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Good']
        
        df.d_2.es$sd.1[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$sd[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Good']
        
        df.d_2.es$m.2[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$dprime[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Neutral']
        
        df.d_2.es$sd.2[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$sd[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Neutral']
      } else {
        df.d_2.es$N[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$N[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Neutral']
        
        df.d_2.es$m.1[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$dprime[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Neutral']
        
        df.d_2.es$sd.1[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$sd[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Neutral']
        
        df.d_2.es$m.2[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$dprime[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Bad']
        
        df.d_2.es$sd.2[df.d_2.es$exp == expName & df.d_2.es$Identity == id & df.d_2.es$condition == cond] <- 
          df.d_2.sum$sd[df.d_2.sum$exp == expName & df.d_2.sum$Identity == id & df.d_2.sum$Morality =='Bad']
      }
    }
  }
}

# calculate the effect size for each pair:
df.d_2.es[,c('d','var.d')] <- data.frame(d.sgpp(m.1 = df.d_2.es$m.1,m.2 = df.d_2.es$m.2,
                                                sd.1 = df.d_2.es$sd.1, sd.2 = df.d_2.es$sd.2,
                                                n = df.d_2.es$N,
                                                r = df.d_2.es$r))

### To check the results with data from exp1a ####
# df1a_d <- read.csv('exp1a_dprime_long.csv',header = T, sep = ',',
#                 stringsAsFactors=FALSE,na.strings=c("","NA"))
# df1a.d.sum <- summarySE(df1a_d,measurevar = 'dprime',groupvars = c('Morality'))
# df1a.cor.mrl_imm <- cor(x = df1a_d$dprime[df1a_d$Morality == "Moral"],df1a_d$dprime[df1a_d$Morality == "Immoral"])
# df1a.d.es.mrl_imm <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[2], m.2  = df1a.d.sum$dprime[1], 
#                           sd.1 = df1a.d.sum$sd[2],     sd.2 = df1a.d.sum$sd[1], 
#                           n    = df1a.d.sum$N[1],
#                           r    = df1a.cor.mrl_imm))
#df1a.d.es.mrl_imm$exp <- 'exp1a'
#df1a.d.es.mrl_imm$cond <- 'Good-Bad'
#df1a.cor.mrl_neut <- cor(x = df1a_d$dprime[df1a_d$Morality == "Moral"],df1a_d$dprime[df1a_d$Morality == "Neutral"])
#df1a.d.es.mrl_neut <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[2], m.2  = df1a.d.sum$dprime[3], 
#                                       sd.1 = df1a.d.sum$sd[2],     sd.2 = df1a.d.sum$sd[3], 
#                                       n    = df1a.d.sum$N[1],
#                                       r    = df1a.cor.mrl_neut))
#
#df1a.cor.neut_imm <- cor(x = df1a_d$dprime[df1a_d$Morality == "Neutral"],df1a_d$dprime[df1a_d$Morality == "Immoral"])
#df1a.d.es.neut_imm <- data.frame(d.sgpp(m.1 = df1a.d.sum$dprime[3], m.2  = df1a.d.sum$dprime[1], 
#                                        sd.1 = df1a.d.sum$sd[3],    sd.2 = df1a.d.sum$sd[1], 
#                                        n    = df1a.d.sum$N[1],
#                                        r    = df1a.cor.neut_imm))
# df1a.d.es <- rbind(df1a.d.es.mrl_neut,df1a.d.es.mrl_imm,df1a.d.es.neut_imm)


write.csv(df.d_1.es,'Effect_size_dprime_moral_valence.csv',row.names = F)
write.csv(df.d_2.es,'Effect_size_dprime_self_other_sep.csv',row.names = F)

# meta-analysis for d prime ####
# moral vs. neutral
df.d_1_goodNeutral <- subset(df.d_1.es, condition == "Good-Neut")
M_D_mrl_neut <- rma(yi = df.d_1.es$d[df.d_1.es$cond == 'Good-Neut'], 
                    vi = df.d_1.es$var.d[df.d_1.es$cond == 'Good-Neut'],
                    slab = df.d_1.es$exp[df.d_1.es$cond == 'Good-Neut'])
forest(M_D_mrl_neut)

M_D_mrl_imm <- rma(yi = df.d_1.es$d[df.d_1.es$cond == 'Good-Bad'], 
                   vi = df.d_1.es$var.d[df.d_1.es$cond == 'Good-Bad'],
                   slab = df.d_1.es$exp[df.d_1.es$cond == 'Good-Bad'])
forest(M_D_mrl_imm)

M_D_neut_imm <- rma(yi = df.d_1.es$d[df.d_1.es$cond == 'Neut-Bad'], 
                    vi = df.d_1.es$var.d[df.d_1.es$cond == 'Neut-Bad'],
                    slab = df.d_1.es$exp[df.d_1.es$cond == 'Neut-Bad'])
forest(M_D_neut_imm)

#### For self condition
M_D_mrl_neut_s <- rma(yi = df.d_2.es$d[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Good-Neut'], 
                      vi = df.d_2.es$var.d[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Good-Neut'],
                      slab = df.d_2.es$exp[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Good-Neut'])
forest(M_D_mrl_neut_s)

M_D_mrl_imm_s <- rma(yi = df.d_2.es$d[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Good-Bad'], 
                     vi = df.d_2.es$var.d[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Good-Bad'],
                     slab = df.d_2.es$exp[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Good-Bad'])
forest(M_D_mrl_imm_s)

M_D_neut_imm_s <- rma(yi = df.d_2.es$d[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Neut-Bad'], 
                      vi = df.d_2.es$var.d[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Neut-Bad'],
                      slab = df.d_2.es$exp[df.d_2.es$Identity == 'Self' & df.d_2.es$cond == 'Neut-Bad'])
forest(M_D_neut_imm_s)

#### For Other condition
M_D_mrl_neut_o <- rma(yi = df.d_2.es$d[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Good-Neut'], 
                      vi = df.d_2.es$var.d[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Good-Neut'],
                      slab = df.d_2.es$exp[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Good-Neut'])
forest(M_D_mrl_neut_o)

M_D_mrl_imm_o <- rma(yi = df.d_2.es$d[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Good-Bad'], 
                     vi = df.d_2.es$var.d[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Good-Bad'],
                     slab = df.d_2.es$exp[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Good-Bad'])
forest(M_D_mrl_imm_o)

M_D_neut_imm_o <- rma(yi = df.d_2.es$d[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Neut-Bad'], 
                      vi = df.d_2.es$var.d[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Neut-Bad'],
                      slab = df.d_2.es$exp[df.d_2.es$Identity == 'Other' & df.d_2.es$cond == 'Neut-Bad'])
forest(M_D_neut_imm_o)

######################################################################################################################
#################################### Meta-analysis for RT ############################################################
#
#
#### Main effect of moral valence
df1a_rt <- read.csv('exp1a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df1a_rt_m <-df1a_rt[df1a_rt$Matchness == 'Match',]

df1a.rt.sum <- summarySE(df1a_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df1a.rt.sum$exp <- 'study 1a'
df1a.rt.sum <- df1a.rt.sum[,c('exp','Morality','RT','sd')]
df1a.rt.sum_w <- dcast(melt(df1a.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
df1a.rt.sum_w$N <- nrow(df1a_rt_m)/3

df1b.rt.sum <- summarySE(df1b_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df1b.rt.sum$exp <- 'study 1b'
df1b.rt.sum <- df1b.rt.sum[,c('exp','Morality','RT','sd')]
df1b.rt.sum_w <- dcast(melt(df1b.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
df1b.rt.sum_w$N <- nrow(df1b_rt_m)/3

df2_rt <- read.csv('exp2_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df2_rt_m <-df2_rt[df2_rt$Matchness == 'Match',]
df2.rt.sum <- summarySE(df2_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df2.rt.sum$exp <- 'study 2'
df2.rt.sum <- df2.rt.sum[,c('exp','Morality','RT','sd')]
df2.rt.sum_w <- dcast(melt(df2.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
df2.rt.sum_w$N <- nrow(df2_rt_m)/3

df3a_rt <- read.csv('exp3a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df3a_rt$exp <- 'study 3a'
df3a_rt_m <-df3a_rt[df3a_rt$Matchness == 'Match',]

df3a.rt.sum <- summarySE(df3a_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df3a.rt.sum$exp <- 'study 3a'
df3a.rt.sum <- df3a.rt.sum[,c('exp','Morality','RT','sd')]
df3a.rt.sum_w <- dcast(melt(df3a.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
df3a.rt.sum_w$N <- nrow(df3a_rt_m)/6

df3b_rt <- read.csv('exp3b_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df3b_rt$Morality[df3b_rt$Morality == 'Good'] <- 'Moral'
df3b_rt$Morality[df3b_rt$Morality == 'Bad'] <- 'Immoral'
df3b_rt$exp <- 'study 3b'
df3b_rt_m <-df3b_rt[df3b_rt$Matchness == 'Match',]

df3b.rt.sum <- summarySE(df3b_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df3b.rt.sum$exp <- 'study 3b'
df3b.rt.sum <- df3b.rt.sum[,c('exp','Morality','RT','sd')]
df3b.rt.sum_w <- dcast(melt(df3b.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
df3b.rt.sum_w$N <- nrow(df3b_rt_m)/6

# study 4a
df4a_rt <- read.csv('exp4a_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df4a_rt$exp <- 'study 4a'
df4a_rt_m <-df4a_rt[df4a_rt$Matchness == 'Match',]

df4a.rt.sum <- summarySE(df4a_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df4a.rt.sum$exp <- 'study 4a'
df4a.rt.sum <- df4a.rt.sum[,c('exp','Morality','RT','sd')]
df4a.rt.sum_w <- dcast(melt(df4a.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
df4a.rt.sum_w$N <- nrow(df4a_rt_m)/6

# study 4b
df4b_rt <- read.csv('exp4b_rt_acc_long.csv',header = T, sep = ',', stringsAsFactors=FALSE,na.strings=c("","NA"))
df4b_rt$exp <- 'study 4b'
df4b_rt_m <-df4b_rt[df4b_rt$Matchness == 'Match',]

df4b.rt.sum <- summarySE(df4b_rt_m,measurevar = 'RT',groupvars = c('Morality'))
df4b.rt.sum$exp <- 'study 4b'
df4b.rt.sum <- df4b.rt.sum[,c('exp','Morality','RT','sd')]
df4b.rt.sum_w <- dcast(melt(df4b.rt.sum, id.vars=c("exp",'Morality')), exp ~ variable + Morality)
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
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Moral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Moral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 2']  <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Moral"], df2_rt_m$RT[df2_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 3a'] <- cor(df3a_rt_m$RT[df3a_rt_m$Morality == "Moral"], df3a_rt_m$RT[df3a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 3b'] <- cor(df3b_rt_m$RT[df3b_rt_m$Morality == "Moral"], df3b_rt_m$RT[df3b_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 4a'] <- cor(df4a_rt_m$RT[df4a_rt_m$Morality == "Moral"], df4a_rt_m$RT[df4a_rt_m$Morality == "Neutral"])
df.rt.sum_w_mrl_neut$Cor[df.rt.sum_w_mrl_neut$exp == 'study 4b'] <- cor(df4b_rt_m$RT[df4b_rt_m$Morality == "Moral"], df4b_rt_m$RT[df4b_rt_m$Morality == "Neutral"])
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
                    slab = df.rt.sum_w_mrl_neut$exp)
forest(M_RT_mrl_neut)

# RT: Moral vs. Immoral
df.rt.sum_w_mrl_imm <- df.rt.sum_w[,c(1,3,2,6,5,8)]
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Moral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Moral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 2'] <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Moral"], df2_rt_m$RT[df2_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 3a'] <- cor(df3a_rt_m$RT[df3a_rt_m$Morality == "Moral"], df3a_rt_m$RT[df3a_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 3b'] <- cor(df3b_rt_m$RT[df3b_rt_m$Morality == "Moral"], df3b_rt_m$RT[df3b_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 4a'] <- cor(df4a_rt_m$RT[df4a_rt_m$Morality == "Moral"], df4a_rt_m$RT[df4a_rt_m$Morality == "Immoral"])
df.rt.sum_w_mrl_imm$Cor[df.rt.sum_w_mrl_imm$exp == 'study 4b'] <- cor(df4b_rt_m$RT[df4b_rt_m$Morality == "Moral"], df4b_rt_m$RT[df4b_rt_m$Morality == "Immoral"])
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
                     slab = df.rt.sum_w_mrl_imm$exp)
forest(M_RT_mrl_imm)

# RT: Neutral v. Immoral
df.rt.sum_w_neut_imm <- df.rt.sum_w[,c(1,4,2,7,5,8)]
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 1a'] <- cor(df1a_rt_m$RT[df1a_rt_m$Morality == "Neutral"], df1a_rt_m$RT[df1a_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 1b'] <- cor(df1b_rt_m$RT[df1b_rt_m$Morality == "Neutral"], df1b_rt_m$RT[df1b_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 2']  <- cor(df2_rt_m$RT[df2_rt_m$Morality == "Neutral"], df2_rt_m$RT[df2_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 3a'] <- cor(df3a_rt_m$RT[df3a_rt_m$Morality == "Neutral"], df3a_rt_m$RT[df3a_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 3b'] <- cor(df3b_rt_m$RT[df3b_rt_m$Morality == "Neutral"], df3b_rt_m$RT[df3b_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 4a'] <- cor(df4a_rt_m$RT[df4a_rt_m$Morality == "Neutral"], df4a_rt_m$RT[df4a_rt_m$Morality == "Immoral"])
df.rt.sum_w_neut_imm$Cor[df.rt.sum_w_neut_imm$exp == 'study 4b'] <- cor(df4b_rt_m$RT[df4b_rt_m$Morality == "Neutral"], df4b_rt_m$RT[df4b_rt_m$Morality == "Immoral"])
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
                    slab = df.rt.sum_w_neut_imm$exp)
forest(M_RT_neut_imm)


#################################################################################################
#####   RT: calculate the effect size for effect of moral valence under self and other conditions
#################################################################################################

df.rt_2 <- rbind(df3a_rt_m,df3b_rt_m,df4a_rt_m,df4b_rt_m)                           # both effect of valence and id
df.rt_2.sum <- summarySE(df.rt_2,measurevar = 'RT',groupvars = c('exp','Identity','Morality'))
## calculate the correlation pairs #### 
# delete df.d_1.cor if this variable already exist.
if (exists('df.rt_2_es')){
  rm(df.rt_2_es)
}
# create the correlation pairs between different conditions:
for (expName in unique(df.rt_2$exp)){
  for (id in unique(df.rt_2$Identity)){
    # if correlation dataset doesn't exist, create it
    if (!exists("df.rt_2_es")){
      df.rt_2_es <-setNames(data.frame(matrix(ncol = 4, nrow = 3)), c("exp", "Identity", "condition",'r'))
      df.rt_2_es$exp <- expName
      df.rt_2_es$Identity <- id
      df.rt_2_es$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
      
      tmp.df <- subset(df.rt_2, exp == expName & Identity == id)
      moralVal = c("Immoral", "Moral", "Neutral") # order: immoral, moral, neutral
      tmp.cor <- sapply(1:length(moralVal),function(i)
        sapply(1:length(moralVal),function(j)
          cor(x = tmp.df$RT[tmp.df$Morality == moralVal[i]], y = tmp.df$RT[tmp.df$Morality == moralVal[j]])))
      df.rt_2_es$r[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == 'Good-Bad']  <- tmp.cor[2,1] # row two (moral) column one (immoral)
      df.rt_2_es$r[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == 'Good-Neut'] <- tmp.cor[3,2] # row three (neutral) column two (moral)
      df.rt_2_es$r[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == 'Neut-Bad']  <- tmp.cor[3,1] # row three (neutral) column one (immoral)
    }
    
    # if the correlation dataset  does exist, append to it
    else if (exists("df.rt_2_es")){
      tmp <- setNames(data.frame(matrix(ncol = 4, nrow = 3)), c("exp", "Identity", "condition",'r'))
      tmp$exp <- expName
      tmp$Identity <- id
      tmp$condition <- c('Good-Bad','Good-Neut','Neut-Bad')
      df.rt_2_es <- rbind(df.rt_2_es,tmp)
      
      tmp.df <- subset(df.rt_2,exp == expName & Identity == id)
      moralVal =  c("Immoral", "Moral", "Neutral")
      tmp.cor <- sapply(1:length(moralVal),function(i)
        sapply(1:length(moralVal),function(j)
          cor(x = tmp.df$RT[tmp.df$Morality == moralVal[i]], y = tmp.df$RT[tmp.df$Morality == moralVal[j]])))
      df.rt_2_es$r[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == 'Good-Bad']  <- tmp.cor[2,1]
      df.rt_2_es$r[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == 'Good-Neut'] <- tmp.cor[3,2]
      df.rt_2_es$r[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == 'Neut-Bad']  <- tmp.cor[3,1]
    }
  }
}

# using for loop to get the mean and sd for each condition
for (expName in unique(df.rt_2$exp)){
  for (id in unique(df.rt_2$Identity)){
    for (cond in c('Good-Bad','Good-Neut','Neut-Bad')){
      if (cond == 'Good-Bad'){  # if the contrast is good vs. bad
        # chose the current experiment (exp = expName) and current condition
        
        df.rt_2_es$N[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$N[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Moral']
        
        df.rt_2_es$m.1[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$RT[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Moral']
        
        df.rt_2_es$sd.1[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$sd[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Moral']
        
        df.rt_2_es$m.2[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$RT[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Immoral']
        
        df.rt_2_es$sd.2[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$sd[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Immoral']
      } else if (cond == 'Good-Neut'){
        df.rt_2_es$N[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$N[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Moral']
        
        df.rt_2_es$m.1[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$RT[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Moral']
        
        df.rt_2_es$sd.1[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$sd[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Moral']
        
        df.rt_2_es$m.2[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$RT[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Neutral']
        
        df.rt_2_es$sd.2[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$sd[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Neutral']
      } else {
        df.rt_2_es$N[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$N[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Neutral']
        
        df.rt_2_es$m.1[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$RT[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Neutral']
        
        df.rt_2_es$sd.1[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$sd[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Neutral']
        
        df.rt_2_es$m.2[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$RT[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Immoral']
        
        df.rt_2_es$sd.2[df.rt_2_es$exp == expName & df.rt_2_es$Identity == id & df.rt_2_es$condition == cond] <- 
          df.rt_2.sum$sd[df.rt_2.sum$exp == expName & df.rt_2.sum$Identity == id & df.rt_2.sum$Morality =='Immoral']
      }
    }
  }
}

# calculate the effect size for each pair:
df.rt_2_es[,c('d','var.d')] <- data.frame(d.sgpp(m.1 = df.rt_2_es$m.1,m.2 = df.rt_2_es$m.2,
                                                sd.1 = df.rt_2_es$sd.1, sd.2 = df.rt_2_es$sd.2,
                                                n = df.rt_2_es$N,
                                                r = df.rt_2_es$r))

#### For self condition

M_RT_mrl_neut_s <- rma(yi = df.rt_2_es$d[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Good-Neut'], 
                      vi = df.rt_2_es$var.d[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Good-Neut'],
                      slab = df.rt_2_es$exp[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Good-Neut'])
forest(M_RT_mrl_neut_s)

M_RT_mrl_imm_s <- rma(yi = df.rt_2_es$d[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Good-Bad'], 
                     vi = df.rt_2_es$var.d[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Good-Bad'],
                     slab = df.rt_2_es$exp[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Good-Bad'])
forest(M_RT_mrl_imm_s)

M_RT_neut_imm_s <- rma(yi = df.rt_2_es$d[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Neut-Bad'], 
                      vi = df.rt_2_es$var.d[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Neut-Bad'],
                      slab = df.rt_2_es$exp[df.rt_2_es$Identity == 'Self' & df.rt_2_es$cond == 'Neut-Bad'])
forest(M_RT_neut_imm_s)

#### For Other condition
M_RT_mrl_neut_o <- rma(yi = df.rt_2_es$d[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Good-Neut'], 
                      vi = df.rt_2_es$var.d[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Good-Neut'],
                      slab = df.rt_2_es$exp[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Good-Neut'])
forest(M_RT_mrl_neut_o)

M_RT_mrl_imm_o <- rma(yi = df.rt_2_es$d[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Good-Bad'], 
                     vi = df.rt_2_es$var.d[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Good-Bad'],
                     slab = df.rt_2_es$exp[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Good-Bad'])
forest(M_RT_mrl_imm_o)

M_RT_neut_imm_o <- rma(yi = df.rt_2_es$d[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Neut-Bad'], 
                      vi = df.rt_2_es$var.d[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Neut-Bad'],
                      slab = df.rt_2_es$exp[df.rt_2_es$Identity == 'Other' & df.rt_2_es$cond == 'Neut-Bad'])
forest(M_RT_neut_imm_o)
