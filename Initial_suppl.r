#############################################################################
## This script is used for initializing the analysis
#############################################################################

## Function of this script:
# 1. choose the feedback language
# 2. load or prepare packages
# 3. define functions for other scripts

#############################################
############ 1. choose the feedback language
#############################################
# set local encoding to English
if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = 'LC_ALL','English_United States.1250')
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}

Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientific notion
options(digits=5)       # limit the number of reporting

# rm(list = setdiff(ls(), lsf.str()))  # remove all data but keep functions

#############################################
##### 2. load or prepare packages
#############################################
# use pacman to manage the packages
if (!require(pacman)){
        install.packages("pacman")
        library(pacman)
}

# use pacman::p_load to load the packages
pacman::p_load('here',              # for choosing directory
               'tidyverse',         # for data wrangling
               'brms',              # for Bayesian stats, main text
               'tidybayes',         # for Bayesian stats, main text
               "BayesFactor",       # for Bayes factor 
               "ggplot2",           # plot general
               'ggridges',          # plot ridges
               'patchwork',         # plot patch plots together
               "papaja",            # core for reproduce the APA format of the manuscript
               "easystats"
               # "Hmisc", 
               # 'matrixStats', 
               # "afex",           # for frequentists' ANOVA, in suppl
               # 'lme4',           # for frequentists' hierarchical model, in suppl
               # 'emmeans',        # for frequentists' hierarchical model, in suppl
               # "metafor",        # for frequentists' meta-analysis, in suppl
               # "corrplot",       # for questionnaire analysis, in suppl
               # "psych",          # for questionnaire analysis, in suppl
               # "lavaan",         # for questionnaire analysis, in suppl
               # "semPlot",        # for questionnaire analysis, in suppl
               # 'devtools'
               )

source("geom_flat_violin.R")       # for plotting the violin plots

# using cmdstanr as backend, need to installed properly
if (!require(cmdstanr)){
        install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
        library(cmdstanr)
}
# set_cmdstan_path('/home/hcp4715/cmdstan')

# Save some time and store APA format-related code in an object so you can easily
# use it in multiple plots
apatheme <- theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              text=element_text(family='Times'),
              legend.title=element_blank(),
              legend.text = element_text(size =12),
              #legend.position='top',
              plot.title = element_text(lineheight=.8, face="bold", size = 16),
#              plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5),
              axis.text = element_text (size = 14, color = 'black'),
#              axis.text.x = element_text(angle = 45, vjust = 0.5),   # x-axis's label font
              axis.title = element_text (size = 14),
              axis.line.x = element_line(color='black', size = 1),   # increase the size of font
              axis.line.y = element_line(color='black', size = 1),   # increase the size of font
              axis.title.x = element_text(margin=margin(10,0,0,0)),  # increase the space betwen title and x axis
              axis.title.y = element_text(margin=margin(0,12,0,0)))  # increase the space between title and y axis

#  a theme with no ext on x-axis
apatheme_x = theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Times'),
        legend.title=element_blank(),
        legend.text = element_text(size =12),
        #legend.position='top',
        plot.title = element_text(lineheight=.8, face="bold", size = 16),
        #              plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5),
        axis.text = element_text (size = 14, color = 'black'),
        #              axis.text.x = element_text(angle = 45, vjust = 0.5),   # x-axis's label font
        axis.title = element_text (size = 14),
        axis.line.x = element_line(color='black', size = 1),   # increase the size of font
        axis.line.y = element_line(color='black', size = 1),   # increase the size of font
        axis.text.x = element_blank(),                         # no ext on x-axis
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(margin=margin(10,0,0,0)),  # increase the sapce betwen title and x axis
        axis.title.y = element_text(margin=margin(0,12,0,0)),
        strip.text.x = element_text(size = 12, colour = "black"),
        strip.text.y = element_text(size = 12, colour = "black"))  # increase the space between title and y axis

# a theme with small font size
apatheme_s = theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Times'),
        legend.title=element_blank(),
        legend.text = element_text(size =12),
        legend.position="none",
        #legend.position='top',
        plot.title = element_text(lineheight=.8, face="bold", size = 8),
        #              plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5),
        axis.text = element_text (size = 6, color = 'black'),
        #              axis.text.x = element_text(angle = 45, vjust = 0.5),   # x-axis's label font
        axis.title = element_text (size = 8),
        axis.line.x = element_line(color='black', size = 1),   # increase the size of font
        axis.line.y = element_line(color='black', size = 1),   # increase the size of font
        axis.title.x = element_text(margin=margin(6,0,0,0)),  # increase the sapce betwen title and x axis
        axis.title.y = element_text(margin=margin(0,8,0,0)),
        strip.text.x = element_text(size = 6, colour = "black"),
        strip.text.y = element_text(size = 6, colour = "black"))  # increase the space between title and y axis

# define the d prime function
dprime <- function(hit,fa) {
  qnorm(hit) - qnorm(fa)
}


## below is the code from blog, and adapted from A C Del Re from email
d.sgpp <- function(m.1,m.2,sd.1,sd.2,n,r=.5){
        # m.1 = mean at time 1
        # m.2 = mean at time 2
        # sd.1 = standard dev. at time 1
        # sd.2 = standard dev. at time 2
        # n = sample size
        # r = correlation between time 1 and 2
        s.within <- (sqrt((sd.1^2 + sd.2^2)-2*r*sd.1*sd.2))/(sqrt(2*(1-r))) 
        d <- ((m.1 - m.2)/s.within)
        var.d <- 2*(1-r) * (1/n + d^2/(2*n))
        out <- cbind(d, var.d)
        return(out)
}

## code for calculate the summary with sE, adopted from cook book for R
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  pacman::p_load(plyr)
  
  # New version of length which can handle NA's : if na.rm == T, don't count the
  length2 <- function(x, na.rm=FALSE){
    if(na.rm) sum(!is.na(x))
    else      length(x)
  }
  
  # this does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx,col){
                   c(N    = length2(xx[[col]],na.rm=na.rm),
                     mean = mean(xx[[col]],na.rm=na.rm),
                     sd   = sd  (xx[[col]],na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column
  
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd /sqrt(datac$N)   # calculate standard error of the mean
  
  # Confidence interval mltiplier for standard error
  # calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df = N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return (datac)
}

### function of multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}
#

########### define a function for the plots ##########
#### For categorization task
CAplots <- function(saveDir = traDir, curDir = curDir, expName = 'exp7', task = 'id', inData){
      inData$Identity <- factor(inData$Identity,levels = c("Self","Other"))
      inData$Morality <- factor(inData$Morality,levels = c("Good","Bad"))
      #inData$Morality[inData$Morality == "Good"] <- 1
      #inData$Morality[inData$Morality == "Bad"]  <- 2
      if(task == 'val'){              # valence-based categorization
            ACCdata <- inData %>%
                  select(Subject,Task,Morality,Identity,ACC) %>% 
                  filter(Task == "Val")
            rtData <- inData %>%
                  select(Subject,Task,Morality,Identity,RT) %>% 
                  filter(Task == "Val")
            
          } else if (task == 'id'){   # id-based categorization
            ACCdata <- inData %>%
                  select(Subject,Task,Morality,Identity,ACC) %>% 
                  filter(Task == "Id")
            rtData <- inData %>%
                  select(Subject,Task,Morality,Identity,RT) %>% 
                  filter(Task == "Id")
          }else{                         #  combined for experiment 1
            ACCdata <- inData %>%
                  select(Subject,Morality,Identity,ACC)
            rtData <- inData %>%
                  select(Subject,Morality,Identity,RT)
            
      }

    P.acc <- ggplot(ACCdata,aes(x = Morality, 
                                y = ACC, fill = Identity))+
          geom_flat_violin(aes(fill = Identity),position = position_nudge(x = 0.1, y = 0),
                           adjust = 1.5, trim = FALSE, alpha = 0.5,color = NA) +
          geom_dotplot(aes(x = Morality,y = ACC, color = Identity), 
                       binaxis='y', binwidth = 0.0125, stackdir='center', dotsize= 0.5,position = position_dodge(0.15)) +
          geom_boxplot(aes(x = Morality,  y = ACC,fill = Identity),outlier.shape = NA,
                       alpha = 0.5, width = 0.1,  color = "black",
                       position = position_dodge(0.15))+ 
          scale_color_brewer(palette = "Dark2")+
          scale_fill_brewer(palette = "Dark2")+
          ylab("Accuracy")+
          #scale_x_discrete(breaks = c(1,2),labels = c("Good","Bad")) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,1))+
          apatheme
    
    fileName = paste0('p_',expName,'_',task,'_ACC','.pdf')
    ggsave(fileName, P.acc, scale = 1,height = 6, width = 6, dpi = 300, family = "Times",path = saveDir)
    
    P.rt <- ggplot(rtData,aes(x = Morality, y = RT, fill = Identity))+
          geom_flat_violin(aes(fill = Identity),position = position_nudge(x = 0.1, y = 0),
                           adjust = 1.5, trim = FALSE, alpha = 0.5,color = NA) +
          #geom_point(aes(x = as.numeric(Morality)-0.15,y = RT, color = Identity), 
          #           position = position_jitter(width = 0.02),size = 1, shape = 20)+
          geom_dotplot(aes(x = Morality,y = RT, color = Identity), 
                       binaxis='y', binwidth = 8, stackdir='center', dotsize= 0.5,position = position_dodge(0.15)) + 
          geom_boxplot(aes(x = Morality,  y = RT,fill = Identity),outlier.shape = NA,
                       alpha = 0.5, width = 0.1,  color = "black",
                       position = position_dodge(0.15))+ 
          scale_color_brewer(palette = "Dark2")+
          scale_fill_brewer(palette = "Dark2")+
          ylab("Reaction Times")+
          #scale_x_discrete(breaks = c(1,2),labels = c("Good","Bad")) +
          scale_y_continuous(expand = c(0, 0),limits = c(200,1000))+
          apatheme
    
    fileName = paste0('p_',expName,'_',task,'_RT','.pdf')
    ggsave(fileName, P.rt, scale = 1,height = 6, width = 6, dpi = 300, family = "Times",path = saveDir)
    
    fileName = paste0('p_',expName,'_',task,'.tiff')
    setwd(saveDir)
    tiff(fileName, width = 9, height = 6, units = 'in', res = 300)
    p_dprime_match <- multiplot(P.rt,P.acc,cols = 2)
    dev.off()
    setwd(curDir)
    return(multiplot(P.rt, P.acc,cols = 2))
}
 
#### For Match task
#Mplots <- function(saveDir = traDir, curDir = curDir, expName = 'exp1', dData,rtData){
Mplots <- function(expName = 'exp1', dData, rtData){
      #dData <- dData %>% dplyr::rename(Valence=Val_sh)
      #rtData <- rtData %>% dplyr::rename(Valence=Val_sh)
      dData$Valence <- factor(dData$Valence,levels = c("Good",'Neutral',"Bad"))
      rtData$Valence <- factor(rtData$Valence,levels = c("Good",'Neutral',"Bad"))
      if ('Identity' %in% colnames(dData)){
          dData$Identity <- factor(dData$Identity,levels = c("Self","Other"))
          rtData$Identity <- factor(rtData$Identity,levels = c("Self","Other"))
          P.dprime <- ggplot(dData,aes(x = Identity, y = dprime, fill = Valence)) +
                geom_flat_violin(aes(fill = Valence),position = position_nudge(x = 0.1, y = 0),
                                 adjust = 1.5, trim = FALSE, alpha = 0.5,color = NA) +
                geom_dotplot(aes(x = Identity, y = dprime, color = Valence), 
                             binaxis='y', binwidth = 0.01, stackdir='center', dotsize= 5.5,
                             position = position_dodge(0.2)) +
                geom_boxplot(aes(x = Identity,  y = dprime, fill = Valence), 
                             outlier.shape = NA, alpha = 0.5, width = 0.1,  color = 'black',
                             position = position_dodge(0.2)) + 
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2") +
                ylab(expression(paste(italic("d"), " prime"))) +
               # scale_x_discrete(breaks = c(1,2),labels = c("Good","Bad")) +
                scale_y_continuous(expand = c(0, 0), limits = c(-1,5)) +
                apatheme
          
          
          P.rt <- ggplot(rtData,aes(x = Identity , y = RT, fill = Valence))+
                geom_flat_violin(aes(fill = Valence),position = position_nudge(x = 0.1, y = 0),
                                 adjust = 1.5, trim = FALSE, alpha = 0.5,color = NA) +
                #geom_point(aes(x = as.numeric(Morality)-0.15,y = RT, color = Identity), 
                #           position = position_jitter(width = 0.02),size = 1, shape = 20)+
                geom_dotplot(aes(x = Identity,y = RT, color = Valence), 
                             binaxis='y', binwidth = 0.8, stackdir='center', dotsize= 8,
                             position = position_dodge(0.2)) + 
                geom_boxplot(aes(x = Identity,  y = RT,fill = Valence),outlier.shape = NA,
                             alpha = 0.5, width = 0.1,  color = "black",
                             position = position_dodge(0.2))+ 
                scale_color_brewer(palette = "Dark2")+
                scale_fill_brewer(palette = "Dark2")+
                ylab("Reaction Times")+
                #scale_x_discrete(breaks = c(1,2),labels = c("Good","Bad")) +
                scale_y_continuous(expand = c(0, 0),limits = c(300,1000))+
                apatheme
          return(multiplot(P.rt,P.dprime,cols = 2))
      } else {
              #
              #dData$Identity <- "Valence"
              #rtData$Identity <- "Valence"
              P.dprime <- ggplot(dData,aes(x = Valence, y = dprime, fill = Valence)) +
                geom_flat_violin(aes(fill = Valence),position = position_nudge(x = 0.1, y = 0),
                                 adjust = 1.5, trim = FALSE, alpha = 0.5,color = NA) +
                geom_dotplot(aes(x = Valence,y = dprime, color = Valence), 
                             binaxis='y', binwidth = 0.01, stackdir='center', dotsize= 8,
                             position = position_dodge(0.2)) +
                geom_boxplot(aes(x = Valence,  y = dprime,fill = Valence),outlier.shape = NA,
                             alpha = 0.5, width = 0.1,  color = "black",
                             position = position_dodge(0.2)) + 
                scale_color_brewer(palette = "Dark2") +
                scale_fill_brewer(palette = "Dark2") +
                  #xlab("Valence")+
                ylab(expression(paste(italic("d"), " prime"))) +
                  #scale_x_discrete(breaks = c(1,2,3),labels = c("Good","Neutral","Bad")) +
                scale_y_continuous(expand = c(0, 0), limits = c(-2,6)) +
                theme_bw()+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      text=element_text(family='Times'),
                      legend.title =element_blank(),
                      legend.text  = element_text(size =12),
                      #legend.position='top',
                      plot.title = element_text(lineheight=.8, face="bold", size = 16),
                      axis.text = element_text (size = 14, color = 'black'),
                      #              axis.text.x = element_text(angle = 45, vjust = 0.5),   # x-axis's label font
                      axis.title = element_text (size = 14),
                      axis.line.x = element_line(color='black', size = 1),   # increase the size of font
                      axis.line.y = element_line(color='black', size = 1),   # increase the size of font
                      axis.title.x = element_blank(),
                      #axis.title.x = element_text(margin=margin(10,0,0,0)),  # increase the sapce betwen title and x axis
                      axis.title.y = element_text(margin=margin(0,12,0,0)))  # increase the space between title and y axis
                
              P.rt <- ggplot(rtData,aes(x = Valence , y = RT, fill = Valence))+
                geom_flat_violin(aes(fill = Valence),position = position_nudge(x = 0.1, y = 0),
                                 adjust = 1.5, trim = FALSE, alpha = 0.5,color = NA) +
                  #geom_point(aes(x = as.numeric(Morality)-0.15,y = RT, color = Identity), 
                  #           position = position_jitter(width = 0.02),size = 1, shape = 20)+
                geom_dotplot(aes(x = Valence,y = RT, color = Valence), 
                             binaxis='y', binwidth = 0.4, stackdir='center', dotsize= 16,
                             position = position_dodge(0.2)) + 
                geom_boxplot(aes(x = Valence,  y = RT,fill = Valence),outlier.shape = NA,
                             alpha = 0.5, width = 0.1,  color = "black",
                             position = position_dodge(0.2))+ 
                scale_color_brewer(palette = "Dark2")+
                scale_fill_brewer(palette = "Dark2")+
                ylab("Reaction Times")+
                  #scale_x_discrete(breaks = 'Morality',label = 'Morality') +
                  #xlab("Valence") +
                scale_y_continuous(expand = c(0, 0),limits = c(200,1000))+
                theme_bw()+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      text=element_text(family='Times'),
                      legend.title =element_blank(),
                      legend.text  = element_text(size =12),
                      #legend.position='top',
                      plot.title = element_text(lineheight=.8, face="bold", size = 16),
                      axis.text = element_text (size = 14, color = 'black'),
                      #              axis.text.x = element_text(angle = 45, vjust = 0.5),   # x-axis's label font
                      axis.title = element_text (size = 14),
                      axis.line.x = element_line(color='black', size = 1),   # increase the size of font
                      axis.line.y = element_line(color='black', size = 1),   # increase the size of font
                      axis.title.x = element_blank(),
                      #axis.title.x = element_text(margin=margin(10,0,0,0)),  # increase the sapce betwen title and x axis
                      axis.title.y = element_text(margin=margin(0,12,0,0)))  # increase the space between title and y axis
              return(multiplot(P.rt,P.dprime,cols = 2))
      }
}


make_table <- function(df, ali = "left", aw = 0.5){
  t <- regulartable(data = df)
  t <- fontsize(t, size = 18, part = "all")
  t <- font(t, fontname = "Arial", part = "all")
  t <- autofit(t, add_w = aw)
  t <- align(t, align = ali, part = "all")
  t <- theme_zebra(t)
  t <- border(t, border = fp_border(), part = "all") 
  return(t)
}

# new plot for valence effect
Val_plot_NHST <- function(df.rt, df.d){
  df.plot <- df.rt %>%
    dplyr::filter(Matchness == 'Match') %>%  # select matching data for plotting only.
    dplyr::rename(RT = RT_m) %>%
    dplyr::full_join(., df.d) %>%  
    tidyr::pivot_longer(., cols = c(RT, dprime), 
                        names_to = 'DVs', 
                        values_to = "value") %>% # to longer format
    dplyr::mutate(Valence =factor(Valence, levels = c('Good','Neutral', 'Bad')),
                  DVs = factor(DVs, levels = c('RT', 'dprime')),
                  # create an extra column for ploting the individual data cross different conditions.
                  Conds = ifelse(Valence == 'Good', 1, 
                                 ifelse(Valence == 'Neutral', 2, 3))
                  # Conds = mosaic::derivedFactor("1" = (Valence == 'Good'), 
                  #                               "2" = (Valence == 'Neutral'),
                  #                               "3" = (Valence == 'Bad'),
                  #                               method ="first", .default = NA),
                  # Conds = as.numeric(as.character(Conds)),
    ) 
  
  df.plot$Conds_j <- jitter(df.plot$Conds, amount=.09) # add gitter to x
  
  # New facet label names for panel variable
  # https://stackoverflow.com/questions/34040376/cannot-italicize-facet-labels-with-labeller-label-parsed
  levels(df.plot$DVs ) <- c("RT"=expression(paste("Reaction ", "times (ms)")),
                            "dprime"=expression(paste(italic("d"), ' prime')))
  levels(df.plot$DVs ) <- c("RT"=expression(paste("Reaction ", "times (ms)")),
                            "dprime"=expression(paste(italic("d"), ' prime')))
  
  df.plot.sum_p <- summarySE(df.plot, measurevar = "value", groupvars = c('Valence',"DVs")) %>%
    dplyr::mutate(Val_num = ifelse(Valence == 'Good', 1,
                                   ifelse(Valence == 'Neutral', 2, 3)))
  
  pd1 <- position_dodge(0.5)
  scaleFUN <- function(x) sprintf("%.2f", x)
  scales_y <- list(
    RT = scale_y_continuous(limits = c(400, 900)),
    dprime = scale_y_continuous(labels=scaleFUN)
  )
  
  p_df_sum <- df.plot  %>% # dplyr::filter(DVs== 'RT') %>%
    ggplot(., aes(x = Valence, y = value, colour = as.factor(Valence))) +
    geom_line(aes(x = Conds_j, y = value, group = Subject),         # link individual's points by transparent grey lines
              linetype = 1, size = 0.8, colour = "#000000", alpha = 0.06) + 
    geom_point(aes(x = Conds_j, y = value, group = Subject),   # plot individual points
               colour = "#000000",
               size = 3, shape = 20, alpha = 0.1) +
    geom_line(data = df.plot.sum_p, aes(x = as.numeric(Valence), # plot the group means  
                                        y = value, 
                                        #group = Identity, 
                                        colour = as.factor(Valence),
    ), 
    linetype = 1, position = pd1, size = 2)+
    geom_point(data = df.plot.sum_p, aes(x = as.numeric(Valence), # group mean
                                         y = value, 
                                         #group = Identity, 
                                         colour = as.factor(Valence),
    ), 
    shape = 18, position = pd1, size = 5) +
    geom_errorbar(data = df.plot.sum_p, aes(x = as.numeric(Valence),  # group error bar.
                                            y = value, # group = Identity, 
                                            colour = as.factor(Valence),
                                            ymin = value- 1.96*se, 
                                            ymax = value+ 1.96*se), 
                  width = .05, position = pd1, size = 2, alpha = 0.75) +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks=c(1, 2, 3),
                       labels=c("Good", "Neutral", "Bad")) +
    scale_fill_brewer(palette = "Dark2") +
    #ggtitle("A. Matching task") +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          text=element_text(family='Times'),
          legend.title=element_blank(),
          #legend.text = element_text(size =6),
          legend.text = element_blank(),
          legend.position = 'none',
          plot.title = element_text(lineheight=.8, face="bold", size = 18, margin=margin(0,0,20,0)),
          axis.text = element_text (size = 8, color = 'black'),
          axis.title = element_text (size = 8),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_line(color='black', size = 1),    # increase the size of font
          axis.line.y = element_line(color='black', size = 1),    # increase the size of font
          strip.text = element_text (size = 6, color = 'black'), # size of text in strips, face = "bold"
          panel.spacing = unit(1.5, "lines")
    ) +
    facet_wrap( ~ DVs,
                scales = "free_y", nrow = 1,
                labeller = label_parsed)
  return(p_df_sum)
}


Val_id_plot_NHST <- function(df.rt, df.d){
  df.plot <- df.rt %>%
    dplyr::filter(Matchness == 'Match') %>%  # select matching data for plotting only.
    dplyr::rename(RT = RT_m) %>%
    dplyr::full_join(., df.d) %>%  
    tidyr::pivot_longer(., cols = c(RT, dprime), 
                        names_to = 'DVs', 
                        values_to = "value") %>% # to longer format
    dplyr::mutate(Valence =factor(Valence, levels = c('Good','Neutral', 'Bad')),
                  Identity = factor(Identity, levels = c("Self", 'Other')),
                  DVs = factor(DVs, levels = c('RT', 'dprime')),
                  #,
                  # create an extra column for plotting the individual data cross different conditions.
                  Conds = dplyr::case_when(
                                    (Valence == 'Good' & Identity == 'Self') ~ "0.8",
                                    (Valence == 'Good' & Identity == 'Other') ~ "1.2",
                                    (Valence == 'Neutral' & Identity == 'Self') ~ "1.8",
                                    (Valence == 'Neutral' & Identity == 'Other') ~ "2.2",
                                    (Valence == 'Bad' & Identity == 'Self') ~ "2.8",
                                    (Valence == 'Bad' & Identity == 'Other') ~ "3.2"),
                  # Conds = mosaic::derivedFactor("0.8" = (Valence == 'Good' & Identity == 'Self'),
                  #                               "1.2" = (Valence == 'Good' & Identity == 'Other'),
                  #                               "1.8" = (Valence == 'Neutral' & Identity == 'Self'),
                  #                               "2.2" = (Valence == 'Neutral' & Identity == 'Other'),
                  #                               "2.8" = (Valence == 'Bad' & Identity == 'Self'),
                  #                               "3.2" = (Valence == 'Bad' & Identity == 'Other'),
                  #                               method ="first", .default = NA),
                  Conds = as.numeric(Conds),
    )
  
  df.plot$Conds_j <- jitter(df.plot$Conds, amount=.05) # add gitter to x

  levels(df.plot$DVs ) <- c("RT"=expression(paste("Reaction ", "times (ms)")),
                            "dprime"=expression(paste(italic("d"), ' prime')))
  
  df.plot.sum_p <- summarySE(df.plot, measurevar = "value", groupvars = c('Valence', 'Identity',"DVs")) %>%
    dplyr::mutate(Val_num = ifelse(Valence == 'Good', 1,
                                   ifelse(Valence == 'Neutral', 2, 3)))
  
  pd1 <- position_dodge(0.8)
  scaleFUN <- function(x) sprintf("%.2f", x)
  scales_y <- list(
    RT = scale_y_continuous(limits = c(400, 900)),
    dprime = scale_y_continuous(labels=scaleFUN)
  )
  
  p_df_sum <- df.plot  %>% # dplyr::filter(DVs== 'RT') %>%
    ggplot(., aes(x = Valence, y = value)) +
    geom_line(aes(x = Conds_j, y = value, group = Subject),         # link individual's points by transparent grey lines
              linetype = 1, size = 0.8, colour = "#000000", alpha = 0.03) + 
    geom_point(aes(x = Conds_j, y = value, group = Subject, colour = as.factor(Identity)),   # plot individual points
               #colour = "#000000",
               size = 3, shape = 20, alpha = 0.15) +
    geom_line(data = df.plot.sum_p, aes(x = as.numeric(Valence), # plot the group means  
                                        y = value, 
                                        group = Identity, 
                                        colour = as.factor(Identity),
    ), 
    linetype = 1, position = pd1, size = 2) +
    geom_point(data = df.plot.sum_p, aes(x = as.numeric(Valence), # group mean
                                         y = value, 
                                         group = Identity, 
                                         colour = as.factor(Identity),
    ), 
    shape = 18, position = pd1, size = 6) +
    geom_errorbar(data = df.plot.sum_p, aes(x = as.numeric(Valence),  # group error bar.
                                            y = value, group = Identity, 
                                            colour = as.factor(Identity),
                                            ymin = value- 1.96*se, 
                                            ymax = value+ 1.96*se), 
                  width = .05, position = pd1, size = 2, alpha = 0.75) +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks=c(1, 2, 3),
                       labels=c("Good", "Neutral", "Bad")) +
    scale_fill_brewer(palette = "Dark2") +
    #ggtitle("A. Matching task") +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          text=element_text(family='Times'),
          legend.title=element_blank(),
          legend.text = element_text(size =16),
          plot.title = element_text(lineheight=.8, face="bold", size = 18, margin=margin(0,0,20,0)),
          axis.text = element_text (size = 16, color = 'black'),
          axis.title = element_text (size = 16),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_line(color='black', size = 1),    # increase the size of font
          axis.line.y = element_line(color='black', size = 1),    # increase the size of font
          strip.text = element_text (size = 16, color = 'black'), # size of text in strips, face = "bold"
          panel.spacing = unit(3, "lines")
    ) +
    facet_wrap( ~ DVs,
                scales = "free_y", nrow = 1,
                labeller = label_parsed)
  return(p_df_sum)
}


# define a function to run the sdt GLMM for all exp with Matchness * Valence design
# for 1a, 1b, 1c, 2, 6a
fun_sdt_val <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_sdt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        
        m <- df %>%
                dplyr::filter(!is.na(RESP)) %>% # filter trials without response
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              saymatch = ifelse((Matchness == 'Match' & ACC == 1) | 
                                                        (Matchness == 'Mismatch' & ACC == 0), 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                brms::brm(saymatch ~ 0 + Valence + ismatch:Valence + 
                                  (0 + Valence + ismatch:Valence | Subject),
                          family = bernoulli(link="probit"),
                          data = .,
                          control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

fun_plot_sdt_val <- function(m_sdt) {
        # extract c
        tmp_c <- m_sdt %>% 
                tidybayes::gather_draws(b_ValenceBad, b_ValenceNeutral, b_ValenceGood) %>%
                dplyr::rename(Valence = .variable, sdt_c = .value) %>% dplyr::ungroup() %>%
                dplyr::mutate(Valence = gsub("b_", "", Valence)) %>%
                dplyr::mutate(Valence = ifelse(stringr::str_detect(Valence, 'Bad'), 'Bad',
                                               ifelse(stringr::str_detect(Valence, 'Good'), 'Good', 'Neutral')))
        
        # dprime
        tmp_d <- m_sdt %>% 
                tidybayes::gather_draws(`b_ValenceBad:ismatch`, `b_ValenceNeutral:ismatch`, 
                                        `b_ValenceGood:ismatch`) %>%
                dplyr::rename(Valence = .variable, sdt_d = .value) %>% dplyr::ungroup() %>%
                dplyr::mutate(Valence = gsub("b_", "", Valence)) %>%
                dplyr::mutate(Valence = ifelse(stringr::str_detect(Valence, 'Bad'), 'Bad',
                                               ifelse(stringr::str_detect(Valence, 'Good'), 'Good', 'Neutral')))
        
        # plot summaries with densities
        p_sdt_d_sum <- tmp_d %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                ggplot2::ggplot(aes(x = sdt_d, y = Valence)) +
                tidybayes::stat_halfeyeh() + 
                labs(x = "sensitivity (d')", y = 'Posterior') +
                theme_classic()
        
        p_sdt_c_sum <- tmp_c %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                ggplot2::ggplot(aes(x = sdt_c, y = Valence)) +
                tidybayes::stat_halfeyeh() + 
                labs(x = "criteria (c)", y = 'Posterior') +
                theme_classic()
        
        # plot comparison
        p_sdt_d <- tmp_d %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                tidybayes::compare_levels(sdt_d, by = Valence) %>%
                ggplot2::ggplot(aes(x = sdt_d, y = Valence, fill = after_stat(x > 0))) +
                tidybayes::stat_halfeyeh() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(x = "sensitivity (d')", y = 'Comparison') +
                theme_classic()
        
        p_sdt_c <- tmp_c %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                tidybayes::compare_levels(sdt_c, by = Valence) %>%
                ggplot2::ggplot(aes(x = sdt_c, y = Valence, fill = after_stat(x > 0))) +
                tidybayes::stat_halfeyeh() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(x = "criteria (c)", y = 'Comparison') +
                theme_classic()
        return(list(p_sdt_d_sum, p_sdt_c_sum, p_sdt_d, p_sdt_c))
}

# define a function (shifted_lognormal) to run the RT GLMM for all exp with Matchness * Valence design
fun_rt_val <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_rt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        m <- df %>%
                dplyr::mutate(RT_sec = RT/1000) %>% # log RT in seconds
                dplyr::filter(ACC == 1) %>%
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                brms::brm(RT_sec ~ ismatch*Valence + (ismatch*Valence | Subject),
                          family = shifted_lognormal(),
                          data = ., control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

fun_plot_rt_val <- function(m_rt) {
        library(patchwork)
        tmp_rt <- m_rt %>% 
                tidybayes::spread_draws(b_Intercept, b_ValenceBad, b_ValenceGood, 
                                        b_ismatch,   `b_ValenceBad:ismatch`, `b_ValenceGood:ismatch`) %>%
                dplyr::mutate(Neut_MM = b_Intercept,
                              Bad_MM = Neut_MM + b_ValenceBad,
                              Good_MM = Neut_MM + b_ValenceGood,
                              Neut_M = Neut_MM + b_ismatch,
                              Bad_M = Neut_MM + b_ismatch + `b_ValenceBad:ismatch`,
                              Good_M = Neut_MM + b_ismatch + `b_ValenceGood:ismatch`) %>%
                dplyr::select(-contains('b_')) %>%
                tidyr::pivot_longer(cols = Neut_MM:Good_M,
                                    names_to = 'cond',
                                    values_to = 'logRT') %>%
                dplyr::mutate(RT = exp(logRT)*1000,
                              Matchness = dplyr::case_when(grepl("_MM$", cond) ~ "Mismatch",
                                                           grepl("_M$", cond) ~ "Match"),
                              Valence = dplyr::case_when(grepl("Neut", cond) ~ "Neutral",
                                                         grepl("Bad", cond) ~ "Bad",
                                                         grepl("Good", cond) ~ "Good")
                              # Matchness = dplyr::case_when(cond == 'Neut_MM' | cond == 'Bad_MM' | cond == 'Good_MM' ~ 'Mismatch',
                              #                              cond == 'Neut_M'  | cond == 'Bad_M'  | cond == 'Good_M' ~ 'Match'),
                              # Valence = dplyr::case_when(cond == 'Neut_MM' | cond == 'Neut_M' ~ 'Neutral',
                              #                            cond == 'Bad_MM'  | cond == 'Bad_M'  ~ 'Bad', 
                              #                            cond == 'Good_MM' | cond == 'Good_M' ~ 'Good')
                )
        p_exp1b_rt_m_sum <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                dplyr::filter(Matchness == 'Match') %>%
                ggplot2::ggplot(aes(x = RT, y = Valence)) +
                tidybayes::stat_halfeye() + 
                labs(x = "RTs (Matching, ms)", y = 'Posterior') +
                theme_classic()
        p_exp1b_rt_mm_sum <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                dplyr::filter(Matchness == 'Mismatch') %>%
                ggplot2::ggplot(aes(x = RT, y = Valence)) +
                tidybayes::stat_halfeye() + 
                labs(tag = 'D', x = "RTs (Nonmatching, ms)", y = 'Posterior') +
                theme_classic()
        
        # plot comparison
        p_exp1b_rt_m <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                dplyr::filter(Matchness == 'Match') %>%
                tidybayes::compare_levels(RT, by = Valence) %>%
                ggplot2::ggplot(aes(x = RT, y = Valence, fill = after_stat(x < 0))) +
                tidybayes::stat_halfeye() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(tag = 'C', x = "RTs (Matching, ms)", y = 'Comparison') +
                theme_classic()
        p_exp1b_rt_mm <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                dplyr::filter(Matchness == 'Mismatch') %>%
                tidybayes::compare_levels(RT, by = Valence) %>%
                ggplot2::ggplot(aes(x = RT, y = Valence, fill = after_stat(x < 0))) +
                tidybayes::stat_halfeye() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(tag = 'D', x = "RTs (Mismatching, ms)", y = 'Comparison') +
                theme_classic()
        return(list(p_exp1b_rt_m_sum, p_exp1b_rt_mm_sum, p_exp1b_rt_m, p_exp1b_rt_mm))
}

# function for SDT with Match by identity by valence design
fun_sdt_val_id <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_sdt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        
        m <- df %>%
                dplyr::filter(!is.na(RESP)) %>% # filter trials without response
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              saymatch = ifelse((Matchness == 'Match' & ACC == 1) | 
                                                        (Matchness == 'Mismatch' & ACC == 0), 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good')),
                              Identity = factor(Identity, levels = c('Self', 'Other'))) %>%
                brms::brm(saymatch ~ 0 + Identity:Valence + ismatch:Identity:Valence + 
                                  (0 + Identity:Valence + ismatch:Identity:Valence | Subject),
                          family = bernoulli(link="probit"),
                          data = .,
                          control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

# define a function (shifted_lognormal) to run the RT GLMM for all exp with Matchness * Identity * Valence design
fun_rt_val_id <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_rt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        m <- df %>%
                dplyr::mutate(RT_sec = RT/1000) %>% # log RT in seconds
                dplyr::filter(ACC == 1) %>%
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good')),
                              Identity = factor(Identity, levels=c('Self', 'Other'))) %>%
                brms::brm(RT_sec ~ ismatch*Identity*Valence + (ismatch*Identity*Valence | Subject),
                          family = shifted_lognormal(),
                          data = ., control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

