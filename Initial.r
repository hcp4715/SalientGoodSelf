# this script is used for initializing the analysis
# preparing necessary functions used in current analysis

#Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientifc notion
options(digits=5)       # limit the number of reporting
#rm(list = setdiff(ls(), lsf.str()))  # remove all data but keep functions
pkgTest <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep = TRUE)
                if(!require(x,character.only = TRUE)) stop("Package not found")
        }
}

pkgNeeded <- (c("tidyverse","ggplot2", "afex", 'emmeans','lsmeans',
                "BayesFactor","psych","corrplot","readr", 'lme4',
                'mosaic', 'here'))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable development verions from GitHub
if(!"papaja" %in% rownames(installed.packages())) devtools::install_github("crsh/papaja")

# run the geo_flat_violin.r, which is from:https://gist.githubusercontent.com/
# benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R
source("geom_flat_violin.R")

# Save some time and stor APA format-related code in an object so you can easily
# use it in multiple plots
#windowsFonts(Times=windowsFont("TT Times New Roman")) # explicit mapping to "times"
apatheme = theme_bw()+
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
              axis.title.x = element_text(margin=margin(10,0,0,0)),  # increase the sapce betwen title and x axis
              axis.title.y = element_text(margin=margin(0,12,0,0)))  # increase the space between title and y axis


# define the d prime function
dprime <- function(hit,fa) {
  qnorm(hit) - qnorm(fa)
}


## below is the code from blog, and adapted from A C Del Re from email
d.sgpp <- function(m.1,m.2,sd.1,sd.2,n,r=.5)
{
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
CAplots <- function(saveDir = traDir, curDir = curDir,expName = 'exp7', task = 'id', inData){
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
    return(multiplot(P.rt,P.acc,cols = 2))
}
 
#### For Match task
#Mplots <- function(saveDir = traDir, curDir = curDir, expName = 'exp1', dData,rtData){
Mplots <- function(expName = 'exp1', dData,rtData){
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
          # save the d-prime plot
          #fileName = paste0('p_',expName,'_match_dprime','.pdf')
          #ggsave(fileName, P.dprime, scale = 1,height = 6, width = 7, dpi = 300, family = "Times")
          # save the RT plot
          #fileName = paste0('p_',expName,'_match_RT','.pdf')
          #ggsave(fileName, P.rt, scale = 1,height = 6, width = 7, dpi = 300, family = "Times")
          # save the both      
          #fileName = paste0('p_',expName,'_match_','.tiff')
          #setwd(saveDir)
          #tiff(fileName, width = 14, height = 6, units = 'in', res = 300)
          #p_dprime_match <- multiplot(P.rt,P.dprime,cols = 2)
          #dev.off()
          #setwd(curDir)
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
              
              # save the d-prime plot
              #fileName = paste0('p_',expName,'_match_dprime','.pdf')
              #ggsave(fileName, P.dprime, scale = 1,height = 6, width = 5, dpi = 300, family = "Times",path = saveDir)
              # save the RT plot
              #fileName = paste0('p_',expName,'_match_RT','.pdf')
              #ggsave(fileName, P.rt, scale = 1,height = 6, width = 5, dpi = 300, family = "Times",path = saveDir)
              # save the both      
              #fileName = paste0('p_',expName,'_match_','.tiff')
              #setwd(saveDir)
              #tiff(fileName, width = 12, height = 10, units = 'in', res = 300)
              #p_dprime_match <- multiplot(P.rt,P.dprime,cols = 2)
              #dev.off()
              #setwd(curDir)
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
