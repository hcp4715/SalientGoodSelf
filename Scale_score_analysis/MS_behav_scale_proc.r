### This script is used for processing the scal score data

### Start and load the data
rm(list = ls())     # remove all variables

# get the directory of the current file and set working directory to the folder
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)

### load libraries
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
library("tidyverse")  # use tidyverse for data manipulation

if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")    # psych for reliability analysis

### load data
dft1 <- read.csv('MS_behav_scale_T1_all_clean.csv',header = T)
dft2 <- read.csv('MS_behav_scale_T2_all_clean.csv',header = T)


### define the scale names:
IdNames     <- c('expID','subjID','session')           # identify subjects
demoNames   <- c('age','gender','Edu')
objSESNames <- c( "famIncome","faEdu", "moEdu", "faOccu", "moOccu") # objecitve SES
subjSESName <- "subjSES"

personDist   <- c(paste('SelfGood',1:4, sep = '_'),
                  paste('SelfNeut',1:4, sep = '_'),
                  paste('SelfBad', 1:4, sep = '_'),
                  paste('SelfStra',1:4, sep = '_'),
                  paste('GoodBad', 1:4, sep = '_'),
                  paste('GoodNeut',1:4, sep = '_'),
                  paste('NeutBad', 1:4, sep = '_'),
                  'SelfSelf')
disPrac       <- c('dist_prac1','dist_prac2')

SlfEstNames   <- c(paste('SE',1:10,sep = ''))            # colnames of self-esteem
mrlIdNames    <- c(paste('morId_',1:15, sep = ''))       # colnames of moral Identity

IRINames      <- c(paste('IRI_cn_',1:14,sep = ''))       # colanmes of inter-personal Reaction index
RelSlfEstname <- c(paste('Relat_SE_',1:8, sep = ''))     # colnames of Relational self-esteem

justSenNames  <- c(paste("justsens_",1:8,sep = ''))                   # justice sensitivity
intuitNames   <- c("Intuitest_1","Intuitest_2","Intuitest_3")         # reflective 

disgNames     <- c(paste('disgust_',1:30,sep = ''))
belJustW      <- c(paste('BelievJust',1:13, sep = '_'))

BFI_Names     <- c(paste('BFI_A',1:9,sep = ''),
               paste('BFI_C',1:9,sep = ''),
               paste('BFI_N',1:8,sep = ''),
               paste('BFI_O',1:10,sep = ''),
               paste('BFI_E',1:8,sep = ''))
IPCNames      <- c(paste("IPC",1:24,sep = '_'))
FADName       <- c(paste('FAD',1:27, sep = '_'))

mrlSlfImgNames <- c(paste('morSlfImg',1:9,sep = '_'))

### reliability of questionnaires
# self esteem (n = 410)
SlfEstKey2  <- c("SE1","SE2","-SE3","SE4","-SE5","SE6","SE7","-SE8","-SE9","-SE10")
SlfEstKey1  <- c(1,2,-3,4,-5,6,7,-8,-9,-10)
SlfEstAlpha <- psych::alpha(dft1[,SlfEstNames],na.rm = T) # 0.87 95%CI[0.85 0.89]
SlfEstscore <- psych::scoreItems(SlfEstKey2,dft1[,SlfEstNames],totals = F)
# moral identity (n = 263)
mrlIDKey <- c('morID_1', 'morID_2', 'morID_3', 'morID_4', '-morID_5',
              'morID_6', 'morID_7', 'morID_8', 'morID_9', 'morID_10',
              'morID_11', 'morID_12', 'morID_13', 'morID_14', 'morID_15')
mrlIDAlpha  <- psych::alpha(dft1[,mrlIdNames],c(-5), na.rm = T)
