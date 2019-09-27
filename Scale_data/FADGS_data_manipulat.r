#### FADGS_data_manipulat ####
#
###Purpose-------------------------------------------------
#
# code for preprocessing the questionnaire data from the revision of a Chinese version of Free Will and Determinism Plus scale and Experiments on the Perceptual Prioritization of the Good self (FADGS)
# overview of the project: https://osf.io/t2nsw/
#
#
# Qing-Lan Liu, Fei Wang, Wenjing Yan, Kaiping Peng, Jie Sui, Chuan-Peng Hu*
# 
# 
#
###Preparing-------------------------------------------------

rm(list = ls())     # remove all variables

# get the directory of the current file and set working directory to the folder
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)

pkgTest <- function(x)
{
  if(!require(x,character.only = TRUE))
  {
    install.packages(x,dep = TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
#packages
pkgNeeded <- (c("tidyverse","xlsx","magrittr","stats"))

lapply(pkgNeeded,pkgTest)
rm('pkgNeeded') # remove the variable 'pkgNeeded';

### load the data-------------------------------------------------

df1 <- xlsx::read.xlsx("FADGS_dataset1.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                            
df2 <- xlsx::read.xlsx("FADGS_dataset2.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                            
df2A <- xlsx::read.xlsx("FADGS_re_dataset2A.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                         #retest 1 of df2
df2B <- xlsx::read.xlsx("FADGS_re_dataset2B.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                         #retest 2 of df2
df2C <- xlsx::read.xlsx("FADGS_re_dataset2C.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                         #retest 3 of df2
df2D <- xlsx::read.xlsx("FADGS_re_dataset2D.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                         #retest 4 of df2
df3 <- xlsx::read.xlsx("FADGS_dataset3.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                              
df3A <- xlsx::read.xlsx("FADGS_re_dataset3A.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                         #retest 1 of df3
df3B <- xlsx::read.xlsx("FADGS_re_dataset3B.xlsx", 'All_Data_Original', stringsAsFactors=FALSE)                         #retest 2 of df3
df4A <- xlsx::read.xlsx("FADGS_dataset4A.xlsx",'Sheet1',stringsAsFactors=FALSE,encoding = "UTF-8")                      #part 1 of df4
df4B <- xlsx::read.xlsx("FADGS_dataset4B.xlsx",'Data',stringsAsFactors=FALSE)                                           #part 2 of df4
df4C <- xlsx::read.xlsx("FADGS_dataset4C.xlsx",'Data',stringsAsFactors=FALSE)                                           #part 3 of df4
df4D <- xlsx::read.xlsx("FADGS_dataset4D.xlsx",'All_Data_Original',stringsAsFactors=FALSE)                              #part 4 of df4
df4E <- xlsx::read.xlsx("FADGS_dataset4E.xlsx",'Sheet1',stringsAsFactors=FALSE)                                         #part 5 of df4
df4F <- xlsx::read.xlsx("FADGS_dataset4F.xlsx",'Q4_All_Data',stringsAsFactors=FALSE)                                    #part 6 of df4
df4G <- xlsx::read.xlsx("FADGS_dataset4G.xlsx",'All_Data',stringsAsFactors=FALSE)                                       #part 7 of df4

### name variables -------------------------------------------------

IdNames       <- c('expID','subjID','session')
demoNames     <- c('gender','age','edu')
objSESnames   <- c('faEdu','moEdu','faOccu','moOccu')              #colnames of objective Socioeconomic status
SESNames      <- c(paste('SES',1:10,sep = ''))                     #colnames of Rosenberg Self-esteem Scale
mrlIdNames    <- c(paste('morId_',1:15, sep = ''))                 #colnames of moral identity
justSenNames  <- c(paste("justsens_",1:8,sep = ''))                #colnames of justice sensitivity
intuitNames   <- c("Intuitest_1","Intuitest_2","Intuitest_3")      #colnames of intuitive thinking test
IRINames      <- c(paste('IRI_cn_',1:14,sep = ''))                 #colnames of interpersonal reactivity index
RelSlfEstname <- c(paste('Relat_SE_',1:8, sep = ''))               #colnames of relational self-esteem scale
personDist    <- c(paste('SelfGood',1:4, sep = '_'),               #colnames of psychological distance task
                    paste('SelfNeut',1:4, sep = '_'),
                    paste('SelfBad', 1:4, sep = '_'),
                    paste('SelfStra',1:4, sep = '_'),
                    paste('GoodBad', 1:4, sep = '_'),
                    paste('GoodNeut',1:4, sep = '_'),
                    paste('NeutBad', 1:4, sep = '_'),
                    'SelfSelf')
disgNames     <- c(paste('disgust_',1:30,sep = ''))                #colnames of disgust sensitivity scale
belJustW      <- c(paste('BelievJust',1:13, sep = '_'))            #colnames of general and personal belief in just world scale
subjSESName   <- "subjSES"                                         #colnames of subjective SES
mrlSlfImgNames <- c(paste('morSlfImg',1:9,sep = '_'))              #colnames of moral self-image scale
BFINames <- c(paste('BFI_A',1:9,sep = ''),                         #colnames of Big Five Inventory
               paste('BFI_C',1:9,sep = ''),                      
               paste('BFI_N',1:8,sep = ''),
               paste('BFI_O',1:10,sep = ''),
               paste('BFI_E',1:8,sep = ''))
MLOCNames <- c(paste("MLOC",1:24,sep = ''))                        #colnames of multidemensional locus of control
FADNames  <- c("FD1", "SD2", "UP3", "FW4", "FD5", "SD6", "UP7",    #colnames of Free will and Determinism Plus
               "FW8", "FD9", "SD10", "UP11", "FW12", "FD13", 
               "SD14", "UP15", "FW16", "FD17", "SD18", "UP19", 
               "UP20","FW21", "check","SD22", "FW23", "SD24", 
               "UP25","FW26", "UP27" )
redemoNames <- c('reage','regender','reedu')
remrlSlfImgNames <- c(paste('remorSlfImg',1:9,sep = '_'))          #colnames of retest for moral self-image scale
reobjSESnames <- c('refaEdu','remoEdu','refaOccu','remoOccu')      #colnames of retest for objective Socioeconomic status
reSESNames <- c(paste('reSES',1:10,sep = ''))                      #colnames of retest for Rosenberg self-esteem scale                      
remrlIdNames  <- c(paste('remorId_',1:15, sep = ''))               #colnames of retest for moral identity             
rejustSenNames <- c(paste("rejustsens_",1:8,sep = ''))                         #colnames of retest for justice sensitivity
reintuitNames  <- c("reIntuitest_1","reIntuitest_2","reIntuitest_3")           #colnames of retest for intuitive thinking test
repersonDist   <- c(paste('reSelfGood',1:4, sep = '_'),                        #colnames of retest for psychological distance task
                  paste('reSelfNeut',1:4, sep = '_'),
                  paste('reSelfBad', 1:4, sep = '_'),
                  paste('reSelfStra',1:4, sep = '_'),
                  paste('reGoodBad', 1:4, sep = '_'),
                  paste('reGoodNeut',1:4, sep = '_'),
                  paste('reNeutBad', 1:4, sep = '_'),
                  'reSelfSelf')
redisgNames  <- c(paste('redisgust_',1:30,sep = ''))                           #colnames of retest for disgust sensitivity scale
rebelJustW   <- c(paste('reBelievJust',1:13, sep = '_'))                       #colnames of retest for general and personal belief in just world scale
resubjSESName <- "resubjSES"
reBFINames <- c(paste('reBFI_A',1:9,sep = ''),                                 #colnames of retest for Big Five Inventory 
                paste('reBFI_C',1:9,sep = ''),                      
                paste('reBFI_N',1:8,sep = ''),
                paste('reBFI_O',1:10,sep = ''),
                paste('reBFI_E',1:8,sep = ''))
reMLOCNames <- c(paste("reMLOC",1:24,sep = ''))                                #colnames of retest for multidemensional locus of control
reFADNames <- c("reFD1","reSD2","reUP3","reFW4","reFD5","reSD6",               #colnames of retest for Free will and Determinism Plus
                "reUP7","reFW8","reFD9","reSD10","reUP11","reFW12",  
                "reFD13","reSD14", "reUP15", "reFW16","reFD17",   
                "reSD18", "reUP19","reUP20","reFW21", "reSD22", 
                "reFW23","reSD24", "reUP25","reFW26", "reUP27" )

### rename colnames for datasets -------------------------------------------------

# from df1
colnames(df1)
df1 <- df1 %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df1) %in% c('email','weixin_nickname',
                                         'weixin_sex','weixin_addr',
                                         'start','finish','status')))
names(df1)[2:29] <- FADNames 
names(df1)[30:31] <- c("gender","age")
names(df1) [32:35] <- objSESnames

# from df2
colnames(df2)
df2 <- df2 %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df2) %in% c('email', 'weixin_nickname',
                                         'weixin_sex', 'weixin_addr',
                                         'start','finish','status','Q36')))     # Q36 is "it it ok for us to conact you with email ?"

names(df2)[2:29] <-FADNames 
names(df2)[30:32] <- demoNames                             
names(df2)[33:36] <- objSESnames
df2 <- transform(df2, age = as.numeric(age)) 

# from df2A
colnames(df2A)
df2A <- df2A %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df2A) %in% c('email','weixin_nickname',
                                          'weixin_sex','weixin_addr',
                                          'start','finish','status')))
                                        
names(df2A)[2:34] <- c("reSD24", "DU28", "reUP27", "reFW4", "DU29", "reFW26",                     #DU is dualism
                       "reFW23", "reUP11", "reFD13", "reFD5", "reFD1", "reSD14",
                       "DU30", "recheck", "reFW21", "reFD9", "reSD10", "DU31",
                       "reUP25", "DU32", "reUP20", "reUP7", "reFW8", "reFD17",
                       "reSD6", "reFW16", "reUP15", "reUP19", "reUP3", "reSD18", 
                       "reFW12", "reSD22", "reSD2")
names(df2A)[35:37] <- c("regender", "reedu", "reage")

# fromdf2B
colnames(df2B)
df2B <- df2B %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df2B) %in% c('email','weixin_nickname',
                                          'weixin_sex','weixin_addr',
                                          'start','finish','status'))) 
                                                                 
names(df2B)[2:34] <- c ("reUP20", "reUP3", "reSD22", "DU32", "reFD9", "reFW16",
                        "reUP19", "reFD17", "reSD18", "reUP7", "reUP25","reSD6",
                        "reSD10", "DU28", "recheck", "reFW8", "reFW23", "DU29",
                        "DU30", "reUP27", "reUP11","reFW26", "reFD1", "reSD2", 
                        "reSD24", "reSD14",  "reFD13", "DU31", "reFW12", 
                        "reFW21", "reFD5", "reUP15", "reFW4")
names(df2B)[35:37] <- c("regender", "reedu", "reage")

# from df2C
colnames(df2C)
df2C <- df2C %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df2C) %in% c('email','weixin_nickname',
                                          'weixin_sex','weixin_addr',
                                          'start','finish','status'))) 
                                              
names(df2C)[2:34] <- c ("reFD1", "reFW16", "reUP20", "recheck", "DU30", 
                        "reUP19", "reFD17", "reSD10", "reUP11", "reUP25",
                        "DU28", "reSD22", "reFD13", "reUP27", "reFW8",
                        "DU31", "reFW12", "reSD6", "reUP15", "reFD9", 
                        "reUP7", "DU32",  "reSD2", "reUP3", "reSD14",
                        "reFW26", "reFW4", "reSD18", "DU29", "reFW21",
                        "reSD24", "reFD5", "reFW23")
names(df2C)[35:37] <- c("regender", "reedu", "reage")

# from df2D
colnames(df2D)
df2D <- df2D %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df2D) %in% c('email','weixin_nickname',
                                          'weixin_sex','weixin_addr','start',
                                          'finish','status')))                             
names(df2D)[2:34] <- c ("reFD1", "DU29", "reFD9", "reUP3", "recheck", "reFW8", 
                        "reSD24","reFW26", "reSD22", "DU28", "reFW23", "DU31", 
                        "reSD18", "reFD5", "reUP19", "reUP15", "DU30", "reUP11", 
                        "reFW4","reSD10", "reUP20", "reSD14", "reFW16","reFD13", 
                        "reFW21", "reUP25", "reUP7", "reSD6", "reFD17", "reSD2",
                        "reFW12", "reUP27", "DU32")
names(df2D)[35:37] <- c("regender", "reedu", "reage")

# merge the all the retest of df2:2A,2B,2C,2D
df2.t2 <- rbind(df2A,df2B,df2C,df2D)

# from df3
colnames(df3)
df3 <- df3 %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df3) %in% c('email','weixin_nickname',
                                         'weixin_sex','weixin_addr' ,'start',
                                         'finish','status',"Q37_open" )))              #Q37_open is "where are you from if you are a internationla studetn"
                                        
names(df3)[2:29] <- c ("FD1", "SD2","UP3", "FW4","FD5","SD6",  
                       "UP7", "FW8", "FD9", "SD10", "UP11", 
                       "FW12", "FD13", "SD14", "UP15","FW16",
                       "FD17", "SD18", "UP19", "UP20","check",  
                       "FW21", "SD22", "FW23", "SD24","UP25",
                       "FW26", "UP27")
names(df3)[30:32] <- demoNames 
names(df3)[33:36] <- objSESnames
names(df3)[37]    <-  "native"

# from df3A
colnames(df3A)
df3A <- df3A %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df3A) %in% c('email','weixin_nickname',
                                          'weixin_sex','weixin_addr','start',
                                          'finish','status' ))) 
                                              
names(df3A)[2:34] <- c ("reUP20", "reUP3", "reSD22", "DU32", "reFD9", "reFW16",
                        "reUP19","reFD17", "reSD18", "reUP7", "reUP25", "reSD6",
                        "reSD10", "DU28", "reFW8","reFW23", "recheck","DU29",
                        "DU30", "reUP27", "reUP11", "reFW26", "reFD1", "reSD2",
                        "reSD24", "reSD14", "reFD13", "DU31", "reFW12", "reFW21",
                        "reFD5","reUP15", "reFW4")
                        
names(df3A)[35:37] <- c("regender", "reage", "reedu")

# from df3B
colnames(df3B)
df3B <- df3B %>%
  dplyr::filter(complete.cases(subjID)) %>%
  dplyr::select(-which(names(df3B) %in% c('email','weixin_nickname',
                                          'weixin_sex','weixin_addr',
                                          'start','finish','status'))) 
                                               
names(df3B)[2:34] <- c ("reFD1", "reFW16", "reUP20", "DU30", "reUP19", "reFD17", 
                        "reSD10", "reUP11", "reUP25", "DU28", "reSD22", "recheck", 
                        "reFD13", "reUP27", "reFW8", "DU31", "reFW12", "reSD6",
                        "reUP15", "reFD9", "reUP7", "DU32", "reSD2", "reUP3", 
                        "reSD14", "reFW26", "reFW4", "reSD18", "DU29", "reFW21", 
                        "reSD24", "reFD5", "reFW23")
names(df3B)[35:37] <- c("regender", "reage", "reedu")

# merge all the retest of df3:3A,3B
df3.t2 <- rbind(df3A, df3B)

#from df4A
colnames(df4A)
df4A <- df4A %>%
  dplyr::filter(complete.cases(expID))

IdNames     <- c('expID','subjID','session')                                 #identify subjects
names(df4A) [3:12]   <- SESNames                                             #colnames of self-esteem Scale
mrlIdNames                                                                   #colnames of moral Identity
IRINames                                                                     #colanmes of inter-personal Reaction index
RelSlfEstname                                                                #colnames of Relational self-esteem

#from df4B (no intersection with df4A)
colnames(df4B)
df4B <- df4B %>%
  dplyr::filter(complete.cases(expID)) %>%
  dplyr::select(-which(names(df4B) %in% c('date','finishTime','seq')))

#colnames(df4B)[colnames(df4B) == 'Name4B'] <- 'name4B'

#from df4C (no intersection with df4B)
colnames(df4C)
df4C <- df4C %>%
  dplyr::filter(complete.cases(expID,subjID)) %>%
  dplyr::select(-which(names(df4C) %in% c('date','subjID_old','finishtime','seq')))
disPrac      <- c('dist_prac1','dist_prac2')
#df4C <- df4C[,-which(names(df4C) %in% c('name2','date','subjID_old','finishtime','seq'))]

#from df4D
colnames(df4D)
df4D <- df4D %>%
  dplyr::filter(complete.cases(expID,subjID)) %>%
  dplyr::select(-which(names(df4D) %in% c('startTime','subjID_old','finishTime','status','seq')))

#df4D <- df4D[,-which(names(df4D) %in% c('name','startTime','subjID_old','finishTime','status','seq'))]

#from df4E
colnames(df4E)
df4E <- df4E %>%
  dplyr::filter(complete.cases(expID,subjID)) %>%
  dplyr::select(-which(names(df4E) %in% c('duration','finishTime','seq')))
  
disPrac2      <- c('prac_1_GoodBad','prac_2_SelfBad')

#from df4F
colnames(df4F)
df4F <- df4F %>%
  dplyr::filter(complete.cases(expID,subjID)) %>%
  dplyr::select(-which(names(df4F) %in% c('start','finish','seq')))

#df4F <- df4F[,-which(names(df4_F) %in% c('start','finish','name','seq'))]
FADNames_noche <- FADNames[!FADNames %in% c('check')] # not include check item in the FAD-Plus
names(df4F)[4:47] <-  BFINames 
names(df4F)[48:57] <-  SESNames
names(df4F)[58:81] <-  MLOCNames 
names(df4F)[82:108] <- FADNames_noche  

#recode the BFI,SES,MLOC,FAD-Plus
df4F <- df4F %>%                                                                # recode BFI
     mutate_at(c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6",
                 "BFI_A7","BFI_A8","BFI_A9","BFI_C1","BFI_C2","BFI_C3",
                 "BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9",
                 "BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6",
                 "BFI_N7","BFI_N8","BFI_O1","BFI_O2","BFI_O3","BFI_O4",
                 "BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10",
                 "BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6",
                 "BFI_E7","BFI_E8"),
               funs(dplyr::recode(., `3`=1,`4`=2,`5`=3,`6`=4,`7`=5)))
df4F <- df4F %>%                                                                # recode SES
    mutate_at(c("SES1","SES2","SES3","SES4","SES5","SES6","SES7","SES8",
                "SES9","SES10"),
              funs(dplyr::recode(., `3`=1,`4`=2,`5`=3,`6`=4)))

df4F <- df4F %>%                                                                # recode MLOC
  mutate_at(c("MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7","MLOC8",
              "MLOC9","MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15",
              "MLOC16","MLOC17","MLOC18","MLOC19","MLOC20","MLOC21","MLOC22",
              "MLOC23","MLOC24"),
            funs(dplyr::recode(., `3`=-1,`4`=-2,`5`=-3,`6`=1,`7`=2,`8`=3))) 
df4F <- df4F %>%                                                                #recode FAD-PLUS
  mutate_at(c("FD1","SD2","UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10",   
              "UP11","FW12","FD13","SD14","UP15","FW16","FD17","SD18","UP19", 
              "UP20","FW21","SD22","FW23","SD24","UP25","FW26","UP27"),
            funs(dplyr::recode(., `3`=1,`4`=2,`5`=3,`6`=4,`7`=5)))

#from df4G
colnames(df4G)
df4G <- df4G %>%
  dplyr::filter(complete.cases(expID,subjID)) %>%
  dplyr::select(-which(names(df4G) %in% c('startTim','finishTime','status','seq')))
# df4G <- df4G[,-which(names(df4G) %in% c('startTim','finishTime','status','subjName','seq'))]
mrlIdNames
mrlSlfImgNames <- c(paste('morSlfImg',1:9,sep = '_'))

#df1$edu <- 5                                        # "edu = 5" means college students 
#df1$session <- 1
#df1$dataset <- 1
df1 <- df1 %>%
  dplyr::mutate(edu = 5,
                session = 1,
                dataset = 1)

#df2$session <- 1  
#df2$dataset <- 2
#df2$native <- 2                                   # "native = 2" means native Chinese speaker
df2 <- df2 %>%
  dplyr::mutate(session = 1,
                dataset = 2,
                native = 2)

#df2.t2$resession <- 2 
#df2.t2$dataset <- 2
df2.t2 <- df2.t2 %>%
  dplyr::mutate(resession = 2,
                dataset = 2)

#df3$session <- 1  
#df3$dataset <- 3
df3 <- df3 %>%
  dplyr::mutate(session = 1,
                dataset = 3)

#df3.t2$resession <- 2 
#df3.t2$dataset <- 3
df3.t2 <- df3.t2 %>%
  dplyr::mutate(resession = 2,
                dataset = 3)
df4A$session <- 1                                   # add one row for session
df4A.t1 <- df4A

df4B.t1 <- df4B %>%
  dplyr::filter(session == 1)
df4B.t2 <- df4B %>%
  dplyr::filter(session == 2)

df4C$session <- 1
df4C.t1 <- df4C

df4D.t1 <- df4D %>%
  dplyr::filter(session == 1)
df4D.t2 <- df4D %>%
  dplyr::filter(session == 2)

df4E.t1 <- df4E %>%
  dplyr::filter(session == 1)
df4E.t2 <- df4E %>%
  dplyr::filter(session == 2)

df4F.t1 <- df4F %>%
  dplyr::filter(session == 1)
df4F.t2 <- df4F %>%
  dplyr::filter(session == 2)

df4G.t1 <- df4G %>%
  dplyr::filter(session == 1)
df4G.t2 <- df4G %>%
  dplyr::filter(session == 2)


# check duplicated participants
#dupsubj4A <-df4A.t1$subjID[duplicated(df4A.t1$subjID)]
#dupsubj4B <-df4B.t1$subjID[duplicated(df4B.t1$subjID)]
#dupsubj4C <-df4C.t1$subjID[duplicated(df4C.t1$subjID)]
#dupsubj4D <-df4D.t1$subjID[duplicated(df4D.t1$subjID)]
#dupsubj4E <-df4E.t1$subjID[duplicated(df4E.t1$subjID)]
#dupsubj4F <-df4F.t1$subjID[duplicated(df4F.t1$subjID)]
#dupsubj4G <-df4G.t1$subjID[duplicated(df4G.t1$subjID)]

#dupsubj4B <-df4B.t2$subjID[duplicated(df4B.t2$subjID)]
#dupsubj4D <-df4D.t2$subjID[duplicated(df4D.t2$subjID)]
#dupsubj4E <-df4E.t2$subjID[duplicated(df4E.t2$subjID)]
#dupsubj4F <-df4F.t2$subjID[duplicated(df4F.t2$subjID)]
#dupsubj4G <-df4G.t2$subjID[duplicated(df4G.t2$subjID)]

#check the subject intersection between df4A & df4B
intersect(df4A.t1$subjID,df4B.t1$subjID)     # 0, then combine all the columns
dft1.4BA <- dplyr::full_join(x = df4B.t1, y = df4A.t1, by = intersect(colnames(df4A.t1),colnames(df4B.t1)))
dft1.4BA <- dft1.4BA[order(dft1.4BA$subjID),]

#check the subject intersection between dft1.4BA & df4C.t1
intersect(dft1.4BA$subjID,df4C.t1$subjID)     # 0, then combine all the columns
dft1.4BAC <- dplyr::full_join(x = dft1.4BA, y = df4C.t1, by = intersect(colnames(dft1.4BA),colnames(df4C.t1)))
dft1.4BAC <- dft1.4BAC[order(dft1.4BAC$subjID),]

#merge df4D
length(intersect(dft1.4BAC$subjID,df4D.t1$subjID))     # 467 row,425 row, 423 row overlap, be careful here
intersect(colnames(dft1.4BAC),colnames(df4D.t1))       # only three ID columns are overlap
dft1.4DBAC <- dplyr::full_join(x = dft1.4BAC, y = df4D.t1, by = intersect(colnames(dft1.4BAC),colnames(df4D.t1)))

#merge df4E
length(intersect(dft1.4DBAC$subjID,df4E.t1$subjID))    # 469 row, 312 row, 311 row overlap

colnames(df4E.t1)[colnames(df4E.t1) == "prac_1_GoodBad"] <- 'dist_prac1'
colnames(df4E.t1)[colnames(df4E.t1) == "prac_2_SelfBad"] <- 'dist_prac2'
cmName <- intersect(colnames(dft1.4DBAC),colnames(df4E.t1))   # common names 34 (after merge: 144 + 34 - 34 = 144)
cmName <- cmName[4:34]
#dft1.4DBAC$session <- as.numeric(dft1.4DBAC$session)
dft1.4EDCBA <- dplyr::full_join(x = dft1.4DBAC, y = df4E.t1, by = IdNames)

#coalesce the column with the same column names in two dataframe
tmp1 <- dft1.4EDCBA[,paste(cmName,'x',sep = '.')]
colnames(tmp1) <- cmName
tmp2 <- dft1.4EDCBA[,paste(cmName,'y',sep = '.')]
colnames(tmp2) <- cmName

# tmp3 <- setNames(data.frame(matrix(ncol = ncol(tmp1), nrow = nrow(tmp1))), cmName)
for (i in cmName){
  dft1.4EDCBA[[i]] <- dplyr::coalesce(tmp1[[i]],tmp2[[i]])
}
# dft1.4EDCBA <- cbind(dft1.4EDCBA,tmp3)
#df4_11 <- subset(df4_11, select=-c(paste(personDist,'x',sep = '.'),paste(personDist,'y', sep = '.')))
dft1.4EDCBA <- dft1.4EDCBA %>%
  dplyr::select(-which(names(dft1.4EDCBA) %in% c(paste(cmName,'x',sep = '.'),paste(cmName,'y', sep = '.'))))

#colnames(df4DBAC)[colnames(df4DBAC) == c('seq','session','duration','finishTime','name')] <- 
#      c(paste(c('seq','session','duration','finishTime','name'),'q2',sep = '_'))

### colnames of df4E
#colnames(df4E)
#personDist

#intersect(df4DBAC$subjID,df4E$subjID)  # 310 subject
#intersect(df4DBAC$expID,df4E$expID)
#df4_11 <- dplyr::full_join(x = df4DBAC, y = df4E, by = c('expID','subjID','session'))

#dfdist <- df4DBAC[,c('expID','subjID',personDist)]

#dfdist %>% 
#      dplyr::left_join(df4E, by = c('expID','subjID')) %>% 
#      dplyr::mutate(personDist = coalesce(paste(personDist,'x',sep = '.'), paste(personDist,'y',sep = '.'))) %>% 
#      dplyr::select(-paste(personDist,'x',sep = '.'), -paste(personDist,'y',sep = '.')))

#dfdist <- dplyr::inner_join(dfdist,df4E, by = intersect(colnames(dfdist),colnames(df4E)))
#df4_11[,personDist][df4DBAC$expID ==df4E$expID & df4DBAC$subjID == df4E$subjID] <- 
#      df4E[,personDist][df4DBAC$expID ==df4E$expID & df4DBAC$subjID == df4E$subjID]

#merge df4F
length(intersect(dft1.4EDCBA$subjID,df4F.t1$subjID))     # 470 row, 377 row, 340 row overlap
intersect(colnames(dft1.4EDCBA),colnames(df4F.t1))       # three ID columns and self esteem are overlap
dft1.4FEDCBA <- dplyr::full_join(x = dft1.4EDCBA, y = df4F.t1, by = IdNames) #  144 col, 108 col, 13 overlap, after:239
tmp1 <- dft1.4FEDCBA[,paste(SESNames,'x',sep = '.')]
colnames(tmp1) <- SESNames
tmp2 <- dft1.4FEDCBA[,paste(SESNames,'y',sep = '.')]
colnames(tmp2) <- SESNames

for (i in SESNames){
  dft1.4FEDCBA[[i]] <- dplyr::coalesce(tmp1[[i]],tmp2[[i]])
}

dft1.4FEDCBA <- dft1.4FEDCBA[,-which(names(dft1.4FEDCBA) %in% c(paste(SESNames,'x',sep = '.'),paste(SESNames,'y', sep = '.')))]

#merge df4G
length(intersect(dft1.4FEDCBA$subjID,df4G.t1$subjID))     # 507 row, 230 row, 230 row overlap, after: 507
intersect(colnames(dft1.4FEDCBA),colnames(df4G.t1))       # 244 col, 28 col, 19 overlop, after:249

dft1.4GFEDCBA <- dplyr::full_join(x = dft1.4FEDCBA, y = df4G.t1, by = c('expID','subjID','session'))

tmp1 <- dft1.4GFEDCBA[,paste(mrlIdNames,'x',sep = '.')]
colnames(tmp1) <- mrlIdNames
tmp2 <- dft1.4GFEDCBA[,paste(mrlIdNames,'y',sep = '.')]
colnames(tmp2) <- mrlIdNames

for (i in mrlIdNames){
  dft1.4GFEDCBA[[i]] <- dplyr::coalesce(tmp1[[i]],tmp2[[i]])
}

dft1.4GFEDCBA <- dft1.4GFEDCBA %>%
  dplyr::select(-which(names(dft1.4GFEDCBA) %in% c(paste(mrlIdNames,'x',sep = '.'),paste(mrlIdNames,'y', sep = '.'))))

dft1.4GFEDCBA$dataset <- 4

###merge data from time 2
# merge df4B.t2, df4D.t2
length(intersect(df4B.t2$subjID,df4D.t2$subjID))     # 131 row, 115 row, 114 overlap, after: 132
intersect(colnames(df4B.t2),colnames(df4D.t2))       # only IdNames are overlapped
dft2.4BD <- dplyr::full_join(x = df4B.t2, y = df4D.t2, by = intersect(colnames(df4B.t2),colnames(df4D.t2)))
#dft2.4BD <- dft1.4BA[order(dft1.4BA$subjID),]

# merge df4E.t2
length(intersect(dft2.4BD$subjID,df4E.t2$subjID))     # 132 row, 98 row, 98 overlap, after: 132
intersect(colnames(dft2.4BD),colnames(df4E.t2))       # 66 col, 34 col, 3 overlap, after:97
dft2.4EBD <- dplyr::full_join(x = dft2.4BD, y = df4E.t2, by = intersect(colnames(dft2.4BD),colnames(df4E.t2)))

# merge df4F.t2
length(intersect(dft2.4EBD$subjID,df4F.t2$subjID))     # 132 row, 129 row, 129 overlap, after: 132
intersect(colnames(dft2.4EBD),colnames(df4F.t2))       # 97 col, 108 col, 3 overlap, after:202

dft2.4FEBD <- dplyr::full_join(x = dft2.4EBD, y = df4F.t2, by = intersect(colnames(dft2.4EBD),colnames(df4F.t2)))

# merge df4G.t2
length(intersect(dft2.4FEBD$subjID,df4G.t2$subjID))     # 132 row, 40 row, 40 overlap, after: 132
intersect(colnames(dft2.4FEBD),colnames(df4G.t2))       # 202 col, 28 col, 3 overlap, after:227
dft2.4GFEBD <- dplyr::full_join(x = dft2.4FEBD, y = df4G.t2, by = intersect(colnames(dft2.4FEBD),colnames(df4G.t2)))

dft2.4GFEBD$dataset <-4

#rename the dft2.4GFEBD
colnames(dft2.4GFEBD)
names(dft2.4GFEBD)[3]       <- "resession"
names (dft2.4GFEBD)[4:11]   <- rejustSenNames
names(dft2.4GFEBD)[12:14]   <- reintuitNames
names(dft2.4GFEBD)[15]      <- "reage"
names(dft2.4GFEBD)[16]      <- "regender"
names(dft2.4GFEBD)[17]      <- "refamIncome"
names(dft2.4GFEBD) [18]     <- "reedu"
names(dft2.4GFEBD) [19:22]  <- reobjSESnames
names(dft2.4GFEBD) [23:52]  <- redisgNames
names(dft2.4GFEBD) [53:65]  <- rebelJustW
names(dft2.4GFEBD) [66]     <- resubjSESName
names(dft2.4GFEBD) [69:97]  <- repersonDist
names(dft2.4GFEBD) [98:141] <- reBFINames
names(dft2.4GFEBD) [142:151]<- reSESNames
names(dft2.4GFEBD) [152:175]<- reMLOCNames
names(dft2.4GFEBD) [176:202]<- reFADNames
names(dft2.4GFEBD) [203:217]<- remrlIdNames
names(dft2.4GFEBD) [218]    <- "remorId_16"
names(dft2.4GFEBD) [219:227]<- remrlSlfImgNames

### reorder data frame -------------------------------------------------
# from df1
colnames(df1)
col_order <- c("dataset","session","subjID","gender","age","edu","faEdu",
               "moEdu","faOccu","moOccu","FD1","SD2","UP3","FW4","FD5","SD6",   
               "UP7","FW8","FD9","SD10","UP11","FW12","FD13","SD14", 
               "UP15","FW16","FD17","SD18","UP19","UP20","FW21","check",
               "SD22","FW23","SD24","UP25","FW26","UP27")
df1 <- df1[,col_order]

# from df2,df2.t2
colnames(df2) 
col_order <- c("dataset","session","subjID","gender","age","edu","faEdu",
               "moEdu","faOccu","moOccu","native","FD1","SD2","UP3","FW4",    
               "FD5","SD6","UP7","FW8","FD9","SD10","UP11","FW12","FD13",
               "SD14","UP15","FW16","FD17","SD18","UP19","UP20","FW21",
               "check","SD22","FW23","SD24","UP25","FW26","UP27")
df2 <- df2[,col_order]
colnames(df2.t2)
col_order <- c ("dataset","resession","subjID","regender","reage", 
                "reedu","reFD1","reSD2", "reUP3","reFW4", "reFD5",
                "reSD6","reUP7","reFW8","reFD9", "reSD10", "reUP11",
                "reFW12","reFD13","reSD14","reUP15","reFW16","reFD17",
                "reSD18","reUP19","reUP20","reFW21","recheck","reSD22",
                "reFW23","reSD24","reUP25","reFW26","reUP27","DU28",
                "DU29","DU30", "DU31","DU32")
df2.t2<-df2.t2[,col_order]

# from df3,df3.t2
colnames(df3)
col_order <- c ("dataset","session","subjID", "gender", "age", "edu",
                "faEdu","moEdu","faOccu","moOccu","native","FD1",
                "SD2", "UP3","FW4","FD5","SD6", "UP7","FW8","FD9",
                "SD10","UP11","FW12", "FD13", "SD14","UP15", "FW16",
                "FD17","SD18","UP19","UP20", "FW21","check","SD22",
                "FW23", "SD24","UP25", "FW26", "UP27")
df3 <- df3[,col_order]
colnames(df3.t2)
col_order <- c ("dataset","resession","subjID","regender", "reage", "reedu","reFD1",
                "reSD2", "reUP3","reFW4","reFD5","reSD6", "reUP7","reFW8","reFD9",
                "reSD10","reUP11","reFW12", "reFD13", "reSD14","reUP15", "reFW16",
                "reFD17","reSD18","reUP19","reUP20", "reFW21","recheck","reSD22",
                "reFW23", "reSD24","reUP25", "reFW26", "reUP27","DU28","DU29","DU30",
                "DU31","DU32")
df3.t2<-df3.t2[,col_order]

# from dft1.4GFEDCBA, dft2.4GFEBD
colnames(dft1.4GFEDCBA)
col_order <- c("dataset","expID","subjID","session","gender","age","edu",
               "faEdu","moEdu","faOccu","moOccu","famIncome","subjSES","justsens_1",
               "justsens_2","justsens_3","justsens_4","justsens_5","justsens_6",
               "justsens_7","justsens_8","Intuitest_1", "Intuitest_2","Intuitest_3",  
               "IRI_cn_1","IRI_cn_2","IRI_cn_3","IRI_cn_4","IRI_cn_5","IRI_cn_6",
               "IRI_cn_7", "IRI_cn_8","IRI_cn_9","IRI_cn_10","IRI_cn_11","IRI_cn_12",
               "IRI_cn_13","IRI_cn_14","Relat_SE_1","Relat_SE_2","Relat_SE_3", 
               "Relat_SE_4","Relat_SE_5","Relat_SE_6","Relat_SE_7","Relat_SE_8",   
               "disgust_1","disgust_2", "disgust_3", "disgust_4","disgust_5",   
               "disgust_6","disgust_7","disgust_8","disgust_9", "disgust_10",     
               "disgust_11","disgust_12","disgust_13", "disgust_14","disgust_15",    
               "disgust_16","disgust_17", "disgust_18","disgust_19", "disgust_20",   
               "disgust_21","disgust_22","disgust_23","disgust_24", "disgust_25",  
               "disgust_26","disgust_27","disgust_28","disgust_29","disgust_30",   
               "BelievJust_1","BelievJust_2","BelievJust_3","BelievJust_4","BelievJust_5",
               "BelievJust_6","BelievJust_7","BelievJust_8","BelievJust_9","BelievJust_10",
               "BelievJust_11","BelievJust_12","BelievJust_13","dist_prac1","dist_prac2",
               "GoodNeut_4","GoodBad_1","SelfNeut_1","SelfSelf","SelfBad_1","SelfStra_1",   
               "NeutBad_1","GoodBad_2","SelfGood_1","SelfBad_2","GoodNeut_1","SelfGood_2",   
               "NeutBad_2","SelfStra_2","SelfNeut_2","GoodNeut_2","GoodBad_3","GoodBad_4",    
               "NeutBad_3","SelfGood_3","SelfStra_3","NeutBad_4","SelfNeut_3","SelfBad_3",
               "SelfBad_4","SelfGood_4","GoodNeut_3","SelfNeut_4","SelfStra_4", "BFI_A1", 
               "BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9",
               "BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8",
               "BFI_C9","BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6", "BFI_N7",
               "BFI_N8","BFI_O1", "BFI_O2","BFI_O3" , "BFI_O4","BFI_O5","BFI_O6","BFI_O7",
               "BFI_O8","BFI_O9","BFI_O10","BFI_E1","BFI_E2","BFI_E3","BFI_E4", "BFI_E5",
               "BFI_E6","BFI_E7","BFI_E8","MLOC1","MLOC2","MLOC3","MLOC4","MLOC5","MLOC6","MLOC7",
               "MLOC8","MLOC9","MLOC10","MLOC11","MLOC12","MLOC13","MLOC14","MLOC15","MLOC16" ,
               "MLOC17","MLOC18","MLOC19","MLOC20","MLOC21","MLOC22","MLOC23","MLOC24", "FD1","SD2",
               "UP3","FW4","FD5","SD6","UP7","FW8","FD9","SD10","UP11","FW12", "FD13",
               "SD14","UP15","FW16","FD17","SD18","UP19","UP20","FW21","SD22","FW23","SD24",
               "UP25","FW26","UP27","SES1","SES2","SES3", "SES4","SES5","SES6","SES7","SES8",
               "SES9","SES10","morId_16","morSlfImg_1","morSlfImg_2","morSlfImg_3","morSlfImg_4",
               "morSlfImg_5","morSlfImg_6","morSlfImg_7","morSlfImg_8","morSlfImg_9","morId_1",
               "morId_2","morId_3","morId_4","morId_5","morId_6","morId_7","morId_8","morId_9",
               "morId_10","morId_11","morId_12","morId_13", "morId_14","morId_15")
dft1.4GFEDCBA <- dft1.4GFEDCBA[,col_order]

colnames(dft2.4GFEBD)
col_order <- c( "dataset", "expID", "subjID", "resession","regender", "reage","reedu","refaEdu", "remoEdu",
                "refaOccu","remoOccu","refamIncome","resubjSES","rejustsens_1","rejustsens_2","rejustsens_3",
                "rejustsens_4","rejustsens_5", "rejustsens_6","rejustsens_7", "rejustsens_8","reIntuitest_1", 
                "reIntuitest_2","reIntuitest_3", "redisgust_1", "redisgust_2","redisgust_3","redisgust_4",
                "redisgust_5","redisgust_6","redisgust_7","redisgust_8","redisgust_9","redisgust_10","redisgust_11",       
                "redisgust_12","redisgust_13","redisgust_14","redisgust_15","redisgust_16","redisgust_17","redisgust_18", 
                "redisgust_19","redisgust_20","redisgust_21","redisgust_22","redisgust_23","redisgust_24","redisgust_25",    
                "redisgust_26","redisgust_27", "redisgust_28", "redisgust_29","redisgust_30","reBelievJust_1",     
                "reBelievJust_2","reBelievJust_3","reBelievJust_4","reBelievJust_5","reBelievJust_6","reBelievJust_7",
                "reBelievJust_8","reBelievJust_9","reBelievJust_10","reBelievJust_11","reBelievJust_12","reBelievJust_13",  
                "prac_1_GoodBad","prac_2_SelfBad","reGoodNeut_1","reGoodBad_1","reSelfNeut_1","reSelfSelf","reSelfBad_1",   
                "reSelfStra_2","reNeutBad_1","reGoodBad_2","reSelfGood_1","reSelfBad_2","reGoodNeut_2","reSelfGood_2",
                "reNeutBad_2", "reSelfStra_3","reSelfNeut_2","reGoodNeut_3","reGoodBad_3","reNeutBad_3","reGoodBad_4",
                "reSelfGood_3","reSelfStra_1","reNeutBad_4","reSelfNeut_3","reSelfBad_3","reSelfBad_4","reSelfGood_4",        
                "reGoodNeut_4","reSelfNeut_4","reSelfStra_4","reBFI_A1","reBFI_A2","reBFI_A3","reBFI_A4","reBFI_A5","reBFI_A6",
                "reBFI_A7","reBFI_A8","reBFI_A9","reBFI_C1","reBFI_C2","reBFI_C3","reBFI_C4","reBFI_C5", "reBFI_C6","reBFI_C7",       
                "reBFI_C8","reBFI_C9","reBFI_N1","reBFI_N2","reBFI_N3","reBFI_N4","reBFI_N5","reBFI_N6","reBFI_N7",     
                "reBFI_N8","reBFI_O1","reBFI_O2","reBFI_O3","reBFI_O4","reBFI_O5","reBFI_O6","reBFI_O7","reBFI_O8","reBFI_O9",
                "reBFI_O10","reBFI_E1","reBFI_E2","reBFI_E3","reBFI_E4","reBFI_E5","reBFI_E6","reBFI_E7","reBFI_E8","reSES1",
                "reSES2","reSES3","reSES4","reSES5","reSES6","reSES7","reSES8","reSES9","reSES10","reMLOC1","reMLOC2","reMLOC3",
                "reMLOC4","reMLOC5","reMLOC6","reMLOC7","reMLOC8","reMLOC9","reMLOC10","reMLOC11","reMLOC12","reMLOC13","reMLOC14",
                "reMLOC15","reMLOC16","reMLOC17","reMLOC18","reMLOC19","reMLOC20","reMLOC21","reMLOC22","reMLOC23","reMLOC24",
                "reFD1","reSD2","reUP3","reFW4","reFD5","reSD6","reUP7","reFW8", "reFD9","reSD10","reUP11","reFW12","reFD13",              
                "reSD14","reUP15","reFW16","reFD17","reSD18","reUP19","reUP20", "reFW21","reSD22","reFW23","reSD24","reUP25",
                "reFW26","reUP27","remorId_1","remorId_2","remorId_3","remorId_4","remorId_5","remorId_6","remorId_7","remorId_8",
                "remorId_9","remorId_10","remorId_11","remorId_12","remorId_13","remorId_14","remorId_15","remorId_16","remorSlfImg_1",
                "remorSlfImg_2","remorSlfImg_3","remorSlfImg_4","remorSlfImg_5","remorSlfImg_6","remorSlfImg_7","remorSlfImg_8",
                "remorSlfImg_9") 
dft2.4GFEBD <- dft2.4GFEBD[,col_order]

### merge the data -------------------------------------------------
#from the df2 and df2,t2
length(intersect(df2$subjID,df2.t2$subjID))      # 597 row, 86 row, 82 overlap, after: 601
intersect(colnames(df2),colnames(df2.t2))        # 39 col, 39 col, 2 overlap, after:76

df2.total <- dplyr::full_join(x = df2, y = df2.t2, by = intersect(colnames(df2),colnames(df2.t2)))    
#4 retest can't match because these participants give information in retest that are different to their first test

#from df3 and df3.t2
length(intersect(df3$subjID,df3.t2$subjID))      # 60 row, 58 row, 45 overlap, after: 73
intersect(colnames(df3),colnames(df3.t2))        # 39 col, 39 col, 2 overlap, after:76

df3.total <- dplyr::full_join(x = df3, y = df3.t2, by = intersect(colnames(df3),colnames(df3.t2)))    
#13 retest can't match the first test

# from dft1.4GFEDCBA and dft2.4GFEBD
length(intersect(dft1.4GFEDCBA$subjID,dft2.4GFEBD$subjID))   # 507 row, 132 row, 131 overlap, after: 508
intersect(colnames(dft1.4GFEDCBA),colnames(dft2.4GFEBD))     # 250 col, 228 col, 3 overlap, after:475 

df4.total <- dplyr::full_join(x = dft1.4GFEDCBA, y = dft2.4GFEBD, by = intersect(colnames(dft1.4GFEDCBA),colnames(dft2.4GFEBD)))    
#1 retest can't match the first test

# df1 and df2.total
length(intersect(df1$dataset,df2.total$dataset))            # 68 row, 601 row, 0 overlap, after: 669
intersect(colnames(df1),colnames(df2.total))                # 38 col, 76 col, 38 overlap, after: 76

df21 <- dplyr::full_join(x = df1, y = df2.total, by = intersect(colnames(df1),colnames(df2.total)))

# df21 and df3.total
length(intersect(df21$dataset,df3.total$dataset))          # 669 row, 73 row, 0 overlap, after: 742
intersect(colnames(df21),colnames(df3.total))              # 76 col, 76 col, 76 overlap, after: 76

df321 <- dplyr::full_join(x = df21, y = df3.total, by = intersect(colnames(df21),colnames(df3.total)))

#df321 and df4.total
length(intersect(df321$dataset,df4.total$dataset))        # 742 row, 508 row, 0 overlap, after: 1250
intersect(colnames(df321),colnames(df4.total))            # 76 col, 475 col, 68 overlap, after: 483

df4321 <- dplyr::full_join(x = df321, y = df4.total, by = intersect(colnames(df321),colnames(df4.total)))

###caculate the reliability for each scale -------------------------------------------------
#from the Justice Sensitivity-Short Form (JS)
justSenNames
justSenKeys <- c(1,2,3,4,5,6,7,8)
rejustSenNames
df4321$justSens   <- rowSums(df4321[,justSenNames],na.rm = F)/length(justSenNames) # average score for time 1
df4321$rejustSens <- rowSums(df4321[,rejustSenNames],na.rm = F)/length(rejustSenNames) # average score for time 2

justSenAlpha <-  psych::alpha(df4321[,justSenNames], keys=justSenKeys)  # calculate the alpha coefficient of JS
print(justSenAlpha$total)  # print the alpha for JS
# sta alpha = .76

justSenOmega <- psych::omega(df4321[,justSenNames])  
print(c(justSenOmega$omega_h,justSenOmega$omega.tot)) 
# omega.tot = .85

stats::cor.test(df4321$justSens,df4321$rejustSens,method = "pearson",alternative = "greater") # caculate test-retest reliability
#cor = .69 [.60,1]

#from the Interpersonal Reactivity Index
df4321[, paste0("IRI_cn_",c(2,3,7,8,9))] <- 6- df4321[,paste0("IRI_cn_",c(2,3,7,8,9))]
IRINames
#IRIKeys <- c(1,-2,-3,4,5,6,-7,-8,-9,10,11,12,13,14) # original reverse coding
IRIKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)       # reverse coded as negative
df4321$IRI<- rowSums(df4321[,IRINames],na.rm = F)/length(IRINames) 

IRIAlpha <-  psych::alpha(df4321[,IRINames], keys=IRIKeys)  # calculate the alpha coefficient of IRI
print(IRIAlpha$total)  # print the alpha for IRI
# sta alpha = .73

IRIOmega <- psych::omega(df4321[,IRINames])  
print(c(IRIOmega$omega_h,IRIOmega$omega.tot)) 
# omega.tot = .84

#from the Relational Self-Esteem
RelSlfEstname
RelSlfEstKeys <- c(1,2,3,4,5,6,7,8)

df4321$RelSlfEst <- rowSums(df4321[,RelSlfEstname],na.rm = F)/length(RelSlfEstname) # average score

RelSlfEstAlpha <-  psych::alpha(df4321[,RelSlfEstname], keys=RelSlfEstKeys)  # calculate the alpha coefficient of Relational Self-Esteem
print(RelSlfEstAlpha$total)  # print the alpha for Relational Self-Esteem
# sta alpha = .77

RelSlfEstOmega <- psych::omega(df4321[,RelSlfEstname])  
print(c(RelSlfEstOmega$omega_h,RelSlfEstOmega$omega.tot)) 
# omega.tot = .87

# from the Disgust scale
df4321[, paste0("disgust_",c(2,3,5,7,8,10))] <- 5- df4321[,paste0("disgust_",c(2,3,5,7,8,10))]
df4321[, paste0("redisgust_",c(2,3,5,7,8,10))] <- 5- df4321[,paste0("redisgust_",c(2,3,5,7,8,10))]
disgNames
redisgNames
#disgKeys <- c(1,-2,-3,4,-5,6,-7,-8,9,-10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30) #original reverse coding
disgKeys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)    # reverse coded as negative

df4321$disg <- rowSums(df4321[,disgNames])/length(disgNames) # average score for time 1
df4321$redisg <- rowSums(df4321[,redisgNames])/length(redisgNames) # average score for time 2
disgAlpha <-  psych::alpha(df4321[,disgNames], keys=disgKeys)  # calculate the alpha coefficient of disgust scale
print(disgAlpha$total)  # print the alpha for disgust scale
# sta alpha = .87

disgOmega <- psych::omega(df4321[,disgNames])  
print(c(disgOmega$omega_h,disgOmega$omega.tot)) 
# omega.tot = .89

stats::cor.test(df4321$disg,df4321$redisg,method = "pearson",alternative = "greater") # caculate test-retest reliability
#cor = .87 [.82,1]

#from the Big Five Inventory
df4321[, paste0("BFI_A",c(6,7,8,9))] <- 6 - df4321[,paste0("BFI_A",c(6,7,8,9))]
df4321[, paste0("reBFI_A",c(6,7,8,9))] <- 6 - df4321[,paste0("reBFI_A",c(6,7,8,9))]
BFI_ANames <- c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9")
reBFI_ANames <- c("reBFI_A1","reBFI_A2","reBFI_A3","reBFI_A4","reBFI_A5","reBFI_A6","reBFI_A7","reBFI_A8","reBFI_A9")
#BFI_AKeys <- c(1,2,3,4,5,-6,-7,-8,-9)    #original reverse coding
BFI_AKeys <- c(1,2,3,4,5,6,7,8,9)         # reverse coded as negative
df4321$BFI_A <- rowSums(df4321[,BFI_ANames])/length(BFI_ANames) # average score for time 1
df4321$reBFI_A <- rowSums(df4321[,reBFI_ANames])/length(reBFI_ANames) # average score for time 2

BFI_AAlpha <-  psych::alpha(df4321[,BFI_ANames], keys=BFI_AKeys)  # calculate the alpha coefficient of Agreeableness
print(BFI_AAlpha$total)  # print the alpha for Agreeableness
# sta alpha = .67

BFI_AOmega <- psych::omega(df4321[,BFI_ANames])  
print(c(BFI_AOmega$omega_h,BFI_AOmega$omega.tot)) 
# omega.tot = .75

stats::cor.test(df4321$BFI_A,df4321$reBFI_A,alternative = "greater") # caculate test-retest reliability
#cor = .70 [.62,1]

df4321[, paste0("BFI_C",c(6,7,8,9))] <- 6 - df4321[,paste0("BFI_C",c(6,7,8,9))]
df4321[, paste0("reBFI_C",c(6,7,8,9))] <- 6 - df4321[,paste0("reBFI_C",c(6,7,8,9))]
BFI_CNames <- c("BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9")
reBFI_CNames <- c("reBFI_C1","reBFI_C2","reBFI_C3","reBFI_C4","reBFI_C5","reBFI_C6","reBFI_C7","reBFI_C8","reBFI_C9")
#BFI_CKeys <- c(1,2,3,4,5,-6,-7,-8,-9)    #original reverse coding
BFI_CKeys <- c(1,2,3,4,5,6,7,8,9)         # reverse coded as negative

df4321$BFI_C <- rowSums(df4321[,BFI_CNames])/length(BFI_CNames) # average score for time 1
df4321$reBFI_C <- rowSums(df4321[,reBFI_CNames])/length(reBFI_CNames) # average score for time 2
BFI_CAlpha <-  psych::alpha(df4321[,BFI_CNames], keys=BFI_CKeys)  # calculate the alpha coefficient of Conscientiousness
print(BFI_CAlpha$total)  # print the alpha for Conscientiousness
# sta alpha = .76

BFI_COmega <- psych::omega(df4321[,BFI_CNames])  
print(c(BFI_COmega$omega_h,BFI_COmega$omega.tot)) 
# omega.tot = .80

stats::cor.test(df4321$BFI_C,df4321$reBFI_C,alternative = "greater") # caculate test-retest reliability
#cor = .76 [.69,1]

df4321[, paste0("BFI_N",c(6,7,8))] <- 6 - df4321[,paste0("BFI_N",c(6,7,8))]
df4321[, paste0("reBFI_N",c(6,7,8))] <- 6 - df4321[,paste0("reBFI_N",c(6,7,8))]
BFI_NNames <- c("BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8")
reBFI_NNames <- c("reBFI_N1","reBFI_N2","reBFI_N3","reBFI_N4","reBFI_N5","reBFI_N6","reBFI_N7","reBFI_N8")
#BFI_NKeys <- c(1,2,3,4,5,-6,-7,-8)    #original reverse coding
BFI_NKeys <- c(1,2,3,4,5,6,7,8)         # reverse coded as negative

df4321$BFI_N <- rowSums(df4321[,BFI_NNames])/length(BFI_NNames)  # average score for time 1
df4321$reBFI_N <- rowSums(df4321[,reBFI_NNames])/length(reBFI_NNames)  # average score for time 2
BFI_NAlpha <-  psych::alpha(df4321[,BFI_NNames], keys=BFI_NKeys)  # calculate the alpha coefficient of Neuroticism
print(BFI_NAlpha$total)  # print the alpha for Neuroticism
# sta alpha = .82

BFI_NOmega <- psych::omega(df4321[,BFI_NNames])  
print(c(BFI_NOmega$omega_h,BFI_NOmega$omega.tot)) 
# omega.tot = .87

stats::cor.test(df4321$BFI_N,df4321$reBFI_N,alternative = "greater") #caculate test-retest reliability
#cor = .70 [.61,1]

df4321[, paste0("BFI_O",c(9,10))] <- 6 - df4321[,paste0("BFI_O",c(9,10))]
df4321[, paste0("reBFI_O",c(9,10))] <- 6 - df4321[,paste0("reBFI_O",c(9,10))]
BFI_ONames <- c("BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10")
reBFI_ONames <- c("reBFI_O1","reBFI_O2","reBFI_O3","reBFI_O4","reBFI_O5","reBFI_O6","reBFI_O7","reBFI_O8","reBFI_O9","reBFI_O10")
#BFI_OKeys <- c(1,2,3,4,5,6,7,8,9,10)    #original reverse coding
BFI_OKeys <- c(1,2,3,4,5,6,7,8,9,10)         # reverse coded as negative

df4321$BFI_O <- rowSums(df4321[,BFI_ONames],na.rm = F)/length(BFI_ONames) # average score for time 1
df4321$reBFI_O <- rowSums(df4321[,reBFI_ONames],na.rm = F)/length(reBFI_ONames) # average score for time 2
BFI_OAlpha <-  psych::alpha(df4321[,BFI_ONames], keys=BFI_OKeys)  # calculate the alpha coefficient of Openness
print(BFI_OAlpha$total)  # print the alpha for Openness
# sta alpha = .79

BFI_OOmega <- psych::omega(df4321[,BFI_ONames])  
print(c(BFI_OOmega$omega_h,BFI_OOmega$omega.tot)) 
# omega.tot = .84

stats::cor.test(df4321$BFI_O,df4321$reBFI_O,alternative = "greater") # caculate test-retest reliability
#cor = .77 [.71,1]

df4321[, paste0("BFI_E",c(6,7,8))] <- 6 - df4321[,paste0("BFI_E",c(6,7,8))]
df4321[, paste0("reBFI_E",c(6,7,8))] <- 6 - df4321[,paste0("reBFI_E",c(6,7,8))]
BFI_ENames <- c("BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")
reBFI_ENames <- c("reBFI_E1","reBFI_E2","reBFI_E3","reBFI_E4","reBFI_E5","reBFI_E6","reBFI_E7","reBFI_E8")
#BFI_OKeys <- c(1,2,3,4,5,6,7,8)    #original reverse coding
BFI_EKeys <- c(1,2,3,4,5,6,7,8)      # reverse coded as negative   

df4321$BFI_E <- rowSums(df4321[,BFI_ENames],na.rm = F)/length(BFI_ENames) # average score for time 1
df4321$reBFI_E <- rowSums(df4321[,reBFI_ENames],na.rm = F)/length(reBFI_ENames) # average score for time 2
BFI_EAlpha <-  psych::alpha(df4321[,BFI_ENames], keys=BFI_EKeys)  # calculate the alpha coefficient of Extraversion
print(BFI_EAlpha$total)  # print the alpha for Extraversion
# sta alpha = .83

BFI_EOmega <- psych::omega(df4321[,BFI_ENames])  
print(c(BFI_EOmega$omega_h,BFI_EOmega$omega.tot)) 
# omega.tot = .89

stats::cor.test(df4321$BFI_E,df4321$reBFI_E,alternative = "greater") # caculate test-retest reliability
# cor = .84[.79,1]

#from the MLOC
MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
reMLOC_INames <- c("reMLOC1","reMLOC4","reMLOC5","reMLOC9","reMLOC18","reMLOC19","reMLOC21","reMLOC23")
MLOC_IKeys <-  c(1,2,3,4,5,6,7,8)        

df4321$MLOC_I <- rowSums(df4321[,MLOC_INames])+24/length(MLOC_INames)  # average score for time 1
df4321$reMLOC_I <- rowSums(df4321[,reMLOC_INames])+24/length(reMLOC_INames)  # average score for time 2
MLOC_IAlpha <-  psych::alpha(df4321[,MLOC_INames], keys=MLOC_IKeys)  # calculate the alpha coefficient of Internal
print(MLOC_IAlpha$total)  # print the alpha for Internal
# sta alpha = .58

MLOC_IOmega <- psych::omega(df4321[,MLOC_INames])  
print(c(MLOC_IOmega$omega_h,MLOC_IOmega$omega.tot)) 
# omega.tot = .66

stats::cor.test(df4321$MLOC_I,df4321$reMLOC_I,alternative = "greater") # caculate test-retest reliability
# cor = .55 [.44,1]

MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
reMLOC_PNames <- c("reMLOC3","reMLOC8","reMLOC11","reMLOC13","reMLOC15","reMLOC17","reMLOC20","reMLOC22")
MLOC_PKeys <-  c(1,2,3,4,5,6,7,8)        

df4321$MLOC_P <- rowSums(df4321[,MLOC_PNames])+24/length(MLOC_PNames)   # average score for time 1
df4321$reMLOC_P <- rowSums(df4321[,reMLOC_PNames])+24/length(reMLOC_PNames)   # average score for time 2
MLOC_PAlpha <-  psych::alpha(df4321[,MLOC_PNames], keys=MLOC_PKeys)     # calculate the alpha coefficient of Powerful others
print(MLOC_PAlpha$total)  # print the alpha for Powerful others
# sta alpha = .68

MLOC_POmega <- psych::omega(df4321[,MLOC_PNames])  
print(c(MLOC_POmega$omega_h,MLOC_POmega$omega.tot)) 
# omega.tot = .77

stats::cor.test(df4321$MLOC_P,df4321$reMLOC_P,alternative = "greater") # caculate test-retest reliability
# cor = .71 [.63,1]

MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")
reMLOC_CNames <- c("reMLOC2","reMLOC6","reMLOC7","reMLOC10","reMLOC12","reMLOC14","reMLOC16","reMLOC24")
MLOC_CKeys <-  c(1,2,3,4,5,6,7,8)        

df4321$MLOC_C <- (rowSums(df4321[,MLOC_CNames])+24)/length(MLOC_CNames)   # average score for time 1
df4321$reMLOC_C <- (rowSums(df4321[,reMLOC_CNames])+24)/length(reMLOC_CNames)   # average score for time 2
MLOC_CAlpha <-  psych::alpha(df4321[,MLOC_CNames], keys=MLOC_CKeys)     # calculate the alpha coefficient of Chance
print(MLOC_CAlpha$total)  # print the alpha for Chance
# sta alpha = .61

MLOC_COmega <- psych::omega(df4321[,MLOC_CNames])  
print(c(MLOC_COmega$omega_h,MLOC_COmega$omega.tot)) 
# omega.tot = .68

stats::cor.test(df4321$MLOC_C,df4321$reMLOC_C,alternative = "greater") # caculate test-retest reliability
# cor = .62 [.52,1]

# from the 27-item FAD+
FDNames <- c("FD1","FD5","FD9","FD13", "FD17")
reFDNames <- c("reFD1","reFD5","reFD9","reFD13", "reFD17")
FDKeys <- c(1,2,3,4,5)

df4321$FD <- rowSums(df4321[,FDNames],na.rm = F)/length(FDNames) # average score for time 1
df4321$reFD <- rowSums(df4321[,reFDNames],na.rm = F)/length(reFDNames) # average score for time 2
FDAlpha <-  psych::alpha(df4321[,FDNames], keys=FDKeys)  # calculate the alpha coefficient of Fatalistic Determinism
print(FDAlpha$total)  # print the alpha for Fatalistic Determinism
# sta alpha = .74

FDOmega <- psych::omega(df4321[,FDNames])  
print(c(FDOmega$omega_h,FDOmega$omega.tot)) 
# omega.tot = .81

stats::cor.test(df4321$FD,df4321$reFD,alternative = "greater") # caculate test-retest reliability
# cor = .58 [50,1]

SDNames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
reSDNames <- c("reSD2","reSD6","reSD10","reSD14","reSD18","reSD22","reSD24")
SDKeys <- c(1,2,3,4,5,6,7)

df4321$SD <- rowSums(df4321[,SDNames],na.rm = F)/length(SDNames) # average score for time 1
df4321$reSD <- rowSums(df4321[,reSDNames],na.rm = F)/length(reSDNames) # average score for time 2
SDAlpha <-  psych::alpha(df4321[,SDNames], keys=SDKeys)  # calculate the alpha coefficient of Scientific Determinism
print(SDAlpha$total)  # print the alpha for Scientific Determinism
# sta alpha = .59

SDOmega <- psych::omega(df4321[,SDNames])  
print(c(SDOmega$omega_h,SDOmega$omega.tot)) 
# omega.tot = .69

stats::cor.test(df4321$SD,df4321$reSD,alternative = "greater") # caculate test-retest reliability
# cor = .54 [.47,1]

UPNames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
reUPNames <- c("reUP3","reUP7","reUP11","reUP15","reUP19","reUP20","reUP25","reUP27")
UPKeys <- c(1,2,3,4,5,6,7,8)

df4321$UP <- rowSums(df4321[,UPNames],na.rm = F)/length(UPNames) # average score for time 1
df4321$reUP <- rowSums(df4321[,reUPNames],na.rm = F)/length(reUPNames) # average score for time 2
UPAlpha <-  psych::alpha(df4321[,UPNames], keys=UPKeys)  # calculate the alpha coefficient of Unpredictability
print(UPAlpha$total)  # print the alpha for Unpredictability
# sta alpha = .70

UPOmega <- psych::omega(df4321[,UPNames])  
print(c(UPOmega$omega_h,UPOmega$omega.tot)) 
# omega.tot = .78

stats::cor.test(df4321$UP,df4321$reUP,alternative = "greater") # caculate test-retest reliability
# cor = .54 [.47,1]

FWNames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")
reFWNames <- c("reFW4","reFW8","reFW12","reFW16","reFW21","reFW23","reFW26")
FWKeys <- c(1,2,3,4,5,6,7)

df4321$FW <- rowSums(df4321[,FWNames],na.rm = F)/length(FWNames) # average score for time 1
df4321$reFW <- rowSums(df4321[,reFWNames],na.rm = F)/length(reFWNames) # average score for time 2
FWAlpha <-  psych::alpha(df4321[,FWNames], keys=FWKeys)  # calculate the alpha coefficient of Free Will
print(FWAlpha$total)  # print the alpha for Free Will
# sta alpha = .66

FWOmega <- psych::omega(df4321[,FWNames])  
print(c(FWOmega$omega_h,FWOmega$omega.tot)) 
# omega.tot = .74

stats::cor.test(df4321$FW,df4321$reFW,alternative = "greater") # caculate test-retest reliability
# cor = .58 [.50,1]

#from the Dualism/anti-reduction
DUNames <- c("DU28","DU29","DU30","DU31", "DU32")
DUKeys <- c(1,2,3,4,5)

df4321$DU <- rowSums(df4321[,DUNames],na.rm = F)/length(DUNames) # average score
DUAlpha <-  psych::alpha(df4321[,DUNames], keys=DUKeys)  # calculate the alpha coefficient of Dualism/anti-reduction
print(DUAlpha$total)  # print the alpha for Dualism/anti-reduction
# sta alpha = .52

DUOmega <- psych::omega(df4321[,DUNames])  
print(c(DUOmega$omega_h,DUOmega$omega.tot)) 
# omega.tot = .63

#from the Rosenberg Self-esteem
df4321[, paste0("SES",c(3,5,9,10))] <- 5 - df4321[,paste0("SES",c(3,5,9,10))]
df4321[, paste0("reSES",c(3,5,9,10))] <- 5 - df4321[,paste0("reSES",c(3,5,9,10))]
SESNames <- c("SES1","SES2","SES3","SES4","SES5","SES6","SES7","SES8","SES9","SES10")
reSESNames <- c("reSES1","reSES2","reSES3","reSES4","reSES5","reSES6","reSES7","reSES8","reSES9","reSES10")
# SESKeys <- c(1,2,-3,4,-5,6,7,8,-9,-10)   #original reverse coding
SESKeys <- c(1,2,3,4,5,6,7,8,9,10) # reverse coded as negative

df4321$SES <- rowSums(df4321[,SESNames],na.rm = F)/length(SESNames) # average score for time 1
df4321$reSES <- rowSums(df4321[,reSESNames],na.rm = F)/length(reSESNames) # average score for time 2
SESAlpha <-  psych::alpha(df4321[,SESNames], keys=SESKeys)  # calculate the alpha coefficient of SES
print(SESAlpha$total)  # print the alpha for SES
# sta alpha = .87

SESOmega <- psych::omega(df4321[,SESNames])  
print(c(SESOmega$omega_h,SESOmega$omega.tot)) 
# omega.tot = .90
stats::cor.test(df4321$SES,df4321$reSES,alternative = "greater") # caculate test-retest reliability
# cor = .76 [.69,1]

# for the moral identity
mrlIdNames <- c("morId_1","morId_2","morId_3","morId_4", "morId_5","morId_6",
                "morId_7","morId_8","morId_9","morId_10","morId_11","morId_12",
                "morId_13","morId_14","morId_15")
remrlIdNames <- c("remorId_1","remorId_2","remorId_3","remorId_4", "remorId_5","remorId_6",
                "remorId_7","remorId_8","remorId_9","remorId_10","remorId_11","remorId_12",
                "remorId_13","remorId_14","remorId_15")
mrlIdKeys <- c(1,2,3,4,5,6,7,8,9) 

df4321$mrlId <- rowSums(df4321[,mrlIdNames],na.rm = F)/length(mrlIdNames) # average score
df4321$remrlId <- rowSums(df4321[,remrlIdNames ],na.rm = F)/length(remrlIdNames ) # average score
mrlIdAlpha <-  psych::alpha(df4321[,mrlIdNames], keys=mrlIdKeys)  # calculate the alpha coefficient of moral identity
print(mrlIdAlpha$total)  # print the alpha for moral identity
# sta alpha = .98

mrlIdOmega <- psych::omega(df4321[,mrlIdNames])  
print(c(mrlIdOmega$omega_h,mrlIdOmega$omega.tot)) 
# omega.tot = .99

stats::cor.test(df4321$mrlId,df4321$remrlId,alternative = "greater") # caculate test-retest reliability
# cor = .34 [.08,1]

# for the moral self-image scale
mrlslfImgNames <- c("morSlfImg_1","morSlfImg_2","morSlfImg_3","morSlfImg_4",
                    "morSlfImg_5","morSlfImg_6","morSlfImg_7","morSlfImg_8","morSlfImg_9")
remrlslfImgNames <- c("remorSlfImg_1","remorSlfImg_2","remorSlfImg_3","remorSlfImg_4",
                    "remorSlfImg_5","remorSlfImg_6","remorSlfImg_7","remorSlfImg_8","remorSlfImg_9")
mrlslfImgKeys <- c(1,2,3,4,5,6,7,8,9) 

df4321$mrlslfImg <- rowSums(df4321[,mrlslfImgNames],na.rm = F)/length(mrlslfImgNames) # average score for time 1
df4321$remrlslfImg <- rowSums(df4321[,remrlslfImgNames],na.rm = F)/length(remrlslfImgNames) # average score for time 2
mrlslfImgAlpha <-  psych::alpha(df4321[,mrlslfImgNames], keys=mrlslfImgKeys)  # calculate the alpha coefficient of moral self-image scale
print(mrlslfImgAlpha$total)  # print the alpha for moral self-image scale
# sta alpha = .88

mrlslfImgOmega <- psych::omega(df4321[,mrlslfImgNames])  
print(c(mrlslfImgOmega$omega_h,mrlslfImgOmega$omega.tot)) 
# omega.tot = .91

stats::cor.test(df4321$mrlslfImg,df4321$remrlslfImg,alternative = "greater") # caculate test-retest reliability
# cor = .62 [.42,1]

# for the genaral and personal belief in just world scale
PbeljustwNames <- c("BelievJust_1","BelievJust_2","BelievJust_3","BelievJust_4","BelievJust_5",
                    "BelievJust_6","BelievJust_7")
rePbeljustwNames <- c("reBelievJust_1","reBelievJust_2","reBelievJust_3","reBelievJust_4",
                      "reBelievJust_5","reBelievJust_6","reBelievJust_7")
PbeljustwKeys <- c(1,2,3,4,5,6,7)

df4321$Pbeljustw <- rowSums(df4321[,PbeljustwNames],na.rm = F)/length(PbeljustwNames) # average score for time 1
df4321$rePbeljustw <- rowSums(df4321[,rePbeljustwNames],na.rm = F)/length(rePbeljustwNames) # average score for time 2
PbeljustwAlpha <-  psych::alpha(df4321[,PbeljustwNames], keys=PbeljustwKeys)  # calculate the alpha coefficient of the personal belief in juest world
print(PbeljustwAlpha$total)  # print the alpha 
# sta alpha = .82

PbeljustwOmega <- psych::omega(df4321[,PbeljustwNames])  
print(c(PbeljustwOmega$omega_h,PbeljustwOmega$omega.tot)) 
# omega.tot = .87

stats::cor.test(df4321$Pbeljustw,df4321$rePbeljustw,alternative = "greater") # caculate test-retest reliability
# cor = .74 [.65,1]

GbeljustwNames <- c("BelievJust_8","BelievJust_9","BelievJust_10","BelievJust_11","BelievJust_12",
                    "BelievJust_13")
reGbeljustwNames <- c("reBelievJust_8","reBelievJust_9","reBelievJust_10","reBelievJust_11",
                      "reBelievJust_12","reBelievJust_13")
GbeljustwKeys <- c(1,2,3,4,5,6)

df4321$Gbeljustw <- rowSums(df4321[,GbeljustwNames],na.rm = F)/length(GbeljustwNames) # average score for time 1
df4321$reGbeljustw <- rowSums(df4321[,reGbeljustwNames],na.rm = F)/length(reGbeljustwNames) # average score for time 2
GbeljustwAlpha <-  psych::alpha(df4321[,GbeljustwNames], keys=GbeljustwKeys)  # calculate the alpha coefficient of the genaral belief in juest world
print(GbeljustwAlpha$total)  # print the alpha 
# sta alpha = .79

GbeljustwOmega <- psych::omega(df4321[,GbeljustwNames])  
print(c(GbeljustwOmega$omega_h,GbeljustwOmega$omega.tot)) 
# omega.tot = .85

stats::cor.test(df4321$Gbeljustw,df4321$reGbeljustw,alternative = "greater") # caculate test-retest reliability
# cor = .72 [.63,1]

### save all the scales-------------------------------------------------
df1$subjID <- NULL        #delet the subjID to protect the personal informaton
df2.total$subjID <- NULL 
df3.total$subjID <- NULL
df4.total$subjID <- NULL

# sava data
write.csv(df1, 'FADGS_dataset1_clean.csv', row.names = F)                  # dataset1 without retest data
write.csv(df2.total, 'FADGS_dataset2_clean.csv', row.names = F)            # dataset2 with retest data
write.csv(df3.total, 'FADGS_dataset3_clean.csv', row.names = F)            # dataset3 with retest data
write.csv(df4.total, 'FADGS_dataset4_clean.csv', row.names = F)  








