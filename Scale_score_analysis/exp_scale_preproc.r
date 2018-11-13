# this code is used for preprocessing the scale data from my PhD experiments

rm(list = ls())     # remove all variables

# get the directory of the current file and set working directory to the folder
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)

### load libraries
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")    # psych for reliability analysis

if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
library("tidyverse")  # use tidyverse for data manipulation

if (!require(xlsx)) {install.packages("xlsx",repos = "http://cran.us.r-project.org"); require(xlsx)}
library("xlsx")       # use xlsx for data manipulation

### load the data
df.1 <- xlsx::read.xlsx("exp7_unicode_code_colname_c_33.xlsx",'Sheet1',stringsAsFactors=FALSE)
df.2 <- xlsx::read.xlsx("Q1.1_unicode_code_colnames_c_479_201811.xlsx",'Data',stringsAsFactors=FALSE)
df.3 <- xlsx::read.xlsx("Q1_unicode_colname_c_94_201510.xlsx",'Data',stringsAsFactors=FALSE)
df.4 <- xlsx::read.xlsx("Q2_unicode_colname_c_579_201705.xlsx",'All_Data_Original',stringsAsFactors=FALSE)
df.5 <- xlsx::read.xlsx("Q3_unicode_colname_c_413_201811.xlsx",'Sheet1',stringsAsFactors=FALSE)
df.6 <- xlsx::read.xlsx("Q4_unicode_code_colnames_c_546_201807.xlsx",'Q4_All_Data',stringsAsFactors=FALSE)
df.7 <- xlsx::read.xlsx("Q5_unicode_code_colnames_c_267_201807.xlsx",'All_Data',stringsAsFactors=FALSE)

### Get All Colnames 
# from df.1 colnames(df.1)
IdNames     <- c('expID','subjID','session')           # identify subjects
SlfEstNames <- c(paste('SE',1:10,sep = ''))            # colnames of self-esteem
mrlIdNames  <- c(paste('morId_',1:15, sep = ''))       # colnames of moral Identity
IRINames    <- c(paste('IRI_cn_',1:14,sep = ''))       # colanmes of inter-personal Reaction index
RelSlfEstname <- c(paste('Relat_SE_',1:8, sep = ''))   # colnames of Relational self-esteem

# from df.2 (no intersection with df.3)
colnames(df.2)
demoNames    <- c('age','gender','Edu')
justSenNames <- c(paste("justsens_",1:8,sep = ''))                   # justice sensitivity
intuitNames  <- c("Intuitest_1","Intuitest_2","Intuitest_3")         # reflective 
objSESNames  <- c( "famIncome","faEdu", "moEdu", "faOccu", "moOccu") # objecitve SES
df.2 <- df.2[,-which(names(df.2) %in% c('name','Name2','date','finishTime','seq'))]

# colnames(df.2)[colnames(df.2) == 'Name2'] <- 'name2'

# from colnames of df.3 (no intersection with df.2)
colnames(df.3)
demoNames
justSenNames
intuitNames
personDist   <- c(paste('SelfGood',1:4, sep = '_'),
                  paste('SelfNeut',1:4, sep = '_'),
                  paste('SelfBad', 1:4, sep = '_'),
                  paste('SelfStra',1:4, sep = '_'),
                  paste('GoodBad', 1:4, sep = '_'),
                  paste('GoodNeut',1:4, sep = '_'),
                  paste('NeutBad', 1:4, sep = '_'),
                  'SelfSelf')
disPrac      <- c('dist_prac1','dist_prac2')
df.3 <- df.3[,-which(names(df.3) %in% c('name2','date','finishtime','seq'))]


# from df.4
colnames(df.4)
df.4 <- df.4[,-which(names(df.4) %in% c('name','startTime','finishTime','status','seq'))]
disgNames  <- c(paste('disgust_',1:30,sep = ''))
belJustW   <- c(paste('BelievJust',1:13, sep = '_'))
subjSESName <- "subjSES"

# from df.5
colnames(df.5)
df.5 <- df.5[,-which(names(df.5) %in% c('name','duration','finishTime','seq'))]
disPrac2      <- c('prac_1_GoodBad','prac_2_SelfBad')
personDist

# from df.6
colnames(df.6)
df.6 <- df.6[,-which(names(df.6) %in% c('start','finish','name','seq'))]
BFI_Names <- c(paste('BFI_A',1:9,sep = ''),
               paste('BFI_C',1:9,sep = ''),
               paste('BFI_N',1:8,sep = ''),
               paste('BFI_O',1:10,sep = ''),
               paste('BFI_E',1:8,sep = ''))
SlfEstNames
IPCNames <- c(paste("IPC",1:24,sep = '_'))
FADName  <- c(paste('FAD',1:27, sep = '_'))

### Get colnames of df.7
colnames(df.7)
df.7 <- df.7[,-which(names(df.7) %in% c('startTim','finishTime','status','subjName','seq'))]
mrlIdNames
mrlSlfImgNames <- c(paste('morSlfImg',1:9,sep = '_'))

### check the subject intersection between df.1 & df.2
intersect(df.1$subjID,df.2$subjID)     # 0, then combine all the columns
df.8 <- dplyr::full_join(x = df.2, y = df.1, by = intersect(colnames(df.1),colnames(df.2)))
df.8 <- df.8[order(df.8$subjID),]


### check the subject intersection between df.1 & df.2
intersect(df.8$subjID,df.3$subjID)     # 0, then combine all the columns
df.9 <- dplyr::full_join(x = df.8, y = df.3, by = intersect(colnames(df.8),colnames(df.3)))
df.9 <- df.9[order(df.9$subjID),]

### merge df.4
length(intersect(df.9$subjID,df.4$subjID))     # 383, be careful here
intersect(colnames(df.9),colnames(df.4))
df.10 <- dplyr::full_join(x = df.9, y = df.4, by = intersect(colnames(df.9),colnames(df.4)))

### merge df.5
intersect(colnames(df.10),colnames(df.5))
df.10$session <- as.numeric(df.10$session)
df.11 <- dplyr::full_join(x = df.10, y = df.5, by = IdNames)
tmp1 <- df.11[,paste(personDist,'x',sep = '.')]
colnames(tmp1) <- personDist
tmp2 <- df.11[,paste(personDist,'y',sep = '.')]
colnames(tmp2) <- personDist

tmp3 <- setNames(data.frame(matrix(ncol = ncol(tmp1), nrow = nrow(tmp1))), personDist)
for (i in personDist){
      tmp3[[i]] <- dplyr::coalesce(tmp1[[i]],tmp2[[i]])
}
df.11 <- cbind(df.11,tmp3)
#df.11 <- subset(df.11, select=-c(paste(personDist,'x',sep = '.'),paste(personDist,'y', sep = '.')))
df.11 <- df.11[,-which(names(df.11) %in% c(paste(personDist,'x',sep = '.'),paste(personDist,'y', sep = '.')))]

colnames(df.11)[colnames(df.11) == 'seq'] <- 'seq_q3'
colnames(df.11)[colnames(df.11) == 'name'] <- 'name_q3'
#colnames(df.10)[colnames(df.10) == c('seq','session','duration','finishTime','name')] <- 
#      c(paste(c('seq','session','duration','finishTime','name'),'q2',sep = '_'))

### colnames of df.5
#colnames(df.5)
#personDist

#intersect(df.10$subjID,df.5$subjID)  # 310 subject
#intersect(df.10$expID,df.5$expID)
#df.11 <- dplyr::full_join(x = df.10, y = df.5, by = c('expID','subjID','session'))

#df.dist <- df.10[,c('expID','subjID',personDist)]

#df.dist %>% 
#      dplyr::left_join(df.5, by = c('expID','subjID')) %>% 
#      dplyr::mutate(personDist = coalesce(paste(personDist,'x',sep = '.'), paste(personDist,'y',sep = '.'))) %>% 
#      dplyr::select(-paste(personDist,'x',sep = '.'), -paste(personDist,'y',sep = '.')))

#df.dist <- dplyr::inner_join(df.dist,df.5, by = intersect(colnames(df.dist),colnames(df.5)))
#df.11[,personDist][df.10$expID ==df.5$expID & df.10$subjID == df.5$subjID] <- 
#      df.5[,personDist][df.10$expID ==df.5$expID & df.10$subjID == df.5$subjID]


df.12 <- dplyr::full_join(x = df.11, y = df.6, by = c('expID','subjID','session'))

tmp1 <- df.12[,paste(SlfEstNames,'x',sep = '.')]
colnames(tmp1) <- SlfEstNames
tmp2 <- df.12[,paste(SlfEstNames,'y',sep = '.')]
colnames(tmp2) <- SlfEstNames

for (i in SlfEstNames){
      df.12[[i]] <- dplyr::coalesce(tmp1[[i]],tmp2[[i]])
}

df.12 <- df.12[,-which(names(df.12) %in% c(paste(SlfEstNames,'x',sep = '.'),paste(SlfEstNames,'y', sep = '.')))]

### Get colnames of df.7
colnames(df.7)
mrlIdNames
mrlSlfImgNames <- c(paste('morSlfImg',1:9,sep = '_'))

df.12 <- df.12[,-which(names(df.12) %in% c('seq','status','finishTime'))]
df.7 <- df.7[,-which(names(df.7) %in% c('seq','status','finishTime'))]

df.13 <- dplyr::full_join(x = df.12, y = df.7, by = c('expID','subjID','session'))

tmp1 <- df.13[,paste(mrlIdNames,'x',sep = '.')]
colnames(tmp1) <- mrlIdNames
tmp2 <- df.13[,paste(mrlIdNames,'y',sep = '.')]
colnames(tmp2) <- mrlIdNames

for (i in mrlIdNames){
      df.13[[i]] <- dplyr::coalesce(tmp1[[i]],tmp2[[i]])
}

df.13 <- df.13[,-which(names(df.13) %in% c(paste(mrlIdNames,'x',sep = '.'),paste(mrlIdNames,'y', sep = '.')))]
