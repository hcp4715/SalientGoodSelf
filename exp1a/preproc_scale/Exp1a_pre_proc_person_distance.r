##### Purpose ####
# This script is for pre-processing the scale data for experiment 1a
# espeically for the psychological-distance data

# initial
#setwd("..")         # back to parent folder
source('Initial.r')

# load data
# read the file name in this folder
fpath  <- getwd()
fNames <- list.files(path = fpath, pattern = '.csv')
# fNames2 <- list.files(path = fpath, pattern = '.out')
# read file and combine them into one datafrom
#pdist <- do.call("rbind",lapply(fNames,FUN=function(files){read.table(files, header=TRUE, sep="\t")}))

# get the dimension for each file
for (file in fNames){
  dataset <- read.table(file, header=TRUE, sep= ",")
  dataset <- dataset[1:36,1:11]
  print(dim(dataset))
  print(dataset[1,])

}


# read and combine the data
for (file in fNames){
    
  # if the scoredata doesn't exist, create it
  if (!exists("scoredata")){
          scoredata <- read.table(file, header=TRUE, sep=",")
          scoredata <- scoredata[1:36,1:11]
  }
  
  # if the scoredata does exist, append to it
  else if (exists("scoredata")){
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    temp_dataset <- temp_dataset[1:36,1:11]
    scoredata<-rbind(scoredata, temp_dataset)
    rm(temp_dataset)
  }
  
}

# transfer to absolute value
scoredata$lengthDraw_r <- abs(as.numeric(scoredata$lengthDraw))

# transfer to personal distance
# % self = 1 ; good = 2 ; bad = 3; normal = 4; lover = 5;
scoredata$distLabel[(scoredata$firstTarget == 1 & scoredata$secondTarget ==2) |
                    (scoredata$firstTarget == 2 & scoredata$secondTarget == 1)] <- "SelfGood"
scoredata$distLabel[(scoredata$firstTarget == 1 & scoredata$secondTarget == 3) |
                    (scoredata$firstTarget == 3 & scoredata$secondTarget == 1)] <- "SelfBad"
scoredata$distLabel[(scoredata$firstTarget == 1 & scoredata$secondTarget == 4) |
                    (scoredata$firstTarget == 4 & scoredata$secondTarget == 1)] <- "SelfNormal"
scoredata$distLabel[(scoredata$firstTarget == 2 & scoredata$secondTarget == 3) |
                    (scoredata$firstTarget == 3 & scoredata$secondTarget == 2)] <- "GoodBad"
scoredata$distLabel[(scoredata$firstTarget == 2 & scoredata$secondTarget == 4) |
                    (scoredata$firstTarget == 4 & scoredata$secondTarget == 2)] <- "GoodNormal"
scoredata$distLabel[(scoredata$firstTarget == 4 & scoredata$secondTarget == 3) |
                    (scoredata$firstTarget == 3 & scoredata$secondTarget == 4)] <- "BadNormal"

# extract the relevant data
scoredata_r <- scoredata[,c("SubjectID", "Age", "Gender","distLabel","lengthDraw_r")]
# calculated the summary
scoredata_r.sum <- summarySEwithin(scoredata_r,measurevar = 'lengthDraw_r', withinvar = c('SubjectID','distLabel'), 
                                 idvar = 'SubjectID',na.rm = TRUE)

# from long to wide format
scoredata_r.sum_w <- dcast(scoredata_r.sum, SubjectID ~ distLabel ,value.var = "lengthDraw_r") 

# normalize the personal distance
scoredata_r.sum_w$totalDis <- rowSums(scoredata_r.sum_w[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
scoredata_r.sum_w$SelfGood_r <- scoredata_r.sum_w$SelfGood/scoredata_r.sum_w$totalDis
scoredata_r.sum_w$SelfNormal_r <- scoredata_r.sum_w$SelfNormal/scoredata_r.sum_w$totalDis
scoredata_r.sum_w$SelfBad_r <- scoredata_r.sum_w$SelfBad/scoredata_r.sum_w$totalDis
scoredata_r.sum_w$GoodBad_r <- scoredata_r.sum_w$GoodBad/scoredata_r.sum_w$totalDis
scoredata_r.sum_w$GoodNormal_r <- scoredata_r.sum_w$GoodNormal/scoredata_r.sum_w$totalDis
scoredata_r.sum_w$BadNormal_r <- scoredata_r.sum_w$BadNormal/scoredata_r.sum_w$totalDis

scoredata_r.sum_w_normalized <- scoredata_r.sum_w[,c("SubjectID","SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]
scoredata_r.sum_w_normalized$expID <- "exp1.0"
scoredata_r.sum_w_normalized$session <- 1
scoredata_r.sum_w_normalized <- scoredata_r.sum_w_normalized[,c("SubjectID","expID","session",
                                                                "SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]
# save
setwd("..")         # back to parent folder
write.csv(scoredata_r.sum_w_normalized,'Data_exp1a_personaldistance_2014.csv',row.names = F)

# read the normalized data from the first part of the data
df1a_s1 <- scoredata_r.sum_w_normalized
df1a_s1$SubjectID <- as.integer(df1a_s1$SubjectID)
df1a_s2 <- read.csv("exp1a_data_personal_distance_2017.csv",header = T, sep = ',',
                   stringsAsFactors=FALSE,na.strings=c("","NA"))
# get the distnace data
df1a_s2$SelfGood   <- (df1a_s2$PerDis9 + df1a_s2$PerDis12 + df1a_s2$PerDis20 + df1a_s2$ PerDis26)/4
df1a_s2$SelfNormal <- (df1a_s2$PerDis3 + df1a_s2$PerDis15 + df1a_s2$PerDis23 + df1a_s2$ PerDis28)/4
df1a_s2$SelfBad    <- (df1a_s2$PerDis5 + df1a_s2$PerDis10 + df1a_s2$PerDis24 + df1a_s2$ PerDis25)/4
df1a_s2$SelfOther  <- (df1a_s2$PerDis6 + df1a_s2$PerDis14 + df1a_s2$PerDis21 + df1a_s2$ PerDis29)/4

df1a_s2$GoodBad    <- (df1a_s2$PerDis2 + df1a_s2$PerDis8  + df1a_s2$PerDis17 + df1a_s2$ PerDis18)/4
df1a_s2$GoodNormal <- (df1a_s2$PerDis1 + df1a_s2$PerDis11 + df1a_s2$PerDis16 + df1a_s2$ PerDis27)/4
df1a_s2$BadNormal  <- (df1a_s2$PerDis7 + df1a_s2$PerDis13 + df1a_s2$PerDis19 + df1a_s2$ PerDis22)/4

# set the self-other distance as NA for the earlier data
df1a_s2$SelfOther[df1a_s2$SelfOther == 0] <- NA

# remove participant that don't understand the instruction
df1a_s2_v <- df1a_s2[df1a_s2$PerDis4 < 50,]

# extract the relevant data
df1a_s2_perdis <- df1a_s2_v[,c("expID","subID","SessionID", "SelfGood","SelfNormal",
                         "SelfBad","GoodBad","GoodNormal","BadNormal")]

df1a_s2_perdis$totalDis <- rowSums(df1a_s2_perdis[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
df1a_s2_perdis$SelfGood_r <- df1a_s2_perdis$SelfGood/df1a_s2_perdis$totalDis
df1a_s2_perdis$SelfNormal_r <- df1a_s2_perdis$SelfNormal/df1a_s2_perdis$totalDis
df1a_s2_perdis$SelfBad_r <- df1a_s2_perdis$SelfBad/df1a_s2_perdis$totalDis
df1a_s2_perdis$GoodBad_r <- df1a_s2_perdis$GoodBad/df1a_s2_perdis$totalDis
df1a_s2_perdis$GoodNormal_r <- df1a_s2_perdis$GoodNormal/df1a_s2_perdis$totalDis
df1a_s2_perdis$BadNormal_r <- df1a_s2_perdis$BadNormal/df1a_s2_perdis$totalDis
df1a_s2_perdis_normalized <- df1a_s2_perdis[,c("expID","subID","SessionID",
                                        "SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]
df1a_s1 <- df1a_s1[,c("expID","SubjectID","session",
                    "SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]
colnames(df1a_s1) <- c("expID","subID","session",
                      "SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")
colnames(df1a_s2_perdis_normalized) <- c("expID","subID","session",
                      "SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")
df1a_perdis <- rbind(df1a_s1,df1a_s2_perdis_normalized)

write.csv(df1a_perdis,'exp1a_personaldistance.csv',row.names = F)
