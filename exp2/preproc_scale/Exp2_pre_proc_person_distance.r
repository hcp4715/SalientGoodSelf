##### Purpose ####
# This script is for pre-processing the scale data for experiment a
# espeically for the psychological-distance data

# initial
source('Initial.r')

# load data
# read the file name in this folder
fpath  <- getwd()
fNames <- list.files(path = fpath, pattern = '^exp2_personalDistance_sub.*.csv')
fNames2 <- list.files(path = fpath, pattern = '^personalDistanceMoral.*.out')
# read file and combine them into one datafrom
#pdist <- do.call("rbind",lapply(fNames,FUN=function(files){read.table(files, header=TRUE, sep="\t")}))

# get the dimension for each file
# for (file in fNames){
#  dataset <- read.table(file, header=TRUE, sep= ",")
#  dataset <- dataset[1:36,1:11]
#  print(dim(dataset))
# # print(dataset[1,])
#}
# rm(dataset)

# read and combine the data
for (file in fNames){
    
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep=",")
    dataset <- dataset[1:36,1:11]
  }
  
  # if the merged dataset does exist, append to it
  else if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    temp_dataset <- temp_dataset[1:36,1:11]
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

# transfer to absolute value
dataset$lengthDraw_r <- abs(as.numeric(dataset$lengthDraw))

# transfer to personal distance
# % self = 1 ; good = 2 ; bad = 3; normal = 4; lover = 5;
dataset$distLabel[(dataset$firstTarget == 1 & dataset$secondTarget ==2) |
                    (dataset$firstTarget == 2 & dataset$secondTarget == 1)] <- "SelfGood"
dataset$distLabel[(dataset$firstTarget == 1 & dataset$secondTarget == 3) |
                    (dataset$firstTarget == 3 & dataset$secondTarget == 1)] <- "SelfBad"
dataset$distLabel[(dataset$firstTarget == 1 & dataset$secondTarget == 4) |
                    (dataset$firstTarget == 4 & dataset$secondTarget == 1)] <- "SelfNormal"
dataset$distLabel[(dataset$firstTarget == 2 & dataset$secondTarget == 3) |
                    (dataset$firstTarget == 3 & dataset$secondTarget == 2)] <- "GoodBad"
dataset$distLabel[(dataset$firstTarget == 2 & dataset$secondTarget == 4) |
                    (dataset$firstTarget == 4 & dataset$secondTarget == 2)] <- "GoodNormal"
dataset$distLabel[(dataset$firstTarget == 4 & dataset$secondTarget == 3) |
                    (dataset$firstTarget == 3 & dataset$secondTarget == 4)] <- "BadNormal"

# extract the relevant data
dataset_r <- dataset[,c("SubjectID", "Age", "Gender","distLabel","lengthDraw_r")]
# calculated the summary
dataset_r.sum <- summarySEwithin(dataset_r,measurevar = 'lengthDraw_r', withinvar = c('SubjectID','distLabel'), 
                                 idvar = 'SubjectID',na.rm = TRUE)

# from long to wide format
dataset_r.sum_w <- dcast(dataset_r.sum, SubjectID ~ distLabel ,value.var = "lengthDraw_r") 

dataset_r.sum_w$totalDis <- rowSums(dataset_r.sum_w[c("SelfGood","SelfNormal","SelfBad","GoodBad","GoodNormal","BadNormal")])
dataset_r.sum_w$SelfGood_r <- dataset_r.sum_w$SelfGood/dataset_r.sum_w$totalDis
dataset_r.sum_w$SelfNormal_r <- dataset_r.sum_w$SelfNormal/dataset_r.sum_w$totalDis
dataset_r.sum_w$SelfBad_r <- dataset_r.sum_w$SelfBad/dataset_r.sum_w$totalDis
dataset_r.sum_w$GoodBad_r <- dataset_r.sum_w$GoodBad/dataset_r.sum_w$totalDis
dataset_r.sum_w$GoodNormal_r <- dataset_r.sum_w$GoodNormal/dataset_r.sum_w$totalDis
dataset_r.sum_w$BadNormal_r <- dataset_r.sum_w$BadNormal/dataset_r.sum_w$totalDis

dataset_r.sum_w_normalized <- dataset_r.sum_w[,c("SubjectID","SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]
dataset_r.sum_w_normalized$expID <- "exp2"
dataset_r.sum_w_normalized$session <- 1
dataset_r.sum_w_normalized <- dataset_r.sum_w_normalized[,c("SubjectID","expID","session",
                                                                "SelfGood_r","SelfNormal_r","SelfBad_r","GoodBad_r","GoodNormal_r","BadNormal_r")]


# save
write.csv(dataset_r.sum_w_normalized,'exp2_personaldistance.csv',row.names = F)
write.csv(dataset_r.sum_w,'exp2_personaldistance_more.csv',row.names = F)
