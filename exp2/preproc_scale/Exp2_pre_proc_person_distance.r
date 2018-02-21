##### Purpose ####
# This script is for pre-processing the scale data for experiment 1a
# espeically for the psychological-distance data

# initial
source('Initial.r')

# load data
# set wd to sub-folder 'scale_preproc'
setwd('./scale_preproc')

# read the file name in this folder
fpath  <- getwd()
fNames <- list.files(path = fpath, pattern = '.csv')
fNames2 <- list.files(path = fpath, pattern = '.out')
# read file and combine them into one datafrom
#pdist <- do.call("rbind",lapply(fNames,FUN=function(files){read.table(files, header=TRUE, sep="\t")}))

# get the dimension for each file
for (file in fNames){
  dataset <- read.table(file, header=TRUE, sep= ",")
  dataset <- dataset[1:36,1:11]
  print(dim(dataset))
 # print(dataset[1,])

}
rm(dataset)

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

# save
write.csv(dataset_r.sum_w,'summaryData_exp2_personaldistance.csv',row.names = F)

