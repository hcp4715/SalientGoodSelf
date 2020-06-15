
###### Script for loading and preparing data ######
rm(list = ls())

if(!"tidyverse" %in% rownames(installed.packages())) install.packages("tidyverse")
if(!"here" %in% rownames(installed.packages())) install.packages("here")
if(!"mosaic" %in% rownames(installed.packages())) install.packages("mosaic")
library(tidyverse)
library(here)

set.seed(42)
CommonColnames_d  <- c("ExpID", "Site", "Subject", "Age", "Sex", 'Domain', "Identity", "Valence", "dprime")
CommonColnames_rt <- c("ExpID", "Site", "Subject", "Age", "Sex", 'Domain', "Matchness", "Identity", "Valence", "RT", 'RT_SD')

# Exp 1a ----
# data from THU
df1a_1 <- read.csv(here::here('exp1a','rawdata_behav_exp1a_201404_2019_export.csv'), header = TRUE,
                   sep = ",", stringsAsFactors=FALSE, na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%  # using 'fileEncoding="UTF-8-BOM"' can solve the 1st column issue but make other issues
        dplyr::mutate(Site = "THU", Subject = Subject + 1000,
                      Val_lab = ifelse(Label == "好人", "Good",                      # re-code the label
                                       ifelse(Label == "常人", "Neutral", "Bad")))

# data collected in Wenzhou U
df1a_2 <- read.csv(here::here('exp1a', 'rawdata_behav_exp1a_201704_2019_export.csv'),header = TRUE,
                   sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::mutate(Site = "WZU",
                      Val_lab = ifelse(Label == "好人", "Good", 
                                       ifelse(Label == "常人", "Neutral", "Bad")))
# combine data and clean
df1a   <- rbind(df1a_1,df1a_2) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = Shape) %>%
        dplyr::mutate(Valence = ifelse(Valence == "Normal", "Neutral", Valence),   # recode values
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age), # if the min age is 0, that age is missing
                      Identity = NA,
                      Site = factor(Site),
                      Shape = ifelse(Target == "C.bmp", "circle",  # the shape of each target picture.
                                     ifelse(Target == "S.bmp", "square", 'triangle')))%>%
        dplyr::arrange(Subject) %>%
        dplyr::select(-Label, -Target)

rm(df1a_1,df1a_2)

df1a.T.basic     <- df1a %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2),
                         Age_missing = sum(is.na(Age)),
                         Sex_missing = sum(is.na(Sex)))

# number of participant who didn't finished the experiment
nQuit <- length(unique(df1a$Subject[is.na(df1a$BlockNo)])) - length(unique(df1a$Subject[!is.na(df1a$BlockNo)]))

df1a.excld.sub <-  df1a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        dplyr::summarise(N = length(ACC),                    # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df1a.invalid_trial_rate   <- df1a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df1a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT))

df1a.v   <- df1a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df1a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df1a.v.basic <- df1a.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2),
                         Age_missing = sum(is.na(Age)),
                         Sex_missing = sum(is.na(Sex)))

# calculate d prime
df1a.v.dprime_l <- df1a.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"),      # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),   # code as correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),    # code as miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),   # code as false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                      # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N, fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                     # hit rate
                      FAR  = FA/(FA+CR)) %>%                                       # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%        # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, dprime) %>%                # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")))


# get the mean RT for each particpant and each condition for ANOVA
df1a.v.rt_m <- df1a.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")))


### prepare for the later meta-analysis
df1a.meta.d <- df1a.v.dprime_l %>% 
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp1a',
                      Domain = "Morality") %>%  # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df1a.meta.rt <- df1a.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp1a',
                      Domain = "Morality") %>%  # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 1b ====
# data collected in Tsinghua U
df1b_1 <- read.csv(here::here('exp1b', 'rawdata_behav_exp1b_201410_2019_export.csv'), header = TRUE,
                   sep = ",", stringsAsFactors=FALSE, na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::mutate(Site = "THU", Subject = Subject + 1100,
                      Val_lab = ifelse(Label == "善人", "Good",                      # re-code the label
                                       ifelse(Label == "常人", "Neutral", "Bad")))

# data collected in Wenzhou U
df1b_2 <- read.csv(here::here('exp1b', 'rawdata_behav_exp1b_201705_2019_export.csv'),header = TRUE,
                   sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::mutate(Site = "WZU", Subject = Subject,
                      Val_lab = ifelse(Label == "善人", "Good",                      # re-code the label
                                       ifelse(Label == "常人", "Neutral", "Bad")))

# combine data and clean
df1b   <- rbind(df1b_1,df1b_2) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = Shape) %>%
        dplyr::mutate(Valence = ifelse(Valence == "Normal", "Neutral", Valence),   # recode values
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age),
                      Identity = NA,
                      Site = factor(Site)) %>% # if the min age is 0, that age is missing
        dplyr::arrange(Subject)

rm(df1b_1,df1b_2)

df1b.T.basic     <- df1b %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# number of participant who didn't finished the experiment
nQuit <- length(unique(df1b$Subject[is.na(df1b$BlockNo)])) - length(unique(df1b$Subject[!is.na(df1b$BlockNo)]))

# participants should be excluded
df1b.excld.sub <-  df1b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #  dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df1b.invalid_trial_rate   <- df1b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df1b.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT))

df1b.v   <- df1b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df1b.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df1b.v.basic     <- df1b.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculate d prime
df1b.v.dprime_l <- df1b.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"),     # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),  # code as correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),   # code as miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),  # code as false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                      # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                           # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                     # hit rate
                      FAR  = FA/(FA+CR)) %>%                                       # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%        # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, dprime) %>%                # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")))

# calculate the mean RT of each condition for each participants for ANOVA
df1b.v.rt_m <- df1b.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site, Subject, Age, Sex, Matchness, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df1b.meta.d <- df1b.v.dprime_l %>% 
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp1b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df1b.meta.rt <- df1b.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp1b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 1c ====
df1c <- read.csv(here::here('exp1c', 'rawdata_behav_exp1c_export2019.csv'), header = TRUE,
                 sep = ",", stringsAsFactors=FALSE, na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::mutate(Site = "THU", Subject = Subject + 1200,
                      Val_lab = ifelse(Label == "好人", "Good",                      # re-code the label
                                       ifelse(Label == "常人", "Neutral", "Bad"))) %>%
        dplyr::select(Site, Subject,Age, Handedness,Sex,TrialList1,BlockList,
                      SubTrial, Shape,Val_lab, YesNoResp,Target1.CRESP,             # select necessary columns
                      Target1.ACC,Target1.RESP, Target1.RT) %>%
        dplyr::rename(BlockNo = BlockList,
                      TrialNo = SubTrial,
                      ACC = Target1.ACC,                         # rename columns
                      CRESP = Target1.CRESP,
                      RT = Target1.RT, RESP =Target1.RESP,
                      Matchness = YesNoResp, Valence = Shape) %>%
        dplyr::mutate(Valence = ifelse(Valence == "Normal", "Neutral", Valence),    # change value
                      Valence = factor(Valence, levels=c("Good", "Neutral","Bad")),
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Matchness = factor(Matchness, levels=c("Match", "Mismatch"))) %>%
        dplyr::arrange(Subject)

# distinguish between practice and formal data
df1c.subj_P <- df1c %>%                      # subjet for practice
        dplyr::filter(is.na(BlockNo)) %>%
        dplyr::distinct(Subject)

df1c.subj_T <- df1c %>%                       # subjects in formal exp
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::distinct(Subject)

# number of participant who didn't finished the experiment
nQuit <- length(df1c.subj_P) - length(df1c.subj_T)

df1c.T.basic     <- df1c %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# participants should be excluded
df1c.excld.sub <-  df1c %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df1c.invalid_trial_rate   <- df1c %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df1c.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT))

df1c.v   <- df1c %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df1c.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df1c.v.basic     <- df1c.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculate d prime
df1c.v.dprime_l <- df1c.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"),     # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),  # code as correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),   # code as miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),  # code as false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                      # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                           # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                     # hit rate
                      FAR  = FA/(FA+CR)) %>%                                       # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%        # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, dprime) %>%                # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")))

# calculate mean RT for anova
df1c.v.rt_m <- df1c.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site, Subject, Age, Sex, Matchness, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df1c.meta.d <- df1c.v.dprime_l %>% 
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp1c',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df1c.meta.rt <- df1c.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp1c',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 2 ----
# data collected in Tsinghua U
df2 <- read.csv(here::here('exp2', 'rawdata_behav_exp2_201405_2019_export.csv'), header = TRUE,
                sep = ",", stringsAsFactors=FALSE, na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = Shape) %>%
        dplyr::mutate(Site = "THU", 
                      Subject = ifelse(Subject < 40, Subject + 1000, Subject + 2000), # recode subject ID, some from exp 1a
                      Val_lab = ifelse(Label == "好人", "Good",                       # re-code the label
                                       ifelse(Label == "常人", "Neutral", "Bad")),
                      Valence = ifelse(Valence == "Normal", "Neutral", Valence),      # recode values
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age))  

df2.T.basic     <- df2 %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# number of participant who practiced but not in the formal experiment
nQuit <- length(unique(df2$Subject[is.na(df2$BlockNo)])) - length(unique(df2$Subject[!is.na(df2$BlockNo)]))

# participants should be excluded
df2.excld.sub <-  df2 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df2.invalid_trial_rate   <- df2 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df2.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT))

df2.v   <- df2 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df2.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df2.v.basic     <- df2.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculate d prime
df2.v.dprime_l <- df2.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"),     # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),  # code as correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),   # code as miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),  # code as false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                      # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                           # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                     # hit rate
                      FAR  = FA/(FA+CR)) %>%                                       # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%        # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, dprime) %>%                # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")))

# calculated means RT for ANOVA
df2.v.rt_m <- df2.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site, Subject, Age, Sex, Matchness, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df2.meta.d <- df2.v.dprime_l %>% 
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp2',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df2.meta.rt <- df2.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(Identity = NA,
                      ExpID = 'Exp2',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 3a ----
# data collected in Tsinghua U
df3a <- read.csv(here::here('exp3a', 'rawdata_behav_exp3a_2014_export_2019.csv'), header = TRUE,
                 sep = ",", stringsAsFactors=FALSE, na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = morality,
                      Identity = self) %>%
        # in this experiment we need to get the value of valence and identity from shape
        dplyr::mutate(Valence = ifelse(Shape == "Goodself" | Shape == "Goodother", "Good", 
                                       ifelse(Shape == "Normalself" | Shape == "Normalother","Neutral", "Bad")),
                      Identity = ifelse(Shape == "Goodself" | Shape == "Normalself" | Shape == "Badself", 
                                        "Self", "Other"),
                      Val_lab = ifelse(Label == "好人" | Label == "好我", "Good",                       # re-code the label
                                       ifelse(Label == "凡人"| Label == "凡我", "Neutral", "Bad")),
                      ID_lab = ifelse(Label == "好我" | Label == "凡我" | Label == "坏我", "Self", "Other"),  # re-code the label
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age),
                      Site = "THU",
                      Subject = Subject + 3000)                                       # re-code the subject id

df3a.T.basic     <- df3a %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# number of participant who practiced but not in the formal experiment
df3a.nQuit <- length(unique(df3a$Subject[is.na(df3a$BlockNo)])) - length(unique(df3a$Subject[!is.na(df3a$BlockNo)]))

# participants should be excluded
df3a.excld.sub <-  df3a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        #dplyr::filter(!(ACC == 1 & RT <= 200)) %>%
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df3a.invalid_trial_rate   <- df3a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df3a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df3a.v   <- df3a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df3a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df3a.v.basic     <- df3a.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculate d prime
df3a.v.dprime_l <- df3a.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                    # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%     # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

# calculate the mean RT for each condition of each participant
df3a.v.rt_m <- df3a.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df3a.meta.d <- df3a.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp3a',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df3a.meta.rt <- df3a.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp3a',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 3b ----
# data collected in Wenzhou U
df3b <- read.csv(here::here('exp3b', 'rawdata_behav_exp3b_201704_export_2019.csv'), header = TRUE,
                 sep = ",", stringsAsFactors=FALSE, na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = Morality) %>%
        
        # in this experiment we need to get the value of valence and identity from shape
        dplyr::mutate(#Valence = ifelse(Shape == "Goodself" | Shape == "Goodother", "Good", 
                #                 ifelse(Shape == "Neutralself" | Shape == "NeutralOther","Neutral", "Bad")),
                Identity = ifelse(Identity == "self" | Identity == "Self", 
                                  "Self", "Other"),
                Val_lab = ifelse(Label == "好他" | Label == "好我" | Label == "好她" , "Good",         # re-code the label
                                 ifelse(Label == "常他"| Label == "常我" | Label == "常她", "Neutral", "Bad")),
                ID_lab = ifelse(Label == "好我" | Label == "常我" | Label == "坏我", "Self", "Other"),  # re-code the label
                Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                Age = ifelse(Age == 0, NA, Age),  # if the min age is 0, that age is missing
                BlockNo = dplyr::coalesce(otherBlocklList.Sample, selfBlockList.Sample),
                TrialNo = dplyr::coalesce(OtherTrialist.Sample, selfTrialList.Sample),
                Site = "WZU") 

df3b.T.basic     <- df3b %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# participants should be excluded
df3b.excld.sub <-  df3b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        #dplyr::filter(!(ACC == 1 & RT <= 200)) %>%
        dplyr::summarise(N = length(ACC),                   # calculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exclude the participants with less than 60% overall accuracy
        dplyr::select(Subject)

df3b.excld.sub2 <- df3b.excld.sub
df3b.excld.sub2[5,1] <- 31003 # the participant whose hit rate is zero under one condition.

# The rate of excluded trials in valid data
df3b.invalid_trial_rate   <- df3b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df3b.excld.sub2$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df3b.v   <- df3b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df3b.excld.sub2$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df3b.v.basic     <- df3b.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculate d prime
df3b.v.dprime_l <- df3b.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                    # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%   # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

# calculate mean RT for each condition of each participant
df3b.v.rt_m <- df3b.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site, Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df3b.meta.d <- df3b.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp3b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df3b.meta.rt <- df3b.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp3b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 4a ----

df4a_1 <- read.csv(here::here('exp4a', 'rawdata_behav_exp4a_2015_export_2019.csv'),header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(Morality = morality,
                      Identity = self) %>%
        dplyr::mutate(Site = "THU") %>%
        dplyr::mutate(Subject = Subject + 4100)  

df4a_2 <- read.csv(here::here('exp4a', 'rawdata_behav_exp4a_2017_export_2019.csv'),header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::mutate(Site = "WZU")

df4a <- rbind(df4a_1,df4a_2) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = Morality) %>%
        dplyr::mutate(Valence  = ifelse(Valence == "Normal", "Neutral", Valence),
                      Identity = ifelse(Identity == "self" | Identity == "Self", "Self", "Other"),
                      ID_lab = ifelse(Label == "自己", 'Self', 'Other'),
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age))
#Subject = factor(Subject))

rm(df4a_1,df4a_2) # remove the temporary variables.

df4a.T.basic     <- df4a %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# number of participant who practiced but not in the formal experiment
nQuit <- length(unique(df4a$Subject[is.na(df4a$BlockNo)])) - length(unique(df4a$Subject[!is.na(df4a$BlockNo)]))

# participants should be excluded
df4a.excld.sub <-  df4a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        #dplyr::filter(!(ACC == 1 & RT <= 200)) %>%
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df4a.invalid_trial_rate   <- df4a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df4a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df4a.v   <- df4a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df4a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df4a.v.basic     <- df4a.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculating the dprime 
df4a.v.dprime_l <- df4a.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                    # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%   # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

df4a.v.rt_m <- df4a.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df4a.meta.d <- df4a.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp4a',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df4a.meta.rt <- df4a.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp4a',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 4b ----

df4b_1 <- read.csv(here::here('exp4b', 'rawdata_behav_exp4b_2015_export_2019.csv'),header = TRUE, 
                   sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(Morality = morality) %>%
        dplyr::mutate(Site = "THU")

df4b_2 <- read.csv(here::here('exp4b', 'rawdata_behav_exp4b_2017_export_2019.csv'),header = TRUE, 
                   sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(Morality = morality) %>%
        dplyr::mutate(Site = "WZU")

df4b <- rbind(df4b_1,df4b_2) %>%
        dplyr::rename(ACC = Target.ACC,           # rename columns
                      RT  = Target.RT,
                      CRESP = Target.CRESP,
                      BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      RESP = Target.RESP,
                      Matchness = YesNoResp,
                      Valence = Morality) %>%
        dplyr::mutate(Valence  = ifelse(Valence == "Ord", "Neutral", Valence),
                      Val_sh = ifelse(Label == "好人", "Good",
                                      ifelse(Label == "常人",'Neutral', 'Bad')),
                      Identity = ifelse(Identity == "self" | Identity == "Self", "Self", "Other"),
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age))                             # if the min age is 0, that age is missing
#ExpID = 'Exp4b', Domain = "Morality") %>%
#  dplyr::select(CommonColnames)

rm(df4b_1,df4b_2) # remove the temporary variables.

df4b.T.basic     <- df4b %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# number of participant who practiced but not in the formal experiment
nQuit <- length(unique(df4b$Subject[is.na(df4b$BlockNo)])) - length(unique(df4b$Subject[!is.na(df4b$BlockNo)]))

# participants should be excluded
df4b.excld.sub <-  df4b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df4b.invalid_trial_rate   <- df4b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df4b.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df4b.v   <- df4b %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df4b.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df4b.v.basic     <- df4b.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculating the dprime 
df4b.v.dprime_l <- df4b.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                    # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%   # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

df4b.v.rt_m <- df4b.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df4b.meta.d <- df4b.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp4b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df4b.meta.rt <- df4b.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp4b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 5 ----
#df5 <- xlsx::read.xlsx(here::here('exp5_specificity', 'rawdata_behav_exp5_2016_export2019.xlsx'), 1)
df5 <- read.csv(here::here('exp5_specificity', 'rawdata_behav_exp5_2016_export2019_2.csv'), header = TRUE, 
                sep = ",",
                stringsAsFactors=FALSE, na.strings=c(""), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(BlockListM.Sample = BlockListMoral.Sample, 
                      Matchness = YesNoResp, CRESP = CorrectAnswer) %>%                            # rename the columns
        dplyr::mutate(Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      taskType = mosaic::derivedFactor("Emotion"  = (Label == "sad" | Label == "happy" | Label == "neutral"),
                                               "Morality" = (Label == "bad" | Label == "good" | Label == "ordinary"),
                                               "Person"   = (Label == "uglyP" | Label == "beautyP" | Label == "normalP"),
                                               "Scene"    = (Label == "uglyS" | Label == "beautyS" | Label == "normalS"), 
                                               .method ="first", .default = NA),
                      Valence = mosaic::derivedFactor("Good"= (Label ==  "good" | Label == "happy" | Label == "beautyP"  | Label == "beautyS"),
                                              "Bad" = (Label == "bad"  | Label == "sad"   | Label == "uglyP"    | Label == "uglyS"),
                                              "Neutral" = (Label == "ordinary" | Label == "neutral" | Label == "normalP" | Label == "normalS"),
                                              .method ="first", .default = NA)) %>%
        tidyr::replace_na(list(PracListE="",    PracListM="",    PracListP="",    PracListS="")) %>%          # replace NA with "" for later unite
        tidyr::unite("PracList", PracListE,PracListM,PracListP,PracListS, sep = "")  %>%                # unite all praclist
        dplyr::mutate_at(c('TargetE.ACC',  'TargetM.ACC',  'TargetP.ACC',  'TargetS.ACC'), as.integer) %>%
        dplyr::mutate(Site = "THU", 
                      ExpID = "Exp5",
                      RESP = dplyr::coalesce(TargetE.RESP,  TargetM.RESP,  TargetP.RESP,  TargetS.RESP),
                      ACC = dplyr::coalesce(TargetE.ACC,  TargetM.ACC,  TargetP.ACC,  TargetS.ACC), 
                      RT = dplyr::coalesce(TargetE.RT,  TargetM.RT,  TargetP.RT,  TargetS.RT),
                      BlockNo = dplyr::coalesce(BlockListE.Sample,BlockListM.Sample,BlockListP.Sample,BlockListS.Sample),
                      TrialNo = dplyr::coalesce(TrialListE.Sample,TrialListM.Sample,TrialListP.Sample,TrialListS.Sample)) %>% 
        #BlockNo = as.numeric(BlockNo),
        #TrialNo = as.numeric(TrialNo)) %>% 
        dplyr::mutate_if(is_character, list(~na_if(.,""))) %>%    # blank to NA
        dplyr::select(-c(TargetE.RESP,  TargetM.RESP, TargetP.RESP,  TargetS.RESP,
                         TargetE.ACC,  TargetM.ACC,  TargetP.ACC,  TargetS.ACC,
                         TargetE.RT,  TargetM.RT,  TargetP.RT,  TargetS.RT,
                         BlockListE.Sample,BlockListM.Sample,BlockListP.Sample,BlockListS.Sample,
                         TrialListE.Sample,TrialListM.Sample,TrialListP.Sample,TrialListS.Sample))

df5.T.basic     <- df5 %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# find the participants who practiced but not finish experiment
subjPrac <- df5 %>% dplyr::filter(is.na(df5$BlockNo)) %>% dplyr::distinct(Subject)
subjFinish <- df5 %>% dplyr::filter(!is.na(df5$BlockNo)) %>% dplyr::distinct(Subject)
df5.nQuit <- subjPrac$Subject[which(!subjPrac$Subject %in% subjFinish$Subject)] 

# participants should be excluded
df5.excld.sub <-  df5 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df5.invalid_trial_rate   <- df5 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% subjQuit)) %>%                 # exclude the invalid subjects
        dplyr::filter(!(Subject %in% df5.excld.sub$Subject)) %>%    # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df5.v   <- df5 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df5.nQuit)) %>%                 # exclude the invalid subjects
        dplyr::filter(!(Subject %in% df5.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1)) %>%                      # exclude < 200 trials
        dplyr::arrange(Subject)

df5.v.basic     <- df5.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculating the dprime 
df5.v.dprime_l <- df5.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, taskType, Subject, Age, Sex, Valence,sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                    # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, taskType,Valence, dprime) %>%   # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      taskType = factor(taskType, levels = c('Morality', 'Emotion',"Person", "Scene")))
# anova for RT with 2*2 design
df5.v.rt_m <- df5.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness,taskType,Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      taskType = factor(taskType, levels = c('Morality', 'Emotion',"Person", "Scene")))

### prepare for the later meta-analysis
df5.meta.d <- df5.v.dprime_l %>% 
        dplyr::rename(Domain = taskType) %>%
        dplyr::mutate(ExpID = 'Exp5',
                      Identity = NA) %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df5.meta.rt <- df5.v.rt_m %>% 
        dplyr::rename(Domain = taskType,
                      RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp5',
                      Identity = NA) %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 6a ----
df6a <- read.csv(here::here('exp6a_erp1', 'rawdata_ERP_exp6a_201412_export_2019.csv'),header = TRUE, sep = ",",
                 stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        dplyr::rename(BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      Matchness = YesNoResp, 
                      Valence = Shape,
                      ACC = Target.ACC, 
                      CRESP = CorrectAnswer,                   # rename the columns
                      RESP = Target.RESP, RT = Target.RT) %>%    #
        dplyr::mutate(Valence = ifelse(Valence == "Normal", "Neutral", Valence),                   # re-code the data
                      Val_sh = ifelse(Label == "好人", 'Good',
                                      ifelse(Label == '常人', 'Neutral', 'Bad')),
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age)) %>%
        dplyr::mutate(Site = "THU",
                      Subject = Subject + 6100) # here started with 61XX instead of 60XX, which is the id for anther project.

# number of participant who practiced but not in the formal experiment
nQuit <- length(unique(df6a$Subject[is.na(df6a$BlockNo)])) - length(unique(df6a$Subject[!is.na(df6a$BlockNo)]))

df6a.T.basic     <- df6a %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# participants should be excluded
df6a.excld.sub <-  df6a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df6a.invalid_trial_rate   <- df6a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df6a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df6a.v   <- df6a %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df6a.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df6a.v.basic     <- df6a.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculating the dprime 
df6a.v.dprime_l <- df6a.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                    # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, dprime) %>%   # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")))

# Mean RT for each condition of each participant.
df6a.v.rt_m <- df6a.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df6a.meta.d <- df6a.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp6a',
                      Domain = "Morality",
                      Identity = NA) %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df6a.meta.rt <- df6a.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp6a',
                      Domain = "Morality",
                      Identity = NA) %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 6b ----
df6b_d1 <- read.csv(here::here('exp6b_erp2', 'rawdata_erp_exp6b_d1_2016_export_2019.csv'),header = TRUE, sep = ",",
                    stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%                                                # select only form exp
        dplyr::rename(BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      Matchness = YesNoResp,
                      Identity = identity,
                      Valence = morality,
                      CRESP = CorrectAnswer) %>%                   # rename the columns
        dplyr::mutate(RT = dplyr::coalesce(Target.RT, Targetprac.RT),
                      ACC = dplyr::coalesce(Target.ACC, Targetprac.ACC),
                      RESP = dplyr::coalesce(Target.RESP,Targetprac.RESP),
                      Identity = ifelse(Identity == "self" | Identity == 'Self', "Self", 'Other'),     # re-code the data
                      Valence = mosaic::derivedFactor("Bad" = (Valence == "bad" | Valence == "immoral"), 
                                              "Good" = (Valence == "good" | Valence == 'moral'), 
                                              "Neutral" = (Valence == "normal"), 
                                              .method ="first", .default = NA),
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age),
                      Val_lab = mosaic::derivedFactor("Bad" = (Label == "BadSelf.bmp" | Label == "BadOther.bmp"), 
                                              "Good" = (Label == "GoodSelf.bmp" | Label == 'GoodOther.bmp'), 
                                              "Neutral" = (Label == "NormalSelf.bmp" | Label == 'NormalOther.bmp'), 
                                              .method ="first", .default = NA),
                      ID_lab = ifelse(Label == "BadSelf.bmp" | Label == "GoodSelf.bmp" | Label == "NormalSelf.bmp", 
                                      "Self", 'Other'),
                      Site = "THU")

df6b_d2 <- read.csv(here::here('exp6b_erp2', 'rawdata_erp_exp6b_d2_2016_export_2019.csv'),header = TRUE, sep = ",",
                    stringsAsFactors=FALSE,na.strings=c("","NA"), encoding="UTF-8") %>%
        dplyr::rename(Subject = 1) %>%
        #dplyr::filter(!is.na(BlockList.Sample)) %>%                                                   # select only form exp
        dplyr::rename(BlockNo = BlockList.Sample,
                      TrialNo = SubTrial,
                      Matchness = YesNoResp,
                      Identity = identity,
                      Valence = morality,
                      CRESP = CorrectAnswer) %>%    #
        dplyr::mutate(RT = dplyr::coalesce(Target.RT, Targetprac.RT),
                      ACC = dplyr::coalesce(Target.ACC, Targetprac.ACC),
                      RESP = dplyr::coalesce(Target.RESP,Targetprac.RESP),
                      Identity = ifelse(Identity == "self" | Identity == 'Self', "Self", 'Other'),     # re-code the data
                      Valence = mosaic::derivedFactor("Bad" = (Valence == "bad"), 
                                              "Good" = (Valence == "good"), 
                                              "Neutral" = (Valence == "normal"), 
                                              .method ="first", .default = NA),
                      Matchness = ifelse(Matchness == "Yes", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age),
                      Val_lab = mosaic::derivedFactor("Bad" = (Label == "BadSelf.bmp" | Label == "BadOther.bmp"), 
                                              "Good" = (Label == "GoodSelf.bmp" | Label == 'GoodOther.bmp'), 
                                              "Neutral" = (Label == "NormalSelf.bmp" | Label == 'NormalOther.bmp'), 
                                              .method ="first", .default = NA),
                      ID_lab = ifelse(Label == "BadSelf.bmp" | Label == "GoodSelf.bmp" | Label == "NormalSelf.bmp", 
                                      "Self", 'Other'),
                      Site = "THU")

# number of participant who practiced but not in the formal experiment
nQuit <- length(unique(df6b_d1$Subject[is.na(df6b_d1$BlockNo)])) - length(unique(df6b_d1$Subject[!is.na(df6b_d1$BlockNo)]))

df6b_d1.T.basic     <- df6b_d1 %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

df6b_d2.T.basic     <- df6b_d2 %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# participants should be excluded
df6b_d1.excld.sub <-  df6b_d1 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)

# The rate of excluded trials in valid data
df6b_d1.invalid_trial_rate   <- df6b_d1 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df6b_d1.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::summarize(rate = length(RT[RT <= 200 & ACC == 1])/length(RT)) %>%
        dplyr::pull()

df6b_d1.v   <- df6b_d1 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df6b_d1.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df6b_d1.v.basic     <- df6b_d1.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

df6b_d2.excld.sub <-  df6b_d2 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::group_by(Subject) %>%
        #dplyr::mutate(ACC = ifelse(RT <= 200, 0, ACC)) %>%  # set less than 200 ms response as wrong
        dplyr::summarise(N = length(ACC),                   # caculate the overall accuracy for each subject
                         N_crrct = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::filter(ACC < 0.6) %>%                        # exlucde the participants with less than 60% overall accuracy
        dplyr::select(Subject)


df6b_d2.v   <- df6b_d2 %>%
        dplyr::filter(!is.na(BlockNo)) %>%
        dplyr::filter(!(Subject %in% df6b_d2.excld.sub$Subject)) %>%   # exclude the invalid subjects
        dplyr::filter(!(RT <= 200 & ACC == 1))                      # exclude < 200 trials

df6b_d2.v.basic     <- df6b_d2.v %>%
        dplyr::select(Site, Subject, Age, Sex) %>%
        dplyr::distinct(Subject, .keep_all = TRUE) %>%
        dplyr::summarise(N = length(Subject),
                         N_thu = length(Site[Site == "THU"]),
                         N_wzu = length(Site[Site == "WZU"]),
                         Nf = length(Sex[Sex == "female"]),
                         Nm = length(Sex[Sex == "male"]),
                         Age_mean = round(mean(Age,na.rm=TRUE),2),
                         Age_sd = round(sd(Age,na.rm=TRUE),2))

# calculate d prime
df6b_d1.v.dprime_l <- df6b_d1.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                     # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%     # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Neutral","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

#  prepare the mean RT for each participant
df6b_d1.v.rt_m <- df6b_d1.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT),
                         RT_SD = sd(RT),
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df6b.meta.d <- df6b_d1.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp6b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df6b.meta.rt <- df6b_d1.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp6b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 7a matching task ----
## load data and clean data of exp 7a (copy the code from Hu et al., 2020, collabra) 
df7a_m <- read.csv(here::here('exp7', 'rawdata_behav_exp7a_2016.csv'),header = TRUE, sep = ",",
                   stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
        #dplyr::filter(!is.na(BlockList.Sample)) %>%                                                   # select only form exp
        dplyr::rename(Subject = SubjectID, 
                      Sex = Gender,
                      Matchness = Match,
                      Valence = Morality,
                      ACC = Accuracy, 
                      RESP = ResponseKey) %>%    #
        dplyr::mutate(Identity = ifelse(Identity == "self" | Identity == 'Self', "Self", 'Other'),    # re-code the data
                      Valence = ifelse(Valence == "moral" | Valence == "Moral", "Good", "Bad"),
                      Matchness = ifelse(Matchness == "Match" | Matchness == "match", "Match", "Mismatch"),
                      Age = ifelse(Age == 0, NA, Age)) %>%
        dplyr::filter(!Subject %in% c(2027, 7, 8, 7035))   # exclude participants

# exclude trials, criterio 2: practicing trials in matching task (first 48 trials)
subNo <- unique(df7a_m$Subject)
for (subj in subNo) {
        if (exists('df.M.fm')){
                df.tmp <- df7a_m[df7a_m$Subject == subj,]
                df.tmp <- df.tmp[49:nrow(df.tmp),]
                df.M.fm <- rbind(df.M.fm,df.tmp)
        } else {
                df.M.fm <- df7a_m[df7a_m$Subject == subj,]
                df.M.fm <- df.M.fm[49:nrow(df.M.fm),]
        }
}   # df.M.fm should be 14880 rows

df7a_m <- df.M.fm %>%          # all the experimental trials
        dplyr::mutate(ACC = ifelse(ACC == -1, 0, ACC))

rm(subNo,df.M.fm,df.tmp, subj) # remove the intermediate variables

# calculate the overall accuracy for matching task
df7a_m.M.acc.g <-  df7a_m %>%
        dplyr::group_by(Subject) %>%
        dplyr::summarise(N = length(ACC),
                         countN = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%
        dplyr::ungroup()

df7a_m.excldSub2.M <- df7a_m.M.acc.g %>% dplyr::filter(ACC < 0.5) %>%
        dplyr::select(Subject) %>% dplyr::pull()# < 50% accuracy in matching task

df7a_m.v <- df7a_m %>%
        dplyr::filter(!Subject %in% df7a_m.excldSub2.M) %>% # exclude the invalid subjects
        dplyr::mutate(RT = RT * 1000, Site = "THU") %>%
        dplyr::filter(RT > 200) 

# calculate d prime
df7a_m.v.dprime_l <- df7a_m.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                     # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%     # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

#  prepare the mean RT for each participant
df7a_m.v.rt_m <- df7a_m.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT), RT_SD = sd(RT), Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df7a_m.meta.d <- df7a_m.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp7a',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df7a_m.meta.rt <- df7a_m.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp7a',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))

# Exp 7b matching task ----
## Load and prepare for exp7b
df7b_m <- read.csv(here::here('exp7', 'rawdata_behav_exp7b_2018.csv'),header = TRUE, sep = ",",
                   stringsAsFactors=FALSE,na.strings=c("","NA"))

### Rule 1: wrong trials numbers because of procedure errors
df7b.excldSub1_M <- df7b_m %>%
        dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
        dplyr::group_by(Subject, Match, Identity,Morality) %>%
        dplyr::summarise(N = length(ACC)) %>%  # count the trial # for each condition of each subject
        dplyr::ungroup() %>%
        dplyr::filter(N != 75) %>%             # filter the rows that trial Number is not 75
        dplyr::distinct(Subject) %>%           # find the unique subject ID
        dplyr::pull(Subject)                   # pull the subj ID as vector

### Rule 2:  overall accuracy < 0.5
df7b.excldSub2_M <- df7b_m %>%
        dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
        dplyr::group_by(Subject) %>%
        dplyr::summarise(N = length(ACC),
                         countN = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%  # count the trial # for each condition of each subject
        dplyr::ungroup() %>%
        dplyr::filter(ACC < .5) %>%             # filter the subjects with over all ACC < 0.5
        dplyr::distinct(Subject) %>%             # find the unique subject ID
        dplyr::pull(Subject)                     # pull the subj ID as vector

### Rule 3:  one condition with zero ACC
df7b.excldSub3_M <- df7b_m %>%
        dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0))  %>%  # no response as wrong
        dplyr::group_by(Subject, Match, Identity,Morality) %>%
        dplyr::summarise(N = length(ACC),
                         countN = sum(ACC),
                         ACC = sum(ACC)/length(ACC)) %>%  # count the trial # for each condition of each subject
        dplyr::ungroup() %>%
        dplyr::filter(ACC == 0) %>%             # filter the subjects with over all ACC < 0.5
        dplyr::distinct(Subject) %>%             # find the unique subject ID
        dplyr::pull(Subject)                     # pull the subj ID as vector

# all participants excluded
df7b.excldSub_M   <- c(df7b.excldSub1_M, df7b.excldSub2_M, df7b.excldSub3_M) # 7302, 7303

# select valid data for further analysis
df7b_m.v <- df7b_m %>%
        dplyr::rename(Matchness = Match,
                      Valence = Morality) %>%
        dplyr::mutate(ACC = ifelse(ACC == 1, 1, 0),                  # no response as wrong
                      Site = "THU",
                      Matchness = ifelse(Matchness == 'match', 'Match', 'Mismatch'))  %>% 
        dplyr::filter(!Subject %in% df7b.excldSub_M)    # exclude the invalid subjects


# calculate d prime
df7b_m.v.dprime_l <- df7b_m.v %>%
        dplyr::mutate(sdt = mosaic::derivedFactor("hit" = (ACC ==1 & Matchness == "Match"), # code as hit
                                                  "CR" = (ACC ==1  & Matchness == "Mismatch"),      # correct reject
                                                  "miss" = (ACC == 0 & Matchness == "Match"),       # miss
                                                  "FA" = (ACC == 0 & Matchness == "Mismatch"),      # false alarm
                                                  .method ="first",  .default = NA)) %>% 
        dplyr::group_by(Site, Subject, Age, Sex, Valence,Identity, sdt) %>%
        dplyr::summarise(N = length(sdt)) %>%                                     # calculate the counts for each 
        dplyr::ungroup() %>%
        tidyr::spread(key = sdt, value = N,fill = 0) %>%                          # long-to-wide format
        dplyr::mutate(hitR = hit/(hit + miss),                                    # hit rate
                      FAR  = FA/(FA+CR)) %>%                                      # fa rate
        dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),     # if hit rate is 1, standardize it
                      FAR  = ifelse(FAR == 0, 1/(2*(hit + miss)), FAR)) %>%       # if FA rate is 0, standardize it
        dplyr::mutate(dprime = qnorm(hitR) - qnorm(FAR)) %>%
        dplyr::select(Site, Subject, Age, Sex, Valence, Identity, dprime) %>%     # select relevant columns
        dplyr::mutate(Valence = factor(Valence, levels = c("Good","Bad")),
                      Identity = factor(Identity, levels = c('Self', 'Other')))

#  prepare the mean RT for each participant
df7b_m.v.rt_m <- df7b_m.v %>%
        dplyr::filter(ACC == 1) %>%
        dplyr::group_by(Site,Subject, Age, Sex, Matchness, Identity, Valence) %>%
        dplyr::summarise(RT_m = mean(RT)*1000,
                         RT_SD = sd(RT)*1000,
                         Ntrial = length(RT)) %>%
        dplyr::ungroup()

### prepare for the later meta-analysis
df7b_m.meta.d <- df7b_m.v.dprime_l %>% 
        dplyr::mutate(ExpID = 'Exp7b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_d))

df7b_m.meta.rt <- df7b_m.v.rt_m %>% 
        dplyr::rename(RT = RT_m) %>%
        dplyr::mutate(ExpID = 'Exp7b',
                      Domain = "Morality") %>%        # add domain as morality, to be comparable with experiment 5.
        dplyr::select(all_of(CommonColnames_rt))


# ---- for questionnaire data -----
# prepare questionnare data
df.scales <- read.csv(here::here("Scale_data", "FADGS_dataset4_1_clean.csv"), header = TRUE, sep = ",",
                      stringsAsFactors=FALSE,na.strings=c("","NA")) %>%
        dplyr::mutate(expID = mosaic::derivedFactor("Exp1a" = (expID == "exp1.0"), 
                                                    "Exp1b" = (expID == "exp1.1"),
                                                    "Exp3a" = (expID == "exp3"),
                                                    "Exp3b" = (expID == "exp3.1"),
                                                    "Exp4a" = (expID == "exp4.1"),
                                                    "Exp4b" = (expID == "exp4.2"),
                                                    "Exp5" = (expID == "exp5.2"),
                                                    "Exp6b" = (expID == "exp6.2"),
                                                    "Exp7a" = (expID == "exp7.1"),
                                                    "Exp7b" = (expID == "exp7r"),
                                                    "Exp_dpr" = (expID == "exp6"),
                                                    .method ="first", .default = NA),
                      expID = as.character(expID))

# ---------- save all dataframes as 'data.rdata' -----
#dfs <- Filter(function(x) is.data.frame(get(x)) , ls())
rdata_save <- Filter(function(x) length(get(x)) !=0 , ls())
save(list=rdata_save, file="AllData.RData")

# Prepare the data for hddm ----

# dataList <- list(df1a.v, df1b.v, df1c.v, df2.v, df3a.v, df3b.v, df4a.v, df4b.v, df5.v, df6a.v, df6b_d1.v, df7a_m.v, df7b_m.v)
# hddmNameList <- c('df1a.v', 'df1b.v', 'df1c.v', 'df2.v', 'df3a.v', 'df3b.v', 'df4a.v', 'df4b.v', 'df5.v', 'df6a.v' ,'df6b_d1.v', 'df7a_m.v', 'df7b_m.v')
# for (indx in 1:13) {
# for (dfname in hddmNameList){
#  current.df <- dataList[[indx]]
#  if ('Resp' %in% colnames(current.df)){
#    current.df <- current.df %>% dplyr::rename(RESP = Resp)
#  }
#  current.name <- paste(hddmNameList[indx],'.hddm_stim.csv', sep = '')
#  if (indx %in% c(1:4, 10)){                                           # for exp 1a and 1b, 2*3 design
#      cur.df.hddm_stim <- current.df %>%
#dplyr::filter(Subject %in% subj.common) %>%          # only participants with Questionnaire data
#      dplyr::filter(!is.na(RESP)) %>%                       # exclude trials without response or with wrong keys
#      dplyr::mutate(RT = RT/1000,
#                    stim = ifelse(Matchness == "Match", 1, 0),          
#                    response = ifelse((Matchness == "Match" & ACC ==1) | (Matchness == "Mismatch" & ACC ==0), 1, 0)) %>%
#      dplyr::select(Subject, Matchness, Valence, stim, response, RT) %>%           # select columns
#      dplyr::rename(subj_idx = Subject, match = Matchness, val = Valence, rt = RT) # rename columns
#  } else if (indx == 9){    # exp5                                      # for exp 5, only select morality task
#      cur.df.hddm_stim <- current.df %>%
#dplyr::filter(taskType == 'Morality') %>%
#dplyr::filter(Subject %in% subj.common) %>%      # only participants with Questionnaire data
#      dplyr::filter(!is.na(RESP)) %>%                   # exclude trials without response or with wrong keys
#      dplyr::mutate(RT = RT/1000,
#                    stim = ifelse(Matchness == "Match", 1, 0),          
#                    response = ifelse((Matchness == "Match" & ACC ==1) | (Matchness == "Mismatch" & ACC ==0), 1, 0)) %>%
#      dplyr::select(Subject, Matchness, taskType, Valence, stim, response, RT) %>%           # select columns
#      dplyr::rename(subj_idx = Subject, match = Matchness, domain = taskType, val = Valence, rt = RT) # rename columns

#  } 
#    else if (indx %in% c(5:8, 10:12))             # for expa4a, 4b, 6b, 7a, 3b,  three-way design,
#      {
#      cur.df.hddm_stim <- current.df %>%
#      #dplyr::filter(Subject %in% subj.common) %>%                       # only participants with Questionnaire data
#      dplyr::filter(!is.na(RESP)) %>%                                   # exclude trials without response or with wrong keys
#      dplyr::mutate(RT = RT/1000,
#                    stim = ifelse(Matchness == "Match", 1, 0),          
#                    response = ifelse((Matchness == "Match" & ACC ==1) | (Matchness == "Mismatch" & ACC ==0), 1, 0)) %>%
#      dplyr::select(Subject, Matchness, Identity,Valence, stim, response, RT) %>%           # select columns
#      dplyr::rename(subj_idx = Subject, match = Matchness, id = Identity, val = Valence, rt = RT) # rename columns
#  } 
#    else if (indx == 13)
#    {                                                    # for exp7b,  three-way design, the RT already in seconds
#      cur.df.hddm_stim <- current.df %>%
#dplyr::filter(Subject %in% subj.common) %>%          # only participants with Questionnaire data
#      dplyr::filter(!is.na(RESP)) %>%                       # exclude trials without response or with wrong keys
#      dplyr::mutate(stim = ifelse(Matchness == "Match", 1, 0),          
#                    response = ifelse((Matchness == "Match" & ACC ==1) | (Matchness == "Mismatch" & ACC ==0), 1, 0)) %>%
#      dplyr::select(Subject, Matchness, Identity,Valence, stim, response, RT) %>%           # select columns
#      dplyr::rename(subj_idx = Subject, match = Matchness, id = Identity, val = Valence, rt = RT) # rename columns
#  } 
# write.csv(cur.df.hddm_stim, file = paste(curDir,'/HDDM/', current.name, sep = ''), row.names = F) 
#}

# rm(dataList)


