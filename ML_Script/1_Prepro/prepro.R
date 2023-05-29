# import packages and load dataset
library(tidyverse)
library(here)
here()
load("/Users/zhengyuanrui/SalientGoodSelf/AllData.RData")
###### Preprocessing
### Rule1: combine the datasets include same Self_ref
### Rule2: only use the RT longer than 200ms


df1a.v.ml <- df1a.v %>%
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp1a", 
                Self_ref = "None")

df1a.v.ml_basic <- df1a.v.ml %>% 
  group_by(Subject, BlockNo) %>% 
  summarise(n = n())


df1b.v.ml <- df1b.v %>%
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp1b", 
                Self_ref = "None")

df1b.v.ml_basic <- df1b.v.ml %>% 
  group_by(Subject, BlockNo) %>% 
  summarise(n = n())

df1c.v.ml <- df1c.v %>%
  dplyr::mutate(Identity = NA) %>% 
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp1c", 
                Self_ref = "None")

df1c.v.ml_basic <- df1c.v.ml %>% 
  group_by(Subject, BlockNo) %>% 
  summarise(n = n())

df3a.v.ml <- df3a.v %>%
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp3a", 
                Self_ref = "Explicit")

df3a.v.ml_basic <- df3a.v.ml %>% 
  group_by(Subject, BlockNo) %>% 
  summarise(n = n())

df3b.v.ml <- df3b.v %>%
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp3b" ,
                Self_ref = "Explicit")

df3b.v.ml_basic <- df3b.v.ml %>% 
  dplyr::group_by(Subject, BlockNo) %>% 
  dplyr::summarise(n = n())

df4a.v.ml <- df4a.v %>%
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp4a", 
                Self_ref = "Implicit")

df4a.v.ml_basic <- df4a.v.ml %>% 
  dplyr::group_by(Subject, BlockNo) %>% 
  dplyr::summarise(n = n())

df4b.v.ml <- df4b.v %>%
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp4b", 
                Self_ref = "Implicit")

df4b.v.ml_basic <- df4b.v.ml %>% 
  dplyr::group_by(Subject, BlockNo) %>% 
  dplyr::summarise(n = n())

df5.v.ml <- df5.v %>%
  dplyr::mutate(Identity = NA) %>% 
  dplyr::filter(taskType == "Morality") %>% #only filter the morality condition
  dplyr::select(Subject, BlockNo, TrialNo, Valence, Matchness, ACC, RT, Identity) %>%
  dplyr::mutate(ExpNo = "Exp5", 
                Self_ref = "None")

df5.v.ml_basic <- df5.v.ml %>% 
  dplyr::group_by(Subject, BlockNo) %>% 
  dplyr::summarise(n = n())
######### Combine datasets
df.No_self <- rbind(df1a.v.ml, 
                    df1b.v.ml, 
                    df1c.v.ml, 
                    df5.v.ml) %>% 
  dplyr::filter(!(RT <= 200)) %>% 
  dplyr::mutate(Valence = case_when(
    Valence == "Good" ~ 0,
    Valence == "Neutral" ~ 1,
    Valence == "Bad" ~ 2
  ), 
  Matchness = if_else(Matchness == "Match", 1, 0))


df.explicit_self <- rbind(df3a.v.ml, 
                          df3b.v.ml) %>% 
  dplyr::filter(!(RT <= 200)) %>% 
  dplyr::mutate(Valence = case_when(
    Valence == "Good" ~ 0,
    Valence == "Neutral" ~ 1,
    Valence == "Bad" ~ 2
  ), 
  Matchness = if_else(Matchness == "Match", 1, 0))

df.implicit_self <- rbind(df4a.v.ml, 
                          df4b.v.ml) %>% 
  dplyr::filter(!(RT <= 200)) %>% 
  dplyr::mutate(Valence = case_when(
    Valence == "Good" ~ 0,# 
    Valence == "Neutral" ~ 1,
    Valence == "Bad" ~ 2
  ), 
  Matchness = if_else(Matchness == "Match", 1, 0))#dummy coding

######### save the dataset

path <- here::here("ML_script", "2_Data")

write_csv(df.No_self, paste(path, "No_self.csv", sep = "/"))
write_csv(df.explicit_self, paste(path, "Explicit_self.csv", sep = "/"))
write_csv(df.implicit_self, paste(path, "Implicit_self.csv", sep = "/"))


