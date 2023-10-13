library(tidyverse)
score_within <- read_csv("/Users/zhengyuanrui/SalientGoodSelf/ML_Script/4_Result/within_score.csv")

score_cross <- read_csv("/Users/zhengyuanrui/SalientGoodSelf/ML_Script/4_Result/cross_score.csv")

df_all <- rbind(score_within,score_cross) %>% 
  select(-1)
df_sum <- df_all %>% 
  group_by(source, target) %>% 
  summarise(ROC_AUC_mean = mean(score), 
            ROC_AUC_median = median(score),
            SD = sd(score)) %>% 
  arrange(ROC_AUC)


write_csv(df_sum, "summarize.csv")

