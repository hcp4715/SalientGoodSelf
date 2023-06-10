library(tidyverse)
score_within <- read_csv("/Users/zhengyuanrui/SalientGoodSelf/ML_Script/4_Result/within_score.csv")
within_plot <- score_within %>% 
  unite("direct", c("source", "target"), sep = "→") %>% 
  ggplot(aes(x = direct, y = score, fill = direct)) +
  geom_violin() + 
  geom_boxplot(aes(middle = mean(score)), width = 0.2, color = "grey", outlier.shape = NA) +
  geom_hline(yintercept = 0.5, color = "red", alpha = 1, linetype = "dotted") +
  facet_wrap(vars(Identity, Condition), scales = "free_x", labeller = label_both) + 
  papaja::theme_apa() + 
  theme(axis.text.x = element_text(size = 7, angle = 90), 
        legend.position = "none")
ggsave("withinplot.png", width = 10, height = 7, dpi = 300)


score_cross <- read_csv("/Users/zhengyuanrui/SalientGoodSelf/ML_Script/4_Result/cross_score.csv")
cross_plot <- score_cross %>% 
  unite("direct", c("source", "target"), sep = "→") %>% 
  ggplot(aes(x = direct, y = score, fill = direct)) +
  geom_violin() + 
  geom_boxplot(aes(middle = mean(score)), width = 0.15, color = "grey", outlier.shape = NA) +
  geom_hline(yintercept = 0.5, color = "red", alpha = 1, linetype = "dotted") +
  facet_wrap(vars(Identity, Condition), scales = "free_x", labeller = label_both) + 
  papaja::theme_apa() + 
  theme(axis.text.x = element_text(size = 7, angle = 90), 
        legend.position = "none")


ggsave("crossplot.png", width = 30, height = 24, dpi = 300)


df_within <- score_within %>% 
  unite("direct", c("source", "target"), sep = "→") 

df_cross <- score_cross %>% 
  unite("direct", c("source", "target"), sep = "→") 

df_all <- rbind(df_within, df_cross) %>% 
  select(-1)
df_sum <- df_all %>% 
  group_by(Identity, Condition) %>% 
  summarise(ROC_AUC = mean(score), 
            SD = sd(score)) %>% 
  arrange(ROC_AUC)
write_csv(df_sum, "summarize.csv")
dummy_cross <- read_csv("/Users/zhengyuanrui/SalientGoodSelf/ML_Script/4_Result/dummycross_score.csv")
