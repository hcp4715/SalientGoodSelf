#############################################################################
## This script is used for initializing the analysis
#############################################################################

## Function of this script:
# 1. choose the feedback language
# 2. load or prepare packages
# 3. define functions for other scripts

#############################################
############ 1. choose the feedback language
#############################################
# set local encoding to English
if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = 'LC_ALL','English_United States.1250')
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}

Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientific notion
options(digits=5)       # limit the number of reporting

# rm(list = setdiff(ls(), lsf.str()))  # remove all data but keep functions

#############################################
##### 2. load or prepare packages
#############################################
# use pacman to manage the packages
if (!require(pacman)){
        install.packages("pacman")
        library(pacman)
}

# use pacman::p_load to load the packages
pacman::p_load('here',              # for choosing directory
               'tidyverse',         # for data wrangling
               'brms',              # for Bayesian stats, main text
               'tidybayes',         # for Bayesian stats, main text
               "BayesFactor",       # for Bayes factor 
               "ggplot2",           # plot general
               'ggridges',          # plot ridges
               'patchwork',         # plot patch plots together
               "papaja"            # core for reproduce the APA format of the manuscript
               )

# source("geom_flat_violin.R")       # for plotting the violin plots

# using cmdstanr as backend, need to installed properly
if (!require(cmdstanr)){
        install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
        library(cmdstanr)
}

# set_cmdstan_path('/home/hcp4715/cmdstan')

# define a function to run the sdt GLMM for all exp with Matchness * Valence design
# for 1a, 1b, 1c, 2, 6a
fun_sdt_val <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_sdt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        
        m <- df %>%
                dplyr::filter(!is.na(RESP)) %>% # filter trials without response
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              saymatch = ifelse((Matchness == 'Match' & ACC == 1) | 
                                                        (Matchness == 'Mismatch' & ACC == 0), 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                brms::brm(saymatch ~ 0 + Valence + ismatch:Valence + 
                                  (0 + Valence + ismatch:Valence | Subject),
                          family = bernoulli(link="probit"),
                          data = .,
                          control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

fun_plot_sdt_val <- function(m_sdt) {
        # extract c
        tmp_c <- m_sdt %>% 
                tidybayes::gather_draws(b_ValenceBad, b_ValenceNeutral, b_ValenceGood) %>%
                dplyr::rename(Valence = .variable, sdt_c = .value) %>% dplyr::ungroup() %>%
                dplyr::mutate(Valence = gsub("b_", "", Valence)) %>%
                dplyr::mutate(Valence = ifelse(stringr::str_detect(Valence, 'Bad'), 'Bad',
                                               ifelse(stringr::str_detect(Valence, 'Good'), 'Good', 'Neutral')))
        
        # dprime
        tmp_d <- m_sdt %>% 
                tidybayes::gather_draws(`b_ValenceBad:ismatch`, `b_ValenceNeutral:ismatch`, 
                                        `b_ValenceGood:ismatch`) %>%
                dplyr::rename(Valence = .variable, sdt_d = .value) %>% dplyr::ungroup() %>%
                dplyr::mutate(Valence = gsub("b_", "", Valence)) %>%
                dplyr::mutate(Valence = ifelse(stringr::str_detect(Valence, 'Bad'), 'Bad',
                                               ifelse(stringr::str_detect(Valence, 'Good'), 'Good', 'Neutral')))
        
        # plot summaries with densities
        p_sdt_d_sum <- tmp_d %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                ggplot2::ggplot(aes(x = sdt_d, y = Valence)) +
                tidybayes::stat_halfeyeh() + 
                labs(x = "sensitivity (d')", y = 'Posterior') +
                theme_classic()
        
        p_sdt_c_sum <- tmp_c %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                ggplot2::ggplot(aes(x = sdt_c, y = Valence)) +
                tidybayes::stat_halfeyeh() + 
                labs(x = "criteria (c)", y = 'Posterior') +
                theme_classic()
        
        # plot comparison
        p_sdt_d <- tmp_d %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                tidybayes::compare_levels(sdt_d, by = Valence) %>%
                ggplot2::ggplot(aes(x = sdt_d, y = Valence, fill = after_stat(x > 0))) +
                tidybayes::stat_halfeyeh() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(x = "sensitivity (d')", y = 'Comparison') +
                theme_classic()
        
        p_sdt_c <- tmp_c %>%
                dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                tidybayes::compare_levels(sdt_c, by = Valence) %>%
                ggplot2::ggplot(aes(x = sdt_c, y = Valence, fill = after_stat(x > 0))) +
                tidybayes::stat_halfeyeh() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(x = "criteria (c)", y = 'Comparison') +
                theme_classic()
        
        return(list(p_sdt_d_sum, p_sdt_c_sum, p_sdt_d, p_sdt_c))
}

# define a function (shifted_lognormal) to run the RT GLMM for all exp with Matchness * Valence design
fun_rt_val <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_rt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        m <- df %>%
                dplyr::mutate(RT_sec = RT/1000) %>% # log RT in seconds
                dplyr::filter(ACC == 1) %>%
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                brms::brm(RT_sec ~ ismatch*Valence + (ismatch*Valence | Subject),
                          family = shifted_lognormal(),
                          data = ., control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

fun_plot_rt_val <- function(m_rt) {
        tmp_rt <- m_rt %>% 
                tidybayes::spread_draws(b_Intercept, b_ValenceBad, b_ValenceGood, 
                                        b_ismatch,   `b_ValenceBad:ismatch`, `b_ValenceGood:ismatch`) %>%
                dplyr::mutate(Neut_MM = b_Intercept,
                              Bad_MM = Neut_MM + b_ValenceBad,
                              Good_MM = Neut_MM + b_ValenceGood,
                              Neut_M = Neut_MM + b_ismatch,
                              Bad_M = Neut_MM + b_ismatch + `b_ValenceBad:ismatch`,
                              Good_M = Neut_MM + b_ismatch + `b_ValenceGood:ismatch`) %>%
                dplyr::select(-contains('b_')) %>%
                tidyr::pivot_longer(cols = Neut_MM:Good_M,
                                    names_to = 'cond',
                                    values_to = 'logRT') %>%
                dplyr::mutate(RT = exp(logRT)*1000,
                              Matchness = dplyr::case_when(grepl("_MM$", cond) ~ "Mismatch",
                                                           grepl("_M$", .variable) ~ "Match"),
                              Valence = dplyr::case_when(grepl("Neut", cond) ~ "Neutral",
                                                         grepl("Bad", cond) ~ "Bad",
                                                         grepl("Good", cond) ~ "Good")
                              # Matchness = dplyr::case_when(cond == 'Neut_MM' | cond == 'Bad_MM' | cond == 'Good_MM' ~ 'Mismatch',
                              #                              cond == 'Neut_M'  | cond == 'Bad_M'  | cond == 'Good_M' ~ 'Match'),
                              # Valence = dplyr::case_when(cond == 'Neut_MM' | cond == 'Neut_M' ~ 'Neutral',
                              #                            cond == 'Bad_MM'  | cond == 'Bad_M'  ~ 'Bad', 
                              #                            cond == 'Good_MM' | cond == 'Good_M' ~ 'Good')
                )
        p_exp1b_rt_m_sum <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                dplyr::filter(Matchness == 'Match') %>%
                ggplot2::ggplot(aes(x = RT, y = Valence)) +
                tidybayes::stat_halfeyeh() + 
                labs(x = "RTs (Matching, ms)", y = 'Posterior') +
                theme_classic()
        p_exp1b_rt_mm_sum <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good'))) %>%
                dplyr::filter(Matchness == 'Mismatch') %>%
                ggplot2::ggplot(aes(x = RT, y = Valence)) +
                tidybayes::stat_halfeyeh() + 
                labs(tag = 'D', x = "RTs (Mismatching, ms)", y = 'Posterior') +
                theme_classic()
        
        # plot comparison
        p_exp1b_rt_m <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                dplyr::filter(Matchness == 'Match') %>%
                tidybayes::compare_levels(RT, by = Valence) %>%
                ggplot2::ggplot(aes(x = RT, y = Valence, fill = after_stat(x < 0))) +
                tidybayes::stat_halfeyeh() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(tag = 'C', x = "RTs (Matching, ms)", y = 'Comparison') +
                theme_classic()
        p_exp1b_rt_mm <- tmp_rt %>% dplyr::mutate(Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good'))) %>%
                dplyr::filter(Matchness == 'Mismatch') %>%
                tidybayes::compare_levels(RT, by = Valence) %>%
                ggplot2::ggplot(aes(x = RT, y = Valence, fill = after_stat(x < 0))) +
                tidybayes::stat_halfeyeh() + 
                geom_vline(xintercept =0, linetype = "dashed") +
                scale_fill_manual(values = c("gray80", "skyblue")) +
                labs(tag = 'D', x = "RTs (Mismatching, ms)", y = 'Comparison') +
                theme_classic()
        return(list(p_exp1b_rt_m_sum, p_exp1b_rt_mm_sum, p_exp1b_rt_m, p_exp1b_rt_mm))
}

# function for SDT with Match by identity by valence design
fun_sdt_val_id <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_sdt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        
        m <- df %>%
                dplyr::filter(!is.na(RESP)) %>% # filter trials without response
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              saymatch = ifelse((Matchness == 'Match' & ACC == 1) | 
                                                        (Matchness == 'Mismatch' & ACC == 0), 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good')),
                              Identity = factor(Identity, levels = c('Self', 'Other'))) %>%
                brms::brm(saymatch ~ 0 + Identity:Valence + ismatch:Identity:Valence + 
                                  (0 + Identity:Valence + ismatch:Identity:Valence | Subject),
                          family = bernoulli(link="probit"),
                          data = .,
                          control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

# define a function (shifted_lognormal) to run the RT GLMM for all exp with Matchness * Identity * Valence design
fun_rt_val_id <- function(exp_name) {
        df_name <- paste('df', exp_name, '.v', sep = '')
        m_name <- paste("glmmModels/exp", exp_name, "_rt_m1_DummyCode", sep = '')
        df <- get(df_name)  # get the data by string
        m <- df %>%
                dplyr::mutate(RT_sec = RT/1000) %>% # log RT in seconds
                dplyr::filter(ACC == 1) %>%
                dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1, 0),
                              Valence = factor(Valence, levels = c('Neutral', 'Bad', 'Good')),
                              Identity = factor(Identity, levels=c('Self', 'Other'))) %>%
                brms::brm(RT_sec ~ ismatch*Identity*Valence + (ismatch*Identity*Valence | Subject),
                          family = shifted_lognormal(),
                          data = ., control = list(adapt_delta = .99),
                          iter = 4000,
                          thin = 2,
                          cores = parallel::detectCores(),
                          file = here::here(m_name))
        return(m)
}

