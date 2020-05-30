### This is a temporary script for testing both frequentists and bayesian generalized linear mixed model.

### Bayesian Hierarchical model for Signal Detection Theory
# here I will use the equal variance Gaussian SDT (EVSDT)
# tutorial: https://vuorre.netlify.com/post/2017/10/12/bayesian-estimation-of-signal-detection-theory-models-part-2/

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English
library(tidyverse)
library(brms)
#library(bhsdtr2)
#library(rstan)

Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

dplyr::as_tibble(df1a.v)

df1a.v.resp <- df1a.v %>% 
  dplyr::filter(!is.na(RESP)) %>% 
  dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1,0),
                saymatch = ifelse((Matchness == 'Match' & ACC == 1) | (Matchness == 'Mismatch' & ACC == 0), 1,0)) %>%
  dplyr::select(Subject, Valence, Matchness, CRESP, RESP, ACC, ismatch, saymatch)

# try bhsdtr2
df1a.v.resp$resp_binary <- bhsdtr2::combined.response(df1a.v.resp$ismatch, 
                                                      accuracy = df1a.v.resp$ACC)
#df1a.v.model.data <- bhsdtr2::aggregate_responses(df1a.v.resp, 'ismatch', 'resp_binary',
#                                                  c('Subject','Valence'))
m_1 <- bhsdtr2::bhsdtr(c(dprim ~ Valence + (Valence | Subject), 
                         thr ~ Valence + (Valence | Subject)),
                       resp_binary ~ ismatch, 
                       df1a.v.resp,
                       method = 'stan',
                       cores = 4,
                       chain = 4)
bhsdtr2::samples(m_1,'dprim')
samples(m_1, 'thr')

# fit a glmm without the Valence
fitglmm <- brms::brm(saymatch ~ 1 + ismatch + (1 + ismatch | Subject),
                     family = bernoulli(link="probit"),
                     data = df1a.v.resp,
                     cores = 4,
                     file = here::here("glmmModels/sdtmodel_1"))

summary(fitglmm)

# add predictor valence
# recode the matchness as effect coding
# https://rstudio-pubs-static.s3.amazonaws.com/480255_9baa652276b540d0a239188b9513a026.html#(11)
df1a.v.resp <- df1a.v.resp %>% 
  dplyr::mutate(ismatch_num = ifelse(Matchness == 'Match', 0.5, -0.5))
  
# traditional way (frequentist's aproach)
sdt_line <- df1a.v.resp %>%
  dplyr::mutate(type = "hit",
                type = ifelse(Matchness == 'Match' & ACC == 0, "miss", type),
                type = ifelse(Matchness == 'Mismatch' & ACC == 1, "cr", type),
                type = ifelse(Matchness == 'Mismatch' & ACC == 0, "fa", type)) %>%
  dplyr::group_by(Subject, Valence, type) %>%
  dplyr::summarise(count = n()) %>%
  tidyr::spread(key = type, value = count,  fill = 0) %>%
  dplyr::mutate(hitR = hit/(hit+miss),
                faR  = fa/(fa + cr)) %>%
  dplyr::mutate(hitR = ifelse(hitR == 1, 1 - 1/(2*(hit + miss)), hitR),      # if hit rate is 1, standardize it
                faR  = ifelse(faR == 0, 1/(2*(fa + cr)), faR)) %>%           # if the fa rate is 0,
  dplyr::mutate(zhr = qnorm(hitR),
                zfa = qnorm(faR),
                dprime = zhr - zfa,
                crit = -zfa,
                c = -0.5 * (zhr + zfa),
                beta = exp(0.5* (zfa^2 - zhr^2))) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# fit a glmm, GLMM, linear, effect coding
fitglmm_2 <- brms::brm(saymatch ~ 0 + Valence + Valence:ismatch_num + 
                         (0 + Valence + Valence:ismatch_num | Subject),
                     family = bernoulli(link="probit"),
                     data = df1a.v.resp,
                     control = list(adapt_delta = .99),
                     cores = parallel::detectCores(),
                     file = here::here("glmmModels/sdtmodel_2"))

summary(fitglmm_2)
brms::posterior_summary(fitglmm_2)

# fit a glmm, GLMM, linear, dummy coding
fitglmm_2_1 <- brms::brm(saymatch ~ 0 + Valence + Valence:ismatch + 
                         (0 + Valence + Valence:ismatch | Subject),
                       family = bernoulli(link="probit"),
                       data = df1a.v.resp,
                       control = list(adapt_delta = .99),
                       cores = parallel::detectCores(),
                       file = here::here("glmmModels/sdtmodel_2.1"))

summary(fitglmm_2_1)
brms::posterior_summary(fitglmm_2)


# fit a glmm, GLMM, linear, effect coding, with intercept
fitglmm_2_2 <- brms::brm(saymatch ~ Valence*ismatch_num + 
                         (Valence*ismatch_num | Subject),
                       family = bernoulli(link="probit"),
                       data = df1a.v.resp,
                       control = list(adapt_delta = .99),
                       cores = parallel::detectCores(),
                       file = here::here("glmmModels/sdtmodel_2.2"))

summary(fitglmm_2_2)

# fit a glmm, GLMM, linear, dummy coding, with intercept
fitglmm_2_3 <- brms::brm(saymatch ~ Valence*ismatch + 
                           (Valence*ismatch | Subject),
                         family = bernoulli(link="probit"),
                         data = df1a.v.resp,
                         control = list(adapt_delta = .99),
                         cores = parallel::detectCores(),
                         file = here::here("glmmModels/sdtmodel_2.3"))

summary(fitglmm_2_3)
stancode(fitglmm_2_3)

make_stancode(saymatch ~ Valence*ismatch + 
                (Valence*ismatch | Subject),
              family = bernoulli(link="probit"),
              data = df1a.v.resp,
              control = list(adapt_delta = .99),
              cores = parallel::detectCores(),
              file = here::here("glmmModels/sdtmodel_2.3"))

# fit a glmm, GLMM, nonlinear
glmm2_4 <- bf(saymatch ~ Phi(dprime*ismatch - c),
            dprime ~ 1 + (1 |s| Subject),
            c ~ 1 + (1 |s| Subject),
            nl = FALSE)
Priors_glmm2_4 <- c(prior(normal(0, 3), par = 'dprime', lb=0),
                  prior(normal(0, 3), par = 'c'),
                  prior(student_t(10, 0, 1), class = 'sd', par = 'dprime'),
                  prior(student_t(10, 0, 1), class = 'sd', par = 'c'),
                  prior(lkj(4), class = 'cor'))

fitglmm_3 <- brms::brm(glmm3,
                       family = bernoulli(link="identity"),
                       data = df1a.v.resp,
                       prior = Priors_glmm3,
                       control = list(adapt_delta = .99),
                       cores = 6, inits = 0,
                       file = here::here("glmmModels/sdtmodel_3"))

summary(fitglmm_3)

preds_dprime_c <- fitglmm_2$data %>%
  dplyr::select(Subject, Valence, ismatch_num) %>%
  tidybayes::add_predicted_draws(model = fitglmm_2, n = 500)

sdt_line_pred <- preds_dprime_c %>%
  dplyr::group_by(Subject, Valence, .draw) %>%
  dplyr::mutate(type = "hit",
         type = ifelse(ismatch_num > 0 & .prediction == 0, "miss", type),
         type = ifelse(ismatch_num < 0 & .prediction == 0, "cr", type),
         type = ifelse(ismatch_num < 0 & .prediction == 1, "fa", type)) %>%
  dplyr::ungroup()

sdt_line_pred <- sdt_line_pred %>%
  dplyr::group_by(Subject, Valence, .draw, type) %>%
  dplyr::summarise(count = n()) %>%
  tidyr::spread(type, count)


sdt_line_pred <- sdt_line_pred %>%
  dplyr::mutate(zhr = qnorm(hit/(hit+miss)),
                zfa = qnorm(fa/(fa + cr)),
                dprime = zhr - zfa,
                crit = -zfa,
                c = -0.5 * (zhr + zfa),
                beta = exp(0.5* (zfa^2 - zhr^2))) %>%
  dplyr::mutate_if(is.numeric, round, 3)

sdt_line_pred %>%
  dplyr::group_by(Subject, Valence) %>%
  tidybayes::median_qi(dprime, .width = c(.80, .95), na.rm = TRUE) %>%
  ggplot2::ggplot(aes(y = dprime, x = Valence)) +
  tidybayes::geom_pointinterval(fatten_point = 2,
                     interval_size_range = c(0.6, 2),
                     color = "black") +
  geom_point(aes(y = dprime, x = Valence),
             data = sdt_line,
             shape = 21, color = "black",
             fill = "pink", stroke = 1, size = 3) +
  geom_hline(yintercept = 0, linetype = 3, color = "grey", alpha = 0.9) +
  facet_wrap(~Subject)

# population level
plot(fitglmm_2, 'b_Valence')


plot(hypothesis(fitglmm_2,
                "ValenceGood:ismatch_num > ValenceBad:ismatch_num"))
plot(hypothesis(fitglmm_2,
                "ValenceGood:ismatch_num > ValenceNeutral:ismatch_num"))

### try meta-analysis 1a, 1b, 1c, 2, 5 and 6a
#selected_columns <- c('Subject','Age', 'Sex')
df1a.v_meta$ExpID <- 'Exp1a'
df1b.v_meta$ExpID <- 'Exp1b'
df1c.v_meta$ExpID <- 'Exp1c'
df2.v_meta$ExpID <- 'Exp2'
df5.v_meta$ExpID <- 'Exp5'
df6a.v_meta$ExpID <- 'Exp6a'

selected_columns <- c('ExpID', 'Site', 'Subject','Age', 'Sex', 'Matchness','Valence', 'RESP', 'ACC','RT')
df_moral <- dplyr::bind_rows(df1a.v_meta[selected_columns],
                             df1b.v_meta[selected_columns],
                             df1c.v_meta[selected_columns],
                             df2.v_meta[selected_columns],
                             df5.v_meta[selected_columns],
                             df6a.v_meta[selected_columns]) %>%
  dplyr::mutate(ExpID_new = paste(ExpID, Site, sep = "_")) %>%
  dplyr::mutate(Valence = factor(Valence, levels = c('Bad', 'Neutral', 'Good')))

df_moral_subj <- df_moral %>%
  dplyr::group_by(ExpID_new, Site) %>%
  dplyr::summarize(N = n_distinct(Subject))

# randomly select 20 participants from each study
#tmp1 <- sample(unique(df1a.v$Subject), 15)
#tmp2 <- sample(unique(df1b.v$Subject), 15)
#tmp3 <- sample(unique(df1c.v$Subject), 15)
#tmp4 <- sample(unique(df5.v$Subject), 15)
#tmp <- c(tmp1, tmp2, tmp3, tmp4)

df_moral <- df_moral %>%
  # dplyr::filter(Subject %in% tmp) %>%
  dplyr::filter(!is.na(RESP)) %>% # filter trials without response
  dplyr::mutate(ismatch = ifelse(Matchness == 'Match', 1,0),
                saymatch = ifelse((Matchness == 'Match' & ACC == 1) | (Matchness == 'Mismatch' & ACC == 0), 1,0)) %>%
  dplyr::select(ExpID_new, Subject, Valence, Matchness, RESP, ACC, ismatch, saymatch) %>%
  dplyr::mutate(ismatch_num = ifelse(Matchness == 'Match', 0.5, -0.5))

# plot the nested structure of the data
with(df_moral, table(Subject, ExpID_new)) %>%
  image(
    col = grey.colors(80, start = 1, end = 0), 
    axes = TRUE, 
    xlab = "Subject", 
    ylab = "ExpID"
  )

# fit the model for all valence effect
fitglmm_6 <- brms::brm(saymatch ~ 1 + Valence*ismatch + 
                         (1 + Valence*ismatch | ExpID_new) + 
                         (1 + Valence*ismatch | ExpID_new:Subject),
                       family = bernoulli(link="probit"),
                       data = df_moral,
                       control = list(adapt_delta = .90),
                       cores = parallel::detectCores(),
                       file = here::here("glmmModels/sdtmodel6"))

summary(fitglmm_6)
stancode(fitglmm_6)

plot(hypothesis(fitglmm_6,
                "ValenceGood:ismatch > 0"))



### multiple level BGLMM
bf_1 <- bf(saymatch ~ Phi(dprime_indv*ismatch*Valence - c_indv*Valence),
           dprime_indv ~ 1 + (1 | Subject),
           c_indv ~ 1 + (1 | Subject),
           
           )


glmm3 <- bf(saymatch ~ Phi(dprime*ismatch - c),
            dprime ~ 1 + (1 |s| Subject),
            c ~ 1 + (1 |s| Subject),
            nl = TRUE)
Priors_glmm3 <- c(prior(normal(0, 3), nlpar = 'dprime', lb=0),
                  prior(normal(0, 3), nlpar = 'c'),
                  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'dprime'),
                  prior(student_t(10, 0, 1), class = 'sd', nlpar = 'c'),
                  prior(lkj(4), class = 'cor'))


fitglmm_4 <- brms::brm(saymatch ~ 0 + Valence + Valence:ismatch_num + 
                         (0 + Valence + Valence:ismatch_num | Subject/ExpID),
                       family = bernoulli(link="probit"),
                       data = df_moral,
                       control = list(adapt_delta = .99),
                       cores = parallel::detectCores(),
                       file = here::here("glmmModels/sdtmodel4"))

summary(fitglmm_4)

fitglmm_5 <- brms::brm(saymatch ~ 0 + Valence + Valence:ismatch_num + 
                         (0 + Valence + Valence:ismatch_num | ExpID) + 
                         (0 + Valence + Valence:ismatch_num | ExpID:Subject),
                       family = bernoulli(link="probit"),
                       data = df_moral,
                       control = list(adapt_delta = .99),
                       cores = parallel::detectCores(),
                       file = here::here("glmmModels/sdtmodel5"))
summary(fitglmm_5)
stancode(fitglmm_5)

hypothesis(fitglmm_5, "ValenceGood:ismatch_num - ValenceBad:ismatch_num > 0") 
hypothesis(fitglmm_5, "ValenceGood:ismatch_num - ValenceNeutral:ismatch_num > 0") 

plot(hypothesis(fitglmm_5,
                "ValenceGood:ismatch_num > ValenceBad:ismatch_num"))
plot(hypothesis(fitglmm_5,
                "ValenceGood:ismatch_num > ValenceNeutral:ismatch_num"))
plot(hypothesis(fitglmm_5,
                "ValenceBad:ismatch_num > ValenceNeutral:ismatch_num"))

plot(fitglmm_5, 'b_Valence')

# effect size r^2
bayes_R2(fitglmm_5)

# Plot the d prime 'b_Valence' for each experiment.


### try LMM ---- exp1a
df1a.v.rt <- df1a.v %>% dplyr::filter(ACC == 1) %>% dplyr::mutate(logRT = ifelse(RT>0, log(RT),0)) 
m1 <- lme4::lmer(logRT ~ Val_sh * Matchness + (1|Subject) + (1|Shape), df1a.v.rt)
m2 <- lme4::lmer(logRT ~ Val_sh * Matchness + (1|Subject), df1a.v.rt)
m3 <- lme4::lmer(logRT ~ Val_sh * Matchness)
anova(m1,m2)

df1a.v.rt.m <- df1a.v.rt %>% dplyr::filter(Matchness == "Match")
m4 <- lme4::lmer(RT ~ Val_sh + (1|Subject) + (1|Shape), df1a.v.rt.m)
m5 <- lme4::lmer(RT ~ Val_sh + (1|Subject), df1a.v.rt.m)
anova(m4,m5)
summary(m4)
summary(m5)


### Bayesian LMM
# without random items
library(ggmcmc)
library(mcmcplots) 

df1a.v.rt$RT_sec <- df1a.v.rt$RT/1000
df1a.v.rt$logRT_sec <- log(df1a.v.rt$RT_sec)


# Define a weakly informed prior
prior_null <- c(
  set_prior('normal(-1, 0.5)', class = 'Intercept'),  # around exp(-1) = 0.36 secs, sd = exp(0.5) = 1.687
  set_prior('normal(0.4, 0.3)', class = 'sigma'),     # SD of individual rts in log-units
  set_prior('normal(0, 0.5)', class = 'b')#,           # around exp(-1) - exp(-1 + 0.5) = 0.2386 seconds effect in either direction
  #  set_prior('normal(0.3, 0.1)', class = 'sd')         # some variability between participants
)

# check prior
prior_null_int <- exp(rnorm(n=1e5, mean=-1, sd=0.7))  # chosen intercept prior
prior_null_sig <- exp(rnorm(n=1e5, mean=-0.7, sd=.8))      # chosen intercept prior
prior_null_b <- exp(rnorm(n=1e5, mean=-0.7, sd=0.5))          # chosen intercept prior

HDI95_null_int <- unname(quantile(prior_null_int, c(.025, .975)))

ggplot() + geom_density(aes(x = prior_null_int)) + xlim(-10, 10) + geom_vline(xintercept = HDI95_null_int[1]) +  geom_vline(xintercept = HDI95_null_int[2])

HDI95_null_b <- unname(quantile(prior_null_b, c(.025, .975)))

ggplot() + geom_density(aes(x = prior_null_b)) + xlim(-5, 5) + geom_vline(xintercept = HDI95_null_b[1]) +  geom_vline(xintercept = HDI95_null_b[2])


brms::get_prior(RT_sec ~ Valence * Matchness + (Valence * Matchness | Subject), 
          data = df1a.v.rt, 
          family = shifted_lognormal())

m6 <- brms::brm(RT_sec ~ Valence * Matchness + (Valence * Matchness | Subject), 
                data = df1a.v.rt, 
                family = shifted_lognormal(),
                chains = 4, cores = 4, warmup = 1000, iter = 2000,
                prior = prior_null,
                file = here::here("glmmModels/rtmodel_m6"))

# check convegence:
m6_tranformed <- ggmcmc::ggs(m6) 

ggplot2::ggplot(filter(m6_tranformed, Parameter %in% c("b_Intercept", "b_Val_shGood", "b_Val_shNeutral", "b_MatchnessMismatch", "b_Val_shGood:MatchnessMismatch", "b_Val_shNeutral:MatchnessMismatch", "sigma", "ndt"),
                       Iteration > 1000),
                aes(x   = Iteration,
                    y   = value, 
                    col = as.factor(Chain)))+
  geom_line() +
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y') +
  labs(title = "Caterpillar Plots",
       col   = "Chains")

# obtain the disganosis
model_6_posterior <- as.mcmc(m6) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
gelman.diag(model_6_posterior[, 1:8])

gelman.plot(model_6_posterior[, 1:8])

#To obtain the Geweke diagnostic use:
geweke.diag(model_6_posterior[, 1:5])

# check posterior
brms::stanplot(m6, type = "hist")

# To obtain information about autocorrelation the following syntax can be used:
autocorr.diag(modelposterior[,1:8], lags = c(0, 1,2,3,4, 5, 10, 50))

# 
stanplot(m6, pars = 1:4, type = "dens")

pp_check(m6)

# with random intercept
prior_int <- c(
  set_prior('normal(-1, 0.5)', class = 'Intercept'),  # around exp(-1) = 0.36 secs
  set_prior('normal(-.7, 0.4)', class = 'sigma'),     # SD of individual rts in log-units
  set_prior('normal(-0.7,0.4)', class = 'b'),           # around exp(-1) - exp(-1 + 0.5) = 0.2386 seconds effect in either direction
  set_prior('normal(-0.5, 0.3)', class = 'sd')         # some variability between participants
)

m7 <- brms::brm(RT_sec ~ Val_sh * Matchness + (1|Subject), 
                data = df1a.v.rt, 
                family = shifted_lognormal(),
                chains = 4, cores = 4, warmup = 1000, iter = 2000,
                prior = prior_int,
                file = 'exp1a_int_log_my_prior')

m6 <- brms::brm(RT_sec ~ Val_sh * Matchness, 
                data = df1a.v.rt, 
                family = shifted_lognormal(),
                chains = 4, cores = 4, warmup = 1000, iter = 2000,
                prior = prior_null,
                file = 'exp1a_null_log')

# do not use customized prior
df1a.v.rt$Subject <- as.factor(df1a.v.rt$Subject) 
df1a.v.rt_filt <- df1a.v.rt %>%
  group_by(Subject) %>%
  arrange(RT) %>%
  slice(5:n())

#m7_1 <- brms::brm(formula = bf(RT_sec ~ Val_sh*Matchness + (1|Subject), 
#                             ndt ~ Subject), 
m7_1 <- brms::brm(RT_sec ~ Val_sh*Matchness + (1|Subject), 
                  data = df1a.v.rt_filt, 
                  family = shifted_lognormal(),
                  chains = 4, cores = 4, warmup = 1500, iter = 3000,
                  #prior = prior_int,
                  file = 'exp1a_int_log_default_prior'
)

# customed prior, with random intercept
#m7 <- brms::brm(formula = bf(RT_sec ~ Val_sh*Matchness + (1|Subject), 
#                             ndt ~ Subject),
m7 <- brms::brm(RT_sec ~ Val_sh*Matchness + (1|Subject),
                data = df1a.v.rt_filt, 
                family = shifted_lognormal(),
                chains = 4, cores = 4, warmup = 2000, iter = 5000,
                prior = prior_int,
                file = 'exp1a_int_log_my_prior'
)

#formula = bf(rt ~ condition + (1|id), 
#             ndt ~ id)

pp_check(m7)

m7_tranformed <- ggmcmc::ggs(m7) 

ggplot2::ggplot(filter(m7_tranformed, Parameter %in% c("b_Intercept", "b_Val_shGood", "b_Val_shNeutral", "b_MatchnessMismatch", "b_Val_shGood:MatchnessMismatch", "b_Val_shNeutral:MatchnessMismatch", "sigma", "ndt"),
                       Iteration > 1000),
                aes(x   = Iteration,
                    y   = value, 
                    col = as.factor(Chain)))+
  geom_line() +
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y') +
  labs(title = "Caterpillar Plots",
       col   = "Chains")

summary(m7)

model_7_posterior <- as.mcmc(m7) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options

gelman.diag(model_7_posterior[, 1:8])  # the results is horrible!!
gelman.plot(model_7_posterior[, 1:8])


# with random intercept and random slope
m8 <- brms::brm(RT ~ Val_sh + Matchness + Val_sh:Matchness + (1 + Val_sh * Matchness|Subject), 
                data = df1a.v.rt.m, 
                family = shifted_lognormal(), 
                chains = 4, cores = 4, warmup = 1000, iter = 2000,
                file = 'exp1a_inte_slop_log')


marginal_effects(m6)  # Back-transformed parameter estimates
marginal_effects(m6, method='predict')  # Same, but for responses

#m7 <- brms::brm(formua = bf(RT ~ Val_sh + (1|Subject), ndt ~ Subject), df1a.v.rt.m, family = shifted_lognormal())
summary(m7)

marginal_effects(m7)  # Back-transformed parameter estimates
marginal_effects(m7, method='predict')  # Same, but for responses

default_priors <- make_table(get_prior(RT ~ Val_sh + (1 + Val_sh|Subject),
                                       data = df1a.v.rt.m))
default_priors <- make_table(prior_summary(m7))
default_priors

# plot the priors
set.seed(1234) # make the random sampling reproducible by setting a seed

def_int_sim <- rstudent_t(1e5, 3, 7, 10)  # default intercept prior
HDI95_def_int <- unname(quantile(def_int_sim, c(.025, .975)))

ggplot() + geom_density(aes(x = def_int_sim)) + xlim(-80, 80) + geom_vline(xintercept = HDI95_def_int[1]) +  geom_vline(xintercept = HDI95_def_int[2])


def_sd_sim <- rstudent_t(1e5, 3, 0, 10)  # default sd prior
HDI95_def_sd <- unname(quantile(def_sd_sim, c(.025, .975)))

ggplot() + geom_density(aes(x = def_sd_sim)) + xlim(-80, 80) + geom_vline(xintercept = HDI95_def_sd[1]) +  geom_vline(xintercept = HDI95_def_sd[2])

int_prior <- rnorm(n=1e5, mean=-1, sd=0.5)  # chosen intercept prior
HDI95_int_prior <- unname(quantile(int_prior, c(.025, .975)))

ggplot() + geom_density(aes(x = int_prior)) + xlim(-10, 10) + geom_vline(xintercept = HDI95_int_prior[1]) +  geom_vline(xintercept = HDI95_int_prior[2])



loo(m6, m7)