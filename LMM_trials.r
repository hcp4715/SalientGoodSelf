### try LMM ---- exp1a
df1a.v.rt <- df1a.v %>% dplyr::filter(ACC == 1) %>% dplyr::mutate(logRT = ifelse(RT>0, log(RT),0)) 
m1 <- lme4::lmer(logRT ~ Val_sh * Matchness + (1|Subject) + (1|Shape), df1a.v.rt)
m2 <- lme4::lmer(logRT ~ Val_sh * Matchness + (1|Subject), df1a.v.rt)
m3 <- lme4::lmer(logRT ~ Val_sh * Matchness, df1a.v.rt)
summary(m1)
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


get_prior(RT_sec ~ Val_sh * Matchness, 
          data = df1a.v.rt, 
          family = shifted_lognormal())

m6 <- brms::brm(RT_sec ~ Val_sh * Matchness, 
                data = df1a.v.rt, 
                family = shifted_lognormal(),
                chains = 4, cores = 4, warmup = 1000, iter = 2000,
                prior = prior_null,
                file = 'exp1a_null_log')

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
stanplot(m6, type = "hist")

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