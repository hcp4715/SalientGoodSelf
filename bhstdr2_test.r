## go through hbsdtr2
## Not really used it then

library(bhsdtr2)
library(rstan)

head(gabor[,-1])

gabor$r <- combined.response(gabor$stim, gabor$rating, gabor$acc)

gabor$r.binary <- combined.response(gabor$stim, accuracy = gabor$acc)
unique(gabor[order(gabor$r), c('stim', 'r.binary', 'acc', 'rating',  'r')])

# simple model: one condition, one subject's data
m <- bhsdtr(c(dprim ~ 1, thr ~ 1),
           r.binary ~ stim,
           gabor[gabor$order == 'DECISION-RATING' & gabor$duration == '32 ms' &
                         gabor$id == 1,])
samples(m, 'dprim')

## Here we introduce strong priors which imply that d' is near zero
m.alt <- set.prior(m, delta_prior_fixed_mu = log(.5), delta_prior_fixed_sd = .5)
m.alt <- fit(m.alt)
samples(m.alt, 'dprim')


## model with one within-subj variable (duration) and btw-subj variable (order)
m_1 <- bhsdtr(c(dprim ~ duration * order + (duration | id), thr ~ order + (1 | id)),
           r ~ stim, 
           gabor,
           method = 'stan')

samples(m_1, 'dprim')
summary(m_1)
m_1
