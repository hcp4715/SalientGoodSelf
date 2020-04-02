#The following contains source code for the signal detection analyses reported
#In the supplement to Greene & Naveh-Benjamin. A Specificity Principle of Memory: Evidence from Aging and Associative Memory
#Information about this modeling approach is also available at https://vuorre.netlify.com/post/2017/10/09/bayesian-estimation-of-signal-detection-theory-models-part-1/ 

library(brms)


####EXPERIMENT 1

###Intact-Related discrimination 
#For this analysis, we retained only those trials where participants responded
#"intact" or "related" to Intact and Related probes; i.e., we dropped all Unrelated probe trials
#And all "unrelated" responses to Intact and Related probes, to bin the 3 x 3 response matrix
#Into a standard dichotomous format, where hits are defined as responding "intact" to Intact probes,
#Misses are responding "related" to Intact probes; correct rejections are responding "related" to Related probes,
#And false alarms are responding "intact" to Related probes.

e2 <- read.csv("exp1IntRel.csv") #Taken from countsExp1.csv
e2$Group <- factor(e2$Group) #0 = YA, 1 = OA
e2$Delay <- factor(e2$Delay) #0 = short, 1 = long
e2$Subj <- factor(e2$Subj) #Subject number
e2$Probe <- factor(e2$Probe) #0 = Unrelated, 1 = Intact

##First, tests for delay effects within each age group

#Effect of delay for YAs? 
e2.y <- subset(e2, Group==0, select=c(Subj, Delay, Probe, Resp))

e2yir1 <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
              family = bernoulli(link="probit"),
              prior = c(set_prior("normal(0,1)", class="b")),
              data=e2.y, chains = 3, 
              cores = 3, sample_prior =  TRUE,
              file = "e1yir1")
summary(e2yir1)
pp_check(e2yir1)

#Use the Savage-Dickey method to test for null effect of delay
hypothesis(e2yir1, "Probe1:Delay1=0") #Same d' across delay?
hypothesis(e2yir1, "Delay1=0") #Same criterion across delay?


#Effect of delay for OAs? 
e2.o <- subset(e2, Group==1, select=c(Subj, Delay, Probe, Resp))

e2oir1 <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
              family = bernoulli(link="probit"),
              prior = c(set_prior("normal(0,1)", class="b")),
              data=e2.o, chains = 3, 
              cores = 3, sample_prior =  TRUE,
              file = "e2oir1")
summary(e2oir1)
pp_check(e2oir1)

#Savage-Dickey method
hypothesis(e2oir1, "Probe1:Delay1=0") #Same d' across delay?
hypothesis(e2oir1, "Delay1=0") #Same criterion across delay?


#Next, test for an age effect
ageir1 <- brm(Resp ~ Probe*Group + (Probe | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=e2, chains = 3, 
              cores = 3, sample_prior =  TRUE,
              file = "ageir1.exp1")
summary(ageir1)
pp_check(ageir1)

#Use the Savage-Dickey method
hypothesis(ageir1, "Group1=0") #Same criterion for both age groups?
hypothesis(ageir1, "Probe1:Group1=0") #Same d' for both age groups?



#Marginal estimates of d' and c in for each age group within each delay

#YA - Short Delay
e2.ys <- subset(e2.y, Delay==0, select=c(Subj, Probe, Resp))

e2yirs1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=e2.ys, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "e2yirs1")
summary(e2yirs1)
pp_check(e2yirs1)

#YA - Long Delay
e2.yl <- subset(e2.y, Delay==1, select=c(Subj, Probe, Resp))

e2yirl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=e2.yl, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "e2yirl1")
summary(e2yirl1)
pp_check(e2yirl1)

#OA - Short Delay
e2.os <- subset(e2.o, Delay==0, select=c(Subj, Probe, Resp))

e2oirs1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=e2.os, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "e2oirs1")
summary(e2oirs1)
pp_check(e2oirs1)

#OA - Long Delay
e2.ol <- subset(e2.o, Delay==1, select=c(Subj, Probe, Resp))

e2oirl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=e2.ol, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "e2oirl1")
summary(e2oirl1)
pp_check(e2oirl1)


##Composite Retrieval SDT Analyses 
#These derive an estimate of overall retrieval (verbatim plus gist memory) by comparing
#Responses to Intact/Related probes versus Unrelated probes.
#Hits are responding either "intact" or "related" to Intact and Related probes
#Misses are responding "unrelated" to Intact and Related probes
#Correct rejections are responding "unrelated" to Unrelated probes
#False alarms are responding "intact" or "related" to Unrelated probes


g1 <- read.csv("exp1Gist.csv") #For resp, 1 = "intact" or "related", 0 = "unrelated
g1$Group <- factor(g1$Group) #0 = YA, 1 = OA
g1$Delay <- factor(g1$Delay) #0 = short, 1 = long
g1$Subj <- factor(g1$Subj) #Subject number
g1$Probe <- factor(g1$Probe) #0 = Unrelated, 1 = Intact/Related

#Start with within-subject delay effects

#Effect of delay for YAs? 
g1.y <- subset(g1, Group==0, select=c(Subj, Delay, Probe, Resp))

g1y2 <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
            family = bernoulli(link="probit"),
            prior = c(set_prior("normal(0,1)", class="b")),
            data=g1.y, chains = 3, 
            cores = 3, sample_prior =  TRUE,
            file = "g1y2")
summary(g1y2)
pp_check(g1y2)

#Savage-Dickey hypothesis tests
hypothesis(g1y2, "Probe1:Delay1=0") #Same d' across delay?
hypothesis(g1y2, "Delay1=0") #Same criterion across delay?


#Effect of delay for OAs? 
g1.o <- subset(g1, Group==1, select=c(Subj, Delay, Probe, Resp))

g1o1 <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
            family = bernoulli(link="probit"),
            prior = c(set_prior("normal(0,1)", class="b")),
            data=g1.o, chains = 3, 
            cores = 3, sample_prior =  TRUE,
            file = "g1o1")
summary(g1o1)
pp_check(g1o1)

#Savage-Dickey hypothesis tests
hypothesis(g1o1, "Probe1:Delay1=0") #Same d' across delay?
hypothesis(g1o1, "Delay1=0") #Same criterion across delay?


#Now test for a between-subject Age effect
g1age <- brm(Resp ~ Probe*Group + (Probe | Subj),
             family = bernoulli(link="probit"),
             prior=c(set_prior("normal(0,1)", class="b")),
             data=g1, chains = 3, 
             cores = 3, sample_prior =  TRUE,
             file = "g1age")
summary(g1age)
pp_check(g1age)

#Savage-Dickey hypothesis tests
hypothesis(g1age, "Group1=0") #Same criterion for both age groups?
hypothesis(g1age, "Probe1:Group1=0") #Same d' for both age groups?


##Marginal estimates of d' and c for each age group within each delay

#YA - Short Delay
g1.ys <- subset(g1.y, Delay==0, select=c(Subj, Probe, Resp))

g1ys1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
             family = bernoulli(link="probit"),
             prior = c(set_prior("normal(0,1)", class="b")),
             data=g1.ys, chains = 3, 
             cores = 3, sample_prior =  TRUE,
             file = "g1ys1")
summary(g1ys1)
pp_check(g1ys1)

#YA - Long Delay
g1.yl <- subset(g1.y, Delay==1, select=c(Subj, Probe, Resp))

g1yl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
             family = bernoulli(link="probit"),
             prior = c(set_prior("normal(0,1)", class="b")),
             data=g1.yl, chains = 3, 
             cores = 3, sample_prior =  TRUE,
             file = "g1yl1")
summary(g1yl1)
pp_check(g1yl1)

#OA - Short Delay
g1.os <- subset(g1.o, Delay==0, select=c(Subj, Probe, Resp))

g1os1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
             family = bernoulli(link="probit"),
             prior = c(set_prior("normal(0,1)", class="b")),
             data=g1.os, chains = 3, 
             cores = 3, sample_prior =  TRUE,
             file = "g1os1")
summary(g1os1)
pp_check(g1os1)

#OA - Long Delay
g1.ol <- subset(g1.o, Delay==1, select=c(Subj, Probe, Resp))

g1ol1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
             family = bernoulli(link="probit"),
             prior = c(set_prior("normal(0,1)", class="b")),
             data=g1.ol, chains = 3, 
             cores = 3, sample_prior =  TRUE,
             file = "g1ol1")
summary(g1ol1)
pp_check(g1os1)


###EXPERIMENT 2

#Intact-Related discrimination

p1 <- read.csv("exp2IRd.csv") #from countsExp2.csv
p1$Group <- factor(p1$Group) #0 = YA, 1 = OA
p1$Delay <- factor(p1$Delay) #0 = short, 1 = long
p1$Subj <- factor(p1$Subj) #Subject number
p1$Probe <- factor(p1$Probe) #0 = Related, 1 = Intact

##Delay effects?

#Effect of delay in YA?
p1.y <- subset(p1, Group==0, select=c(Subj, Delay, Probe, Resp))

e2yair1 <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
               family = bernoulli(link="probit"),
               prior=c(set_prior("normal(0,1)", class="b")),
               data=p1.y, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "e2yair1")
summary(e2yair1)

#Savage-Dickey hypothesis tests
hypothesis(e2yair1, "Delay1=0")
hypothesis(e2yair1, "Probe1:Delay1=0")

#Effect of delay in OA?
p1.o <- subset(p1, Group==1, select=c(Subj, Delay, Probe, Resp))

e2oair1 <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
               family = bernoulli(link="probit"),
               prior=c(set_prior("normal(0,1)", class="b")),
               data=p1.o, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "e2oair1")
summary(e2oair1)
pp_check(e2oair1)

#Savage-dickey tests
hypothesis(e2oair1, "Delay1=0")
hypothesis(e2oair1, "Probe1:Delay1=0")

##Between-subject effect of age?
p1age1 <- brm(Resp ~ Probe*Group + (Probe | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=p1, chains = 3, 
              cores = 3, sample_prior =  TRUE,
              file = "p1age1.exp2")
summary(p1age1)
pp_check(p1age1)

#Savage-Dickey hypothesis tests
hypothesis(p1age1, "Group1=0")
hypothesis(p1age1, "Probe1:Group1=0")

##Marginal estimates of d' and c for each age group within each delay

#YA - Short Delay
p1.ys <- subset(p1.y, Delay==0, select=c(Subj, Probe, Resp))

p1yirs1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=p1.ys, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "p1yirs1")
summary(p1yirs1)
pp_check(p1yirs1)

#YA - Long Delay
p1.yl <- subset(p1.y, Delay==1, select=c(Subj, Probe, Resp))

p1yirl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=p1.yl, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "p1yirl1")
summary(p1yirl1)
pp_check(p1yirl1)

#OA - Short Delay
p1.os <- subset(p1.o, Delay==0, select=c(Subj, Probe, Resp))

p1oirs1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=p1.os, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "p1oirs1")
summary(p1oirs1)
pp_check(p1oirs1)

#OA - Long Delay
p1.ol <- subset(p1.o, Delay==1, select=c(Subj, Probe, Resp))

p1oirl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=p1.ol, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "p1oirl1")
summary(p1oirl1)
pp_check(p1oirl1)


##Composite retrieval SDT analyses
#Model for 2 types of Unrelated probes

g3 <- read.csv("exp2gistU.csv")
g3$Group <- factor(g3$Group) #0 = YA, 1 = OA
g3$Delay <- factor(g3$Delay) #0 = short, 1 = long
g3$Subj <- factor(g3$Subj) #Subject number
g3$Type <- factor(g3$Type) #0 = Unr-opposite, 1 = Unr-within
g3$Probe <- factor(g3$Probe)

##First, we test for effects of type of Unrelated probe on d' and c within each age group

##Effect of type in YA
g3.y <- subset(g3, Group==0, select=c(Subj, Delay, Probe, Resp, Type))

g3y1 <- brm(Resp ~ Probe*Type + (Probe*Type | Subj),
            family = bernoulli(link="probit"),
            prior=c(set_prior("normal(0,1)", class="b")),
            data=g3.y, chains = 3, 
            cores = 3, sample_prior =  TRUE,
            file = "g3y1")
summary(g3y1) 
pp_check(g3y1)

#Savage-Dickey tests
hypothesis(g3y1, "Type1=0")
hypothesis(g3y1, "Probe:Type1=0")

##Effect of type in YA by delay
g3.ys <- subset(g3.y, Delay==0, select=c(Subj, Probe, Resp, Type))
g3.yl <- subset(g3.y, Delay==1, select=c(Subj, Probe, Resp, Type))

#short-delay
g3ys1 <- brm(Resp ~ Probe*Type + (Probe*Type | Subj),
             family = bernoulli(link="probit"),
             prior=c(set_prior("normal(0,1)", class="b")),
             data=g3.ys, chains = 3, 
             control = list(adapt_delta = .99),
             cores = 3, sample_prior =  TRUE,
             file = "g3ys1")
summary(g3ys1)
pp_check(g3ys1)

#Long-Delay
g3yl1 <- brm(Resp ~ Probe*Type + (Probe*Type | Subj),
             family = bernoulli(link="probit"),
             prior=c(set_prior("normal(0,1)", class="b")),
             data=g3.yl, chains = 3, 
             control = list(adapt_delta = .99),
             cores = 3, sample_prior =  TRUE,
             file = "g3yl1")
summary(g3yl1)
pp_check(g3ys1)

##Effect of type in OA
g3.o <- subset(g3, Group==1, select=c(Subj, Delay, Probe, Resp, Type))

g3oa1 <- brm(Resp ~ Probe*Type + (Probe*Type | Subj),
             family = bernoulli(link="probit"),
             prior=c(set_prior("normal(0,1)", class="b")),
             data=g3.o, chains = 3, 
             control = list(adapt_delta = .99),
             cores = 3, sample_prior =  TRUE,
             file = "g3oa1")
summary(g3oa1) 
pp_check(g3oa1)

#Savage-Dickey tests
hypothesis(g3oa1, "Type1=0")
hypothesis(g3oa1, "Probe1:Type1=0")

##Effect of type in OA by delay
g3.os <- subset(g3.o, Delay==0, select=c(Subj, Probe, Resp, Type))
g3.ol <- subset(g3.o, Delay==1, select=c(Subj, Probe, Resp, Type))

#Short delay
g3oas1 <- brm(Resp ~ Probe*Type + (Probe*Type | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=g3.os, chains = 3, 
              control = list(adapt_delta = .99),
              cores = 3, sample_prior =  TRUE,
              file = "g3oas1")
summary(g3oas1)
pp_check(g3oas1)

#Long delay
g3oal1 <- brm(Resp ~ Probe*Type + (Probe*Type | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=g3.ol, chains = 3, 
              control = list(adapt_delta = .99),
              cores = 3, sample_prior =  TRUE,
              file = "g3oal1")
summary(g3oal1)
pp_check(g3oal1)

##Now just focusing on the Unrelated-Opposite probes

g3.uo <- subset(g3, Type==0, select=c(Subj, Group, Delay, Probe, Resp))

##Delay effects

#Effect of delay in YA?
g3.uoy <- subset(g3.uo, Group==0, select=c(Subj, Delay, Probe, Resp))

g3uoyd <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=g3.uoy, chains = 3, 
              control = list(adapt_delta = .99),
              cores = 3, sample_prior =  TRUE,
              file = "g3uoyd")
summary(g3uoyd)
pp_check(g3uoyd)

#Savage-dickey tests
hypothesis(g3uoyd, "Delay1=0") 
hypothesis(g3uoyd, "Probe1:Delay1=0")

#Effect of delay in OA?
g3.uoo <- subset(g3.uo, Group==1, select=c(Subj, Delay, Probe, Resp))

g3uood <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=g3.uoo, chains = 3, 
              control = list(adapt_delta = .99),
              cores = 3, sample_prior =  TRUE,
              file = "g3uood")
summary(g3uood)
pp_check(g3uood)

#Savage-Dickey Hypothesis Tests
hypothesis(g3uood, "Delay1=0") 
hypothesis(g3uood, "Probe1:Delay1=0") 

##Age effects

g3.uoage <- brm(Resp ~ Probe*Group + (Probe | Subj),
                family = bernoulli(link="probit"),
                prior=c(set_prior("normal(0,1)", class="b")),
                data=g3.uo, chains = 3, 
                control = list(adapt_delta = .99),
                cores = 3, sample_prior =  TRUE,
                file = "g3uoage")
summary(g3.uoage)
pp_check(g3.uoage)

#Savage-Dickey hypothesis tests
hypothesis(g3.uoage, "Group1=0")
hypothesis(g3.uoage, "Probe1:Group1=0")


##Marginal estimates of d' and c in each delay by age

#YA Short Delay
g3.uoys <- subset(g3.uoy, Delay==0, select=c(Subj, Probe, Resp))

g3uoys1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uoys, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uoys1")
summary(g3uoys1)
pp_check(g3uoys1)

#YA Long Delay
g3.uoyl <- subset(g3.uoy, Delay==1, select=c(Subj, Probe, Resp))

g3uoyl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uoyl, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uoyl1")
summary(g3uoyl1)
pp_check(g3uoyl1)

#OA Short Delay
g3.uoos <- subset(g3.uoo, Delay==0, select=c(Subj, Probe, Resp))

g3uoos1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uoos, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uoos1")
summary(g3uoos1)
pp_check(g3uoos1)

#OA Long Delay
g3.uool <- subset(g3.uoo, Delay==1, select=c(Subj, Probe, Resp))

g3uool1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uool, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uool1")
summary(g3uool1)
pp_check(g3uool1)

##Now focusing on the Unrelated-Within probes only

g3.uw <- subset(g3, Type==1, select=c(Subj, Group, Delay, Probe, Resp))

##Delay effects
#Effect of delay in YA?
g3.uwy <- subset(g3.uw, Group==0, select=c(Subj, Delay, Probe, Resp))

g3uwyd <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=g3.uwy, chains = 3, 
              control = list(adapt_delta = .99),
              cores = 3, sample_prior =  TRUE,
              file = "g3uwyd")
summary(g3uwyd)
pp_check(g3uwyd)
#Savage-Dickey hypothesis tests (below)
hypothesis(g3uwyd, "Delay1=0") 
hypothesis(g3uwyd, "Probe1:Delay1=0") 

#Effect of delay in OA?
g3.uwo <- subset(g3.uw, Group==1, select=c(Subj, Delay, Probe, Resp))

g3uwod <- brm(Resp ~ Probe*Delay + (Probe*Delay | Subj),
              family = bernoulli(link="probit"),
              prior=c(set_prior("normal(0,1)", class="b")),
              data=g3.uwo, chains = 3, 
              control = list(adapt_delta = .99),
              cores = 3, sample_prior =  TRUE,
              file = "g3uwod")
summary(g3uwod)
pp_check(g3uwod)
#Savage-Dickey hypothesis tests
hypothesis(g3uwod, "Delay1=0") #5.38
hypothesis(g3uwod, "Probe1:Delay1=0") #4.80

##Age effect

g3.uwage <- brm(Resp ~ Probe*Group + (Probe | Subj),
                family = bernoulli(link="probit"),
                prior=c(set_prior("normal(0,1)", class="b")),
                data=g3.uw, chains = 3, 
                control = list(adapt_delta = .99),
                cores = 3, sample_prior =  TRUE,
                file = "g3uwage")
summary(g3.uwage)
pp_check(g3.uwage)
#Savage-Dickey Hypothesis Tests
hypothesis(g3.uwage, "Group1=0")
hypothesis(g3.uwage, "Probe1:Group1=0")

##Estimates of d' and c within each age group by delay

#YA Short Delay
g3.uwys <- subset(g3.uwy, Delay==0, select=c(Subj, Probe, Resp))

g3uwys1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uwys, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uwys1")
summary(g3uwys1)
pp_check(g3uwys1)

#YA Long Delay
g3.uwyl <- subset(g3.uwy, Delay==1, select=c(Subj, Probe, Resp))

g3uwyl1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uwyl, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uwyl1")
summary(g3uwyl1)
pp_check(g3uwyl1)

#OA Short Delay
g3.uwos <- subset(g3.uwo, Delay==0, select=c(Subj, Probe, Resp))

g3uwos1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uwos, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uwos1")
summary(g3uwos1)
pp_check(g3uwos1)

#OA Long Delay
g3.uwol <- subset(g3.uwo, Delay==1, select=c(Subj, Probe, Resp))

g3uwol1 <- brm(Resp ~ 1 + Probe + (1 + Probe | Subj),
               family = bernoulli(link="probit"),
               prior = c(set_prior("normal(0,1)", class="b")),
               data=g3.uwol, chains = 3, 
               cores = 3, sample_prior =  TRUE,
               file = "g3uwol1")
summary(g3uwol1)
pp_check(g3uwol1)