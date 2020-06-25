
if(!"effsize" %in% rownames(installed.packages())) install.packages("effsize")
if(!"MBESS" %in% rownames(installed.packages())) install.packages("MBESS")
if(!"compute.es" %in% rownames(installed.packages())) install.packages("compute.es")

test.df <- read.csv('df4a.meta_d_w.csv')

## Mothed 0: JASP
# Cohen's d = 0.0095, 95% CI [-0.2457 0.2646]

## Method 1: using effsize and MBESS
d.stat <- effsize::cohen.d(test.df$Other_Neutral, test.df$Other_Bad, conf.level = 0.95,
                           paired=T, hedges.correction = FALSE)$estimate 
# output: 0.0050383

MBESS::ci.smd(smd = d.stat, n.1 = 59, n.2 = 59, conf.level = 0.95)
# gives 95% CI [-0.35583 0.36589]

## Method 2: Using t.test and compute.es
t.stat <- t.test(test.df$Other_Neutral, test.df$Other_Bad, paired = T)$statistic

compute.es::tes(t.stat, n.1 = 59, n.2 = 59, level = 95)
# gives cohen's d = 0.01, 95% CI of d [-0.35 0.38]

## Method 3: using formula from Cooper et al., 2009
d.sgpp <- function(m.1,m.2,sd.1,sd.2,n,r=.5){
        # m.1 = mean at time 1
        # m.2 = mean at time 2
        # sd.1 = standard dev. at time 1
        # sd.2 = standard dev. at time 2
        # n = sample size
        # r = correlation between time 1 and 2
        s.within <- (sqrt((sd.1^2 + sd.2^2)-2*r*sd.1*sd.2))/(sqrt(2*(1-r))) 
        d <- ((m.1 - m.2)/s.within)
        var.d <- 2*(1-r) * (1/n + d^2/(2*n))
        out <- cbind(d, var.d)
        return(out)
}

M1 <- mean(test.df$Other_Neutral)
M2 <- mean(test.df$Other_Bad)
SD1 <- sd(test.df$Other_Neutral)
SD2 <- sd(test.df$Other_Bad)
r_m1_m2 <- cor(test.df$Other_Neutral, test.df$Other_Bad)

# calculate the d and var.d:
coopers_d <- d.sgpp(M1, M2, SD1, SD2, length(test.df$Other_Neutral),r_m1_m2) 
# gives d = 0.0050383, var.d = 0.0047705

# Boundaries of CI
lower_CI_coopers <- coopers_d[1] - 1.96 * sqrt(coopers_d[1])    # -0.13408
higher_CI_coopers <- coopers_d[1] + 1.96 * sqrt(coopers_d[1])   # 0.14416
# gives 95% CI [-0.13408 0.14416]
