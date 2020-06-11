require(ggdmc)
model <- BuildModel(
        p.map     = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1",
                         st0 = "1"),
        match.map = list(M = list(s1 = 1, s2 = 2)),
        factors   = list(S = c("s1", "s2")),
        constants = c(st0 = 0, sd_v = 1),
        responses = c("r1", "r2"),
        type      = "norm")

p.vector <- c(A = .75, B = 1.25, t0 = .15, mean_v.true = 2.5, mean_v.false = 1.5)
dat <- simulate(model, nsim = 30, ps = p.vector)
dmi <- BuildDMI(dat, model)  ## DMI stands for data model instance.

tibble::as_tibble(dmi)

match.map = list(M = list(left = "LEFT", right = "RIGHT"))
responses = c("LEFT", "RIGHT")
factors = list(S = c(“left”, “right”))

model <- BuildModel(p.map = list(A = "1", B = "1", t0 = "1", mean_v = "M",
                                 sd_v = "M", st0 = "1"),
                    constants = c(st0 = 0, sd_v.false = 1, mean_v.false = 0),
                    match.map = list(M = list(left = "LEFT", right = "RIGHT")),
                    factors   = list(S = c("left", "right")),
                    responses = c("LEFT", "RIGHT"),
                    type      = "norm")

pvec1 <- c(A = 1, B = 0, t0 = .2, mean_v.true = 1, sd_v.true = 0.66)
dat1  <- simulate(model, ps = pvec1, nsim = 1e4)
dmi1  <- BuildDMI(dat1, model)

library(dplyr)
dat1$C <- dat1$S == tolower(dat1$R)
d <- dplyr::tbl_df(dat1)
group_by(d, S, C) %>% summarize(m = mean(RT))

group_by(d, S, C) %>% summarize(m = length(RT) / 1e4)

library(data.table)
DT <- data.table(dat1)

DT[, .(MRT = round(mean(RT), 3)), .(S, C)]

prop <- DT[, .N, .(S, C)]
prop[, NN := sum(N), .(S)]
prop[, acc := round(N/NN, 2)]
prop


model <- BuildModel(
        p.map     = list(a = "1", v = "1", z = "1", d = "1", sz = "1", sv = "1",
                         t0 = "1", st0 = "1"),
        match.map = list(M = list(s1 = "r1", s2 = "r2")),
        factors   = list(S = c("s1", "s2")),
        responses = c("r1", "r2"),
        constants = c(st0 = 0, d = 0),
        type      = "rd")

p.vector <- c(a = 1, v = 1.2, z = .38, sz = .25, sv = .2, t0 = .15)
ntrial <- 1e2
dat <- simulate(model, nsim = ntrial, ps = p.vector)
dmi <- BuildDMI(dat, model)

p.prior  <- BuildPrior(
        dists = c(rep("tnorm", 2), "beta", "beta", "tnorm", "beta"),
        p1    = c(a = 1, v = 0, z = 1, sz = 1, sv = 1, t0 = 1),
        p2    = c(a = 1, v = 2, z = 1, sz = 1, sv = 1, t0 = 1),
        lower = c(0, -5, NA, NA, 0, NA),
        upper = c(5,  5, NA, NA, 5, NA))
plot(p.prior, ps = p.vector)

fit0 <- StartNewsamples(dmi, p.prior)
fit  <- run(fit0)
rhat <- gelman(fit, verbose = TRUE)
es   <- effectiveSize(fit)

p0 <- plot(fit0)
## p0 <- plot(fit0, start = 101)
p1 <- plot(fit)

p2 <- plot(fit, pll = FALSE, den= FALSE)
p3 <- plot(fit, pll = FALSE, den= TRUE)

est <- summary(fit, recover = TRUE, ps = p.vector, verbose = TRUE)

DIC(fit)
DIC(fit, BPIC=TRUE)

predict_one <- function(object, npost = 100, rand = TRUE, factors = NA,
                        xlim = NA, seed = NULL)
{
        require(ggdmc)
        if(packageVersion('ggdmc') == '0.2.6.0') {
                message('Using $ to extract object in v 0.2.6.0')
                out <- predict_one0260(object, npost = 100, rand, factors, xlim, seed)
        } else {
                message('Using @ to extract object in v 0.2.6.0')
                out <- predict_one0280(object, npost = 100, rand, factors, xlim, seed)
        }
        return(out)
}

predict_one0260 <- function(object, npost = 100, rand = TRUE, factors = NA,
                            xlim = NA, seed = NULL)
{
        model <- attr(object$data, 'model')
        facs <- names(attr(model, "factors")); 
        
        if (!is.null(factors))
        {
                if (any(is.na(factors))) factors <- facs
                if (!all(factors %in% facs))
                        stop(paste("Factors argument must contain one or more of:", paste(facs, collapse=",")))
        }
        
        resp <- names(attr(model, "responses"))
        ns   <- table(object$data[,facs], dnn = facs)
        npar   <- object$n.pars
        nchain <- object$n.chains
        nmc    <- object$nmc
        ntsample <- nchain * nmc
        pnames   <- object$p.names
        thetas <- matrix(aperm(object$theta, c(3,2,1)), ncol = npar)
        
        colnames(thetas) <- pnames
        
        if (is.na(npost)) {
                use <- 1:ntsample
        } else {
                if (rand) {
                        use <- sample(1:ntsample, npost, replace = F)
                } else {
                        ## Debugging purpose
                        use <- round(seq(1, ntsample, length.out = npost))
                }
        }
        
        npost  <- length(use)
        posts   <- thetas[use, ]
        nttrial <- sum(ns) ## number of total trials
        
        v <- lapply(1:npost, function(i) {
                ggdmc:::simulate_one(model, n = ns, ps = posts[i,], seed = seed)
        })
        
        out <- data.table::rbindlist(v)
        reps <- rep(1:npost, each = nttrial)
        out <- cbind(reps, out)
        
        if (!any(is.na(xlim)))
        {
                out <- out[RT > xlim[1] & RT < xlim[2]]
        }
        
        return(out)
}

predict_one0280 <- function(object, npost = 100, rand = TRUE, factors = NA,
                            xlim = NA, seed = NULL)
{
        ## Update for using S4 class
        model <- object@dmi@model
        facs <- names(attr(model, "factors")); 
        
        if (!is.null(factors))
        {
                if (any(is.na(factors))) factors <- facs
                if (!all(factors %in% facs))
                        stop(paste("Factors argument must contain one or more of:", paste(facs, collapse=",")))
        }
        
        
        resp <- names(attr(model, "responses")); 
        ns   <- table(object@dmi@data[,facs], dnn = facs); 
        npar   <- object@npar
        nchain <- object@nchain
        nmc    <- object@nmc; 
        ntsample <- nchain * nmc
        pnames   <- object@pnames
        
        thetas <- matrix(aperm(object@theta, c(3,2,1)), ncol = npar)
        colnames(thetas) <- pnames
        
        if (is.na(npost)) {
                use <- 1:ntsample
        } else {
                if (rand) {
                        use <- sample(1:ntsample, npost, replace = F)
                } else {
                        ## Debugging purpose
                        use <- round(seq(1, ntsample, length.out = npost))
                }
        }
        
        npost  <- length(use)
        posts   <- thetas[use, ]
        nttrial <- sum(ns) ## number of total trials
        
        v <- lapply(1:npost, function(i) {
                ggdmc:::simulate_one(model, n = ns, ps = posts[i,], seed = seed)
        })
        
        out <- data.table::rbindlist(v)
        reps <- rep(1:npost, each = nttrial)
        out <- cbind(reps, out)
        
        if (!any(is.na(xlim)))
        {
                out <- out[RT > xlim[1] & RT < xlim[2]]
        }
        
        return(out)
}


pp  <- predict_one(fit, xlim = c(0, 5))
## dat <- fit@dmi@data  ## use this line for version > 0.2.7.5
dat <- fit$data   ## use this line for version 0.2.6.0

dat$C <- ifelse(dat$S == "s1"  & dat$R == "r1",  TRUE,
                ifelse(dat$S == "s2" & dat$R == "r2", TRUE,
                       ifelse(dat$S == "s1"  & dat$R == "r2", FALSE,
                              ifelse(dat$S == "s2" & dat$R == "r1",  FALSE, NA))))
pp$C <- ifelse(pp$S == "s1"  & pp$R == "r1",  TRUE,
               ifelse(pp$S == "s2" & pp$R == "r2", TRUE,
                      ifelse(pp$S == "s1"  & pp$R == "r2", FALSE,
                             ifelse(pp$S == "s2" & pp$R == "r1",  FALSE, NA))))

dat$reps <- NA
dat$type <- "Data"
pp$reps <- factor(pp$reps)
pp$type <- "Simulation"

DT <- rbind(dat, pp)

require(ggplot2)
p1 <- ggplot(DT, aes(RT, color = reps, size = type)) +
        geom_freqpoly(binwidth = .05) +
        scale_size_manual(values = c(1, .3)) +
        scale_color_grey(na.value = "black") +
        theme(legend.position = "none") +
        facet_grid(S ~ C)
p1
