library(deSolve)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
source("../R/simulate_sir_deterministic.R")

load("../data_processed/data_processed_d68_nvsn.rda")
load("../fit/fit_nonpi.rda")

fit_nonpi_best <- fit_nonpi %>%
    filter(logLik==max(logLik))

R0 <- fit_nonpi_best$R0[1]
omega <- 2
reductionvec <- seq(0.2, 0.4, by=0.01)
reduction_postvec <- seq(0.2, 0.4, by=0.01)

paramdata <- expand.grid(reductionvec, reduction_postvec)

simlist <- vector("list", nrow(paramdata))

for (i in 1:nrow(paramdata)) {
    print(i)
    pp <- paramdata[i,]

    rr <- pp[[1]]
    rr2 <- pp[[2]]

    ss <- simulate_sir_npi_beta_deterministic(S0=1/R0, I0=1e-5, R0=R0,
                                              omega=2, tmax=30, phi=fit_nonpi_best$phi[1],
                                              npistart=26*52,
                                              npiend=28*52,
                                              reduction = rr,
                                              reduction_post = rr2)

    ss2 <- ss %>%
        mutate(
            C=c(NA, diff(C))*fit_nonpi_best$scale[1]
        ) %>%
        filter(time > 24*52)

    ss2$year <- rep(c(2018:2023), each=52)
    ss2$week <- rep(1:52, 6)

    fitdata <- merge(data_processed_d68_nvsn, ss2)

    lfit <- lm(proxy~-1, data=fitdata,
               offset=C)

    ss$R0 <- R0
    ss$phi <- fit_nonpi_best$phi[1]
    ss$reduction <- rr
    ss$reduction_post <- rr2

    ss$logLik <- logLik(lfit)
    ss$sim <- i

    simlist[[i]] <- ss
}

fit_npi_beta <- simlist %>%
    bind_rows

save("fit_npi_beta", file="fit_npi_beta.rda")
