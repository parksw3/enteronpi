library(deSolve)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
source("../R/simulate_sir_deterministic.R")

load("../data_processed/data_processed_d68_nvsn.rda")

R0 <- 20
omega <- 2
phivec <- seq(0.3, 0.5, by=0.01)

simlist <- vector("list", length(phivec))

for (i in 1:length(phivec)) {
    print(i)
    phi <- phivec[[i]]

    ss <- simulate_sir_nonpi_deterministic(S0=1/R0, I0=1e-5, R0=R0, omega=omega, tmax=30, phi=phi)

    ss2 <- ss %>%
        mutate(
            C=c(NA, diff(C))
        ) %>%
        filter(time > 24*52, time <= 25*52)

    ss2$year <- 2018
    ss2$week <- 1:52

    fitdata <- merge(data_processed_d68_nvsn, ss2)

    lfit <- lm(proxy~-1+C, data=fitdata)

    ss$R0 <- R0
    ss$phi <- phi

    ss$scale <- coef(lfit)[[1]]
    ss$logLik <- logLik(lfit)
    ss$sim <- i

    simlist[[i]] <- ss
}

fit_nonpi <- simlist %>%
    bind_rows

save("fit_nonpi", file="fit_nonpi.rda")
