library(deSolve)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
source("../R/simulate_sir_deterministic.R")

load("../data_processed/data_processed_d68_nvsn.rda")
load("../data_processed/data_processed_mobility_us.rda")
load("fit_nonpi.rda")

fit_nonpi_best <- fit_nonpi %>%
    filter(logLik==max(logLik))

mobility <- data.frame(
    year=c(1993, rep(1994:2023, each=52)),
    week=c(52, rep(1:52, 30)),
    time=0:1560,
    mobility=0
)

mobility[1361:(1361+nrow(data_processed_mobility_us)-1),"mobility"] <- data_processed_mobility_us$mean
# mobility[(1361+nrow(data_processed_mobility_us)):(nrow(mobility)),"mobility"] <- tail(data_processed_mobility_us$mean, 1)

mobilityfun <- function(x) {
    approx(x=mobility$time, y=mobility$mobility, xout=x)$y
}

R0 <- 20
scalevec <- exp(seq(log(0.5), log(2.4), length.out=31))

simlist <- vector("list", length(scalevec))

for (i in 1:length(scalevec)) {
    print(i)
    mobility_scale <- scalevec[i]

    ss <- simulate_sir_npi_mobility_deterministic(S0=1/R0, I0=1e-5, R0=R0, omega=2, tmax=30, phi=fit_nonpi_best$phi[[1]],
                                                  mobilityfun = mobilityfun,
                                                  mobility_scale = mobility_scale)

    ss2 <- ss %>%
        mutate(
            C=c(NA, diff(C))*fit_nonpi_best$scale[[1]]
        ) %>%
        filter(time > 24*52)

    ss2$year <- rep(2018:2023, each=52)
    ss2$week <- rep(1:52, 6)

    plot(ss2$year+ss2$week/52, ss2$C, type="l")
    points(data_processed_d68_nvsn$year+data_processed_d68_nvsn$week/52, data_processed_d68_nvsn$proxy)

    fitdata <- merge(data_processed_d68_nvsn, ss2)

    lfit <- lm(proxy~-1, data=fitdata,
               offset=C)

    ss$R0 <- R0
    ss$phi <- fit_nonpi_best$phi[[1]]

    ss$mobility_scale <- mobility_scale
    ss$logLik <- logLik(lfit)
    ss$sim <- i

    simlist[[i]] <- ss
}

fit_npi_mobility <- simlist %>%
    bind_rows

save("fit_npi_mobility", file="fit_npi_mobility.rda")
