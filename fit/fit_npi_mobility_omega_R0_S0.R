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
    time=0:1560-1248,
    mobility=0
)

mobility[1361:(1361+nrow(data_processed_mobility_us)-1),"mobility"] <- data_processed_mobility_us$mean
mobility[(1361+nrow(data_processed_mobility_us)):(nrow(mobility)),"mobility"] <- tail(data_processed_mobility_us$mean, 1)

mobilityfun <- function(x) {
    approx(x=mobility$time, y=mobility$mobility, xout=x)$y
}

omegavec <- c(0.1, 0.5, 1, 2)
R0vec <- seq(10, 25, length.out=21)
Reffvec <- seq(1.01, 1.2, length.out=21)
paramdata <- expand.grid(omegavec, R0vec, Reffvec)

simlist <- vector("list", nrow(paramdata))

for (i in 1:nrow(paramdata)) {
    print(i)
    pp <- paramdata[i,]
    omega <- pp[[1]]
    R0 <- pp[[2]]
    Reff <- pp[[3]]

    ss <- simulate_sir_npi_mobility_deterministic(S0=1/R0*Reff, I0=1e-6, R0=R0, omega=omega, tmax=6, phi=fit_nonpi_best$phi[[1]],
                                                  mobilityfun = mobilityfun,
                                                  mobility_scale = 1)

    ss2 <- ss %>%
        mutate(
            C=c(NA, diff(C))
        ) %>%
        filter(time > 0)

    ss2$year <- rep(2018:2023, each=52)
    ss2$week <- rep(1:52, 6)

    # plot(ss2$year+ss2$week/52, ss2$C*0.7, type="l")
    # points(data_processed_d68_nvsn$year+data_processed_d68_nvsn$week/52, data_processed_d68_nvsn$proxy)

    fitdata <- merge(data_processed_d68_nvsn, ss2)

    lfit <- lm(proxy~-1+C, data=fitdata)

    ss$omega <- omega
    ss$Reff <- Reff
    ss$R0 <- R0
    ss$phi <- fit_nonpi_best$phi[[1]]

    ss$scale <- coef(lfit)[[1]]
    ss$logLik <- logLik(lfit)
    ss$sim <- i

    simlist[[i]] <- ss
}

fit_npi_mobility_omega_R0_S0 <- simlist %>%
    bind_rows

save("fit_npi_mobility_omega_R0_S0", file="fit_npi_mobility_omega_R0_S0.rda")
