library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(tidyr)
library(dplyr)
library(deSolve)
library(egg)
source("../R/simulate_sir_deterministic.R")

load("../data_processed/data_processed_mobility_us.rda")
load("../data_processed/data_processed_d68_nvsn.rda")
load("../fit/fit_nonpi.rda")
load("../fit/fit_npi_mobility.rda")

fit_nonpi_best <- fit_nonpi %>%
    filter(logLik==max(logLik))

fit_npi_mobility2 <- fit_npi_mobility %>%
    mutate(
        C=c(NA, diff(C))
    )

fit_npi_mobility_best <- fit_npi_mobility2 %>%
    filter(logLik==max(logLik))

g1 <- ggplot(fit_npi_mobility_best) +
    geom_vline(xintercept=2018:2023, col="gray", lty=2) +
    geom_line(data=fit_npi_mobility2, aes(time/52-24+2018, (S-35000)/0.7, group=sim), lwd=1, col="purple", alpha=0.1) +
    geom_line(aes(time/52-24+2018, (S-35000)/0.7), lwd=1, col="purple") +
    geom_line(data=fit_npi_mobility2, aes(time/52-24+2018, c(C*fit_nonpi_best$scale[1]), group=sim), col="black", alpha=0.1) +
    geom_line(aes(time/52-24+2018, c(C*fit_nonpi_best$scale[1])), lwd=1, col="black") +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy)) +
    scale_x_continuous("Year", limits=c(2018, 2023), expand=c(0, 0),
                       breaks=2018:2022) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~(.*0.7+35000)/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

ggsave("figure2_biennial_mob.pdf", g1, width=6, height=4)
