library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(tidyr)
library(dplyr)
library(deSolve)
library(egg)
source("../R/simulate_sir_deterministic.R")

load("../data_processed/data_processed_mobility_us.rda")
load("../data_processed/data_processed_d68_nvsn.rda")
load("../fit/fit_nonpi.rda")
load("../fit/fit_npi_beta.rda")
load("../fit/fit_npi_mobility_omega_R0_S0.rda")

fit_nonpi_best <- fit_nonpi %>%
    filter(logLik==max(logLik))

fit_npi_beta_filter <- fit_npi_beta %>%
    filter(logLik>max(logLik)-2) %>%
    group_by(sim) %>%
    mutate(
        C=c(NA, diff(C))
    )

fit_npi_beta_filter_param <- fit_npi_beta_filter %>%
    filter(time==0)

fit_npi_beta_filter_pred <- apply(fit_npi_beta_filter_param, 1, function(x) {
    out <- simulate_sir_npi_beta_deterministic(S0=1/x[["R0"]], I0=1e-5, R0=x[["R0"]], omega=2, tmax=50, phi=x[["phi"]],
                                        npistart=26*52,
                                        npiend=28*52,
                                        reduction = x[["reduction"]],
                                        reduction_post = x[["reduction_post"]])

    out$sim <- x[["sim"]]

    out
}) %>%
    bind_rows %>%
    group_by(sim) %>%
    mutate(
        C=c(NA, diff(C))
    )

g1 <- ggplot(fit_npi_beta_filter) +
    geom_vline(xintercept=2018:2023, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, (S-35000)/0.7, group=sim), col="purple") +
    geom_line(aes(time/52-24+2018, c(C*fit_nonpi_best$scale[1]), group=sim), col="black") +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy)) +
    scale_x_continuous("Year", limits=c(2018, 2023), expand=c(0, 0),
                       breaks=2018:2022) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~(.*0.7+35000)/1e6,
                                           name="% Susceptible")) +
    ggtitle("A") +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

g2 <- ggplot(fit_npi_beta_filter_pred) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, (S-35000)/0.7, group=sim), col="purple") +
    geom_line(aes(time/52-24+2018, c(C*fit_nonpi_best$scale[1]), group=sim), col="orange") +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy)) +
    scale_x_continuous("Year", limits=c(2023, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~(.*0.7+35000)/1e6,
                                           name="% Susceptible")) +
    ggtitle("B") +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

gcomb <- ggarrange(g1, g2, nrow=1, draw=FALSE,
          widths=c(0.7, 1.5))

ggsave("figure2_alternative.pdf", gcomb, width=8, height=3)
