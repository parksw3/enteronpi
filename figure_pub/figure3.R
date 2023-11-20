library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(tidyr)
library(dplyr)
library(deSolve)
library(egg)
library(gridExtra)
source("../R/simulate_sir_deterministic.R")

load("../data_processed/data_processed_mobility_us.rda")
load("../data_processed/data_processed_d68_nvsn.rda")
load("../fit/fit_npi_mobility_omega_R0_S0.rda")
load("../fit/fit_npi_beta.rda")
load("../fit/fit_nonpi.rda")

fit_npi_beta_best <- fit_npi_beta %>%
    filter(logLik==max(logLik))

ss_beta_2023 <- simulate_sir_npi_beta2_deterministic(S0=1/fit_npi_beta_best$R0[1], I0=1e-5, R0=fit_npi_beta_best$R0[1], omega=2, tmax=50, phi=fit_npi_beta_best$phi[1],
                                                     npistart=26*52,
                                                     npiend=28*52,
                                                     npiend2=29*52,
                                                     reduction = fit_npi_beta_best$reduction[1],
                                                     reduction_post = fit_npi_beta_best$reduction_post[1])

ss_beta_2024 <- simulate_sir_npi_beta2_deterministic(S0=1/fit_npi_beta_best$R0[1], I0=1e-5, R0=fit_npi_beta_best$R0[1], omega=2, tmax=50, phi=fit_npi_beta_best$phi[1],
                                                     npistart=26*52,
                                                     npiend=28*52,
                                                     npiend2=30*52,
                                                     reduction = fit_npi_beta_best$reduction[1],
                                                     reduction_post = fit_npi_beta_best$reduction_post[1])

ss_beta_2025 <- simulate_sir_npi_beta2_deterministic(S0=1/fit_npi_beta_best$R0[1], I0=1e-5, R0=fit_npi_beta_best$R0[1], omega=2, tmax=50, phi=fit_npi_beta_best$phi[1],
                                                     npistart=26*52,
                                                     npiend=28*52,
                                                     npiend2=31*52,
                                                     reduction = fit_npi_beta_best$reduction[1],
                                                     reduction_post = fit_npi_beta_best$reduction_post[1])

fit_npi_mobility_omega_R0_S0_logLik <- fit_npi_mobility_omega_R0_S0 %>%
    group_by(omega, R0, Reff) %>%
    summarize(
        logLik=unique(logLik)
    )

fit_npi_mobility_omega_R0_S0_best <- fit_npi_mobility_omega_R0_S0 %>%
    ungroup %>%
    filter(logLik==max(logLik))

fit_nonpi_best <- fit_nonpi %>%
    filter(logLik==max(logLik))

mobility_base <- data.frame(
    year=c(1993, rep(1994:2050, each=52)),
    week=c(52, rep(1:52, 57)),
    time=0:2964-1248,
    mobility=0
)

mobility_base[1361:(1361+nrow(data_processed_mobility_us)-1),"mobility"] <- data_processed_mobility_us$mean

mobility_2023 <- mobility_2024 <- mobility_2025 <- mobility_base

mobility_2023[(1361+nrow(data_processed_mobility_us)):1510,"mobility"] <-
    seq(tail(data_processed_mobility_us$mean, 1), 0, length.out=11)

mobility_2024[(1361+nrow(data_processed_mobility_us)):1562,"mobility"] <-
    seq(tail(data_processed_mobility_us$mean, 1), 0, length.out=63)

mobility_2025[(1361+nrow(data_processed_mobility_us)):1614,"mobility"] <-
    seq(tail(data_processed_mobility_us$mean, 1), 0, length.out=115)

mobilityfun_2023 <- function(x) {
    approx(x=mobility_2023$time, y=mobility_2023$mobility, xout=x)$y
}

mobilityfun_2024 <- function(x) {
    approx(x=mobility_2024$time, y=mobility_2024$mobility, xout=x)$y
}

mobilityfun_2025 <- function(x) {
    approx(x=mobility_2025$time, y=mobility_2025$mobility, xout=x)$y
}

ss_2023 <- simulate_sir_npi_mobility_deterministic(S0=1/fit_npi_mobility_omega_R0_S0_best$R0[1]*fit_npi_mobility_omega_R0_S0_best$Reff[1],
                                                   I0=1e-6, R0=fit_npi_mobility_omega_R0_S0_best$R0[1], omega=fit_npi_mobility_omega_R0_S0_best$omega[1],
                                                   tmax=50, phi=fit_nonpi_best$phi[[1]],
                                                   mobilityfun = mobilityfun_2023,
                                                   mobility_scale = 1) %>%
    mutate(
        group=(time/52+2018) >= (2020+8/52) & (time/52+2018) <= (2022+42/52),
        group=ifelse((time/52+2018) <= (2022+42/52), group, 2)
    )

ss_2024 <- simulate_sir_npi_mobility_deterministic(S0=1/fit_npi_mobility_omega_R0_S0_best$R0[1]*fit_npi_mobility_omega_R0_S0_best$Reff[1],
                                                   I0=1e-6, R0=fit_npi_mobility_omega_R0_S0_best$R0[1], omega=fit_npi_mobility_omega_R0_S0_best$omega[1],
                                                   tmax=50, phi=fit_nonpi_best$phi[[1]],
                                                   mobilityfun = mobilityfun_2024,
                                                   mobility_scale = 1) %>%
    mutate(
        group=(time/52+2018) >= (2020+8/52) & (time/52+2018) <= (2022+42/52),
        group=ifelse((time/52+2018) <= (2022+42/52), group, 2)
    )

ss_2025 <- simulate_sir_npi_mobility_deterministic(S0=1/fit_npi_mobility_omega_R0_S0_best$R0[1]*fit_npi_mobility_omega_R0_S0_best$Reff[1],
                                                   I0=1e-6, R0=fit_npi_mobility_omega_R0_S0_best$R0[1], omega=fit_npi_mobility_omega_R0_S0_best$omega[1],
                                                   tmax=50, phi=fit_nonpi_best$phi[[1]],
                                                   mobilityfun = mobilityfun_2025,
                                                   mobility_scale = 1) %>%
    mutate(
        group=(time/52+2018) >= (2020+8/52) & (time/52+2018) <= (2022+42/52),
        group=ifelse((time/52+2018) <= (2022+42/52), group, 2)
    )

rects <- data.frame(
    xmin=seq(2018, 2039, by=2),
    xmax=seq(2019, 2040, by=2)
)

g1a <- ggplot(ss_beta_2023) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_hline(yintercept=0, lty=2) +
    geom_line(aes(time/52-24+2018, mobility), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("% Transmission rate", limits=c(-40, 5)) +
    scale_color_manual(values=c("orange", "black", "orange")) +
    ggtitle("A. Assumption 1") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )

g1b <- ggplot(ss_beta_2023) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, S/2), lwd=1, col="purple") +
    geom_line(aes(time/52-24+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~.*2/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple"),
        axis.title.x = element_blank()
    )

g2a <- ggplot(ss_beta_2024) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_hline(yintercept=0, lty=2) +
    geom_line(aes(time/52-24+2018, mobility), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("% Transmission rate", limits=c(-40, 5)) +
    scale_color_manual(values=c("orange", "black", "orange")) +
    ggtitle("B") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )

g2b <- ggplot(ss_beta_2024) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, S/2), lwd=1, col="purple") +
    geom_line(aes(time/52-24+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~.*2/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple"),
        axis.title.x = element_blank()
    )

g3a <- ggplot(ss_beta_2025) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_hline(yintercept=0, lty=2) +
    geom_line(aes(time/52-24+2018, mobility), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("% Transmission rate", limits=c(-40, 5)) +
    scale_color_manual(values=c("orange", "black", "orange")) +
    ggtitle("C") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )

g3b <- ggplot(ss_beta_2025) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, S/2), lwd=1, col="purple") +
    geom_line(aes(time/52-24+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~.*2/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

g4a <- ggplot(ss_2023) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_hline(yintercept=0, lty=2) +
    geom_line(aes(time/52+2018, mobility), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("% Transmission rate", limits=c(-40, 5)) +
    scale_color_manual(values=c("orange", "black", "orange")) +
    ggtitle("D. Assumption 2") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )

g4b <- ggplot(ss_2023) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52+2018, S/2), lwd=1, col="purple") +
    geom_line(aes(time/52+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~.*2/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple"),
        axis.title.x = element_blank()
    )

g5a <- ggplot(ss_2024) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_hline(yintercept=0, lty=2) +
    geom_line(aes(time/52+2018, mobility), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("% Transmission rate", limits=c(-40, 5)) +
    scale_color_manual(values=c("orange", "black", "orange")) +
    ggtitle("E") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )

g5b <- ggplot(ss_2024) +
    geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), alpha=0.05) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52+2018, S/2), lwd=1, col="purple") +
    geom_line(aes(time/52+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~.*2/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple"),
        axis.title.x = element_blank()
    )

g6a <- ggplot(ss_2025) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_hline(yintercept=0, lty=2) +
    geom_line(aes(time/52+2018, mobility), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("% Transmission rate", limits=c(-40, 5)) +
    scale_color_manual(values=c("orange", "black", "orange")) +
    ggtitle("F") +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
    )

g6b <- ggplot(ss_2025) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52+2018, S/2), lwd=1, col="purple") +
    geom_line(aes(time/52+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1) +
    scale_x_continuous("Year", limits=c(2018, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~.*2/1e6,
                                           name="% Susceptible")) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

g1 <- ggarrange(g1a, g1b, nrow=2, heights=c(1, 2), draw=FALSE)

g2 <- ggarrange(g2a, g2b, nrow=2, heights=c(1, 2), draw=FALSE)

g3 <- ggarrange(g3a, g3b, nrow=2, heights=c(1, 2), draw=FALSE)

g4 <- ggarrange(g4a, g4b, nrow=2, heights=c(1, 2), draw=FALSE)

g5 <- ggarrange(g5a, g5b, nrow=2, heights=c(1, 2), draw=FALSE)

g6 <- ggarrange(g6a, g6b, nrow=2, heights=c(1, 2), draw=FALSE)

gcomb <- arrangeGrob(g1, g4,
                     g2, g5,
                     g3, g6,
                     nrow=3)

ggsave("figure3.pdf", gcomb, width=9, height=10)
