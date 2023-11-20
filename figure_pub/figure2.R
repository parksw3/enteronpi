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

fit_npi_beta_best <- fit_npi_beta %>%
    filter(logLik==max(logLik))

fit_npi_beta_logLik <- fit_npi_beta %>%
    group_by(reduction, reduction_post) %>%
    summarize(
        logLik=unique(logLik)
    )

g1_logLik <- ggplot(fit_npi_beta_logLik) +
    geom_tile(aes(100*reduction, 100*reduction_post, fill=logLik)) +
    geom_point(data=filter(ungroup(fit_npi_beta_logLik), logLik==max(logLik)), aes(100*reduction,
                                                                                      100*reduction_post),
               color="white", size=5, shape=1, stroke=3) +
    scale_x_continuous("Percent reduction in contact rates in 2020-2021", expand=c(0, 0)) +
    scale_y_continuous("Percent reduction in contact rates in 2022", expand=c(0, 0)) +
    scale_fill_viridis_c()

ggsave("figure2_logLik_1.pdf", g1_logLik, width=6, height=4)

ss <- simulate_sir_npi_beta_deterministic(S0=1/fit_npi_beta_best$R0[1], I0=1e-5, R0=fit_npi_beta_best$R0[1], omega=2, tmax=30, phi=fit_npi_beta_best$phi[1],
                                          npistart=26*52,
                                          npiend=28*52,
                                          reduction = fit_npi_beta_best$reduction[1],
                                          reduction_post = fit_npi_beta_best$reduction_post[1])

ss2 <- simulate_sir_npi_beta_deterministic(S0=1/fit_npi_beta_best$R0[1], I0=1e-5, R0=fit_npi_beta_best$R0[1], omega=2, tmax=50, phi=fit_npi_beta_best$phi[1],
                                           npistart=26*52,
                                           npiend=28*52,
                                           reduction = fit_npi_beta_best$reduction[1],
                                           reduction_post = fit_npi_beta_best$reduction_post[1])

fit_npi_mobility_omega_R0_S0_logLik <- fit_npi_mobility_omega_R0_S0 %>%
    group_by(omega, R0, Reff) %>%
    summarize(
        logLik=unique(logLik)
    ) %>%
    mutate(
        omega=paste0("omega==",omega)
    )

g3_logLik <- ggplot(fit_npi_mobility_omega_R0_S0_logLik) +
    geom_tile(aes(R0, Reff, fill=logLik)) +
    geom_point(data=filter(ungroup(fit_npi_mobility_omega_R0_S0_logLik), logLik==max(logLik)), aes(R0,
                                                                                   Reff),
               color="white", size=5, shape=1, stroke=3) +
    scale_x_continuous("Basic reproduction number", expand=c(0, 0)) +
    scale_y_continuous("Effective reproduction number at the beginning of 2018", expand=c(0, 0)) +
    scale_fill_viridis_c() +
    facet_wrap(~omega, labeller = label_parsed)

ggsave("figure2_logLik_2.pdf", g3_logLik, width=8, height=8)

fit_npi_mobility_omega_R0_S0_best <- fit_npi_mobility_omega_R0_S0 %>%
    ungroup %>%
    filter(logLik==max(logLik))

which_match1 <- match(data_processed_d68_nvsn$year + data_processed_d68_nvsn$week/52, ss$time/52-24+2018)

qq_1 <- as.data.frame(qqnorm((diff(ss$C)*fit_nonpi_best$scale[1])[which_match1]-data_processed_d68_nvsn$proxy))

g1_qq <- ggplot(qq_1) +
    geom_point(aes(x, y)) +
    geom_qq_line(aes(sample=y), lty=2) +
    xlab("Theoretical quantiles") +
    ylab("Residual quantiles") +
    ggtitle("A. Assumption 1") +
    theme(
        panel.grid = element_blank()
    )

g1 <- ggplot(ss) +
    geom_vline(xintercept=2018:2023, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, (S-35000)/0.7), lwd=1, col="purple") +
    geom_line(aes(time/52-24+2018, c(NA, diff(C)*fit_nonpi_best$scale[1])), lwd=1, col="black") +
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

g2 <- ggplot(ss2) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, (S-35000)/0.7), lwd=1, col="purple") +
    geom_line(aes(time/52-24+2018, c(NA, diff(C)*fit_nonpi_best$scale[1])), lwd=1, col="orange") +
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

g3 <- ss2 %>%
    mutate(
        year=time/52-24+2018,
        group=year <= 2022,
        group=factor(group,
                     levels=c(TRUE, FALSE),
                     labels=c("2018-2022", "2023-")),
        type="Assumption 1"
    ) %>%
    filter(year >= 2018) %>%
    ggplot() +
    geom_path(aes(S/1e6, I/1e6, col=group)) +
    scale_x_continuous("Susceptibles", limits=c(0.038, 0.075), expand=c(0, 0)) +
    scale_y_continuous("Infected", limits=c(0, 0.0039), expand=c(0, 0)) +
    scale_color_manual(values=c("black", "orange")) +
    ggtitle("C") +
    facet_grid(type~.) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_rect(size=1),
        legend.title = element_blank(),
        legend.position = c(0.4, 0.85),
        legend.background = element_rect(fill="transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size=12)
    )

which_match2 <- match(data_processed_d68_nvsn$year + data_processed_d68_nvsn$week/52, fit_npi_mobility_omega_R0_S0_best$time/52+2018)

qq_2 <- as.data.frame(qqnorm((diff(fit_npi_mobility_omega_R0_S0_best$C*fit_npi_mobility_omega_R0_S0_best$scale))[which_match2]-data_processed_d68_nvsn$proxy))

g2_qq <- ggplot(qq_2) +
    geom_point(aes(x, y)) +
    geom_qq_line(aes(sample=y), lty=2) +
    xlab("Theoretical quantiles") +
    ylab("Residual quantiles") +
    ggtitle("B. Assumption 2") +
    theme(
        panel.grid = element_blank()
    )

g4 <- ggplot(fit_npi_mobility_omega_R0_S0_best) +
    geom_vline(xintercept=2018:2023, col="gray", lty=2) +
    geom_line(aes(time/52+2018, (S-35000)/0.7), lwd=1, col="purple") +
    geom_line(aes(time/52+2018, c(NA, diff(C))*scale), lwd=1, col="black") +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy)) +
    scale_x_continuous("Year", limits=c(2018, 2023), expand=c(0, 0),
                       breaks=2018:2022) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~(.*0.7+35000)/1e6,
                                           name="% Susceptible")) +
    ggtitle("D") +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

mobility <- data.frame(
    year=c(1993, rep(1994:2050, each=52)),
    week=c(52, rep(1:52, 57)),
    time=0:2964-1248,
    mobility=0
)

mobility[1361:(1361+nrow(data_processed_mobility_us)-1),"mobility"] <- data_processed_mobility_us$mean
mobility[(1361+nrow(data_processed_mobility_us)):(nrow(mobility)),"mobility"] <- tail(data_processed_mobility_us$mean, 1)

mobilityfun <- function(x) {
    approx(x=mobility$time, y=mobility$mobility, xout=x)$y
}

ss3 <- simulate_sir_npi_mobility_deterministic(S0=1/fit_npi_mobility_omega_R0_S0_best$R0[1]*fit_npi_mobility_omega_R0_S0_best$Reff[1],
                                              I0=1e-6, R0=fit_npi_mobility_omega_R0_S0_best$R0[1], omega=fit_npi_mobility_omega_R0_S0_best$omega[1],
                                              tmax=50, phi=fit_nonpi_best$phi[[1]],
                                              mobilityfun = mobilityfun,
                                              mobility_scale = 1)

g5 <- ggplot(ss3) +
    geom_vline(xintercept=2018:2039, col="gray", lty=2) +
    geom_line(aes(time/52+2018, (S-35000)/0.7), lwd=1, col="purple") +
    geom_line(aes(time/52+2018, c(NA, diff(C))*fit_npi_mobility_omega_R0_S0_best$scale[1]), lwd=1, col="orange") +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy)) +
    scale_x_continuous("Year", limits=c(2023, 2040), expand=c(0, 0),
                       breaks=seq(2018, 2039, by=2)) +
    scale_y_continuous("Incidence proxy", limits=c(0, 55000), expand=c(0, 0),
                       sec.axis = sec_axis(~(.*0.7+35000)/1e6,
                                           name="% Susceptible")) +
    ggtitle("E") +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.line.y.right = element_line(color="purple"),
        axis.title.y.right = element_text(color="purple"),
        axis.text.y.right = element_text(color="purple")
    )

g6 <- ss3 %>%
    mutate(
        year=time/52+2018,
        group=year <= 2022,
        group=factor(group,
                     levels=c(TRUE, FALSE),
                     labels=c("2018-2022", "2023-")),
        type="Assumption 2"
    ) %>%
    ggplot() +
    geom_path(aes(S/1e6, I/1e6, col=group)) +
    scale_x_continuous("Susceptibles", limits=c(0.038, 0.075), expand=c(0, 0)) +
    scale_y_continuous("Infected", limits=c(0, 0.0039), expand=c(0, 0)) +
    scale_color_manual(values=c("black", "orange")) +
    ggtitle("F") +
    facet_grid(type~.) +
    theme(
        panel.grid = element_blank(),
        panel.border = element_rect(size=1),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size=12)
    )

gcomb <- ggarrange(g1, g2, g3, g4, g5, g6, nrow=2, draw=FALSE,
                   widths=c(0.7, 1.5, 0.5))

ggsave("figure2.pdf", gcomb, width=10, height=6)

gcomb2 <- ggarrange(g1_qq, g2_qq, nrow=1)

ggsave("figure2_qq.pdf", gcomb2, width=8, height=4)
