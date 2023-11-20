library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(tidyr)
library(dplyr)
library(egg)

load("../data_processed/data_processed_d68_nvsn.rda")
load("../fit/fit_npi_simple.rda")
load("../fit/fit_nonpi.rda")

fit_nonpi_best <- fit_nonpi %>%
    filter(logLik==max(logLik))

fit_npi_simple_summary <- fit_npi_simple %>%
    group_by(reduction, duration) %>%
    summarize(
        logLik=unique(logLik)
    )

g1 <- ggplot(fit_npi_simple_summary) +
    geom_tile(aes(duration, 100*reduction, fill=logLik)) +
    geom_point(data=filter(ungroup(fit_npi_simple_summary), logLik==max(logLik)), aes(duration, 100*reduction),
               color="white", size=5, shape=1, stroke=3) +
    scale_x_continuous("Duration of NPIs (years)", expand=c(0, 0)) +
    scale_y_continuous("Percent reduction in contact rates", expand=c(0, 0)) +
    scale_fill_viridis_c() +
    ggtitle("A")

fit_npi_simple_best <- fit_npi_simple %>%
    filter(logLik==max(logLik))

g2 <- ggplot(fit_npi_simple_best) +
    geom_vline(xintercept=2018:2022, col="gray", lty=2) +
    geom_line(aes(time/52-24+2018, c(NA, diff(C))*fit_nonpi_best$scale[1]), lwd=1) +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy)) +
    scale_x_continuous("Year", limits=c(2018, 2023.5), expand=c(0, 0),
                       breaks=2018:2023) +
    scale_y_continuous("% ARI x % RV/EV x % D68", limits=c(0, 150000), expand=c(0, 0)) +
    ggtitle("B") +
    theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()
    )

gcomb <- ggarrange(g1, g2, nrow=2, draw=FALSE)

ggsave("figure_fit_npi_simple.pdf", gcomb, width=8, height=8)
