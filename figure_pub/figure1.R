library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(tidyr)
library(dplyr)
library(zoo)
library(egg)

load("../data_processed/data_processed_d68_nvsn.rda")
load("../data_processed/data_processed_mobility_us.rda")

d68_prev <- read.csv("../data_raw/entero_data_US_by_month.csv")

scale <- max(filter(data_processed_d68_nvsn, year==2018)$proxy)/max(filter(d68_prev, year==2018)$prop)

g1 <- ggplot(d68_prev) +
    geom_bar(aes(year+month/12, prop), fill="gray50", lwd=1, stat="identity") +
    geom_vline(xintercept=2014:2023, lty=3) +
    geom_line(data=data_processed_d68_nvsn, aes(year+week/52, proxy/scale, group=as.factor(year)))  +
    geom_point(data=data_processed_d68_nvsn, aes(year+week/52, proxy/scale, group=as.factor(year)), size=0.7) +
    scale_x_continuous("Year", limits=c(2014, 2023), expand=c(0, 0),
                       breaks=2014:2022) +
    scale_y_continuous("Monthly % PER predicted D68 cases", limits=c(0, 0.49),
                       expand=c(0, 0),
                       sec.axis = sec_axis(trans=~.*scale, name="Weekly % ARI x % RV/EV x % D68")) +
    ggtitle("A") +
    theme(
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank()
    )

g2 <- ggplot(data_processed_mobility_us) +
    geom_vline(xintercept=c(as.Date("2014-01-01"),
                            as.Date("2015-01-01"),
                            as.Date("2016-01-01"),
                            as.Date("2017-01-01"),
                            as.Date("2018-01-01"),
                            as.Date("2019-01-01"),
                            as.Date("2020-01-01"),
                            as.Date("2021-01-01"),
                            as.Date("2022-01-01")), lty=3) +
    geom_hline(yintercept=0, lty=2) +
    geom_point(aes(date, mean), size=0.7) +
    geom_line(aes(date, mean)) +
    scale_x_date("Year", limits=c(as.Date("2014-01-01"), as.Date("2022-12-31")),
                 expand=c(0, 0),
                 breaks=c(as.Date("2014-01-01"),
                          as.Date("2015-01-01"),
                          as.Date("2016-01-01"),
                          as.Date("2017-01-01"),
                          as.Date("2018-01-01"),
                          as.Date("2019-01-01"),
                          as.Date("2020-01-01"),
                          as.Date("2021-01-01"),
                          as.Date("2022-01-01")),
                 labels=2014:2022) +
    scale_y_continuous("% Mean Google mobility") +
    scale_color_viridis_d(end=0.9) +
    ggtitle("B") +
    theme(
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank()
    )

gcomb <- ggarrange(g1, g2, nrow=2, draw=FALSE)

ggsave("figure1.pdf", gcomb, width=8, height=6)
