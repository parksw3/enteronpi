library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(dplyr)
library(egg)

load("../data_processed/data_processed_d68_nvsn.rda")
load("../data_processed/data_processed_d68_ili.rda")

g1 <- ggplot(filter(percent_ari)) +
    geom_line(aes(year+week/52, percent_ari)) +
    geom_point(aes(year+week/52, percent_ari), size=0.7) +
    geom_line(data=ili, aes(year+week/52, percent_ili*20), col="red", lty=2) +
    geom_point(data=ili, aes(year+week/52, percent_ili*20), col="red", size=0.7) +
    scale_x_continuous("Year", limits=c(2018, 2023)) +
    scale_y_continuous("% ARI", limits=c(0, NA),
                       sec.axis = sec_axis(trans=~./20,
                                           name="% ILI")) +
    ggtitle("A") +
    theme(
        panel.grid = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        axis.title.y.right = element_text(color="red"),
        axis.text.y.right = element_text(color="red"),
        axis.line.y.right = element_line(color="red"),
        axis.ticks.y.right = element_line(color="red")
    )

g2 <- ggplot(filter(percent_rvev)) +
    geom_line(aes(year+week/52, percent_rvev, group=as.factor(year))) +
    geom_point(aes(year+week/52, percent_rvev, group=as.factor(year)), size=0.7) +
    scale_x_continuous("Year", limits=c(2018, 2023)) +
    scale_y_continuous("% RV/EV", limits=c(0, NA)) +
    ggtitle("B") +
    theme(
        panel.grid = element_blank(),
        legend.position = "none"
    )

g3 <- ggplot(percent_d68) +
    geom_line(aes(year+week/52, percent_d68, group=as.factor(year))) +
    geom_point(aes(year+week/52, percent_d68, group=as.factor(year)), size=0.7) +
    scale_x_continuous("Year", limits=c(2018, 2023)) +
    scale_y_continuous("% D68", limits=c(0, NA)) +
    scale_linetype_discrete("Year") +
    scale_shape_manual("Year", values=c("circle", "triangle", "square", "square")) +
    ggtitle("C") +
    theme(
        panel.grid = element_blank(),
        legend.position = c(0.23, 0.8),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA)
    )

g4 <- ggplot(filter(data_processed_d68_nvsn)) +
    geom_line(aes(year+week/52, proxy, group=as.factor(year))) +
    geom_point(aes(year+week/52, proxy, group=as.factor(year)), size=0.7) +
    geom_line(data=data_processed_d68_ili, aes(year+week/52, proxy*20, group=as.factor(year)), col="red", lty=2) +
    scale_x_continuous("Year", limits=c(2018, 2023)) +
    scale_y_continuous("% D68 x % RV/EV x % ARI", limits=c(0, NA),
                       sec.axis = sec_axis(trans=~./20, name="% D68 x % RV/EV x % ILI")) +
    scale_linetype_discrete("Year") +
    scale_shape_manual("Year", values=c("circle", "triangle", "square", "square")) +
    ggtitle("D") +
    theme(
        panel.grid = element_blank(),
        legend.position = "none",
        axis.title.y.right = element_text(color="red"),
        axis.text.y.right = element_text(color="red"),
        axis.line.y.right = element_line(color="red"),
        axis.ticks.y.right = element_line(color="red")
    )

gcomb <- ggarrange(g1, g2, g3, g4, nrow=2, draw=FALSE)

ggsave("figure_data_processed_d68.pdf", gcomb, width=8, height=6)
