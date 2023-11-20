library(readxl)

percent_d68 <- read_xlsx("../data_raw/data_raw_d68_nvsn.xlsx", sheet=1)
percent_rvev <- read_xlsx("../data_raw/data_raw_d68_nvsn.xlsx", sheet=2)
percent_ari <- read_xlsx("../data_raw/data_raw_d68_nvsn.xlsx", sheet=3)

data_processed_d68_nvsn <- merge(merge(percent_d68, percent_rvev), percent_ari)

data_processed_d68_nvsn$proxy <- data_processed_d68_nvsn$percent_d68 *
    data_processed_d68_nvsn$percent_rvev *
    data_processed_d68_nvsn$percent_ari

save("percent_d68", "percent_rvev", "percent_ari",
     "data_processed_d68_nvsn",
     file="data_processed_d68_nvsn.rda")

write.csv(data_processed_d68_nvsn, file="data_processed_d68_nvsn.csv")
