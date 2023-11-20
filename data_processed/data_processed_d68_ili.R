library(readxl)
library(dplyr)

percent_d68 <- read_xlsx("../data_raw/data_raw_d68_nvsn.xlsx", sheet=1)
percent_rvev <- read_xlsx("../data_raw/data_raw_d68_nvsn.xlsx", sheet=2)
ili <- read.csv("../data_raw/data_raw_ILINet.csv", skip=1) %>%
    select(YEAR, WEEK, X..WEIGHTED.ILI) %>%
    rename(
        year=YEAR,
        week=WEEK,
        percent_ili=X..WEIGHTED.ILI
    )

data_processed_d68_ili <- merge(merge(percent_d68, percent_rvev), ili) %>%
    mutate(
        proxy=percent_d68 * percent_rvev * percent_ili
    )

save("ili",
     "data_processed_d68_ili",
     file="data_processed_d68_ili.rda")

write.csv(data_processed_d68_ili, file="data_processed_d68_ili.csv")
