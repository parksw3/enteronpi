library(vroom)
library(dplyr)
library(lubridate)

stringency <- vroom("../data_raw/OxCGRT_USA_latest.csv")

stringency_us <- stringency %>%
    filter(is.na(RegionName)) %>%
    select(Date, StringencyIndex_Average, GovernmentResponseIndex_Average,
           ContainmentHealthIndex_Average)

data_processed_stringency_us <- stringency_us %>%
    mutate(
        Date=as.Date(as.character(Date), format="%Y%m%d")
    )

save("data_processed_stringency_us", file="data_processed_stringency_us.rda")
