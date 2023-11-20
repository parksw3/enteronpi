library(vroom)
library(dplyr)
library(lubridate)

mobility <- vroom("../data_raw/changes-visitors-covid.csv")

mobility_us <- mobility %>%
    filter(Entity=="United States") %>%
    mutate(
        year=epiyear(Day),
        week=epiweek(Day)
    )

data_processed_mobility_us <- mobility_us %>%
    group_by(year, week) %>%
    summarize(
        date=max(Day),
        retail_and_recreation=mean(retail_and_recreation),
        grocery_and_pharmacy=mean(grocery_and_pharmacy),
        transit_stations=mean(transit_stations),
        workplaces=mean(workplaces)
    ) %>%
    mutate(
        mean=(retail_and_recreation+grocery_and_pharmacy+transit_stations+workplaces)/4
    )

save("data_processed_mobility_us", file="data_processed_mobility_us.rda")
