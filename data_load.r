require(tidyverse)
require(lubridate)

set.seed(203741092)
truck_type1 <- read_csv("truck_type-1.csv") %>%
    rename(type1 = type)
truck_type2 <- read_csv("truck_type-2.csv") %>%
    rename(type2 = type)
truck_type3 <- read_csv("truck_type-3.csv") %>%
    rename(type3 = type)
truck_type4 <- read_csv("truck_type-4.csv") %>%
    rename(type4 = type)
truck_type5 <- read_csv("truck_type-5.csv") %>%
    rename(type5 = type)
factor1 <- read_csv("factor-1.csv") %>%
    rename(factor_type1 = factor_type)
factor2 <- read_csv("factor-2.csv") %>%
    rename(factor_type2 = factor_type)
factor3 <- read_csv("factor-3.csv") %>%
    rename(factor_type3 = factor_type)
factor4 <- read_csv("factor-4.csv") %>%
    rename(factor_type4 = factor_type)
factor5 <- read_csv("factor-5.csv") %>%
    rename(factor_type5 = factor_type)

accident <- read_csv("Motor_Vehicle_Collisions_-_Crashes.csv")

accident <- accident %>%
    mutate(
        type1 = truck_type1$type1,
        type2 = truck_type2$type2,
        type3 = truck_type3$type3,
        type4 = truck_type4$type4,
        type5 = truck_type5$type5,
        factor_type1 = factor1$factor_type1,
        factor_type2 = factor2$factor_type2,
        factor_type3 = factor3$factor_type3,
        factor_type4 = factor4$factor_type4,
        factor_type5 = factor5$factor_type5,
    )

weather12_14 <- read_csv("noaa12_14.csv")
weather15_17 <- read_csv("noaa15_17.csv")
weather18_21 <- read_csv("noaa18_21.csv")
weather12_21 <- weather12_14 %>%
    add_row(weather15_17) %>%
    add_row(weather18_21)


weather <- weather12_21 %>%
    select(-c(STATION, NAME, ELEVATION)) %>%
    arrange(DATE) %>%
    group_by(DATE) %>%
    summarise(across(AWND:WT22, ~ mean(as.numeric(.x), na.rm = TRUE))) %>%
    replace(is.na(.), 0) %>%
    rename(
        Foggy = WT01,
        Heavy.Fog = WT02,
        Ice.Fog = WT22,
        Ground.Fog = WT21,
        Mist = WT13,
        Thunder = WT03,
        Small.Hail = WT04,
        Hail = WT05,
        Rime = WT06,
        Smoke = WT08,
        Blowing.Snow = WT09,
        Snow = WT18,
        Tornado = WT10,
        Damaging.Wind = WT11,
        Drizzle = WT14,
        Freezing.Drizzle = WT15,
        Rain = WT16,
        Unknown.Precipitation = WT19
    )

accident <- accident %>%
    mutate(
        `CRASH DATE` = mdy(`CRASH DATE`)
    )

accident %>%
    summarize(
        `CRASH DATE min` = min(`CRASH DATE`),
        `CRASH DATE max` = max(`CRASH DATE`)
    )

accident %>%
    mutate(
        `CRASH MONTH` = month(`CRASH DATE`),
        `CRASH YEAR` = year(`CRASH DATE`)
    )