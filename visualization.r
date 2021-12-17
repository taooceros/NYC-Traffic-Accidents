accident_c <- accidents_full %>%
    group_by(type1) %>%
    mutate(has.injury = ifelse(
        (`NUMBER OF PERSONS INJURED` > 0 | `NUMBER OF PERSONS KILLED` > 0),
        1,
        0
    ))

accident_b <- accident_c %>%
    group_by(type1) %>%
    filter(type1 != "Unknown") %>%
    count(has.injury) %>%
    drop_na() %>%
    summarize(
        has.injury = has.injury,
        n = n,
        total = sum(n),
        percentage = n / total
    )

condition <- rep(c(0, 1), 10)
# type <- rep(accident_c$type1 %>% unique(), 2)
ggplot(
    accident_b,
    aes(
        fill = as.factor(condition),
        y = percentage, x = type1
    )
) +
    geom_bar(
        position = position_dodge(),
        stat = "identity"
    ) +
    ylab("Percentage of accidents having injured persons") +
    ggtitle(
        "Percentage of Accidents Having Injured Persons For Different Types of Vehicles"
    ) +
    guides(fill = guide_legend(title = "Has Injury")) +
    xlab("Vehicle Type")

accidents_injury_vehicle <- accidents_full %>%
    mutate(
        Ambulance_included = type1 == "Ambulance" |
            type2 == "Ambulance" |
            type3 == "Ambulance" |
            type4 == "Ambulance" |
            type5 == "Ambulance",
        Bike_included = type1 == "Bike" |
            type2 == "Bike" |
            type3 == "Bike" |
            type4 == "Bike" |
            type5 == "Bike",
        Bus_included = type1 == "Bus" |
            type2 == "Bus" |
            type3 == "Bus" |
            type4 == "Bus" |
            type5 == "Bus",
        Commercial_included = type1 == "Commercial" |
            type2 == "Commercial" |
            type3 == "Commercial" |
            type4 == "Commercial" |
            type5 == "Commercial",
        Motor_included = type1 == "Motor" |
            type2 == "Motor" |
            type3 == "Motor" |
            type4 == "Motor" |
            type5 == "Motor",
        Sedan_included = type1 == "Sedan" |
            type2 == "Sedan" |
            type3 == "Sedan" |
            type4 == "Sedan" |
            type5 == "Sedan",
        Special_included = type1 == "Special" |
            type2 == "Special" |
            type3 == "Special" |
            type4 == "Special" |
            type5 == "Special",
        Trail_included = type1 == "Trail" |
            type2 == "Trail" |
            type3 == "Trail" |
            type4 == "Trail" |
            type5 == "Trail",
        Van_included = type1 == "Van" |
            type2 == "Van" |
            type3 == "Van" |
            type4 == "Van" |
            type5 == "Van",
        Truck_included = type1 == "Truck" |
            type2 == "Truck" |
            type3 == "Truck" |
            type4 == "Truck" |
            type5 == "Truck",
    ) %>%
    select(
        `NUMBER OF PERSONS INJURED`, `NUMBER OF PERSONS KILLED`, starts_with("type"), ends_with("included")
    )

accident_included_injury <- accidents_injury_vehicle %>%
    pivot_longer(
        ends_with("included"),
        "type_included"
    ) %>%
    filter(value) %>%
    group_by(type_included) %>%
    drop_na() %>%
    summarise(
        n = sum(`NUMBER OF PERSONS INJURED` | `NUMBER OF PERSONS KILLED`),
        total = n(),
        percentage = n / total,
        percentage_opp = 1 - percentage
    ) %>%
    pivot_longer(
        names_to = "Injuried",
        starts_with("percentage"),
        values_to = "percentage"
    ) %>%
    mutate(
        Injuried = Injuried == "percentage"
    )


accident_included_injury %>%
    ggplot(
        aes(
            x = type_included
        )
    ) +
    geom_bar(
        aes(
            y = percentage,
            fill = Injuried
        ),
        position = position_dodge2(),
        stat = "identity",
        group = 1
    ) +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1)
    ) +
    ggtitle("Percentage of Accidents Having Injured Persons For Different Types of Vehicles Included in the accident") +
    ylab("Percentage of Accidents Having Injured Persons") +
    xlab("Vehicle Type") +
    theme_bw()


accidents_full %>%
    summarise(
        `NoFog` = sum(ifelse((Foggy + Heavy.Fog + Smoke), 0, 1), na.rm = T) / n(),
        Fog = sum(Foggy, na.rm = T) / n(),
        HeavyFog = sum(Heavy.Fog, na.rm = T) / n(),
        Smoke = sum(Smoke, na.rm = T) / n(),
        IceFog = sum(Ice.Fog, na.rm = T) / n(),
    ) %>%
    pivot_longer(
        everything(),
        names_to = "Fog Type",
        values_to = "Proportion"
    ) %>%
    ggplot() +
    geom_col(aes(x = `Fog Type`, y = Proportion), fill = "steelblue") +
    theme_bw() +
    ggtitle("Proportion of Accidents by Fog Type")


accidents_injury_factor <- accidents_full %>%
    mutate(
        `Crazy Driver Included` = `factor_type1` == "Crazy Driver" |
            `factor_type2` == "Crazy Driver" |
            `factor_type3` == "Crazy Driver" |
            `factor_type4` == "Crazy Driver" |
            `factor_type5` == "Crazy Driver",
        `Disobey Traffic Rule Included` = `factor_type1` == "Disobey Rule" |
            `factor_type2` == "Disobey Rule" |
            `factor_type3` == "Disobey Rule" |
            `factor_type4` == "Disobey Rule" |
            `factor_type5` == "Disobey Rule",
        `Distraction Included` = `factor_type1` == "Distraction" |
            `factor_type2` == "Distraction" |
            `factor_type3` == "Distraction" |
            `factor_type4` == "Distraction" |
            `factor_type5` == "Distraction",
        `Drug Included` = `factor_type1` == "Drug" |
            `factor_type2` == "Drug" |
            `factor_type3` == "Drug" |
            `factor_type4` == "Drug" |
            `factor_type5` == "Drug",
        `Environment Included` = `factor_type1` == "Environment" |
            `factor_type2` == "Environment" |
            `factor_type3` == "Environment" |
            `factor_type4` == "Environment" |
            `factor_type5` == "Environment",
        `Failure to Keep Right Included` = `factor_type1` == "Failure to Keep Right" |
            `factor_type2` == "Failure to Keep Right" |
            `factor_type3` == "Failure to Keep Right" |
            `factor_type4` == "Failure to Keep Right" |
            `factor_type5` == "Failure to Keep Right",
        `Fatigue Included` = `factor_type1` == "Fatigue" |
            `factor_type2` == "Fatigue" |
            `factor_type3` == "Fatigue" |
            `factor_type4` == "Fatigue" |
            `factor_type5` == "Fatigue",
        `Health Included` = `factor_type1` == "Health" |
            `factor_type2` == "Health" |
            `factor_type3` == "Health" |
            `factor_type4` == "Health" |
            `factor_type5` == "Health",
        `Majeure Included` = `factor_type1` == "Majeure" |
            `factor_type2` == "Majeure" |
            `factor_type3` == "Majeure" |
            `factor_type4` == "Majeure" |
            `factor_type5` == "Majeure",
        `Phone Held Included` = `factor_type1` == "Phone Held" |
            `factor_type2` == "Phone Held" |
            `factor_type3` == "Phone Held" |
            `factor_type4` == "Phone Held" |
            `factor_type5` == "Phone Held",
        `V Defection Included` = `factor_type1` == "V Defection" |
            `factor_type2` == "V Defection" |
            `factor_type3` == "V Defection" |
            `factor_type4` == "V Defection" |
            `factor_type5` == "V Defection"
    ) %>%
    select(
        `NUMBER OF PERSONS INJURED`, `NUMBER OF PERSONS KILLED`, starts_with("factor"), ends_with("Included")
    ) %>%
    pivot_longer(
        ends_with("Included"),
        "factor_type_included"
    ) %>%
    filter(value) %>%
    group_by(factor_type_included) %>%
    drop_na() %>%
    summarise(
        n = sum(`NUMBER OF PERSONS INJURED` | `NUMBER OF PERSONS KILLED`),
        total = n(),
        percentage = n / total,
        percentage_opp = 1 - percentage
    ) %>%
    pivot_longer(
        names_to = "Injuried",
        starts_with("percentage"),
        values_to = "percentage"
    ) %>%
    mutate(
        Injuried = Injuried == "percentage"
    )

accidents_injury_factor %>%
    ggplot(
        aes(
            x = factor_type_included
        )
    ) +
    geom_bar(
        aes(
            y = percentage,
            fill = Injuried
        ),
        position = position_dodge2(),
        stat = "identity",
        group = 1
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    ggtitle("Percentage of Accidents Having Injured Persons For Different Types of Contributing Factor Included in the accident") +
    ylab("Percentage of Accidents Having Injured Persons") +
    xlab("Contributing Factor")



accidents_full %>%
    filter(`NUMBER OF PERSONS INJURED` > 0) %>%
    summarise(
        `NoFog` = sum(ifelse(Foggy + Heavy.Fog + Smoke, 0, 1), na.rm = T) / n(),
        `Fog` = sum(Foggy, na.rm = T) / n(),
        `HeavyFog` = sum(Heavy.Fog, na.rm = T) / n(),
        `Smoke` = sum(Smoke, na.rm = T) / n(),
        `IceFog` = sum(Ice.Fog, na.rm = T) / n(),
    ) %>%
    pivot_longer(
        everything(),
        names_to = "Fog Type",
        values_to = "Proportion"
    ) %>%
    ggplot() +
    geom_col(
        aes(x = `Fog Type`, y = Proportion),
        fill = "steelblue", width = 0.5
    ) +
    theme_bw() +
    ggtitle("Proportion of Injury Accidents by Fog Type")


accidents_full %>%
    summarise(
        NoFog = mean(`NUMBER OF PERSONS INJURED` *
            (ifelse(Foggy + Heavy.Fog + Smoke, 0, 1)), na.rm = T),
        Fog = mean(`NUMBER OF PERSONS INJURED` * Foggy, na.rm = T),
        HeavyFog = mean(`NUMBER OF PERSONS INJURED` * Heavy.Fog, na.rm = T),
        Smoke = mean(`NUMBER OF PERSONS INJURED` * Smoke, na.rm = T),
        IceFog = mean(`NUMBER OF PERSONS INJURED` * Ice.Fog, na.rm = T)
    ) %>%
    pivot_longer(
        everything(),
        names_to = "Fog Type",
        values_to = "mean_injury"
    ) %>%
    ggplot(aes(`Fog Type`, mean_injury)) +
    geom_col(fill = "steelblue", width = 0.5) +
    xlab("Fog Type") +
    ylab("Mean Injury (Base on degree of fog)") +
    theme_bw() +
    ggtitle("Mean Injury by Fog Type")

accidents_full %>%
    filter(`NUMBER OF PERSONS INJURED` > 5) %>%
    group_by(DayLight) %>%
    summarise(
        DayLight = as.factor(DayLight),
        injury = `NUMBER OF PERSONS INJURED`
    ) %>%
    drop_na() %>%
    ggplot(aes(DayLight, injury)) +
    ylab("Injury") +
    geom_boxplot() +
    ggtitle("Box Plot of Injury by Daylight (Injury Number > 5)")


accidents_full %>%
    mutate(
        prcp_range = case_when(
            PRCP == 0 | is.na(PRCP) ~ "No Rain",
            PRCP < 0.4 ~ "Light Rain",
            PRCP < 1.0 ~ "Moderate Rain",
            PRCP < 2.0 ~ "Heavy Rain",
            PRCP < 4.0 ~ "Very Heavy Rain",
            PRCP < 10.0 ~ "Extreme Rain",
            T ~ "Super Extreme Rain"
        )
    ) %>%
    group_by(prcp_range) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = `prcp_range`, y = injury)
    ) +
    xlab("Precipitation Level") +
    ylab("Injury") +
    geom_col(fill = "steelblue")

(accidents_full) %>%
    drop_na(truck_involved) %>%
    group_by(truck_involved) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = truck_involved, y = injury)
    ) +
    xlab("truck involved") +
    ylab("Injury") +
    geom_col(fill = "steelblue")

(accidents_full) %>%
    drop_na(two_wheel_involved) %>%
    group_by(two_wheel_involved) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = two_wheel_involved, y = injury)
    ) +
    xlab("two wheel vehicle involved") +
    ylab("Injury") +
    geom_col(fill = "steelblue")

(train_df) %>%
    drop_na(unconcious_driver) %>%
    group_by(unconcious_driver) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = unconcious_driver, y = injury)
    ) +
    xlab("unconscious driver") +
    ylab("Injury") +
    geom_col(fill = "steelblue")

(train_df) %>%
    drop_na(crazy_driver) %>%
    group_by(crazy_driver) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = crazy_driver, y = injury)
    ) +
    xlab("crazy driver") +
    ylab("Injury") +
    geom_col(fill = "steelblue")


(accidents_full) %>%
    mutate(
        snow_range = case_when(
            SNOW == 0 | is.na(SNOW) ~ "No Snow",
            SNOW < 0.4 * 13 ~ "Light Snow",
            SNOW < 1.0 * 13 ~ "Moderate Snow",
            SNOW < 2.0 * 13 ~ "Heavy Snow",
            SNOW < 4.0 * 13 ~ "Very Heavy Snow",
            SNOW < 10.0 * 13 ~ "Extreme Snow",
            T ~ "Super Extreme Snow"
        )
    ) %>%
    group_by(snow_range) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = `snow_range`, y = injury)
    ) +
    xlab("Snow Level") +
    ylab("Injury") +
    geom_col(fill = "steelblue")

(accidents_full) %>%
    mutate(
        snow_range = case_when(
            SNOW == 0 | is.na(SNOW) ~ "No Snow",
            SNOW < 0.4 * 13 ~ "Light Snow",
            SNOW < 1.0 * 13 ~ "Moderate Snow",
            SNOW < 2.0 * 13 ~ "Heavy Snow",
            SNOW < 4.0 * 13 ~ "Very Heavy Snow",
            SNOW < 10.0 * 13 ~ "Extreme Snow",
            T ~ "Super Extreme Snow"
        )
    ) %>%
    group_by(snow_range) %>%
    summarise(injury = mean(`NUMBER OF PERSONS INJURED`, na.rm = T)) %>%
    ggplot(
        aes(x = `snow_range`, y = injury)
    ) +
    xlab("Snow Level") +
    ylab("Injury") +
    geom_col(fill = "steelblue")

(accidents_full[sample(nrow(accidents_full), 5000), ]) %>%
    ggplot(aes(PRCP, `NUMBER OF PERSONS INJURED`)) +
    geom_point()