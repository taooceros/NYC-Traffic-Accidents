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
