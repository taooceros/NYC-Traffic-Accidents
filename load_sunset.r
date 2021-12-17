require(tidyverse)
sunset <- read_csv("sunset1.csv")
for (i in 2:12) {
    sunset <- sunset %>% add_row(read_csv(paste0("sunset", i, ".csv")))
}

sunset <- sunset %>%
    mutate(
        Day = paste("2020", str_replace(Day, "^.*,", "")),
        Date = ymd(Day),
        month = month(Date),
        day = day(Date)
    )

accidents <- accident %>%
    mutate(
        month = month(`CRASH DATE`),
        day = day(`CRASH DATE`)
    ) %>%
    full_join(sunset, by = c("month", "day")) %>%
    mutate(
        hour = hour(`CRASH TIME`),
        minute = minute(`CRASH TIME`),
        Sunrise = hms(Sunrise),
        Sunset = hms(Sunset)
    ) %>%
    mutate(
        sunrise_hour = hour(Sunrise),
        sunrise_minute = minutes(Sunrise),
        sunset_hour = hour(Sunset),
        sunset_minute = minute(Sunset)
    ) %>%
    mutate(
        DayLight = ifelse(
            (hour >= sunrise_hour & hour < sunset_hour) |
                (hour == sunrise_hour & minute >= sunrise_minute) |
                (hour == sunset_hour & minute <= sunset_minute),
            1,
            0
        )
    )