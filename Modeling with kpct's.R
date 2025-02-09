## Modeling PA's end in a K using bat_kpct and pit_kpct

season_2024 <- read.csv("percentages_added.csv")

model <- glm(k ~ bat_kpct + pit_kpct,
             family = binomial,
             season_2024)
summary(model)




