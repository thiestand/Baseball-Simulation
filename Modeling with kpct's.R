## Modeling PA's end in a K using bat_kpct and pit_kpct

# Set minimum PA

pa_min <- 100

season_2024 <- read.csv("percentages_added.csv")

# Filter out players without minimum PA

model <- season_2024 |>
  filter(bat_pa >= pa_min,
         pit_pa >= pa_min) |>
  glm(k ~ bat_kpct + pit_kpct,
             family = binomial,
             data = _)
summary(model)


# Predict for batter/pitcher combo

predict(model, 
        newdata= data.frame(bat_kpct = .30, pit_kpct = .17),
        type = "response")


# Create a grid of k percents in 5% increments

pitch <- seq(0.05, 0.4, 0.05)
bat <- seq(0.05, 0.4, 0.05)

data <- data.frame(pitch = pitch,
                   bat = bat) |>
  expand(pitch, bat) |>
  mutate(k_pct = predict(model, 
                         newdata = data.frame(bat_kpct = bat, pit_kpct = pitch),
                         type = "response"))

ggplot(filter(data, pitch > 0.05), aes(x = pitch, y = bat, fill = k_pct)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgray", high = "darkblue") +
  geom_label(aes(label = round(k_pct, 2)), color = "white") +
  theme_minimal() +
  labs(title = "Predicted K%",
       x = "Pitcher K%",
       y = "Batter K%",
       fill = "Predicted\nK%",
       subtitle = "Based on logistic regression model")


# Observed K% graph rounded to nearest 5% (at least 100 PAs)

season_2024 |>
  filter(bat_pa >= pa_min,
         pit_pa >= pa_min) |>
  mutate(pit_kpct = round(pit_kpct * 20) / 20,
         bat_kpct = round(bat_kpct * 20) / 20) |>
  summarize(k_rate = round(mean(k), 2),
            n = n(),
            .by = c(pit_kpct, bat_kpct)) |>
  filter(n > 100) |>
  ggplot(aes(x = pit_kpct, y = bat_kpct, fill = k_rate)) +
  geom_tile() +
  geom_label(aes(label = k_rate), color = "white", size = 5) +
  scale_fill_gradient(low = "lightgray", high = "darkblue") +
  labs(title = "Observed K %",
       x = "Pitcher K %",
       y = "Batter K %",
       fill = "Observed\nK%",
       subtitle = "Minimum 100 PAs for batter/pitcher K% combination") +
  theme_minimal()
