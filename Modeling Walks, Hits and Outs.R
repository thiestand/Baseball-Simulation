# Creating and Modeling walks, hits and outs

season_2024 <- read.csv("percentages_added.csv")

# Adding free_base variable and out variable
season_2024 <- season_2024 |>
  mutate(free_base = hbp + walk) |>
  mutate(out = case_when(free_base + single + double + triple + hr + k == 0 ~ 1,
                         free_base + single + double + triple + hr + k == 1 ~ 0,
                         free_base + single + double + triple + hr + k > 1 ~ -1)) |>
  mutate(adds = free_base + out + single + double + triple + hr + k)

# 1 or 0 determination
table(season_2024$adds)


#### Modeling each at-bat outcome

# Free base model
model_free_base <- season_2024 |>
  glm(free_base ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_free_base)

pred_free_base <- predict(model_free_base, 
        newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
        type = "response")

# Single Model
model_single <- season_2024 |>
  glm(single ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_single)

pred_single <- predict(model_single, 
        newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
        type = "response")

# Double Model
model_double <- season_2024 |>
  glm(double ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_double)

pred_double <- predict(model_double, 
                       newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
                       type = "response")

# Triple Model
model_triple <- season_2024 |>
  glm(triple ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_triple)

pred_triple <- predict(model_triple, 
                       newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
                       type = "response")

# Home Run Model
model_hr <- season_2024 |>
  glm(hr ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_hr)

pred_hr <- predict(model_hr, 
        newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
        type = "response")

# Out Model
model_out <- season_2024 |>
  glm(out ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_out)

pred_out <- predict(model_out, 
        newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
        type = "response")

# K Model
model_k <- season_2024 |>
  glm(k ~ bat_kpct + pit_kpct + samehand,
      family = binomial,
      data = _)
summary(model_k)

pred_k <- predict(model_k, 
        newdata= data.frame(bat_kpct = .30, pit_kpct = .17, samehand = "yes"),
        type = "response")

### Exploration

# Should equal 1?
pred_single + pred_double + pred_triple + pred_hr + pred_free_base + pred_out + pred_k





