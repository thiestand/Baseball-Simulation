# Creating and Modeling walks, hits and outs
library(dplyr)

season_2024 <- read.csv("percentages_added.csv")

# Adding free_base variable and out variable
season_2024 <- season_2024 |>
  mutate(free_base = hbp + walk) |>
  mutate(ip_out = case_when(free_base + single + double + triple + hr + k == 0 ~ 1,
                         free_base + single + double + triple + hr + k == 1 ~ 0,
                         free_base + single + double + triple + hr + k > 1 ~ -1)) |>
  mutate(adds = free_base + ip_out + single + double + triple + hr + k)

# 1 or 0 determination
table(season_2024$adds)


season_2024 <- season_2024 |>
  mutate(bat_fbpct = mean(free_base),
         bat_1Bpct = mean(single),
         bat_2Bpct = mean(double),
         bat_3Bpct = mean(triple),
         bat_HRpct = mean(hr),
         bat_ipoutpct = mean(ip_out),
         .by = batter) |>
  mutate(pit_fbpct = mean(free_base),
         pit_1Bpct = mean(single),
         pit_2Bpct = mean(double),
         pit_3Bpct = mean(triple),
         pit_HRpct = mean(hr),
         pit_ipoutpct = mean(ip_out),
         .by = pitcher)

write.csv(season_2024, 
          "season_2024.csv", 
          row.names=FALSE)

#### Modeling each at-bat outcome

# Free base model
model_free_base <- season_2024 |>
  glm(free_base ~ bat_fbpct + pit_fbpct + samehand,
      family = binomial,
      data = _)
summary(model_free_base)

pred_free_base <- predict(model_free_base, 
        newdata= data.frame(bat_fbpct = .11, pit_fbpct = .07, samehand = "yes"),
        type = "response")

# Single Model
model_single <- season_2024 |>
  glm(single ~ bat_1Bpct + pit_1Bpct + samehand,
      family = binomial,
      data = _)
summary(model_single)

pred_single <- predict(model_single, 
        newdata= data.frame(bat_1Bpct = .15, pit_1Bpct = .12, samehand = "yes"),
        type = "response")

# Double Model
model_double <- season_2024 |>
  glm(double ~ bat_2Bpct + pit_2Bpct + samehand,
      family = binomial,
      data = _)
summary(model_double)

pred_double <- predict(model_double, 
                       newdata= data.frame(bat_2Bpct = .02, pit_2Bpct = .06, samehand = "yes"),
                       type = "response")

# Triple Model
model_triple <- season_2024 |>
  glm(triple ~ bat_3Bpct + pit_3Bpct + samehand,
      family = binomial,
      data = _)
summary(model_triple)

pred_triple <- predict(model_triple, 
                       newdata= data.frame(bat_3Bpct = .01, pit_3Bpct = .01, samehand = "yes"),
                       type = "response")

# Home Run Model
model_hr <- season_2024 |>
  glm(hr ~ bat_HRpct + pit_HRpct + samehand,
      family = binomial,
      data = _)
summary(model_hr)

pred_hr <- predict(model_hr, 
        newdata= data.frame(bat_HRpct = .02, pit_HRpct = .04, samehand = "yes"),
        type = "response")

# Out Model
model_ipout <- season_2024 |>
  glm(out ~ bat_ipoutpct + pit_ipoutpct + samehand,
      family = binomial,
      data = _)
summary(model_ipout)

pred_ipout <- predict(model_ipout, 
        newdata= data.frame(bat_ipoutpct = .50, pit_ipoutpct = .47, samehand = "yes"),
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
pred_single + pred_double + pred_triple + pred_hr + pred_free_base + pred_ipout + pred_k



