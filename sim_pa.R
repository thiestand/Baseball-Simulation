season_2024 <- read.csv("season_2024.csv")

sim_pa <- function(bat = "duraj001", pit = "canng001") {
  
  batter <- season_2024 |>
    filter(batter == bat) |>
    summarize(bat_kpct = mean(k), 
              bat_fbpct = mean(free_base),
              bat_1Bpct = mean(single),
              bat_2Bpct = mean(double),
              bat_3Bpct = mean(triple),
              bat_HRpct = mean(hr),
              bat_ipoutpct = mean(ip_out),
              .by = bathand)
  pitcher <- season_2024 |>
    filter(pitcher == pit) |>
    summarize(pit_kpct = mean(k), 
              pit_fbpct = mean(free_base),
              pit_1Bpct = mean(single),
              pit_2Bpct = mean(double),
              pit_3Bpct = mean(triple),
              pit_HRpct = mean(hr),
              pit_ipoutpct = mean(ip_out),
              .by = pithand)
  
  samehand <- batter$bathand == pitcher$pithand
  
  pred_k <- -3.91984 + 5.93173*batter$bat_kpct + 5.79395*pitcher$pit_kpct + 0.07264*samehand
  pred_k <- exp(pred_k) / (1 + exp(pred_k))
  
  pred_fb <- -4.39809 + 11.42124*batter$bat_fbpct + 11.81275 *pitcher$pit_fbpct - 0.17712*samehand
  pred_fb <- exp(pred_fb) / (1 + exp(pred_fb))
  
  pred_1B <- -4.19363  + 8.32839*batter$bat_1Bpct + 8.47410*pitcher$pit_1Bpct + 0.03093*samehand
  pred_1B <- exp(pred_1B) / (1 + exp(pred_1B))

  pred_2B <- -5.21054  + 24.99990*batter$bat_2Bpct + 24.16751*pitcher$pit_2Bpct - 0.08040*samehand
  pred_2B <- exp(pred_2B) / (1 + exp(pred_2B))
  
  pred_3B <- -7.46952  + 170.10647*batter$bat_3Bpct + 183.24317*pitcher$pit_3Bpct - 0.06724*samehand
  pred_3B <- exp(pred_3B) / (1 + exp(pred_3B))
  
  pred_HR <- -5.47323  + 32.11120*batter$bat_HRpct + 32.16880*pitcher$pit_HRpct - 0.13588*samehand
  pred_HR <- exp(pred_HR) / (1 + exp(pred_HR))
  
  pred_ipout <- -5.47323  + 32.11120*batter$bat_ipoutpct + 32.16880*pitcher$pit_ipoutpct - 0.13588*samehand
  pred_ipout <- exp(pred_ipout) / (1 + exp(pred_ipout))
  
  
  pred_total <- pred_k + pred_fb + pred_1B + pred_2B + pred_3B + pred_HR + pred_ipout
  pred_k <- pred_k / pred_total
  pred_fb <- pred_fb / pred_total
  pred_1B <- pred_1B / pred_total
  pred_2B <- pred_2B / pred_total
  pred_3B <- pred_3B / pred_total
  pred_HR <- pred_HR / pred_total
  pred_ipout <- pred_ipout / pred_total
  
  print(batter)
  print(pitcher)
  
}

sim_pa()

