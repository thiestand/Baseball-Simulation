sim_pa <- function(bat = "duraj001", pit = "canng001") {
  
  batter <- season_2024 |>
    filter(batter == bat) |>
    summarize(bat_kpct = mean(k), 
              # add in bat_fbpct and all the others
              .by = bathand)
  pitcher <- season_2024 |>
    filter(pitcher == pit) |>
    summarize(pit_kpct = mean(k), 
              # add in pit_fbpct and all the others
              .by = pithand)
  
  samehand <- batter$bathand == pitcher$pithand
  
  pred_k <- -3.91984 + 5.93173*batter$bat_kpct + 5.79395*pitcher$pit_kpct + 0.07264*samehand
  pred_k <- exp(pred_k) / (1 + exp(pred_k))
  
  # calculate pred_fb and all the others using previous models
  
  
  # Bonus: Add up all the pred_xxxx. Then divide their values by the sum.
  # pred_total <- pred_k + pred_fb + ...
  # pred_k <- pred_k / pred_total
  
  print(batter)
  print(pitcher)
  print(pred_k)
  
}

sim_pa()

