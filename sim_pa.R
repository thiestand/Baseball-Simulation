library(tidyverse)

season_2024 <- read.csv("season_2024.csv")

# Have user read data into R outside the function
# Include an argument to read the data in

batters <- read.csv("https://raw.githubusercontent.com/thiestand/Baseball-Simulation/refs/heads/main/batter_data.csv")
pitchers <- read.csv("https://raw.githubusercontent.com/thiestand/Baseball-Simulation/refs/heads/main/pitcher_data.csv")

sim_pa <- function(bat = "Jarren Duran", pit = "Griffin Canning", 
                   bat_data = batters, pit_data = pitchers,
                   print = "none") {
  
  batter <- filter(bat_data, name == bat)
  pitcher <- filter(pit_data, name == pit)
  
  samehand <- batter$bathand == pitcher$pithand
  
  pred_k <- -3.92052 + 5.73134*batter$bat_kpct + 5.75805*pitcher$pit_kpct + 0.07522*samehand
  pred_k <- exp(pred_k) / (1 + exp(pred_k))
  
  pred_fb <- -4.29556 + 11.11392*batter$bat_fbpct + 10.38869 *pitcher$pit_fbpct - 0.16132*samehand
  pred_fb <- exp(pred_fb) / (1 + exp(pred_fb))
  
  pred_1B <- -4.13821  + 8.21707*batter$bat_1Bpct + 7.87822*pitcher$pit_1Bpct + 0.02929*samehand
  pred_1B <- exp(pred_1B) / (1 + exp(pred_1B))
  
  pred_2B <- -4.86874  + 21.82715*batter$bat_2Bpct + 18.54212*pitcher$pit_2Bpct - 0.08984*samehand
  pred_2B <- exp(pred_2B) / (1 + exp(pred_2B))
  
  pred_3B <- -6.85267  + 131.42274*batter$bat_3Bpct + 112.69379*pitcher$pit_3Bpct - 0.12138*samehand
  pred_3B <- exp(pred_3B) / (1 + exp(pred_3B))
  
  pred_HR <- -5.16375  + 30.62259*batter$bat_HRpct + 22.53312*pitcher$pit_HRpct - 0.13722*samehand
  pred_HR <- exp(pred_HR) / (1 + exp(pred_HR))
  
  pred_ipout <- -3.966211  + 4.095392*batter$bat_ipoutpct + 4.122515*pitcher$pit_ipoutpct + 0.019550*samehand
  pred_ipout <- exp(pred_ipout) / (1 + exp(pred_ipout))
  
  
  pred_total <- pred_k + pred_fb + pred_1B + pred_2B + pred_3B + pred_HR + pred_ipout
  pred_k <- pred_k / pred_total
  pred_fb <- pred_fb / pred_total
  pred_1B <- pred_1B / pred_total
  pred_2B <- pred_2B / pred_total
  pred_3B <- pred_3B / pred_total
  pred_HR <- pred_HR / pred_total
  pred_ipout <- pred_ipout / pred_total
  
  props <- data.frame(out_ip = pred_ipout,
                      k = pred_k,
                      single = pred_1B,
                      fb = pred_fb,
                      double = pred_2B,
                      homer = pred_HR,
                      triple = pred_3B)
  rn <- runif(1)
  
  props2 <- props |> t() |> cumsum()
  
  # Use case_when() to determine the outcome.
  # Use the order ipout, k, 1b, fb, 2b, hr, 3b
  
  result <- case_when(rn < props2[1] ~ "In Play, Out",
                      rn > props2[1] & rn < props2[2] ~ "Strikeout",
                      rn > props2[2] & rn < props2[3] ~ "Single",
                      rn > props2[3] & rn < props2[4] ~ "Walk",
                      rn > props2[4] & rn < props2[5] ~ "Double",
                      rn > props2[5] & rn < props2[6] ~ "Home Run",
                      rn > props2[6] & rn < props2[7] ~ "Triple")
  
  if (print == "props") {
    print(props)
  }
  
  if (print == "result") {
    if (result == "Strikeout") {
      print(paste(bat, "struck out against", pit))
    }
    else if (result == "In Play, Out") {
      print(paste(bat, "hits into an out against", pit))
    }
    else if (result == "Walk") {
      print(paste(bat, "takes a walk against", pit))
    }
    else {
      print(paste(bat, "hits a", tolower(result), "against", pit))
    }
    
  }
  
  result
  
}


