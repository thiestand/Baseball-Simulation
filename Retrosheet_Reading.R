## Reading in 2024 Retrosheet play-by-play

csv_2024 <- read.csv("2024plays.csv")

## Reading in Bio Data
bio_data <- read.csv("biofile.csv")

## Libraries
library(tidyverse)

## Selecting Bio Data File

bio_data <- bio_data |>
  select("PLAYERID" : "NICKNAME") |>
  rename(playerid = PLAYERID,
         last = LAST,
         first = FIRST,
         nickname = NICKNAME)

## Filtering and Selecting Rows and Columns to work with

useful_2024 <- csv_2024 %>%
  filter(gametype == "regular",
         pa == 1) %>%
  select("ab", "pa", "batter", "pitcher", "single" : "walk", "k", "bathand", "pithand") |>
  rename(playerid = batter,
         playerid = pitcher)

percentages_added <- useful_2024 %>%
  mutate(bat_pa = n(), bat_kpct = sum(k) / bat_pa, 
         .by = batter) %>%
  mutate(pit_pa = n(), pit_kpct = sum(k) / pit_pa, 
         .by = pitcher) %>%
  mutate(samehand = ifelse(bathand == pithand, "yes", "no"))

## Writing the CSV
write.csv(percentages_added, 
          "percentages_added.csv", 
          row.names=FALSE)

# Adding free_base variable and out variable
season_2024 <- percentages_added |>
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


# Summarizing Batter and Pitcher Data
batter <- season_2024 |>
  summarize(bat_kpct = mean(k), 
            bat_fbpct = mean(free_base),
            bat_1Bpct = mean(single),
            bat_2Bpct = mean(double),
            bat_3Bpct = mean(triple),
            bat_HRpct = mean(hr),
            bat_ipoutpct = mean(ip_out),
            bat_pa = n(),
            .by = c(batter, bathand))

#batter <- batter |>
#   left_join(bio_data, by = join_by(playerid))
  
write.csv(batter, 
          "batter_data.csv", 
          row.names=FALSE)


pitcher <- season_2024 |>
  summarize(pit_kpct = mean(k), 
            pit_fbpct = mean(free_base),
            pit_1Bpct = mean(single),
            pit_2Bpct = mean(double),
            pit_3Bpct = mean(triple),
            pit_HRpct = mean(hr),
            pit_ipoutpct = mean(ip_out),
            pit_pa = n(),
            .by = c(pitcher, pithand))

# pitcher <- pitcher |>
#  left_join(bio_data, by = join_by(pitcher))

write.csv(pitcher, 
          "pitcher_data.csv", 
          row.names=FALSE)


