## Reading in 2024 Retrosheet play-by-play

csv_2024 <- read.csv("2024plays.csv")

## Libraries
library(tidyverse)

## Filtering and Selecting Rows and Columns to work with

useful_2024 <- csv_2024 %>%
  select("ab", "pa", "batter", "pitcher", "single" : "k")

percentages_added <- useful_2024 %>%
  mutate(bat_pa = n(), bat_kpct = sum(k) / bat_pa, 
         .by = batter) %>%
  mutate(pit_pa = n(), pit_kpct = sum(k) / pit_pa, 
         .by = pitcher)

## Writing the CSV
write.csv(percentages_added, 
          "percentages_added.csv", 
          row.names=FALSE)


