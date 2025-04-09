library(tidyverse)

spot <- 1
hits <- 0
runs <- 0
box <- <UPDATE THIS TO CREATE OBJECT WITH NULL FOR EACH VARIABLE, INCLUDE INNING COLUMN>

sim_game <- function (){
  
  for (inn in 1:9) {
    result <- sim_inning(lineup = cubs_lineup, pit = "Sonny Gray", spot = spot)
    runs <- runs + result$runs
    hits <- hits + result$hits
    spot <- result$spot
    #print(paste("Inning", inn, "completed"))
    
   # if (inn == 9) {
   #  print(paste("Final Score:", runs, "runs on", hits, "hits"))
   #  }
    box <- rbind(box, cbind(result$box_score), inning = inn))
  }
  
  return(runs)
}

distribution <- replicate(100, sim_game())
table(distribution)
mean(distribution)
