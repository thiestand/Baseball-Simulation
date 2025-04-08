library(tidyverse)

spot <- 1
hits <- 0
runs <- 0

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
    
  }
  
  return(runs)
}

distribution <- replicate(100, sim_game())
table(distribution)
mean(distribution)
