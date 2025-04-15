library(tidyverse)

spot <- 1
hits <- 0
runs <- 0
hr <- 0
box <- NULL
  
sim_game <- function (box_score = TRUE, lineup = cubs_lineup, pitcher = "Sonny Gray",
                      playByPlay = FALSE){
    
  for (inn in 1:9) {
    result <- sim_inning(lineup = lineup, pit = pitcher, spot = spot)
    runs <- runs + result$runs
    hits <- hits + result$hits
    hr <- hr + result$hr
    spot <- result$spot
    inn_runs <- result$runs
    inn_hits <- result$hits
      
    if (playByPlay == TRUE) {
      print(paste(inn_runs, "run(s) on", inn_hits, "hit(s) in inning", inn))
      inn_runs <- 0
      inn_hits <- 0
      if (inn == 9){
        print(paste("Final Score:", runs, "run(s)"))
      }
    }
    
    if (box_score == TRUE) {
      box <- rbind(box, cbind(result$box_score), inning = inn)
      
    }
    
  }
    
    invisible(list(runs = runs,
                box_score = box,
                hits = hits,
                hr = hr))
}

output <- sim_game(playByPlay = TRUE)
output$box_score

distribution <- replicate(100, sim_game())
table(distribution)
mean(distribution)

