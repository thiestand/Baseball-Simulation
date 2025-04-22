library(tidyverse)

spot <- 1
hits <- 0
runs <- 0
hr <- 0
bb <- 0
box <- NULL
  
sim_game <- function (lineup = cubs_lineup, pitcher = "Sonny Gray", 
                      box_score = TRUE, playByPlay = FALSE,
                      print = "none"){
    
  for (inn in 1:9) {
    result <- sim_inning(lineup = lineup, pit = pitcher, spot = spot, print = print)
    runs <- runs + result$runs
    hits <- hits + result$hits
    hr <- hr + result$hr
    bb <- bb + result$bb
    spot <- result$spot
    inn_runs <- result$runs
    inn_hits <- result$hits
    runners <- result$first + result$second + result$third
      
    if (playByPlay == TRUE) {
      print(paste(inn_runs, "run(s) on", inn_hits, "hit(s) and", runners, "runners left on base in inning", inn))
      inn_runs <- 0
      inn_hits <- 0
      if (inn == 9){
        print(paste("Final Score:", runs, "run(s)"))
      }
    }
    
    if (box_score == TRUE) {
      box <- rbind(box, cbind(result$box_score, inning = inn))
      
      bat_box <- box |> 
        summarize(PA = n(),
                H = sum(result %in% c("Single", "Double", "Triple", "Home Run")),
                HR = sum(result == "Home Run"),
                BB = sum(result == "Walk"),
                K = sum(result == "Strikeout"),
                .by = hitter)
      
      pit_box <- box |>
        summarize(BF = n(),
                  H = sum(result %in% c("Single", "Double", "Triple", "Home Run")),
                  R = sum(runs),
                  BB = sum(result == "Walk"),
                  K = sum(result == "Strikeout"),
                  HR = sum(result == "Home Run"),
                  .by = pitcher)
      
      box_list <- list(bat_box, pit_box)
      
    }
    
  }
    
    invisible(list(runs = runs,
                box_score = box_list,
                #bat_box_score = bat_box,
                #pit_box_score = pit_box,
                hits = hits,
                hr = hr,
                bb = bb))
}

output <- sim_game(playByPlay = TRUE)
output$box_score
output$pit_box_score

# output$box_score |>
#   summarize(pa = n(),
#             H = sum(result %in% c("Single", "Double", "Triple", "Home Run")),
#             HR = sum(result == "Home Run"),
#             BB = sum(result == "Walk"),
#             k = sum(result == "Strikeout"),
#             .by = hitter)

distribution <- replicate(100, sim_game())
table(distribution)
mean(distribution)

