library(tidyverse)

sim_inning <- function (lineup = "Aaron Judge", pit = "Paul Skenes", spot = 1,
                        print = "none") {
  pas <- 0
  out <- 0
  results <- NULL
  bases <- c(0,0,0)
  runs <- 0
  hits <- 0
  hr <- 0
  bb <- 0
  box_score <- NULL
  
  if (length(lineup < 9)) {
    lineup <- rep(lineup, length.out = 9)
  }
  
  while(out < 3) {
    pa <- sim_pa(bat = lineup[spot], pit = pit, print = print)
    pas <- pas + 1
    box_score <- rbind(box_score,
                       data.frame(pa = pas,
                                  pitcher = pit,
                                  hitter = lineup[spot],
                                  result = pa))
    
    
    results[pas] <- pas
    
    rn <- runif(1)
    
    # Adding Outs
    if (pa == "In Play, Out" | pa == "Strikeout") {
      out <- out + 1
    }
    
    # Adding Home Runs
    # if (pa == "Home Run") {
    #   adv <- advance_runners("Home Run", bases)
    #   runs <- runs + adv$runs
    #   bases <- adv$bases
    # }
    # 
    # # Adding Singles
    # if (pa == "Single") {
    #   adv <- advance_runners("Single", bases)
    #   runs <- runs + adv$runs
    #   bases <- adv$bases
    # }
    # 
    # # Adding Walks
    # if (pa == "Walk") {
    #   adv <- advance_runners("Walk", bases)
    #   runs <- runs + adv$runs
    #   bases <- adv$bases
    # }
    # 
    # # Adding Doubles
    # if (pa == "Double") {
    #   adv <- advance_runners("Double", bases)
    #   runs <- runs + adv$runs
    #   bases <- adv$bases
    # }
    # 
    # 
    # # Adding Triples
    # if (pa == "Triple") {
    #   adv <- advance_runners("Triple", bases)
    #   runs <- runs + adv$runs
    #   bases <- adv$bases
    # }
    
    adv <- advance_runners(pa, bases)
    runs <- adv$runs + runs
    bases <- adv$bases
    
    spot <- ifelse(spot < 9, spot + 1, 1)
    
    if (pa %in% c("Single", "Double", "Triple", "Home Run")) {
      hits <- hits + 1
    }
    
    if (pa == "Home Run") {
      hr <- hr + 1 
    }
    
    if (pa == "Walk") {
      bb <- bb + 1
    }
    
  }
  
  
  # Hides the output and instead, just prints the result of the inning
  invisible(list(box_score = box_score,
                 hits = hits, 
                 hr = hr,
                 bb = bb,
                 runs = runs, 
                 due_up = lineup[spot],
                 spot = spot,
                 bases = bases))
  
}
