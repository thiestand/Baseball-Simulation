sim_inning <- function (batters = "Aaron Judge", pit = "Paul Skenes", spot = 1,
                        print = "none") {
  require(dplyr)
  source("advance_runners.R")
  source("num_pit.R")
  
  pas <- 0
  out <- 0
  results <- NULL
  bases <- c(0,0,0)
  runs <- 0
  hits <- 0
  hr <- 0
  bb <- 0
  pit_count <- 0
  box_score <- NULL
  
  if (length(batters < 9)) {
    batters <- rep(batters, length.out = 9)
  }
  
  while(out < 3) {
    pa <- sim_pa(bat = batters[spot], pit = pit, print = print)
    pas <- pas + 1
    
    results[pas] <- pa
    
    # Increment pitch count
    pit_count <- pit_count + num_pit(pa)
    
    rn <- runif(1)
    
    if (pa %in% c("In Play, Out", "Strikeout")) {
      out <- out + 1
    }
    
    adv <- advance_runners(pa, bases)
    runs <- adv$runs + runs
    bases <- adv$bases
    
    # Add current PA to box score
    box_score <- rbind(box_score,
                       data.frame(
                         pa = pas,
                         pitcher = pit,
                         hitter = batters[spot],
                         result = pa,
                         runs = adv$runs,
                         pitch_count = pit_count
                       ))
    
    # Update counting stats
    if (pa %in% c("Single", "Double", "Triple", "Home Run")) hits <- hits + 1
    if (pa == "Home Run") hr <- hr + 1
    if (pa == "Walk") bb <- bb + 1
    
    spot <- ifelse(spot < 9, spot + 1, 1)
  }
  
  invisible(list(
    box_score = box_score,
    hits = hits, 
    hr = hr,
    bb = bb,
    runs = runs, 
    due_up = batters[spot],
    spot = spot,
    bases = bases,
    pit_count = pit_count
  ))
}
