library(tidyverse)

cubs_lineup <- c("Ian Happ", "Seiya Suzuki", "Kyle Tucker", 
            "Michael Busch", "Dansby Swanson", "Miguel Amaya", 
            "Pete Crow-Armstrong", "Nico Hoerner", "Justin Turner")

yanks_lineup <- c("Austin Wells", "Juan Soto", "Aaron Judge",
                  "Giancarlo Stanton", "Anthony Rizzo", "Gleyber Torres",
                  "Alex Verdugo", "Anthony Volpe", "Oswald Cabrera")

dodgers_lineup <- c("Mookie Betts", "Shohei Ohtani", "Freddie Freeman", 
                           "Teoscar Hernandez", "Tommy Edman", "Will Smith",
                           "James Outman", "Max Muncy", "Chris Taylor")

cubs <- tibble(cubs_lineup)

dodgers <- tibble(dodgers_lineup)

yankees <- tibble(yanks_lineup)

lineups <- bind_cols(
  tibble(Cubs = cubs_lineup),
  tibble(Yankees = yanks_lineup),
  tibble(Dodgers = dodgers_lineup)
)

sim_inning <- function (lineup = "Aaron Judge", pit = "Paul Skenes", spot = 1) {
  pas <- 0
  out <- 0
  results <- NULL
  first <- 0
  second <- 0
  third <- 0
  runs <- 0
  hits <- 0
  box_score <- <UPDATE THIS TO CREATE OBJECT WITH NULL FOR EACH VARIABLE>
            
  if (length(lineup < 9)) {
    lineup <- rep(lineup, length.out = 9)
  }
  
  while(out < 3) {
    pa <- sim_pa(bat = lineup[spot], pit = pit, print = "none")
    pas <- pas + 1
    box_score <- rbind(box_score,
            data.frame(pa = pas,
                      pitcher = pit,
                      hitter = lineup[spot],
                      result = pa))
    results[pas] <- pas
    
    # Adding Outs
    if (pa == "In Play, Out" | pa == "Strikeout") {
      out <- out + 1
    }
    
    # Adding Home Runs
    if (pa == "Home Run") {
      runs <- runs + first + second + third + 1
      first <- 0
      second <- 0
      third <- 0
    }
    
    # Adding Singles
    if (pa == "Single") {
      if (first == 1 & second == 1 & third == 1) {
        second <- 1
        third <- 1
        runs <- runs + 1
      } else if (first == 1 & second == 1) {
        second <- 1
        third <- 1
      } else if (first == 1 & third == 1) {
        second <- 1
        third <- 0
        runs <- runs + 1
      } else if (second == 1 & third == 1) {
        second <- 0
        third <- 1
        runs <- runs + 1
      } else if (second == 1) {
        second <- 0
        third <- 1
      } else if (third == 1){
        second <- 0
        third <- 0
        runs <- runs +1
      } else if (first == 1) {
        second <- 0
        third <- 1
      }
      first <- 1
      
    }
    
    # Adding Walks
    if (pa == "Walk") {
      if (first == 1 & second == 1 & third == 1) {
        runs <- runs + 1
      } else if (first == 1 & second == 1) {
        third <- 1
        second <- 1
      } else if (first == 1 & third == 1) {
        second <- 1
        third <- 1
      } else if (second == 1 & third == 1) {
        second <- 1
        third <- 1
      } else if (second == 1) {
        second <- 1
        third <- 0
      } else if (third == 1) {
        second <- 0
        third <- 1
      } else if (first == 1) {
        second <- 1
        third <- 0
      }
      first <- 1
      
    }
    
    # Adding Doubles
    if (pa == "Double") {
      if (first == 1 & second == 1 & third == 1) {
        first <- 0
        third <- 1
        runs <- runs + 2
      } else if (first == 1 & second == 1) {
        first <- 0
        third <- 1
        runs <- runs + 1
      } else if (first == 1 & third == 1) {
        first <- 0
        third <- 1
        runs <- runs + 1
      } else if (second == 1 & third == 1) {
        first <- 0
        third <- 0
        runs <- runs + 2
      } else if (second == 1 | third == 1) {
        first <- 0
        third <- 0
        runs <- runs + 1
      } else if (first == 1) {
        first <- 0
        third <- 1
      }
      second <- 1
    }
    
    # Adding Triples
    if (pa == "Triple") {
      if (first == 1 & second == 1 & third == 1) {
        first <- 0
        second <- 0
        runs <- runs + 3
      } else if (first == 1 & second == 1) {
        first <- 0
        second <- 0
        runs <- runs + 2
      } else if (first == 1 & third == 1) {
        first <- 0
        third <- 0
        runs <- runs + 2
      } else if (second == 1 & third == 1) {
        first <- 0
        second <- 0
        runs <- runs + 2
      } else if (second == 1 | third == 1) {
        first <- 0
        second <- 0
        runs <- runs + 1
      } else if (first == 1) {
        first <- 0
        second <- 0
        runs <- runs + 1
      }
      third <- 1
    }
    
    spot <- ifelse(spot < 9, spot + 1, 1)
    
    if (pa == "Single" | pa == "Double" | pa == "Triple" | pa == "Home Run") {
      hits <- hits + 1
    }
    
  }
  

  #print(paste("Plate Appearances: ", pas))
  #print(paste("Runs: ", runs))
  #print(paste("First: ", first))
  #print(paste("Second: ", second))
  #print(paste("Third: ", third))
  #print(paste(hits, "hit(s) and", runs, "run(s),", lineup[spot], "due up next"))
  #list(runs = runs,
  #     hits = hits)
  
  # Hides the output and instead, just prints the result of the inning
  invisible(list(box_score = box_score,
                 hits = hits, 
                 runs = runs, 
                 due_up = lineup[spot],
                 spot = spot))
  
}

sim_inning(lineup = cubs_lineup, pit = "Sonny Gray")

