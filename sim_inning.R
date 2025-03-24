sim_inning <- function (bat = "Jarren Duran", pit = "Paul Skenes", spot = 4) {
  pas <- 0
  out <- 0
  results <- NULL
  first <- 0
  second <- 0
  third <- 0
  runs <- 0
  
  while(out < 3) {
    pa <- sim_pa(output = "result")
    pas <- pas + 1
    results[pas] <- pas
    
    # Adding Outs
    if (pa == "In Play, Out" | pa == "Strikeout") {
      out <- out + 1
    spot <- spot + 1
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
        second <- 0
        third <- 1
        runs <- runs + 2
      } else if (first == 1 & second == 1) {
        second <- 0
        third <- 1
        runs <- runs + 1
      } else if (first == 1 & third == 1) {
        second <- 0
        third <- 1
        runs <- runs + 1
      } else if (second == 1 & third == 1) {
        second <- 0
        third <- 0
        runs <- runs + 2
      } else if (second == 1 | third == 1) {
        second <- 0
        third <- 0
        runs <- runs + 1
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
  }
  
  
  

  print(results)
  print(paste("Runs: ", runs))
  print(paste("First: ", first))
  print(paste("Second: ", second))
  print(paste("Third: ", third))
  print(bat)
  print(pit)
}

sim_inning("Seiya Suzuki", pit = "Yoshinobu Yamamoto")
