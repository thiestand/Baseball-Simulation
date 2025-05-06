
advance_runners <- function(result, bases) {
  runs <- 0
  
  if (result == "Home Run") {
    runs <- 1 + sum(bases)
    bases <- c(0, 0, 0)
    
  } else if (result == "Single") {
    if (all(bases == c(1, 1, 1))) {
      if (runif(1) < 0.7) {
        runs <- 1
        bases <- c(1, 1, 1)
      } else {
        runs <- 2
        bases <- c(1, 0, 1)
      }
    } else if (all(bases == c(1,1,0))) {
      if (runif(1) < 0.7) {
        bases <- c(1, 1, 1)
      } else {
        runs <- 1
        bases <- c(1, 0, 1)
      }
    } else if (all(bases == c(1,0,1))) {
      if (runif(1) < 0.7) {
        runs <- 1
        bases <- c(1,1,0)
      } else {
        runs <- 1
        bases <- c(1, 0, 1)
      }
    } else if (all(bases == c(0, 1, 1))) {
      if (runif(1) < 0.4) {
        runs <- 1
        bases <- c(1, 0, 1)
      } else {
        runs <- 2
        bases <- c(1, 0, 0)
      }
    } else if (all(bases == c(0,1,0))) {
      if (runif(1) < 0.4) {
        bases <- c(1,0,1)
      } else {
        runs <- 1
        bases <- c(1,0,0)
      }
    } else if (all(bases == c(0,0,1))) {
      runs <- 1
      bases <- c(1,0,0)
    } else if (all(bases == c(1,0,0))) {
      if (runif(1) < 0.70) {
        bases <- c(1,1,0)
      } else {
        bases <- c(1,0,1)
      }
    } else if (all(bases == c(0,0,0))) {
      bases <- c(1,0,0)
    }
    
  } else if (result == "Double") {
    if (all(bases == c(1,1,1))) {
      if (runif(1) < 0.35) { 
        runs <- sum(bases)
        bases <- c(0, 1, 0)
      } else {
        runs <- runs + 2
        bases <- c(0, 1, 1)
      }
    } else if (all(bases == c(1,1,0))) {
      if (runif(1) < 0.65) {
        runs <- runs + 1
        bases <- c(0, 1, 1)
      } else {
        runs <- runs + 2
        bases <- c(0, 1, 0)
      }
    } else if (all(bases == c(1,0,1))) {
      if (runif(1) < 0.65) {
        runs <- runs + 2
        bases <- c(0, 1, 0)
      } else {
        runs <- runs + 1
        bases <- c(0, 1, 1)
      }
    } else if (all(bases == c(0,1,1))) {
      runs <- runs + 2
      bases <- c(0,1,0)
    } else if (all(bases == c(0,1,0))) {
      runs <- runs + 1
      bases <- c(0,1,0)
    } else if (all(bases == c(0,0,1))) {
      runs <- runs + 1
      bases <- c(0,1,0)
    } else if (all(bases == c(1,0,0))) {
      if (runif(1) < 0.65) {
        bases <- c(0,1,1)
      } else {
        runs <- runs + 1
        bases <- c(0,1,1)
      }
    } else if (all(bases == c(0,0,0))) {
      bases <- c(0,1,0)
    }
    
  } else if (result == "Triple") {
    runs <- runs + sum(bases)
    bases <- c(0, 0, 1)
    
  } else if (result == "Walk") {
    if (all(bases == c(1,1,1))) {
      runs <- 1
      bases <- c(1,1,1)
    } else if (all(bases == c(1,1,0))) {
      bases <- c(1,1,1)
    } else if (all(bases == c(1,0,1))) {
      bases <- c(1,1,1)
    } else if (all(bases == c(0,1,1))) {
      bases <- c(1,1,1)
    } else if (all(bases == c(0,1,0))) {
      bases <- c(1,1,0)
    } else if (all(bases == c(0,0,1))) {
      bases <- c(1,0,1)
    } else if (all(bases == c(1,0,0))) {
      bases <- c(1,1,0)
    } else if (all(bases == c(0,0,0))) {
      bases <- c(1,0,0)
    }
  }
  
  return(list(bases = bases, runs = runs))
}

state <- c(0,0,0)


advance_runners("Single", state)



