# Load batter and pitcher data for the 2024 MLB Regular Season
batters <- read.csv("batter_data.csv")
pitchers <- read.csv("pitcher_data.csv")
lineup <- read.csv("lineups.csv")
model_coef <- read.csv("model_table.csv")

# Load sim_pa function
source("sim_pa.R")

# Test sim_pa function
sim_pa()

sim_pa(bat="Aaron Judge", pit="Zack Wheeler")


###########################


# Load sim_inning function
source("sim_inning.R")

# Test sim_inning function
out <- sim_inning()
out

out2 <- sim_inning(lineup = lineup$Cubs, pit="Justin Verlander", print="result")
out2


#######################


# Load sim_game function
source("sim_game.R")

# Test sim_game function
sim_game()






model_coef <- read.csv("model_table.csv")
