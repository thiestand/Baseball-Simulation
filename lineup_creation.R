## Tester File
season_2024 <- read.csv("season_2024.csv")

sim_pa(bat = "Aaron Judge", pit = "Patrick Corbin", print = "result")

# Lineup Builder

## NL East

phillies_lineup <- c("Kyle Schwarber", "Trea Turner", "Bryce Harper",
                     "Alec Bohm", "Nick Castellanos", "Bryson Stott",
                     "Brandon Marsh", "Edmundo Sosa", "Johan Rojas")

braves_lineup <- c("Ronald Acuna", "Ozzie Albies", "Marcell Ozuna",
                   "Matt Olson", "Travis d'Arnaud", "Adam Duvall",
                   "Michael Harris", "Orlando Arcia", "Jarred Kelenic")

nats_lineup <- c("C.J. Abrams", "Lane Thomas", "Jesse Winker",
                 "Joey Meneses", "Luis Garcia", "Keiburt Ruiz",
                 "Ildemaro Vargas", "Trey Lipscomb", "Jacob Young")

mets_lineup <- c("Francisco Lindor", "Brandon Nimmo", "J.D. Martinez",
                 "Pete Alonso", "Starling Marte", "Mark Vientos",
                 "Jeff McNeil", "Harrison Bader", "Tomas Nido")

marlins_lineup <- c("Jazz Chisholm", "Bryan De La Cruz", "Josh Bell",
                    "Jesus Sanchez", "Jake Burger", "Otto Lopez",
                    "Nick Gordon", "Vidal Brujan", "Nick Fortes")

## NL Central
cubs_lineup <- c("Ian Happ", "Michael Busch", "Cody Bellinger", 
                 "Christopher Morel", "Isaac Paredes", "Nico Hoerner", 
                 "Dansby Swanson", "Pete Crow-Armstrong", "Miguel Amaya")

brewers_lineup <- c("Brice Turang", "William Contreras", "Christian Yelich",
                    "Willy Adames", "Rhys Hoskins", "Sal Frelick", "Joey Ortiz",
                    "Jackson Chourio", "Blake Perkins")

reds_lineup <- c("Jonathan India", "Elly De La Cruz", "Jeimer Candelario",
                 "Spencer Steer", "Jake Fraley", "Ty France",
                 "Santiago Espinal", "Will Benson", "Luke Maile")

cardinals_lineup <- c("Masyn Winn", "Alec Burleson", "Paul Goldschmidt",
                      "Nolan Arenado", "Brendan Donovan", "Lars Nootbaar",
                      "Nolan Gorman", "Pedro Pages", "Michael Siani")

pirates_lineup <- c("Andrew McCutchen", "Bryan Reynolds", "Oneil Cruz",
                    "Rowdy Tellez", "Nick Gonzales", "Bryan De La Cruz",
                    "Jared Triolo", "Yasmani Grandal", "Michael Taylor")

## NL West
dodgers_lineup <- c("Shohei Ohtani", "Mookie Betts", "Freddie Freeman", 
                    "Teoscar Hernandez", "Max Muncy", "Miguel Rojas",
                    "Andy Pages", "Gavin Lux", "Will Smith")

padres_lineup <- c("Luis Arraez", "Fernando Tatis", "Jake Cronenworth",
                   "Manny Machado", "Xander Bogaerts", "Jackson Merrill",
                   "David Peralta", "Ha-Seong Kim", "Kyle Higashioka")

dbacks_lineup <- c("Corbin Carroll", "Ketel Marte", "Joc Pederson",
                   "Christian Walker", "Yuli Gurriel", "Eugenio Suarez",
                   "Gabriel Moreno", "Kevin Newman", "Geraldo Perdomo")

giants_lineup <- c("Jung Hoo Lee", "Lamonte Wade", "Heliot Ramos",
                   "Matt Chapman", "Michael Conforto", "Mike Yastrzemski",
                   "Thairo Estrada", "Patrick Bailey", "Nick Ahmed")

rockies_lineup <- c("Charlie Blackmon", "Ezequiel Tovar", "Ryan McMahon",
                    "Elias Diaz", "Brendan Rodgers", "Michael Toglia",
                    "Jake Cave", "Hunter Goodman", "Aaron Schunk")


## AL East
yanks_lineup <- c("Anthony Volpe", "Juan Soto", "Aaron Judge",
                  "Alex Verdugo", "Giancarlo Stanton", "Anthony Rizzo",
                  "Gleyber Torres", "Jose Trevino", "Oswaldo Cabrera")

redsox_lineup <- c("Jaren Duran", "Wilyer Abreu", "Tyler O'Neill", 
                   "Rafael Devers", "Connor Wong", "Masataka Yoshida", 
                   "Dominic Smith", "David Hamilton", "Ceddanne Rafaela")

bluejays_lineup <- c("George Springer", "Daulton Varsho", "Vladimir Guerrero",
                     "Justin Turner", "Alejandro Kirk", "Ernie Clement",
                     "David Schneider", "Isiah Kiner-Falefa", "Kevin Kiermaier")

rays_lineup <- c("Yandy Diaz", "Brandon Lowe", "Randy Arozerana",
                 "Isaac Paredes", "Ahmed Rosario", "Richie Palacios", 
                 "Jose Cabellero", "Ben Rortvedt", "Jose Siri")

orioles_lineup <- c("Gunnar Henderson", "Adley Rutschman", "Ryan O'Hearn",
                    "Anthony Santander", "Jordan Westburg", "Colton Cowser",
                    "Cedric Mullins", "Luis Urias", "James McCann")

## AL Central
tigers_lineup <- c("Matt Vierling", "Colt Keith", "Riley Greene",
                   "Kerry Carpenter", "Gio Urshela", "Spencer Torkelson", 
                   "Zach McKinstry", "Javier Baez", "Brendan Rogers")

guards_lineup <- c("Steven Kwan", "Andres Gimenez", "Jose Ramirez",
                   "Josh Naylor", "David Fry", "Will Brennan", 
                   "David Schneenmann", "Brayan Rocchio", "Austin Hedges")

royals_lineup <- c("Maikel Garcia", "Bobby Witt", "Vinnie Pasquantino",
                   "Salvador Perez", "M.J. Melendez", "Hunter Renfroe",
                   "Adam Frazier", "Garrett Hampson", "Kyle Isbel")

twins_lineup <- c("Willi Castro", "Trevor Larnach", "Royce Lewis",
                  "Max Kepler", "Carlos Santana", "Jose Miranda",
                  "Byron Buxton", "Christian Vazquez", "Austin Martin")

whitesox_lineup <- c("Tommy Pham", "Luis Robert", "Andrew Benintendi",
                     "Andrew Vaughn", "Gavin Sheets", "Paul DeJong",
                     "David Fletcher", "Nicky Lopez", "Martin Maldonado")

## AL West
mariners_lineup <- c("JP Crawford", "Julio Rodriguez", "Cal Raleigh",
                     "Randy Arozarena", "Jorge Polanco", "Ty France",
                     "Mitch Haniger", "Dylan Moore", "Josh Rojas")

astros_lineup <- c("Jose Altuve", "Alex Bregman", "Yordan Alvarez",
                   "Yainer Diaz", "Jeremy Pena", "Jon Singleton",
                   "Jake Meyers", "Mauricio Dubon", "Chaz McCormick")

rangers_lineup <- c("Marcus Semien", "Corey Seager", "Josh Smith",
                    "Luis Garcia", "Nathaniel Lowe", "Josh Jung",
                    "Jonah Heim", "Ezequiel Duran", "Leody Taveras")

angels_lineup <- c("Nolan Schanuel", "Zach Neto", "Taylor Ward",
                   "Kole Calhoun", "Logan O'Hoppe", "Mickey Moniak",
                   "Jo Adell", "Matt Thaiss", "Luis Guillorme")

athletics_lineup <- c("Lawrence Butler", "Miguel Andujar", "JJ Bleday",
                      "Shea Langeliers", "Seth Brown", "Zach Gelof",
                      "Kyle McCann", "Brent Rooker", "Max Schuemann")

# Lineup Creation
library(dplyr)

lineups <- bind_cols(
  #NL East
  tibble(Phillies = phillies_lineup),
  tibble(Braves = braves_lineup),
  tibble(Nationals = nats_lineup),
  tibble(Mets = mets_lineup),
  tibble(Marlins = marlins_lineup),
  #NL Central
  tibble(Cubs = cubs_lineup),
  tibble(Brewers = brewers_lineup),
  tibble(Reds = reds_lineup),
  tibble(Cardinals = cardinals_lineup),
  tibble(Pirates = pirates_lineup),
  #NL West
  tibble(Dodgers = dodgers_lineup),
  tibble(Padres = padres_lineup),
  tibble(Diamondbacks = dbacks_lineup),
  tibble(Giants = giants_lineup),
  tibble(Rockies = rockies_lineup),
  #AL East
  tibble(Yankees = yanks_lineup),
  tibble(RedSox = redsox_lineup),
  tibble(BlueJays = bluejays_lineup),
  tibble(Rays = rays_lineup),
  tibble(Orioles = orioles_lineup),
  #AL Central
  tibble(Tigers = tigers_lineup),
  tibble(Guardians = guards_lineup),
  tibble(Royals = royals_lineup),
  tibble(Twins = twins_lineup),
  tibble(WhiteSox = whitesox_lineup),
  #AL West
  tibble(Mariners = mariners_lineup),
  tibble(Astros = astros_lineup),
  tibble(Rangers = rangers_lineup),
  tibble(Angels = angels_lineup),
  tibble(Athletics = athletics_lineup)
  
)

# Creating Lineup CSV
write.csv(lineups, 
          "lineups.csv", 
          row.names=FALSE)



