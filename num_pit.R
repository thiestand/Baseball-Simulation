# num_pit function

num_pit <- function(outcome = "Strikeout", table = pitch_table) {
  require(dplyr)
  df <- dplyr::filter(table, result == outcome)
  num <- sample(df$nump, size = 1, prob = df$pit)
  num
}


