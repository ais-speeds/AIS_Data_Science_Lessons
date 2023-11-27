rm(list = ls()) # clean the workspace

# Load Required Libraries
library(readr)
library(dplyr)

# Load Datasets -----
games <- read_csv("games.csv")
clubs <- read_csv("clubs.csv")
players <- read_csv("players.csv")
player_valuations <- read_csv("player_valuations.csv")

# Inspect the first 5 rows of each dataset
head(games)
head(clubs)
head(players)
head(player_valuations)


# Inner Join and Filtering -----
games_clubs <- games %>%
  inner_join(clubs,
             by = c('home_club_id' = 'club_id'),
             suffix = c('_games', '_clubs')) %>%
  filter(season == 2023 & competition_id == 'GB1' & round == '2. Matchday' & game_id == 4087941) %>%
  inner_join(players,
             by = c('home_club_id' = 'current_club_id'),
             suffix = c('_games_clubs', '_players')) %>%
  filter(last_season_players == 2023)


# Left Join and Comparison to an Inner Join -----

# Select only the most relevant columns from the 'players' dataframe and filter only to the '2023' season for simplicity
players_select <- players %>%
  select(player_id, name, last_season, current_club_id) %>%
  filter(last_season == 2023)

# Subset the 'clubs' dataset to artificially remove some 'club_id' values for illustrative purposes of a 'left' join
clubs_subset <- clubs[sample(1:nrow(clubs), size = 0.7 * nrow(clubs)), ] %>%
  select(club_id, name, domestic_competition_id, squad_size, average_age, national_team_players)

# Perform a 'left' join
players_clubs <- players_select %>%
  left_join(clubs_subset,
            by = c('current_club_id' = 'club_id'),
            suffix = c('_player', '_club'))

# Detect NA values to check if there are any players that miss the name of their current club
players_clubs_na_left <- players_clubs[is.na(players_clubs$name_club), ]

# Comparsion with 'inner' join
players_clubs <- players_select %>%
  inner_join(clubs_subset,
            by = c('current_club_id' = 'club_id'),
            suffix = c('_player', '_club'))

# Detect NA values to check if there are any players that miss the name of their current club
players_clubs_na_inner <- players_clubs[is.na(players_clubs$name_club), ]


# Full Join -----
# Selecting relevant columns and filtering the 'players' dataframe for the 2023 season.
players_select <- players %>%
  filter(last_season == 2023) %>%
  select(player_id, name, height_in_cm)
  

# Creating a subset of 'player_valuations', selecting the first entry for each player.
player_valuations_subset <- player_valuations %>%
  filter(last_season == 2023) %>%
  group_by(player_id) %>%
  slice(1) %>%
  select(player_id, market_value_in_eur)

player_valuations_subset <- player_valuations_subset[sample(1:nrow(player_valuations_subset), size = 0.7 * nrow(player_valuations_subset)), ]

# Performing a full join
players_and_valuation <- players_select %>%
  full_join(player_valuations_subset,
            by = "player_id")

# Filter to show rows with NA values after the full join
players_and_valuation_na <- players_and_valuation %>%
  filter(if_any(everything(), is.na))


# Data Concatenation -----
# Define a function for dataframe subsetting
subset_games <- function(df, season_year, competition, matchday) {
  df %>% 
    filter(season == season_year & competition_id == competition & round == matchday) %>% 
    select(competition_id, season, round, date, home_club_name, away_club_name, attendance)
}

# Apply function to subset data for 2022 and 2023
games_EPL2022 <- subset_games(games, 2022, 'GB1', '1. Matchday')
games_EPL2023 <- subset_games(games, 2023, 'GB1', '1. Matchday')

# Concatenate the data
matchday1_attendance <- bind_rows(games_EPL2022, games_EPL2023)

# Randomly select 8 rows from the concatenated dataframe
random_rows <- sample_n(matchday1_attendance, 6)

# Display the randomly selected rows
print(random_rows)












