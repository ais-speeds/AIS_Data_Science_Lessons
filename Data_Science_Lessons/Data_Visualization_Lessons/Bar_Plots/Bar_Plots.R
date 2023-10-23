rm(list = ls()) # clean the workspace

# Load required libraries
library(StatsBombR)    # For accessing and handling StatsBomb football data
library(tidyverse)     # For data manipulation and visualization
library(ggrepel)       # To avoid label overlap in plots
library(ggsoccer)      # For plotting soccer pitches
library(RColorBrewer)  # For colorblind-friendly palettes
library(grid)          # For lower-level graphics functions
library(png)           # To read and write PNG images
library(gridExtra)     # For arranging multiple grid-based plots
library(cowplot)       # For enhancing and customizing ggplot figures
library(forcats)       # For categorical variable (factor) manipulation

# ---- Data Loading and Cleaning ----

#* Pulling and Organizing StatsBomb Data ----

# Measure the time it takes to execute the code within the system.time brackets (for performance)
system.time({ 
  # Fetch list of all available competitions
  all_comps <- FreeCompetitions() 
  
  # Filter to get only the 2023 FIFA Women's World Cup data (Competition ID 72, Season ID 107)
  Comp <- all_comps %>%
    filter(competition_id == 72 & season_id == 107) 
  
  # Fetch all matches of the selected competition
  Matches <- FreeMatches(Comp) 
  
  # Download all match events and parallelize to speed up the process
  StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) 
  
  # Clean the data
  StatsBombData = allclean(StatsBombData) 
})

# Show the column names of the final StatsBombData dataframe
names(StatsBombData)

#* Tidying Up the Teams' Names ----

# Create a dataframe containing unique names of all teams at FWWC23
WWC_teams <- data.frame(team_name = unique(StatsBombData$team.name)) 

# Remove the " Women's" part from team names for simplicity
StatsBombData$team.name <- gsub(" Women's", "", StatsBombData$team.name) 

# Rename and simplify team names in the 'Matches' data frame
Matches <- Matches %>%
  rename(
    home_team = home_team.home_team_name, 
    away_team = away_team.away_team_name  
  ) %>%
  mutate(
    home_team = gsub(" Women's", "", home_team),
    away_team = gsub(" Women's", "", away_team) 
  )

# ---- Grouped Bar Plot ----
### Shot Outcome by Distance

#* Data Filtering ----

# Select only relevant columns and filter the dataset to include only shots that are not penalties
shot_outcomes <- StatsBombData%>%
  select(player.name, team.name, type.name, shot.type.name, shot.outcome.name, DistToGoal)%>%
  filter(type.name == "Shot" & (shot.type.name != "Penalty"|is.na(shot.type.name)))%>%
  filter(shot.outcome.name %in% c("Goal", "Off T", "Saved", "Saved to Post", "Post"))

# Consolidate similar shot outcomes into broader categories
shot_outcomes <- shot_outcomes %>%
  mutate(shot.outcome.name = case_when(
    shot.outcome.name == "Saved to Post" ~ "Saved",
    shot.outcome.name == "Post" ~ "Off T",
    TRUE ~ shot.outcome.name
  ))

#* Binning the 'Shot Distance To Goal' (DistToGoal variable) ----

# Calculate min and max distance to goal
min_dist <- min(shot_outcomes$DistToGoal, na.rm = TRUE)
max_dist <- max(shot_outcomes$DistToGoal, na.rm = TRUE)

# Display min and max distances
cat("Minimal distance of a shot to goal is: ", min_dist, "\n")
cat("Maximal distance of a shot to goal is: ", max_dist, "\n")

# Create bins for DistToGoal
bin_size = 10
shot_outcomes <- shot_outcomes%>%
  mutate(DistToGoal_Binned = cut(DistToGoal, breaks = seq(0, max(DistToGoal) + 10, by = 10)))

#* Creating the Grouped Bar Plot ----

# Create the bar plot
shot_dist_viz <- ggplot(shot_outcomes) +
  geom_bar(aes(x = DistToGoal_Binned, fill = shot.outcome.name), width = 0.7, position = "dodge",
           color = "black", size = 0.5) +
  scale_y_continuous(
    limits = c(0, 210),
    breaks = seq(0, 210, by = 50), 
    expand = expansion(mult = c(0, 0.05)), 
    position = "left"
  ) + 
  scale_x_discrete(expand = expansion(add = c(0, 0)), labels = function(x) gsub("\\((.*),(.*)\\]", "\\1 - \\2", x)) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines
    panel.grid.major.y = element_line(color = "#A8BAC480", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Customize the title for both axes
    axis.title = element_text(family = "sans", size = 16, color = "black", face = "bold"),
    # Only bottom line is painted in black
    axis.line.x.bottom = element_line(color = "black"),
    # Remove labels from the horizontal axis
    axis.text.x = element_text(family = "sans", size = 16, color = "black"),
    # Customize labels for the vertical axis
    axis.text.y = element_text(family = "sans", size = 16, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(family = "sans", size = 16, color = "black"), 
    legend.position = c(0.9, 0.85)
  ) + 
  labs(title = "Shot Outcome by Distance",
       subtitle = "Counts of non-penalty shot outcomes binned by distance at FIFA Women's World Cup 2023",
       x = "Distance to Goal [m]",
       y = "Count of Shots") +
  theme(
    plot.title = element_text(
      family = "sans", 
      face = "bold",
      size = 16,
      color = "black"
    ),
    plot.subtitle = element_text(
      family = "sans",
      size = 16,
      color = "black"
    )
  ) +
  scale_fill_manual(values=c("#DC2228", "#3371AC", "gold"), labels = c("Goal","Off Target","Saved"))

#* Adding Count Labels On the Bars ----

# Calculate the count of each shot outcome for each distance bin
shot_outcomes_sum <- shot_outcomes %>%
  group_by(DistToGoal_Binned, shot.outcome.name) %>%
  summarise(count = n(), .groups = 'drop')

# Add count labels to the bars
shot_dist_viz <- shot_dist_viz +
  geom_text(data = shot_outcomes_sum, 
            aes(x = DistToGoal_Binned, y = count, label = count, group = shot.outcome.name), 
            vjust = -0.5,
            position = position_dodge(width = 0.7),
            size = 5.5,
            color = "black")

#* Adding the StatsBomb Logo ----

# Specify the path to the StatsBomb logo
img_path <- "SB_logo.png"

# Add the StatsBomb logo to the plot
shot_dist_viz_sb <- ggdraw(shot_dist_viz) +
  draw_image(img_path, x = 0.87, y = 0, width = 0.12, height = 0.06)

shot_dist_viz_sb <- ggdraw(shot_dist_viz_sb) +
  draw_label("Data:", x = 0.84, y = 0.03, size = 16)

# Show the finalized plot with the StatsBomb logo
print(shot_dist_viz_sb)

#* Saving the Visualization to png ----

# ggsave("Shot_Outcome_by_Distance.png", plot = shot_dist_viz_sb, width = 10, height = 8, dpi = 300)


# ---- Stacked Bar Plot ----
### Value Added in Attack

#* Data Filtering ----
# Select relevant columns and filter the StatsBomb data to only include non-penalty shots taken by the Matildas players. Also, select only the particular types of shots we are interested in.
shots_Matildas <- StatsBombData%>%
  select(player.name, player.id, team.name, team.id, type.name, shot.type.name, shot.outcome.name, shot.statsbomb_xg, location.x, location.y, DistToGoal)%>%
  filter(team.name == "Australia")%>%
  filter(type.name == "Shot" & (shot.type.name != "Penalty"|is.na(shot.type.name)))%>%
  filter(shot.outcome.name %in% c("Goal", "Off T", "Saved", "Saved to Post", "Post"))

# Select relevant columns and filter the StatsBomb data to only include passes by the Matildas players that resulted in a shot or a goal.
passes_Matildas <- StatsBombData%>%
  select(player.name, player.id, team.name, team.id, type.name, pass.shot_assist, pass.goal_assist)%>%
  filter(team.name == "Australia")%>%
  filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE)

#* Extracting Players' Last Names ----
# Function to extract the last name from a full name string
extract_last_name <- function(full_name) {
  name_parts <- strsplit(full_name, " ")[[1]]
  
  # Check if the name contains "van"
  if ("van" %in% name_parts) {
    # Concatenate "van" with the last name
    return(paste("van", tail(name_parts, 1), collapse = " "))
  } else {
    # Just return the last name
    return(tail(name_parts, 1))
  }
}

# Add a new column for players' last names
shots_Matildas$last_name <- sapply(shots_Matildas$player.name, extract_last_name)

# Reorder the columns to have last_name appear after player.name
shots_Matildas <- shots_Matildas %>%
  select(player.name, last_name, everything())

# Create a new column for players' last names
passes_Matildas$last_name <- sapply(passes_Matildas$player.name, extract_last_name)

# Reorder the columns to have last_name appear after player.name
passes_Matildas <- passes_Matildas %>%
  select(player.name, last_name, everything())
  

#* Computing Metrics and Normalizing Them to 90 Minutes ----
# Calculate stats for each player
# Group by player and count the number of shots they have taken
shots_Matildas_stats <- shots_Matildas%>%
  group_by(player.name, player.id, last_name)%>%
  summarise(shots=sum(type.name=="Shot", na.rm=TRUE),
            .groups = "drop")

#* Normalize all metrics to 90 minutes ----

# Get the total minutes played by all players
all_players_minutes = get.minutesplayed(StatsBombData)

# Extract Australia's team ID
Australia_team_id <- unique(shots_Matildas$team.id)

# Sum up the total minutes played by each Matilda
Matildas_minutes <- all_players_minutes %>%
  filter(team.id == Australia_team_id) %>%
  group_by(player.id) %>%  # Group data by player.id
  summarise(total_minutes_played = round(sum(MinutesPlayed), digits = 2), # Sum up the total minutes played
            .groups = "drop")  

# Join the minutes data to the shot stats data
shots_Matildas_stats <- shots_Matildas_stats%>%
  left_join(Matildas_minutes, by = "player.id")

# Calculate per 90-minute metrics for shots
shots_Matildas_stats <- shots_Matildas_stats%>%
  mutate(nineties = round(total_minutes_played/90, digits=2))

shots_Matildas_stats <- shots_Matildas_stats%>%
  mutate(shots_p90 = shots/nineties)

# Group by player and calculate total key passes (shot assists + goal assists)
key_passes_Matildas <- passes_Matildas %>%
  group_by(player.id, player.name, last_name) %>% 
  summarise(total_key_passes = sum(pass.shot_assist, na.rm = TRUE) + sum(pass.goal_assist, na.rm = TRUE))

# Merge key_passes_Matildas data with shots_Matildas_stats data - we need this step here to get access to the 'nineties' column in the 'shots_Matildas_stats' dataframe
complete_stats_Matildas <- full_join(shots_Matildas_stats, key_passes_Matildas, by = "player.id")

# Calculate key passes per 90 minutes
complete_stats_Matildas <- complete_stats_Matildas %>%
  mutate(key_passes_p90 = total_key_passes / nineties)

# Calculate value added in attack per 90 minutes
complete_stats_Matildas <- complete_stats_Matildas %>%
  mutate(value_added_attack_p90 = shots_p90 + key_passes_p90)

# Handle any NAs by setting them to zero
complete_stats_Matildas <- complete_stats_Matildas %>%
  mutate(
    shots_p90 = coalesce(shots_p90, 0),  # Replace NA with 0 if any
    key_passes_p90 = coalesce(key_passes_p90, 0),  # Replace NA with 0 if any
    value_added_attack_p90 = shots_p90 + key_passes_p90  # Now this should work correctly
  )

#* Converting Data to Long Format for Plotting ----
chart_data <- complete_stats_Matildas %>%
  ungroup() %>%
  select(last_name.x, shots_p90, key_passes_p90, value_added_attack_p90) %>% 
  arrange(desc(value_added_attack_p90)) %>% # Sorting players by value_added_attack_p90
  pivot_longer(c(shots_p90, key_passes_p90), names_to = "variable", values_to = "value")

#* Creating the Stacked Bar Plot ----
# Create the ggplot object
attack_viz <- ggplot(chart_data, aes(x = reorder(last_name.x, value), y = value, fill=fct_rev(variable))) +
  geom_bar(stat="identity", position = "stack", color = "black", size = 0.5) +
  geom_text(aes(label = ifelse(value > 0, round(value, 2), "")), colour = "black", size = 4, hjust = 0.35, position = position_stack(vjust = 0.61)) + # Labels inside bars
  coord_flip() +
  scale_y_continuous(position = "right",
                     limits = c(0,6.3),
                     breaks = seq(0, 6, by = 1),
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(0, 0.7)) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines
    panel.grid.major.x = element_line(color = "#A8BAC480", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Customize the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Customize labels
    axis.text.y = element_text(family = "sans", size = 16, color = "black"),
    axis.text.x = element_text(family = "sans", size = 16, color = "black"),
    # Customize legend
    legend.title = element_blank(),
    legend.text = element_text(family = "sans", size = 16, color = "black"), 
    legend.position = c(0.8, 0.5)
  ) +
  scale_fill_manual(
    values = c("shots_p90" = "#f1c40f", "key_passes_p90" = "#1abc9c"),
    labels = c("Shots P90", "Key Passes P90")
  ) +
  labs(title = "Matildas' Offensive Prowess",
       subtitle = "Shots Per 90 and Key Passes Per 90 at FIFA Women's World Cup 2023") +
  theme(
    plot.title = element_text(
      family = "sans", 
      face = "bold",
      size = 16,
      color = "black"
    ),
    plot.subtitle = element_text(
      family = "sans",
      size = 16,
      color = "black"
    )
  )

#* Adding the StatsBomb Logo ----
img_path <- "SB_logo.png"

attack_viz_sb <- ggdraw(attack_viz) +
  draw_image(img_path, x = 0.8, y = 0, width = 0.13, height = 0.065)

attack_viz_sb <- ggdraw(attack_viz_sb) +
  draw_label("Data:", x = 0.77, y = 0.033, size = 16)

# Show the final plot
print(attack_viz_sb)

#* Saving the Visualization to png ----

# ggsave("Matildas_Offensive_Prowess.png", plot = attack_viz_sb, width = 10, height = 8, dpi = 300)
