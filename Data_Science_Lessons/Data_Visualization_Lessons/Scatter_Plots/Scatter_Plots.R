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

# ---- Multidimensional Scatter Plot ----
### Matilda's Shooting Efficiency at FIFA Women's World Cup 2023

#* Data Filtering ----
# Extract relevant shot data for Matildas from StatsBombData
shots_Matildas <- StatsBombData%>%
  select(player.name, player.id, team.name, team.id, type.name, shot.type.name, shot.outcome.name, shot.statsbomb_xg, location.x, location.y, DistToGoal)%>%
  filter(team.name == "Australia")%>%
  filter(type.name == "Shot" & (shot.type.name != "Penalty"|is.na(shot.type.name)))%>%
  filter(shot.outcome.name %in% c("Goal", "Off T", "Saved", "Saved to Post", "Post"))

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

# Reorder the columns to have last_name after player.name
shots_Matildas <- shots_Matildas %>%
  select(player.name, last_name, everything())

#* Computing Stats Per Player ----
# Calculate stats for each player
# Group by player and compute the following metrics: the number of shots they have taken, total NPxG, average distance to goal, and whether a player scored a goal
shots_Matildas_stats <- shots_Matildas%>%
  group_by(player.name, player.id, last_name)%>%
  summarise(shots=sum(type.name=="Shot", na.rm=TRUE),
            npxg=sum(shot.statsbomb_xg),
            avg_dist = mean(DistToGoal, na.rm = TRUE),
            scored_goal = any(shot.outcome.name == "Goal"),
            .groups = "drop")

#* Normalizing Metrics to 90 Minutes of Play ----
# Get the total minutes played by each player
all_players_minutes = get.minutesplayed(StatsBombData)

# Extract Australia's team ID
Australia_team_id <- unique(shots_Matildas$team.id)

# Sum up the total minutes played by each Matilda
Matildas_minutes <- all_players_minutes %>%
  filter(team.id == Australia_team_id) %>%
  group_by(player.id) %>%  # Group data by player.id
  summarise(total_minutes_played = round(sum(MinutesPlayed), digits = 2), # Sum up total minutes played
            .groups = "drop")  

# Join the minutes data with the shot stats data
shots_Matildas_stats <- shots_Matildas_stats%>%
  left_join(Matildas_minutes, by = "player.id")

# Normalize shots and NPxG per 90 minutes of play
shots_Matildas_stats <- shots_Matildas_stats%>%
  mutate(nineties = round(total_minutes_played/90, digits=2))

shots_Matildas_stats <- shots_Matildas_stats%>%
  mutate(shots_p90 = shots/nineties,
         npxg_p90 = npxg/nineties)

# Bin the average distance
shots_Matildas_stats <- shots_Matildas_stats %>%
  mutate(avg_dist_binned = cut(avg_dist, breaks = seq(0, max(avg_dist) + 10, by = 10)))

# Convert to factors for easier plotting
shots_Matildas_stats$avg_dist_binned <- as.factor(shots_Matildas_stats$avg_dist_binned)
shots_Matildas_stats$scored_goal <- as.factor(shots_Matildas_stats$scored_goal)

#* Creating the Multidimensional Scatter Plot ----
# Create the scatter plot
shot_effic_viz <- ggplot(data=shots_Matildas_stats,
                         aes(x = shots_p90,
                             y = npxg_p90)) +
  geom_point(aes(fill = avg_dist_binned,
                 shape = scored_goal),
             size = 6,
             stroke = 0.6,
             colour = "black",
             alpha = 0.8) +
  geom_text_repel(data = shots_Matildas_stats,
                  aes(x = shots_p90,
                      y = npxg_p90,
                      label = last_name),
                  size = 5.5, #4.5
                  box.padding = 0.5,
                  point.padding = 0.5) +
  xlim(0, 4) +
  ylim(0, 0.3) +
  scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 24), name ="", 
                     labels = c("Not Scored", "Scored")) +
  scale_fill_manual(values = c("#1abc9c", "#e74c3c", "#9b59b6", "#f1c40f"), name = "Av. Shot Dist. [m]",   
                    labels = c("0-10", "10-20", "20-30", "30-40")) +
  guides(fill = guide_legend(override.aes = list(shape = 22)),
         shape = guide_legend(override.aes = list(fill = "black"))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines
    panel.grid.major.y = element_line(color = "#A8BAC480", size = 0.3),
    # Set the color and the width of the grid lines
    panel.grid.major.x = element_line(color = "#A8BAC480", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Customize the title for both axes
    axis.title = element_text(family = "sans", size = 16, color = "black"),
    # Labels for the horizontal axis
    axis.text.x = element_text(family = "sans", size = 16, color = "black"),
    # Labels for the vertical axis
    axis.text.y = element_text(family = "sans", size = 16, color = "black"),
    legend.text = element_text(family = "sans", size = 16, color = "black"), #12
    legend.position = "right",
    legend.key = element_blank(),
    legend.title = element_text(family = "sans", size = 16, face = "bold", color = "black") #14
  ) + 
  labs(title = "Matildas' Shooting Efficiency at FIFA Women's World Cup 2023",
       subtitle = "Non-Penalty Expected Goals per 90 Minutes (NPxG P90) vs Shots per 90 Minutes\nColored by Average Shot Distance and Shaped by Goal Scoring",
       x = "Shots P90",
       y = "NPxG P90") +
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

# Add the StatsBomb logo to the existing plot
shot_effic_viz_sb <- ggdraw(shot_effic_viz) +
  draw_image(img_path, x = 0.87, y = 0, width = 0.12, height = 0.06)

# Add a label to indicate the data source
shot_effic_viz_sb <- ggdraw(shot_effic_viz_sb) +
  draw_label("Data:", x = 0.84, y = 0.03, size = 16)

# Display the finalized plot
print(shot_effic_viz_sb)

#* Saving the Visualization to png ----

# ggsave("Matildas_Shooting_Efficiency.png", plot = shot_effic_viz_sb, width = 10, height = 8, dpi = 300)


# ---- Tiled Map of Scatter Plots ----
### Matildas' Shot Maps at FIFA Women's World Cup 2023

#* Refining Data ----
# Add a new column to categorize each shot as either a "Goal" or "No Goal"
shots_Matildas <- shots_Matildas %>% 
  mutate(shot_outcome = ifelse(shot.outcome.name == "Goal", "Goal", "No Goal"))

#* Creating the Tiled Map of Scatter Plots ----
# Initialize an empty ggplot object
shot_map_viz <- ggplot() +
  # Draw the soccer pitch
  annotate_pitch(dimensions = pitch_statsbomb,
                 colour = "white",
                 fill = "springgreen4",
                 limits = FALSE) +
  theme_pitch() +
  # Set background color
  theme(panel.background = element_rect(fill = "springgreen4")) +
  # Flip x and y axes to orient the pitch vertically
  coord_flip(xlim = c(60, 120)) +
  # Reverse y-axis
  scale_y_reverse() +
  # Plot each Matildas' shot
  geom_point(data = shots_Matildas,
             aes(x = location.x,
                 y = 80 - location.y,
                 fill = shot_outcome,
                 size = shot.statsbomb_xg),
             shape = 21, # Circle shape
             stroke = 0.3, # Circle border thickness
             colour = "black") + # Circle border color
  # Manually set the colors for shot outcomes
  scale_fill_manual(values = c("Goal" = "#e74c3c", "No Goal" = "#f1c40f"), 
                    name = "Shot Outcome", 
                    labels = c("Goal", "No Goal")) +
  guides(size = guide_legend(override.aes = list(fill = "#f1c40f")),
         fill = guide_legend(override.aes = list(size = 3))) +
  # Create panels for each player's shot map
  facet_wrap(~last_name) +
  # Add statistics for shots per 90 minutes, NPxG per 90 minutes, and average shot distance
  geom_text(data = shots_Matildas_stats,
            aes(x=80,
                y=80-15,
                label = paste("Shots P90:", round(shots_p90, digits=3))),
            size = 2.7) + #2.2
  geom_text(data = shots_Matildas_stats,
            aes(x=74,
                y=80-15,
                label = paste("NPxG P90:", round(npxg_p90, digits=3))),
            size = 2.7) +
  geom_text(data = shots_Matildas_stats,
            aes(x=68,
                y=80-15,
                label = paste("Avg. Dist.:", round(avg_dist, digits=3))),
            size = 2.7) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines
    panel.grid.major.y = element_blank(),
    # Set the color and the width of the grid lines
    panel.grid.major.x = element_blank(),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Customize the title for both axes
    axis.title = element_blank(),
    # Labels for the horizontal axis
    axis.text.x = element_blank(),
    # Labels for the vertical axis
    axis.text.y = element_blank(),
    legend.text = element_text(family = "sans", size = 16, color = "black"), #12
    legend.position = "top",
    legend.key = element_blank(),
    legend.title = element_text(family = "sans", size = 16, face = "bold", color = "black"), # 14
    strip.text = element_text(hjust = 0.5, size = 16, face = "bold"), #12
    strip.background = element_blank()
  ) + 
  labs(title = "Matildas' Shot Maps at FIFA Women's World Cup 2023",
       subtitle = "Only Non-Penalty Shots That Reached the Goal",
       size = "Non-Penalty xG") +
  theme(
    plot.title = element_text(
      family = "sans", 
      face = "bold",
      size = 16,
      color = "black",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      family = "sans",
      size = 16,
      color = "black",
      hjust = 0.5
    )
  )

#* Adding the StatsBomb Logo ----
img_path <- "SB_logo.png"

shot_map_viz_sb <- ggdraw(shot_map_viz) +
  draw_image(img_path, x = 0.7, y = 0, width = 0.13, height = 0.065)

# Add a label indicating the data source
shot_map_viz_sb <- ggdraw(shot_map_viz_sb) +
  draw_label("Data:", x = 0.67, y = 0.033, size = 16)

# Display the final plot
print(shot_map_viz_sb)

#* Saving the Visualization to png ----

# ggsave("Matildas_Shot_Maps.png", plot = shot_map_viz_sb, width = 10, height = 8, dpi = 300)


# ---- Scatter Plot with Kernel Density Estimation ----
### Penalty Shots Map

#* Data Filtering - All Penalties ----
# Check each column to see if it contains the word "Penalty"
penalty_columns <- sapply(StatsBombData, function(x) any(x == "Penalty", na.rm=TRUE))
# Keep the names of columns where "Penalty" is found
penalty_columns <- names(penalty_columns[penalty_columns == TRUE])

# Display columns containing "Penalty"
penalty_columns

# Select specific columns to keep for the penalty analysis
selected_columns <- c("match_id", "player.name", "team.name", penalty_columns,
                      "shot.outcome.name", "shot.end_location.x",
                      "shot.end_location.y", "shot.end_location.z", "period")

# Filter only the rows where shot type is "Penalty"
all_penalties <- StatsBombData%>%
  select(all_of(selected_columns))%>%
  filter(shot.type.name == "Penalty")

# Rename columns to be more readable
all_penalties <- all_penalties %>%
  rename(
    penalty_taker = player.name,
    penalty_taker_team = team.name,
    match_phase = period
  )

#* Feature Engineering ----
# Add a new column 'match_phase' to indicate when the penalty was taken (In-game or Shootout)
all_penalties <- all_penalties%>%
  mutate(
    match_phase = case_when(
      match_phase %in% 1:4 ~ "In-Game",
      match_phase == 5 ~ "Penalty Shootout",
      TRUE ~ as.character(match_phase)
    )
  )

# Join 'Matches' data to get home and away teams
all_penalties <- all_penalties%>%
  left_join(Matches %>% select(match_id, home_team, away_team), by = "match_id")

# Create a column 'shot_outcome' to simplify shot outcome into "Goal" or "Missed"
all_penalties <- all_penalties %>%
  mutate(shot_outcome = ifelse(shot.outcome.name == "Goal", "Goal", "Missed"))

# Reorder the columns
all_penalties <- all_penalties %>%
  select(match_id, home_team, away_team, 
         penalty_taker, penalty_taker_team, 
         shot.type.name, match_phase, 
         shot.outcome.name, shot_outcome, shot.end_location.x, 
         shot.end_location.y, shot.end_location.z)

#* Building the Scatter Plot with Kernel Density Estimation ----

#** Initializing the Plot ----
# Filter successful and unsuccessful penalties
scored_penalties <- all_penalties %>% filter(shot.outcome.name == "Goal")
missed_penalties <- all_penalties %>% filter(shot.outcome.name != "Goal")

# Define dimensions for the goal and plot area
floor = 0
x_left = 36
x_right = 44
y_crossbar = 2.8

canvas_left = 34.9
canvas_right = 45.2
canvas_top = 6.5
canvas_bottom = -0.9

# Initialize ggplot
pen_viz <- ggplot() + 
  xlim(canvas_left, canvas_right) + ylim(canvas_bottom, canvas_top) +
  coord_fixed(ratio = 0.9) +
  labs(title="Penalty Shots Map",
       subtitle = "All penalty shots (both in-game and shootouts) at FIFA Women's World Cup 2023") +
  # Customize plot appearance
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        plot.subtitle = element_text(color="black", size=16, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Define line thickness for the goal frame
goal_frame_thick <- 4

#** Creating a Density Plot ----
# Add density plot for successful penalties
pen_viz <- pen_viz +
  stat_density_2d(data=scored_penalties, geom = "polygon",
                  aes(x=shot.end_location.y, y=shot.end_location.z, fill = after_stat(level)),
                  alpha = 0.2, bins = 500) + 
  scale_fill_gradientn(name = "Penalty Scoring Density",
                       colours=rev(brewer.pal(7, "RdYlBu"))) + #Spectral
  theme(legend.position = c(0.58, 0.06),
        legend.direction = "horizontal",
        legend.title = element_text(size = 14, color = "black"),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = "white")) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

# Clip density plot to the goal frame dimensions
pen_viz <- pen_viz +
  geom_rect(aes(xmin = canvas_left, xmax = x_left, ymin = canvas_bottom, ymax = canvas_top),
            fill = "white") +
  geom_rect(aes(xmin = x_right, xmax = canvas_right, ymin = canvas_bottom, ymax = canvas_top),
            fill = "white") +
  geom_rect(aes(xmin = x_left, xmax = x_right, ymin = y_crossbar, ymax = canvas_top),
            fill = "white") +
  geom_rect(aes(xmin = x_left, xmax = x_right, ymin = canvas_bottom, ymax = floor),
            fill = "white")

# Define data for drawing the goal frame
goal_frame_data <- data.frame(
  x = c(x_left, x_left, x_right, x_right),
  y = c(floor, y_crossbar, y_crossbar, floor)
)

# Draw the goal frame
pen_viz <- pen_viz +
  geom_path(data=goal_frame_data, aes(x=x, y=y), 
            color="black", size=goal_frame_thick)


# Load a ball symbol png file
img <- readPNG("ball_symbol.png")

# Create a raster graphical object
ball <- rasterGrob(img, interpolate=TRUE)

ball_size = 0.15

# Add the red crosses for the 'Missed' penalties
pen_viz <- pen_viz + geom_point(data = missed_penalties,
                                aes(x=shot.end_location.y, y=shot.end_location.z),
                                shape = 4, color = '#e74c3c', size = 3, stroke = 2)

# Loop through each row in the dataframe with 'Goal' penalties
for(i in 1:nrow(scored_penalties)) {
  
  # Extract the current shot's coordinates
  x <- scored_penalties$shot.end_location.y[i]
  y <- scored_penalties$shot.end_location.z[i]
  
  # Annotate with the ball image
  pen_viz <- pen_viz + annotation_custom(ball, 
                                         xmin = x - ball_size, xmax = x + ball_size, 
                                         ymin = y - ball_size, ymax = y + ball_size)
}

#** Adding Annotations ----
# Coordinates for both penalties
symbol_x_scored = 42.2
symbol_y_scored = 0.2

symbol_x_missed = 44.0
symbol_y_missed = 1.6


# Create a data frame containing the points to be annotated
annotate_data <- data.frame(
  x = c(symbol_x_scored, symbol_x_missed),
  y = c(symbol_y_scored, symbol_y_missed),
  label = c("C. Vine", "M. Arnold")
)

pen_viz <- pen_viz +
  geom_text_repel(data = annotate_data,
                  aes(x = x, y = y, label = label),
                  box.padding = 0.5,  # Space around each label
                  point.padding = 0.5,  # Space around each point
                  segment.color = "#9b59b6",
                  segment.size = 1.5,
                  nudge_x = 0.9,  # Horizontal adjustment
                  nudge_y = -0.4,  # Vertical adjustment
                  direction = "both",  # ('x' or 'y'): Direction in which to move labels
                  arrow = arrow(type = "closed", length = unit(2.1, "mm")),
                  size = 6,
                  color = "#9b59b6") # Text size and color

#* Adding the StatsBomb Logo ----
# Insert the Statsbomb logo and the legend ----
img_path_SB_logo <- "SB_logo.png"
img_path_ball <- "ball_symbol.png"

pen_viz_sb <- ggdraw(pen_viz) +
  draw_image(img_path_SB_logo, x = 0.735, y = 0.06, width = 0.13, height = 0.065)
pen_viz_sb <- ggdraw(pen_viz_sb) +
  draw_label("Data:", x = 0.71, y = 0.09, size = 16)

pen_viz_sb <- ggdraw(pen_viz_sb) +
  draw_image(img_path_ball, x = 0.27, y = 0.07, width = 0.07, height = 0.035)
pen_viz_sb <- ggdraw(pen_viz_sb) +
  draw_label("Scored", x = 0.25, y = 0.09, size = 16)


x_center <- 0.43  # x-coordinate of the center of the cross
y_center <- 0.09  # y-coordinate of the center of the cross
length <- 0.02    # length of each arm of the cross from the center
x1 <- x_center - length / sqrt(2)
y1 <- y_center - length / sqrt(2)
x2 <- x_center + length / sqrt(2)
y2 <- y_center + length / sqrt(2)

pen_viz_sb <- pen_viz_sb +
  geom_point() +  
  annotate("segment", x = x1, xend = x2, y = y1, yend = y2, color = "#e74c3c", size = 1) +
  annotate("segment", x = x1, xend = x2, y = y2, yend = y1, color = "#e74c3c", size = 1)


pen_viz_sb <- ggdraw(pen_viz_sb) +
  draw_label("Missed", x = 0.38, y = 0.09, size = 16)

# Show the final visualization
print(pen_viz_sb)

#* Saving the Visualization to png ----
# ggsave("Penalty_Shots_Map.png", plot = pen_viz_sb, width = 10, height = 8, dpi = 300)