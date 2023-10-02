rm(list = ls()) # clean the workspace
# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(naniar)
library(synthpop)

my_seed = 125

##### Data import and exploration #####


### Data import ###

# Import Parvo csv data

path = "/Users/yausa/LTU_Projects/Private_Datasets/Parvo/AIS_Parvo_Ex1.csv"

# Read just the header row (row 26) to get the column names
headers <- read.csv(path, skip = 25, nrows = 1, header = FALSE)
col_names <- as.character(unlist(headers))

# Skip the first 29 rows and read the actual data from row 30 to row 408
# Note: header = FALSE because we'll assign the column names manually
real_data <- read.csv(path, skip = 29, nrows=379, header = FALSE)

# Assign the column names to the real data
colnames(real_data) <- col_names

# Remove all white spaces from the column names
names(real_data) <- gsub("\\s+", "", names(real_data))

# Identify the positions of the columns named "VE/"
positions <- which(names(real_data) == "VE/")

# Rename the first occurrence to "VE1" and the second occurrence to "VE2"
names(real_data)[positions[1]] <- "VE1"
names(real_data)[positions[2]] <- "VE2"

real_data <- real_data %>% rename(VO2_kg = `VO2/kg`)


# Function to convert the "mm:ss" values in the TIME column into a total number of seconds 
convert_to_seconds <- function(time_str) {
  time_parts <- strsplit(time_str, ":")[[1]]
  minutes <- as.numeric(time_parts[1])
  seconds <- as.numeric(time_parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

# Apply the function to create a new vector of total seconds
total_seconds <- sapply(real_data$TIME, convert_to_seconds)

# Add this new vector as a new column immediately after the TIME column
real_data <- real_data %>% add_column(TIME_seconds = total_seconds, .after = "TIME")



# # Time series data visualization
# 
# system.time({
#   # Get all column names from real_data
#   all_columns <- names(real_data)
#   
#   # Remove TIME and TIME_seconds variables from the list of variables to be plotted
#   variables_to_plot <- setdiff(all_columns, c("TIME", "TIME_seconds"))
#   
#   # Create an empty list to store individual plots
#   plot_list <- list()
#   
#   # Loop through the list of variables to create plots
#   for(i in 1:length(variables_to_plot)) {
#     var_name <- variables_to_plot[i]
#     
#     p <- ggplot(real_data, aes(x = TIME_seconds, y = !!sym(var_name)))   + 
#       geom_line(color = "blue") +
#       labs(title = paste("Time Series Plot of", var_name),
#            x = "Time [seconds]",
#            y = var_name) +
#       theme_minimal()
#     
#     # Store the plot in the list
#     plot_list[[i]] <- p
#   }
#   
#   # Loop through the list of plots and display them in batches of 4, arranged in a 2-column grid
#   for(i in seq(1, length(plot_list), by = 4)) {
#     grid.arrange(grobs = plot_list[i:(i+3)], ncol = 2)
# }
# })


# # Descriptive statistics
# summary(real_data)


# ### Data missingness ###
# 
# sum(is.na(real_data)) # check how many missing values there are in the data set
# 
# vis_miss(real_data) # visualize the missingness in the dataset




# Generate synthetic data

# Remove TIME and TIME_seconds variables from the list of variables to be synthesized
vars_to_synth <- setdiff(names(real_data), c("TIME", "TIME_seconds"))

# Number of synthetic datasets to be generated with syn()
m_syn = 10


my.seed <- 176

# Specify the order of variables to be synthesised
variable_order <- c("WorkR", "VO2", "VCO2", "RER", "VE", "RR", "Vt", "FEO2", "FECO2", "PetCO2", "PetO2", "VO2_kg", "VE1", "VE2")
# Get the indices of these variables in the real_data dataframe
visit.sequence.ini <- match(variable_order, names(real_data))

method.ini <- c("", "", rep("cart", length(vars_to_synth)))


synth_data <- syn(real_data, visit.sequence = visit.sequence.ini, method = method.ini, m = 0, print.flag = TRUE, drop.not.used = FALSE)

print(synth_data)


# Customize the predictor matrix
predictor.matrix.corrected <- synth_data$predictor.matrix

# Set TIME_seconds as a predictor for all variables in the list
predictor.matrix.corrected[vars_to_synth, "TIME_seconds"] <- 1

predictor.matrix.corrected


system.time({
  synth_data <- syn(real_data, visit.sequence = visit.sequence.ini, method = method.ini, predictor.matrix = predictor.matrix.corrected, m = m_syn, seed = my.seed)
})

print(synth_data)



################################################################################
# Evaluate the quality of the generated synthetic datasets and pick the best one
################################################################################

# A function to generate basic descriptive statistics metrics
descr_stats <- function(df) {
  
  # Define a list of descriptive statistics functions
  basic_functs <- list(min = min, max = max, mean = mean, median = median, sd =  sd)
  
  # Compute basic descriptive statistics
  basic_stats <- sapply(basic_functs, function(f) {
    sapply(df, f, na.rm = TRUE)
  })
  
  # Transpose the basic_stats matrix so that rows are stats metrics and columns are variables
  basic_stats <- t(basic_stats)
  
  # Compute quantiles
  quantiles <- sapply(df, quantile, probs <- c(0.25, 0.75), na.rm = TRUE)
  
  # Combine basic stats and quantiles
  descr_stats_combined <- rbind(basic_stats, quantiles)
  
  # Convert to dataframe for better readability and flexibiity
  descr_stats_df <- as.data.frame(descr_stats_combined)
  
  return(descr_stats_df)
}



# Compute total quality score for each synthetic dataset

# To compute the quality score, we compute selected utility measures, 
# then compute a difference between descriptive statistics metrics of the real dataset and each of the generated synthetic ones, 
# and the quality score is the sum of the those two computed measures
# The best synthetic dataset is the one with the lowest total score

compute_total_score <- function(synth_data, real_data, vars_to_synth) {
  # # Extract only the selected variables
  synth_data_selected <- synth_data[, vars_to_synth]
  real_data_selected <- real_data[, vars_to_synth]

  # Compare synthetic data with real data
  comparison <- compare(synth_data_selected, real_data_selected,
                        stat = "counts", breaks = 20,
                        utility.stats = c("pMSE", "S_pMSE", "JSD", "SPECKS", "WMabsDD", "MabsDD", "dBhatt"))

  # Compute utility score
  utility_score <- sum(comparison$pMSE, comparison$S_pMSE, comparison$JSD,
                       comparison$SPECKS, comparison$WMabsDD, comparison$MabsDD, comparison$dBhatt)

  # Compute difference in descriptive statistics
  desc_stat_diff <- sum(abs(descr_stats(synth_data_selected) - descr_stats(real_data_selected)))

  # Return total score
  return(utility_score + desc_stat_diff)
}

# Apply the function above to each synthetic dataset and select the best one

# Compute total scores for each synthetic dataset
total_scores <- sapply(1:m_syn, function(i) compute_total_score(synth_data$syn[[i]], real_data, vars_to_synth))
print(total_scores)

# Find the index of the synthetic dataset with the lowest total score
best_synth_index <- which.min(total_scores)
print(best_synth_index)

# Extract the best synthetic dataset
best_synth_data <- synth_data$syn[[best_synth_index]]



# Plot the selected best synthetic dataset and visually compare it to the original one

# Create an empty list to store individual plots
plot_list <- list()

for(i in 1:length(vars_to_synth)) {
  var_name <- vars_to_synth[i]

  p <- ggplot() +
    geom_line(data = real_data, aes(x = TIME_seconds, y = !!sym(var_name)), color = "blue", alpha = 0.7) +
    geom_line(data = best_synth_data, aes(x = TIME_seconds, y = !!sym(var_name)), color = "red", alpha = 0.7) +
    scale_x_continuous(breaks = seq(0, max(real_data$TIME_seconds), by=50)) +
    labs(title = paste("Comparison of", var_name),
         x = "Time [s]",
         y = var_name) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_color_manual(values = c("blue", "red"), name = "Data", labels = c("Real Data", "Synthetic Data")) +
    theme(legend.position = "bottom")

  plot_list[[i]] <- p
}

grid.arrange(grobs = plot_list, ncols = 2)



##### Save the best synthetic dataset to a file #####

# write.csv(best_synth_data, "Parvo_synthetic_synthpop.csv", row.names = FALSE)
