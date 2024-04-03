rm(list = ls()) # clear the workspace

# Libraries required for data manipulation
library(dplyr) # provides functions for manipulating datasets
library(ggplot2) # provides functions for data visualizations
library(stringr) # simplifies the process of working with strings, providing consistent and easy-to-understand functions
library(visdat) # enables visual inspection of data quality and structure
library(speedsR) # collection of sports-specific benchmark datasets - part of the AIS SPEEDS project

# Load Data ----

# Loading datasets directly from the `speedsR` package
dataset_names <- c("cycling_untidy_fictitious_data_1", 
                   "cycling_untidy_fictitious_data_2", 
                   "cycling_untidy_fictitious_data_3", 
                   "cycling_untidy_fictitious_data_4", 
                   "cycling_untidy_fictitious_data_5")

# Concatenating datasets into a single tibble
cycling_data <- bind_rows(lapply(dataset_names, function(name) get(name)))

# Initial Data Exploration ----

# Display the first few rows of the dataset
head(cycling_data)

# Get a statistical summary of the dataset
summary(cycling_data)

# Find out the number of rows in the dataset
nrow_cycling_data <- nrow(cycling_data)

# Cleaning Column Names ----

# Retrieve and clean column names for consistency and ease of use
col_names <- colnames(cycling_data) %>%
  tolower() %>%
  gsub("^\\s+|\\s+$", "", .) %>%
  gsub(" ", "_", .)

# Apply the cleaned column names back to the dataset
colnames(cycling_data) <- col_names

# Manually rename specific columns for clarity
cycling_data <- rename(cycling_data, VO2_max = vo2_max, ID = id)

# Investigating Missingness ----

## Identifying and Addressing Empty Cells ----

# Replace empty cells with NA
cycling_data[cycling_data == ""] <- NA

## Counting Missing Values ----

# Count the number of NA values in each column
NA_count_per_column <- sapply(cycling_data, function(x) sum(is.na(x)))
print(NA_count_per_column)

# Compute the total number of missing values in the dataset
total_NA_count <- sum(is.na(cycling_data))
print(total_NA_count)

## Visualizing Missing Data ----

# Visualize missing data
vis_miss(cycling_data)

# Artificially Add Outliers In The Dataset And Analyze The Outliers (For Demonstration Purposes) ----

## Add Outliers ----

# Adding outliers to the heart_rate column
outlier_entries <- data.frame(
  ID = c(max(cycling_data$ID) + 1, max(cycling_data$ID) + 2),
  race_date = c(NA, NA),
  team = c(NA, NA),
  participant = c("Outlier_1", "Outlier_2"),
  gender_age = c(NA, NA),
  heart_rate = c(230, 260), # Artificially high outliers for demonstration purposes
  distance_km = c(NA, NA),
  VO2_max = c(NA, NA)
)

# Inserting outliers into the cycling_data dataset randomly
set.seed(123) # Ensuring reproducibility
random_rows <- sample(1:nrow(cycling_data), 2)
cycling_data <- rbind(cycling_data, outlier_entries)

## Analyze Mean and Median ----

mean_heart_rate <- mean(cycling_data$heart_rate, na.rm = TRUE)
median_heart_rate <- median(cycling_data$heart_rate, na.rm = TRUE)

print(paste("Mean heart rate:", mean_heart_rate))
print(paste("Median heart rate:", median_heart_rate))

## Visualize Outliers with a Boxplot ----

boxplot(cycling_data$heart_rate,
        main = "Heart Rate Distribution",
        ylab = "Heart Rate (bpm)",
        col = "lightblue",
        notch = FALSE) # Notches indicate the confidence interval around the median

## Calculate the Interquartile Range (IQR) and Define Outliers

Q1 <- quantile(cycling_data$heart_rate, 0.25, na.rm = TRUE)
Q3 <- quantile(cycling_data$heart_rate, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

upper_limit <- Q3 + 1.5 * IQR
lower_limit <- Q1 - 1.5 * IQR

print(paste("Q1 (25th percentile):", Q1))
print(paste("Q3 (75th percentile):", Q3))
print(paste("Interquartile Range (IQR):", IQR))
print(paste("Upper Limit:", upper_limit))
print(paste("Lower Limit:", lower_limit))

## Identify Outliers with Boolean Masks ----

# Boolean mask for outliers
outliers_mask <- with(cycling_data, heart_rate < lower_limit | heart_rate > upper_limit)

# Selecting outliers
outliers <- cycling_data[outliers_mask, ]
print("Outliers based on heart rate:")
print(outliers)

## Visualize Outliers with a Scatter Plot ----

# Scatter plot for heart_rate with outliers highlighted
ggplot(cycling_data, aes(x = ID, y = heart_rate)) +
  geom_point(aes(color = heart_rate < lower_limit | heart_rate > upper_limit), size = 2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(title = "Heart Rate Outliers", x = "Participant ID", y = "Heart Rate (bpm)") +
  theme_minimal() +
  theme(legend.title = element_blank())