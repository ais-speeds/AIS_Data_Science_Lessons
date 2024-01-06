rm(list = ls()) # clear the workspace

library(dplyr) # provides functions for manipulating datasets
library(tidyr) # tools for tidying data, reshaping and restructuring datasets
library(readr) # offers fast and user-friendly functions to read tabular data into R
library(stringr) # simplifies the process of working with strings, providing consistent and easy-to-understand functions
library(visdat) # enables visual inspection of data quality and structure
library(speedsR) # collection of sports-specific benchmark datasets - part of the AIS SPEEDS project



# Load Datasets from speedsR and Save as CSV Files ----

# List of dataset names from speedsR
dataset_names_from_speedsR <- c("cycling_untidy_fictitious_data_1", 
                                "cycling_untidy_fictitious_data_2", 
                                "cycling_untidy_fictitious_data_3", 
                                "cycling_untidy_fictitious_data_4", 
                                "cycling_untidy_fictitious_data_5")

# Loop through each dataset name from speedsR package
for (dataset_name in dataset_names_from_speedsR) {
  
  # Use 'get()' to load the dataset from speedsR using its name
  # 'get()' retrieves an object (here, a dataset) by its name given as a string
  dataset <- get(dataset_name)
  
  # Use 'paste0()' to create a file name for saving the dataset
  # 'paste0()' concatenates strings, here it adds ".csv" to the dataset name
  file_name <- paste0(dataset_name, ".csv")
  
  # Save the loaded dataset as a CSV file with the name created above
  write_csv(dataset, file_name)
}



# Load and Concatenate Datasets ----

# Get a list of all files in the *current directory* that match the pattern “cycling_untidy_fictitious_data_.*csv”
# Store the name of each file in a vector 'cycling_files'
cycling_files <- list.files(pattern = 'cycling_untidy_fictitious_data_.*csv')

# Read each file in 'cycling_files' into a tibble, storing all the tibbles in 'cycling_list'
cycling_list <- lapply(cycling_files, read_csv)

# Concatenate all tibbles in 'cycling_list' together
cycling_data <- bind_rows(cycling_list)

# write_csv(cycling_data, "cycling_untidy_fictitious_data_before_cleaning.csv")



# Explore and Diagnose the Dataset ----

# Display the first few rows of the dataset
head(cycling_data)

# Get a statistical summary of the dataset
summary(cycling_data)

# Find out the number of rows in the dataset
nrow_cycling_data <- nrow(cycling_data)

# Find out the dimensions of the dataset
dims_cycling_data <- dim(cycling_data)



# Clean Column Names ----

# Retrieve and print the original column names from the dataset
col_names <- colnames(cycling_data)
print(col_names)

# Convert all column names to lower case for consistency and ease of use
col_names <- tolower(col_names)
print(col_names)

# Remove any leading and trailing white spaces from the column names
# '^\\s+' matches leading spaces and '\\s+$' matches trailing spaces
col_names <- gsub("^\\s+|\\s+$", "", col_names)

# Replace spaces with underscores to ensure column names follow snake_case format
# This improves readability and adherence to common naming conventions
col_names <- gsub(" ", "_", col_names)

# Apply the cleaned column names back to the dataset
colnames(cycling_data) <- col_names

# Manually rename specific columns for clarity and standardization
# Example: renaming 'vo2_max' to 'VO2_max' and 'id' to 'ID' for better readability or convention
cycling_data <- rename(cycling_data, 
                       VO2_max = vo2_max,
                       ID = id)

# Print the updated column names to verify changes
colnames(cycling_data)



# Handling Missing Data ----

## Empty Cells ----

# Identify and replace empty cells with NA across the entire dataset
cycling_data[cycling_data == ""] <- NA

# Print the dataset to verify that empty cells are replaced with NA
print(cycling_data)

## Missing (NA) Values ----

# Count the number of NA values in each column of the tibble
NA_count_per_column <- sapply(cycling_data, function(x) sum(is.na(x)))
print(NA_count_per_column)

# If you don't need to know the missing values tally per column, 
# you can compute the total number of missing values in the entire tibble
total_NA_count <- sum(is.na(cycling_data))
print(total_NA_count)

# Visualise missing data in the dataset
vis_miss(cycling_data)



# Replacing Certain Values ----

## Replace Values In A Specific Column ----

# Replace values in a particular column ('team' in our example) that meet a specific condition
cycling_data$team[cycling_data$team == "Tasmania_Track"] <- "ACT_Road"

# Print the updated column to verify the changes
print(cycling_data$team)

## Replace A Value In The Entire Dataset ----

# Modify numeric columns in the dataset, excluding the ID column.
# This operation replaces all numeric values less than or equal to 40 with 50,
# while preserving the unique identifiers in the ID column.
# The 'mutate_if' function is used here to selectively apply the replacement
# only to columns that are numeric and not the ID column.
cycling_data <- cycling_data %>%
  mutate_if(~is.numeric(.) && !identical(., cycling_data$ID), ~ifelse(. <= 40, 50, .))

# Optional - 'mutate_all' will apply the specified condition to all columns in the tibble, including the ID column
# Uncomment the two lines below if you want to see how this works
# cycling_data <- cycling_data %>%
#   mutate_all(~ifelse(is.numeric(.) & . <= 40, 50, .))

# Print the updated dataset to verify the changes
print(cycling_data)



# Dealing With Duplicates ----

## Identify Duplicates ----

# Identify duplicates including the ID column
# Useful when checking for completely identical rows including identifiers
duplicates_including_id <- duplicated(cycling_data)
# Print Boolean vector showing duplicates
print(duplicates_including_id)

# Identify duplicates excluding the ID column
# This helps to find duplicates based on data content, ignoring unique identifiers (the ID column)
duplicates_excluding_id <- duplicated(cycling_data[-1])
# Print Boolean vector showing duplicates
print(duplicates_excluding_id)

# Print rows that are duplicates (excluding ID)
# This gives a clear view of which rows are duplicate entries
duplicate_indices_excluding_id <- which(duplicates_excluding_id)
print(cycling_data[duplicate_indices_excluding_id, ])

## Remove Duplicates ----

# Remove duplicates from the dataset, excluding the ID column
# Preserve the order of the dataset by updating the ID column with the 'row_number()' function
cycling_data <- cycling_data %>%
  select(-ID) %>% # Temporarily remove the ID column
  distinct() %>% # Eliminate duplicate rows based on remaining columns
  mutate(ID = row_number()) # Add a new ID column with sequential numbers

# Reorder columns to place the new ID column at the beginning
cycling_data <- cycling_data %>%
  select(ID, everything())

# Verify that duplicates have been removed
duplicates_check <- duplicated(cycling_data[-1])
print(duplicates_check)
# Printing the output of the table() function should yield count 0 for TRUE after cleaning
print(table(duplicates_check))



# Splitting By Index ----

# Extract the first character from 'gender_age' and create a new column 'gender'
# This operation isolates the gender indicator (e.g., 'M' or 'F') from the combined 'gender_age' column
cycling_data <- cycling_data %>%
  mutate(gender = str_sub(gender_age, start=1, end=1))

# Extract the remaining characters (starting from the second character) from 'gender_age' and create a new column 'age'
# This operation separates the age portion from the combined 'gender_age' column
cycling_data <- cycling_data %>%
  mutate(age = str_sub(gender_age, start=2))

head(cycling_data)

# Remove the original 'gender_age' column as it's now split into 'gender' and 'age'
# This step cleans the dataset by removing redundant columns
cycling_data <- cycling_data %>%
  select(-gender_age)

# Reorder columns to place 'gender' and 'age' between 'participant' and 'heart_rate'
cycling_data <- cycling_data %>%
  select(ID, race_date, team, participant, gender, age, everything())

head(cycling_data)



# Splitting By Character ----

# The 'separate()' function is used to split the 'team' column into two new columns: 'state' and 'class'
# The underscore '_' character is used as the separator
cycling_data <- cycling_data %>%
  separate(team, c('state_team', 'class'), '_', extra = 'merge')

head(cycling_data)



# Data Types ----

# Use 'str()' to view the structure of the 'cycling_data' tibble
# This function gives an overview of the types of data in each column
str(cycling_data)

# Attempt to compute the mean of the VO2_max column
# Note: This operation will produce a warning since the column is not numeric
# This issue will be addressed in the next section
cycling_data %>% summarise(mean_VO2_max = mean(VO2_max))



# String Parsing ----

# Remove the non-numeric part (' mL/kg/min') from the VO2_max column
# This prepares the column for conversion to a numeric data type
cycling_data <- cycling_data %>%
  mutate(VO2_max = gsub(' mL/kg/min', '', VO2_max))

# Convert the VO2_max column to a numeric data type
# This allows for numerical operations to be performed on the VO2_max values
cycling_data <- cycling_data %>%
  mutate(VO2_max = as.numeric(VO2_max))

head(cycling_data)

# Recompute the mean of the VO2_max column, excluding NA values for accurate calculation
# The 'na.rm = TRUE' argument ensures that NA (missing) values are ignored in the mean calculation
cycling_data %>% summarise(mean_VO2_max = mean(VO2_max, na.rm = TRUE))



# Dates Formatting and Ordering ----

# Convert 'race_date' from 'DD-MM-YYYY' format to 'YYYY-MM-DD' format
# This specifies the current format of the dates in your dataset
# and standardizes them to the default 'YYYY-MM-DD' format for consistency and easier analysis
cycling_data <- cycling_data %>%
  mutate(race_date = as.Date(race_date, format = "%d-%m-%Y"))

# Arrange the data by 'race_date' in descending order
# This helps in analyzing the data from the most recent to the earliest dates
cycling_data <- cycling_data %>%
  arrange(desc(race_date)) %>%
  mutate(ID = row_number()) # Update IDs to reflect the new order after arranging dates

head(cycling_data)



# Reshaping Data From Wide to Long Format ----

# Transform the 'cycling_data' tibble from wide format to long format.
# The 'gather()' function collapses the 'heart_rate', 'distance_km', and 'VO2_max' columns
# into key-value pairs, making the data structure more flexible for analysis.
# The '-variable' notation is used to exclude certain columns from being gathered.
# Including the '-variable' notation is optional and we include it here for completeness.
cycling_data <- cycling_data %>%
  gather(heart_rate, distance_km, VO2_max, key = "measurement", value = "value", -ID, -race_date, -state_team, -class, -participant,
         -gender, -age)

# Arrange the reshaped data by ID and measurement types.
# This ordering helps to organize the data for each participant in a systematic way.
cycling_data <- cycling_data %>%
  arrange(ID, measurement)

head(cycling_data)

# Count the number of occurrences of each measurement type.
# This helps to understand the distribution of different measurements in the dataset.
measurement_counts <- count(cycling_data, measurement)
print(measurement_counts)

# write_csv(cycling_data, "cycling_tidy_fictitious_data_after_cleaning.csv")