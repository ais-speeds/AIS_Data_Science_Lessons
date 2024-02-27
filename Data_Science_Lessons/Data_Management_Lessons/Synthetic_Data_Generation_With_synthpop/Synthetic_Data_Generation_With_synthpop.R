rm(list = ls()) # clean the workspace
# Load required libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(naniar)
library(mice)
library(corrplot)
library(synthpop)

my_seed = 124

##### Data import and exploration #####


### Data import ###

# Reading the Hbmass Excel file and selecting the data sheet
sheets <- excel_sheets("./AHBM_Data.xlsx")
real_data <- read_excel("./AHBM_Data.xlsx", sheet="AHBM_LONG")

# Create an exact copy of the 'real_data' data frame (this might come handy later)
real_data_copy <- real_data


### Initial data exploration ###

# Descriptive statistics
summary(real_data)

# Pairwise Scatter plots
cont_var_cols <- real_data[, c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM", "RHBM")] # continuous variables columns
pairs(cont_var_cols, pch = 16, cex = 0.6) # pch - plotting character, cex - character expansion

# Distributions
# Create a data frame to store variable names and bin widths
cont_var_distrib <- data.frame(
  var = c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM", "RHBM"),
  bin_width = c(1.5, 5, 1, 2, 0.1, 25, 0.2)
)

# Loop through the data frame to create plots
for(i in 1:nrow(cont_var_distrib)) {
  var_name <- as.character(cont_var_distrib[i, "var"])
  bin_w <- cont_var_distrib[i, "bin_width"]
  
  p <- ggplot(real_data, aes_string(x = var_name)) + 
    geom_histogram(binwidth = bin_w, fill = "skyblue", color = "black") +
    labs(title = paste("Distribution of", var_name))
  
  print(p)
}

# Correlation Matrix
cor_matrix <- cor(cont_var_cols, use = "complete.obs")  # "complete.obs" means that only complete observations (rows where no variables have missing values) will be used in the calculations
corrplot(cor_matrix, method = "circle")


### Data missingness ###

sum(is.na(real_data)) # check how many missing values there are in the data set

vis_miss(real_data) # visualize the missingness in the dataset


##### Imputation of missing values #####


### Predictor matrix ###

# Generate an initial predictor matrix using quickpred function from the mice package
predictor_matrix <- mice::quickpred(real_data)

# Print the initial predictor matrix for inspection
print(predictor_matrix)

# Set predictor flags for columns ("FER", "FE", "TSAT", "TRANS") to 1, indicating they will be used for imputation.
predictor_matrix[c("FER", "FE", "TSAT", "TRANS"), ] <- 1

# Exclude "ID" from the predictor matrix by setting its flag to 0
vars_to_exclude <- c("ID")
predictor_matrix[, vars_to_exclude] <- 0

# Set the diagonal of the predictor matrix to 0, indicating that each variable should not predict itself
diag(predictor_matrix) <- 0

# Print the modified predictor matrix for final inspection
print(predictor_matrix)


### Data imputation with MICE ###

# Initialize a vector of empty strings, the length is the number of columns in the dataset
default_methods <- rep("", ncol(real_data))  

# Name the vector elements with the names of the columns in real_data
names(default_methods) <- colnames(real_data)

# Specify the PMM imputation method for the variables to be imputed
default_methods[c("FER", "FE", "TSAT", "TRANS")] <- "pmm"

# Set seed for reproducibility
set.seed(my_seed)

# Run MICE to perform multiple imputations: m=5 is the number of imputed datasets, maxit=50 is maximum iterations
imputed_data <- mice::mice(real_data, m = 5, method = default_methods, predictorMatrix = predictor_matrix, maxit = 50)

# View the final predictor matrix
print(imputed_data$predictorMatrix)


### Evaluating MICE imputed data quality ###


# Verify hierarchical structure #

completed_data_subset <- complete(imputed_data, 1)[, c("FER", "FE", "TSAT", "TRANS")] # Extract one of the imputed datasets and select only the target variables

# Ensure that there are no rows where FER is missing and the other three columns (FE, TSAT, TRANS) are observed
sum(is.na(completed_data_subset$FER) & (!is.na(completed_data_subset$FE) | !is.na(completed_data_subset$TSAT) | !is.na(completed_data_subset$TRANS)))


# Distributions check #

# Create a subset of the original data that contains only the variables with imputed values
subset_real_data <- real_data[, c("FER", "FE", "TSAT", "TRANS")]

# Create a data frame for variables that have imputed values to store the variable names and bin widths
cont_var_distrib <- data.frame(
  var = c("FER", "FE", "TSAT", "TRANS"),
  bin_width = c(5, 1, 2, 0.1)
) # continuous variables distributions

# Loop through the data frame to create plots
for(i in 1:nrow(cont_var_distrib)) {
  var_name <- as.character(cont_var_distrib[i, "var"])
  bin_w <- cont_var_distrib[i, "bin_width"]
  
  p <- ggplot() +
    geom_histogram(data = completed_data_subset, aes_string(x = var_name), binwidth = bin_w, fill = "red", alpha = 0.5, position = "identity") +
    geom_histogram(data = subset_real_data, aes_string(x = var_name), binwidth = bin_w, fill = "blue", alpha = 0.5, position = "identity") + 
    labs(title = paste("Distribution of", var_name, ": Imputed (Red) vs. Observed (Blue)"))
  # Setting 'position = "identity"' allows the histograms to overlap, facilitating easy comparison
  
  print(p)
}


# Correlations Check #

# Compare the correlation matrices to ensure that the relationships between variables in the imputed data are consistent with the original data
cor_matrix_imputed <- cor(completed_data_subset, use = "complete.obs")
cor_matrix_original <- cor(subset_real_data, use = "complete.obs")

# View the correlation matrices
print(cor_matrix_imputed)
print(cor_matrix_original)

# Visualize the correlations in the original data
corrplot(cor_matrix_original, title = "Original Data", method = "circle")

# Visualize the correlations in the imputed data
corrplot(cor_matrix_imputed, title = "Imputed Data", method = "circle")


# Assessing value range consistency #

# Calculate the range of values for each variable in the original data, ignoring NA values
original_ranges <- sapply(subset_real_data, range, na.rm = TRUE)

# Calculate the range of values for each variable in the imputed data, ignoring NA values
imputed_ranges <- sapply(completed_data_subset, range, na.rm = TRUE)

# Combine the ranges from both original and imputed data into a single data frame for easy comparison
ranges_df <- rbind(original_ranges, imputed_ranges)

# Rename the rows for clarity and print the output
rownames(ranges_df) <- c("Original_Min", "Original_Max", "Imputed_Min", "Imputed_Max")
print(ranges_df)


### Replacing missing original data with imputed columns ###

# Replace the columns with missing values in the original dataset with the imputed ones
real_data[, c("FER", "FE", "TSAT", "TRANS")] <- completed_data_subset[, c("FER", "FE", "TSAT", "TRANS")]


##### Synthetic data generation #####

# Fix the pseudo random number generator seed and make the results reproducible
my.seed <- my_seed

# Convert 'TIME' to factor
real_data$TIME <- as.factor(real_data$TIME)

# Convert 'SEX' to factor
real_data$SEX <- as.factor(real_data$SEX)

# Convert 'SUP_DOSE' to factor
real_data$SUP_DOSE <- as.factor(real_data$SUP_DOSE)

# Check the structure of the data frame to confirm the conversion
# As you can see from the table below, variables  TIME, SEX, and SUP_DOSE have been converted into factors
str(real_data)

# Define the initial visit sequence and methods for synthesis
visit.sequence.ini <- c(5, 6, 7, 8, 9, 10)
method.ini <- c("", "sample", "sample", "sample", "cart", "cart", "cart", "cart", "cart", "cart", "")  #polyreg

# Run initial synthesis
# You'll see a warning "Method "cart" is not valid for a variable without predictors (BM)
# Method has been changed to "sample" ". This is normal as, according to the default predictor matrix, variable BM doesn't have predictors yet, and therefore method 'cart' can't be used for this variable. This is fiex below where we adjust the predictor matrix. 
synth_data.ini <- syn(data = real_data, visit.sequence = visit.sequence.ini, method = method.ini, m = 0, drop.not.used = FALSE)

# Display the default predictor matrix
synth_data.ini$predictor.matrix

# Customize the predictor matrix
predictor.matrix.corrected <- synth_data.ini$predictor.matrix
rows_to_change <- c("TIME", "SEX", "SUP_DOSE", "BM", "FER", "FE", "TSAT", "TRANS", "AHBM")
cols_to_change <- c("TIME", "SEX", "SUP_DOSE", "BM", "FER", "FE", "TSAT", "TRANS", "AHBM")
predictor.matrix.corrected[rows_to_change, cols_to_change] <- 1
diag(predictor.matrix.corrected) <- 0
predictor.matrix.corrected

# Generate synthetic data using the corrected predictor matrix
# You may encounter warnings like "Not synthesised predictor FER removed from predictor.matrix for variable BM." This is expected behavior. Variables are synthesized one-by-one and can't serve as predictors until they've been synthesized. As a result, only the last variable to be synthesized, AHBM, doesn't produce this warning because all its predictors have already been synthesized by that point.
synth_data.corrected <- syn(data = real_data, visit.sequence = visit.sequence.ini, method = method.ini, predictor.matrix = predictor.matrix.corrected, seed = my.seed)

# Update 'RHBM' values in synthetic data
synth_data.corrected$syn$RHBM <- synth_data.corrected$syn$AHBM / synth_data.corrected$syn$BM

# Final synthetic data
synth_data <- synth_data.corrected$syn


##### Synthetic data evaluation #####


### Compare() function ###

# Use the compare() function from the synthpop package to compare the target variables from the synthetic and real data sets
real_data_subset <- real_data[, c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM")]
synth_data_subset <- synth_data[, c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM")]
compare(synth_data_subset, real_data_subset, stat = "counts") # The stat parameter is set to "counts" to get count-based statistics for each variable in the selected subsets


### Visual Comparison ###


# Visualize both real and synthetic data for comparison

# Create a data frame to store variable names and bin widths
cont_var_distrib <- data.frame(
  var = c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM", "RHBM"),
  bin_width = c(1.5, 5, 1, 2, 0.1, 25, 0.2)
)

# Loop through the data frame to create plots
for(i in 1:nrow(cont_var_distrib)) {
  var_name <- as.character(cont_var_distrib[i, "var"])
  bin_w <- cont_var_distrib[i, "bin_width"]
  
  p <- ggplot() +
    geom_histogram(data = real_data, aes_string(x = var_name), binwidth = bin_w, fill = "blue", alpha = 0.5, position = "identity") +
    geom_histogram(data = synth_data, aes_string(x = var_name), binwidth = bin_w, fill = "red", alpha = 0.5, position = "identity") + 
    labs(title = paste("Distribution of", var_name, ": Real (Blue) vs. Synthetic (Red)"))
  
  print(p)
}


### Descriptive statistics ###

# Compare descriptive statistics of the real and synthetic data sets
summary(real_data[, cont_var_distrib$var])
summary(synth_data[, cont_var_distrib$var])


### Statistical comparison: Kolmogorov-Smirnov test ###

# Initialize an empty data frame to store the results of the K-S test
ks_results <- data.frame(
  Variable = character(0),
  D_statistic = numeric(0),
  p_value = numeric(0)
)

# Loop through each variable in the 'var' column of 'cont_var_distrib' to perform the K-S test
for (var in cont_var_distrib$var) {
  # Run the K-S test on each variable and store the results in 'ks_test_result'
  ks_test_result <- ks.test(real_data[[var]], synth_data[[var]])
  
  # Add the results (Variable name, D-statistic, and p-value) to the 'ks_results' data frame
  ks_results <- rbind(ks_results, data.frame(
    Variable = var,
    D_statistic = ks_test_result$statistic,
    p_value = ks_test_result$p.value
  ))
}

# Print the K-S test results
print(ks_results, row.names = FALSE)


### Correlation structure ###

# Check if the synthetic data maintains the correlation structure of the original data

# Extract relevant variable names from cont_var_distrib DataFrame
selected_vars <- as.character(cont_var_distrib$var)

# Filter the original dataset to include only the selected variables
real_data_selected <- real_data[, selected_vars]

# Compute correlation matrices
cor_real_selected <- cor(real_data_selected, use = "complete.obs")
cor_synthetic_selected <- cor(synth_data[, selected_vars], use = "complete.obs")

# Display correlation matrices
print(cor_real_selected)
print(cor_synthetic_selected)

# Visualize correlation matrices
# Original data correlations
corrplot(cor_real_selected, title = "Real Data Correlations", method = "circle", diag = FALSE)
# Synthetic data correlations
corrplot(cor_synthetic_selected, title = "Synthetic Data Correlations", method = "circle", diag = FALSE)


### Model-based evaluation ###

# Train a model on the synthetic data and test it on a real validation set to see how well it generalizes 

# Initialize an empty data frame to store MSE results
mse_results <- data.frame(var = character(),
                          mse_real = numeric(),
                          mse_synthetic = numeric())

# Randomly split the original data into 80% training and 20% test sets
set.seed(my_seed)
train_indices <- sample(1:nrow(real_data), nrow(real_data)*0.8)
train_data <- real_data[train_indices, ]
test_data <- real_data[-train_indices, ]

# Loop through each variable in cont_var_distrib
for (var_name in cont_var_distrib$var) {
  # Train a linear regression model using the synthetic data
  model_synthetic <- lm(as.formula(paste(var_name, "~ .")), data = synth_data)
  
  # Train a linear regression model using the real training data
  model_real <- lm(as.formula(paste(var_name, "~ .")), data = train_data)
  
  # Make predictions using the test set for both models
  predictions_synthetic <- predict(model_synthetic, newdata = test_data)
  predictions_real <- predict(model_real, newdata = test_data)
  
  # Calculate MSE for the models trained on synthetic and real data
  mse_synthetic <- mean((test_data[[var_name]] - predictions_synthetic)^2)
  mse_real <- mean((test_data[[var_name]] - predictions_real)^2)
  
  # Append MSE results to the mse_results data frame
  mse_results <- rbind(mse_results, data.frame(var = var_name, mse_real = mse_real, mse_synthetic = mse_synthetic))
}

# Print the mse_results data frame for comparison
print(mse_results)


##### Saving synthetic data to a file #####

# write.csv(synth_data, "Hbmass_synthetic.csv", row.names = FALSE)