rm(list = ls()) # clear the workspace

# Libraries required for data manipulation and exploratory data analysis
library(dplyr) # Data manipulation: filtering, selecting, transforming, etc.
library(tidyr) # Data tidying: reshaping, transforming, and tidying datasets
library(skimr) # Data summarization: provides a frictionless approach to summary statistics
library(ggplot2) # Data visualization: creating a wide variety of static, animated, and interactive graphics
library(gridExtra) # Arranging multiple grid-based plots on a page
library(moments) # Statistical moments: skewness, kurtosis, etc.
library(GGally) # Extension of ggplot2 for easy generation of complex plots
library(speedsR) # Collection of sports-specific benchmark datasets - part of the AIS SPEEDS project

# Load Data ----

# Access and assign the HbmassSynth dataset to a variable
hbmass_data <- HbmassSynth

# View the first few rows of the dataset to understand its structure
head(hbmass_data)

# Separating Pre and Post-Exposure Data ----

# Separate the data into pre-exposure and post-exposure datasets
pre_exposure_data <- hbmass_data[hbmass_data$TIME == 0, ]
post_exposure_data <- hbmass_data[hbmass_data$TIME == 1, ]

# View the structure of the separated datasets
head(pre_exposure_data)
head(post_exposure_data)

# Univariate Analysis ----

## Descriptive and Summary Statistics ----

# Summary statistics of the pre-exposure data
summary(pre_exposure_data)

# Using skim() to obtain detailed summary statistics
skim(pre_exposure_data)

# Example of calculating the mean and median
mean(pre_exposure_data$AHBM, na.rm = TRUE)
median(pre_exposure_data$AHBM, na.rm = TRUE)

# Example of calculating variance and standard deviation
var(pre_exposure_data$AHBM, na.rm = TRUE)
sd(pre_exposure_data$AHBM, na.rm = TRUE)

# Frequency count of categorical data
table(pre_exposure_data$SEX)

# Proportion of categories
prop.table(table(pre_exposure_data$SEX))


## Graphical Analysis and Variable Distributions ----

# Clean the dataset by removing rows with any non-finite values
pre_exposure_data <- pre_exposure_data %>%
  filter(across(everything(), is.finite))

# Create a dataframe to store variable names and bin widths
cont_var_distrib <- data.frame(
  var = c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM", "RHBM"),
  bin_width = c(1.5, 5, 1, 2, 0.1, 25, 0.2)  # Specify appropriate bin widths
)

# List to store individual plots
histograms <- list()

# Loop through the dataframe to create plots for each variable
for (i in 1:nrow(cont_var_distrib)) {
  var_name <- as.character(cont_var_distrib[i, "var"])
  bin_w <- cont_var_distrib[i, "bin_width"]
  
  plot <- ggplot(pre_exposure_data, aes(x = !!sym(var_name))) +
    geom_histogram(binwidth = bin_w, fill = "skyblue", color = "black") +
    labs(title = paste("Distribution of", var_name))  # Plot title
  
  histograms[[i]] <- plot  # Store the plot in the list
}

# Arrange the plots in a grid for a clearer visualization
gridExtra::grid.arrange(grobs = histograms, ncol = 3)  # 3 columns of plots



# Compute skewness and kurtosis for each numeric variable
skewness_kurtosis <- pre_exposure_data %>%
  select_if(is.numeric) %>%
  summarise(across(everything(), list(
    Skewness = ~skewness(., na.rm = TRUE),
    Kurtosis = ~kurtosis(., na.rm = TRUE) - 3  # Excess kurtosis
  )))

# Convert the summary from wide to long format to match your intended structure
skewness_kurtosis_long <- skewness_kurtosis %>%
  pivot_longer(cols = everything(),
               names_to = "Measurement",
               values_to = "Value") %>%
  separate(Measurement, into = c("Variable", "Metric"), sep = "_") %>%
  pivot_wider(names_from = "Metric", values_from = "Value")

skewness_kurtosis_long



# Create box plots for the quantitative variables
box_plot <- ggplot(pre_exposure_data, aes(x = "", y = AHBM)) + 
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of Absolute Hemoglobin Mass (AHBM)")

print(box_plot)  # Display the box plot


# Create a bar plot for the categorical variable 'SEX'
bar_plot <- ggplot(pre_exposure_data, aes(x = SEX)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Sex in Pre-Exposure Data")

print(bar_plot)  # Display the bar plot




# Create a combined dataset with an additional column indicating the exposure type
combined_data <- bind_rows(
  mutate(pre_exposure_data, Exposure = 'Pre'),
  mutate(post_exposure_data, Exposure = 'Post')
)

# Specify the variables and their corresponding bin widths
variables <- c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM", "RHBM")
bin_widths <- c(1.5, 5, 1, 2, 0.1, 25, 0.2)

# Initialize an empty list to store plots
plots_list <- list()

# Loop through the variables to create overlay histograms
for (i in seq_along(variables)) {
  variable_name <- variables[i]
  bin_width <- bin_widths[i]
  
  # Generate the overlay histogram for the current variable using tidy evaluation
  p <- ggplot(combined_data, aes(x = .data[[variable_name]], fill = Exposure)) +
    geom_histogram(data = filter(combined_data, Exposure == 'Pre'), 
                   binwidth = bin_width, alpha = 0.5, position = 'identity', color = 'black') +
    geom_histogram(data = filter(combined_data, Exposure == 'Post'), 
                   binwidth = bin_width, alpha = 0.5, position = 'identity', color = 'black') +
    labs(title = paste("Pre vs. Post Exposure: ", variable_name)) +
    scale_fill_manual(values = c("Pre" = "skyblue", "Post" = "salmon")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Add the plot to the list
  plots_list[[i]] <- p
}

# Arrange the generated plots in a grid
grid_plot <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3)

# Print the grid plot
print(grid_plot)

# Bivariate and Multivariate Analysis ----

# Pairwise scatter plots of continuous variables
cont_var_cols <- pre_exposure_data[, c("BM", "FER", "FE", "TSAT", "TRANS", "AHBM", "RHBM")]
GGally::ggpairs(cont_var_cols)

# Individual pairwise scatter plot with body mass (BM) and ferritin (FER)
ggplot(pre_exposure_data, aes(x = BM, y = FER)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Pairwise Scatter Plot of BM and FER")

# Computing the correlation matrix for selected variables
cor_matrix <- cor(pre_exposure_data %>% select(BM, FER, FE, TSAT, TRANS, AHBM, RHBM), use = "complete.obs")

# Visualizing the correlation matrix with a circle representation
corrplot::corrplot(cor_matrix, method = "circle")

# Scatter plot of ferritin (FER) against absolute hemoglobin mass (AHBM)
ggplot(pre_exposure_data, aes(x = FER, y = AHBM)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  labs(title = "Relationship Between FER and AHBM")


# Three-way relationship between transferrin (TRANS), AHBM, and SEX
ggplot(pre_exposure_data, aes(x = TRANS, y = AHBM, color = as.factor(SEX))) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = 'lm', se = TRUE) +  # Linear model fit line with standard error
  scale_color_manual(values = c("skyblue", "salmon"),
                     labels = c("Female", "Male"),
                     name = "Sex") +
  labs(title = "Three-Way Relationship between TRANS, AHBM, and SEX") +
  theme_classic()  # Clean classic theme for better readability


# Side-by-side box plots of AHBM across different levels of iron supplementation (SUP_DOSE)
ggplot(pre_exposure_data, aes(x = factor(SUP_DOSE, labels = c("None", "105 mg", "210 mg")), y = AHBM)) +
  geom_boxplot() +
  labs(x = "Iron Supplementation Dose", y = "Absolute Hemoglobin Mass", title = "AHBM Across Different Levels of Iron Supplementation") +
  theme_minimal()