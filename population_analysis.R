# Load necessary libraries
library(readr)
library(e1071)

# 1. Load the population data CSV
# Adjust the path to your data file
population_data <- read_csv("path_to_your_shanghai_population_data.csv")

# Inspect the data to ensure it's loaded correctly
head(population_data)
summary(population_data)

# 2. Save the data as CSV (if needed)
# This step ensures your data is stored in CSV format
write.csv(population_data, "shanghai_population_data.csv", row.names = FALSE)

# 3. Descriptive Statistics: Mean, Standard Deviation, Median, Skewness
# Calculate descriptive statistics for relevant columns
mean_population <- apply(population_data[, -1], 2, mean, na.rm = TRUE)
std_population <- apply(population_data[, -1], 2, sd, na.rm = TRUE)
median_population <- apply(population_data[, -1], 2, median, na.rm = TRUE)
skewness_population <- apply(population_data[, -1], 2, skewness, na.rm = TRUE)

# Combine into a data frame for easier presentation
descriptive_stats <- data.frame(
  Mean = mean_population,
  Standard_Deviation = std_population,
  Median = median_population,
  Skewness = skewness_population
)

# Print the descriptive statistics
print("Descriptive Statistics:")
print(descriptive_stats)

# 4. Regression Analysis
# Analyzing the relationship between permanent and migrant population
model_perm_migrant <- lm(Migrant_Population ~ Permanent_Population, data = population_data)
# Analyzing the relationship between registered and elderly population
model_reg_eld <- lm(Elderly_Population ~ Registered_Population, data = population_data)

# View regression results
print("Regression Model: Permanent vs Migrant Population")
summary(model_perm_migrant)

print("Regression Model: Registered vs Elderly Population")
summary(model_reg_eld)

# 5. Repeat the analysis for your own city data (optional)
# Replace this with your own city's dataset
# city_population_data <- read_csv("path_to_your_city_data.csv")
# Perform descriptive statistics and regression similar to above if required.

# Print completion message
print("Analysis complete. Check the output above.")
