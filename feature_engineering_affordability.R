# Feature Engineering: Affordability Analysis
# Harjot Singh
#
# This script creates affordability-related features using:
# - House Price Index (HPI)
# - Median household income
# - Mortgage interest rates
#
# Important note:
# HPI is an index, not an actual dollar home price.
# So the HPI-to-income ratio should be interpreted as a relative
# affordability indicator, not the literal number of years of income
# needed to buy a house.

library(tidyverse)
library(ggplot2)

options(scipen = 999)

# ------------------------------------------------------------
# Load Data
# Keep this R file in the same folder as the CSV files:
# - CASTHPI_yearly_cleaned.csv
# - cleaned_income_data.csv
# - cleaned_mortgage_rates_annual_avg.csv
# ------------------------------------------------------------

required_files <- c(
  "CASTHPI_yearly_cleaned.csv",
  "cleaned_income_data.csv",
  "cleaned_mortgage_rates_annual_avg.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Missing file(s):",
      paste(missing_files, collapse = ", "),
      "\nCurrent working directory is:",
      getwd(),
      "\nMove this R file into the same folder as the CSV files or set the working directory correctly."
    )
  )
}

hpi_data <- read_csv("CASTHPI_yearly_cleaned.csv")

income_data <- read_csv("cleaned_income_data.csv") %>%
  rename(Year = Date)

mortgage_data <- read_csv(
  "cleaned_mortgage_rates_annual_avg.csv",
  skip = 7,
  col_names = c(
    "Year",
    "Average_Mortgage_Rate",
    "Lowest_Mortgage_Rate",
    "Highest_Mortgage_Rate",
    "Number_Of_Weeks"
  )
)

glimpse(hpi_data)
glimpse(income_data)
glimpse(mortgage_data)

# ------------------------------------------------------------
# Clean and Merge Data
# ------------------------------------------------------------

hpi_data <- hpi_data %>%
  mutate(
    Year = as.numeric(Year),
    Yearly_Average_HPI = as.numeric(Yearly_Average_HPI),
    Lowest_Quarterly_HPI = as.numeric(Lowest_Quarterly_HPI),
    Highest_Quarterly_HPI = as.numeric(Highest_Quarterly_HPI)
  )

income_data <- income_data %>%
  mutate(
    Year = as.numeric(Year),
    Median_Household_Income = as.numeric(Median_Household_Income)
  )

mortgage_data <- mortgage_data %>%
  mutate(
    Year = as.numeric(Year),
    Average_Mortgage_Rate = as.numeric(Average_Mortgage_Rate),
    Lowest_Mortgage_Rate = as.numeric(Lowest_Mortgage_Rate),
    Highest_Mortgage_Rate = as.numeric(Highest_Mortgage_Rate),
    Number_Of_Weeks = as.numeric(Number_Of_Weeks)
  )

combined_data <- hpi_data %>%
  left_join(income_data, by = "Year") %>%
  left_join(mortgage_data, by = "Year") %>%
  arrange(Year) %>%
  drop_na()

glimpse(combined_data)
print(head(combined_data))

# ------------------------------------------------------------
# Feature Engineering
# ------------------------------------------------------------

feature_data <- combined_data %>%
  mutate(
    # Basic HPI-to-income ratio
    # This is a relative affordability indicator.
    HPI_to_Income_Ratio = Yearly_Average_HPI / Median_Household_Income,

    # Easier-to-read ratio:
    # HPI per $100,000 of median household income.
    HPI_per_100k_Income = Yearly_Average_HPI / (Median_Household_Income / 100000),

    # Affordability index compared to the first year.
    # Values above 1 suggest affordability worsened compared to the first year.
    Affordability_Index = HPI_to_Income_Ratio / first(HPI_to_Income_Ratio),

    # Income growth compared to first year.
    Income_Growth_From_Start = Median_Household_Income / first(Median_Household_Income),

    # HPI growth compared to first year.
    HPI_Growth_From_Start = Yearly_Average_HPI / first(Yearly_Average_HPI),

    # Difference between HPI growth and income growth.
    # Positive values mean HPI grew faster than income.
    Affordability_Gap = HPI_Growth_From_Start - Income_Growth_From_Start,

    # Mortgage-adjusted HPI pressure indicator.
    # Higher values suggest more buyer pressure from prices and rates together.
    Mortgage_Adjusted_HPI = Yearly_Average_HPI * Average_Mortgage_Rate
  )

glimpse(feature_data)
print(head(feature_data))

# ------------------------------------------------------------
# Feature Description Table
# ------------------------------------------------------------

feature_descriptions <- tibble(
  Feature = c(
    "HPI_to_Income_Ratio",
    "HPI_per_100k_Income",
    "Affordability_Index",
    "Income_Growth_From_Start",
    "HPI_Growth_From_Start",
    "Affordability_Gap",
    "Mortgage_Adjusted_HPI"
  ),
  Meaning = c(
    "Yearly average HPI divided by median household income",
    "HPI scaled per $100,000 of median household income",
    "HPI-to-income ratio compared to the first year in the dataset",
    "Median household income growth compared to the first year",
    "HPI growth compared to the first year",
    "Difference between HPI growth and income growth",
    "HPI multiplied by average mortgage rate"
  ),
  Interpretation = c(
    "Higher value suggests housing prices are higher relative to income",
    "Higher value suggests worse affordability relative to a fixed income scale",
    "Values above 1 suggest affordability worsened compared to the first year",
    "Values above 1 mean income increased compared to the first year",
    "Values above 1 mean HPI increased compared to the first year",
    "Positive values mean HPI grew faster than income",
    "Higher value suggests more buyer pressure from prices and interest rates together"
  )
)

print(feature_descriptions)
write_csv(feature_descriptions, "feature_descriptions.csv")

# ------------------------------------------------------------
# Plot 1: HPI Relative to Income
# ------------------------------------------------------------

hpi_income_ratio_plot <- ggplot(feature_data, aes(x = Year, y = HPI_per_100k_Income)) +
  geom_line(linewidth = 1, color = "darkred") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Housing Price Index Relative to Income",
    subtitle = "Higher values suggest worsening affordability relative to income",
    x = "Year",
    y = "HPI per $100,000 of Median Household Income"
  )

print(hpi_income_ratio_plot)

ggsave(
  "hpi_relative_to_income.png",
  hpi_income_ratio_plot,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Plot 2: Relative Affordability Index
# ------------------------------------------------------------

affordability_index_plot <- ggplot(feature_data, aes(x = Year, y = Affordability_Index)) +
  geom_line(linewidth = 1, color = "purple") +
  geom_point(color = "purple") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Relative Housing Affordability Index",
    subtitle = "Values above 1 mean affordability worsened compared to the first year",
    x = "Year",
    y = "Affordability Index"
  )

print(affordability_index_plot)

ggsave(
  "affordability_index_plot.png",
  affordability_index_plot,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Plot 3: HPI Growth vs. Income Growth
# ------------------------------------------------------------

growth_data <- feature_data %>%
  select(Year, Income_Growth_From_Start, HPI_Growth_From_Start) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Variable",
    values_to = "Growth_From_Start"
  )

growth_comparison_plot <- ggplot(growth_data, aes(x = Year, y = Growth_From_Start, color = Variable)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "HPI Growth Compared to Income Growth",
    subtitle = "Both variables are indexed to the first year in the dataset",
    x = "Year",
    y = "Growth Compared to First Year",
    color = "Variable"
  )

print(growth_comparison_plot)

ggsave(
  "hpi_growth_vs_income_growth.png",
  growth_comparison_plot,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Plot 4: Affordability Gap
# ------------------------------------------------------------

affordability_gap_plot <- ggplot(feature_data, aes(x = Year, y = Affordability_Gap)) +
  geom_line(linewidth = 1, color = "orange") +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Affordability Gap",
    subtitle = "Positive values mean HPI grew faster than income",
    x = "Year",
    y = "HPI Growth - Income Growth"
  )

print(affordability_gap_plot)

ggsave(
  "affordability_gap_plot.png",
  affordability_gap_plot,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Plot 5: Mortgage-Adjusted HPI
# ------------------------------------------------------------

mortgage_adjusted_hpi_plot <- ggplot(feature_data, aes(x = Year, y = Mortgage_Adjusted_HPI)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Mortgage-Adjusted Housing Pressure",
    subtitle = "Combines HPI and mortgage rates into one pressure indicator",
    x = "Year",
    y = "HPI x Mortgage Rate"
  )

print(mortgage_adjusted_hpi_plot)

ggsave(
  "mortgage_adjusted_hpi_plot.png",
  mortgage_adjusted_hpi_plot,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Scatter Plot: Income vs. HPI with Regression Line
# ------------------------------------------------------------

income_hpi_scatter_plot <- ggplot(
  feature_data,
  aes(x = Median_Household_Income, y = Yearly_Average_HPI)
) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Median Household Income vs. Housing Price Index",
    subtitle = "Regression line shows the linear trend between income and HPI",
    x = "Median Household Income ($)",
    y = "Yearly Average HPI"
  )

print(income_hpi_scatter_plot)

ggsave(
  "income_hpi_regression_scatter.png",
  income_hpi_scatter_plot,
  width = 8,
  height = 5
)

# ------------------------------------------------------------
# Save Feature Engineered Dataset
# ------------------------------------------------------------

write_csv(feature_data, "feature_engineered_affordability_data.csv")

cat("\nSaved feature engineered dataset as: feature_engineered_affordability_data.csv\n")
cat("Saved feature descriptions as: feature_descriptions.csv\n")
cat("Saved affordability plots as PNG files.\n")

# ------------------------------------------------------------
# Written Summary
# ------------------------------------------------------------

cat("\nFeature Engineering Summary\n\n")
cat("Created HPI-to-income ratio as a relative affordability indicator.\n")
cat("Created HPI per $100,000 of income for easier interpretation.\n")
cat("Created an affordability index relative to the first year in the dataset.\n")
cat("Created growth features comparing HPI growth and income growth.\n")
cat("Created an affordability gap feature to show whether HPI grew faster than income.\n")
cat("Created a mortgage-adjusted HPI pressure feature.\n")
cat("Skipped regional indicators because the current datasets are statewide California data.\n")
