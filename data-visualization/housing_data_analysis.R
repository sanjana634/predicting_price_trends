# Harjot Singh

# Housing Price Data Analysis - Step 2
# This is a plain R script version for VS Code.
# Run this file as an R script, not as an R Markdown file.

library(tidyverse)
library(ggplot2)

options(scipen = 999)

# ------------------------------------------------------------
# Load datasets
# These files should be in the same folder as this R script.
# ------------------------------------------------------------

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
# Clean data
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

# ------------------------------------------------------------
# Merge datasets by Year
# ------------------------------------------------------------

combined_data <- hpi_data %>%
  left_join(income_data, by = "Year") %>%
  left_join(mortgage_data, by = "Year") %>%
  mutate(
    HPI_Range = Highest_Quarterly_HPI - Lowest_Quarterly_HPI
  ) %>%
  drop_na()

glimpse(combined_data)
head(combined_data)

# ------------------------------------------------------------
# Trend Graph 1: HPI over time
# ------------------------------------------------------------

hpi_trend_plot <- ggplot(combined_data, aes(x = Year, y = Yearly_Average_HPI)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "California House Price Index Over Time",
    x = "Year",
    y = "Yearly Average HPI"
  )

print(hpi_trend_plot)

ggsave("hpi_trend_plot.png", hpi_trend_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Trend Graph 2: Income over time
# ------------------------------------------------------------

income_trend_plot <- ggplot(combined_data, aes(x = Year, y = Median_Household_Income)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(
    title = "California Median Household Income Over Time",
    x = "Year",
    y = "Median Household Income ($)"
  )

print(income_trend_plot)

ggsave("income_trend_plot.png", income_trend_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Trend Graph 3: Mortgage rates over time
# ------------------------------------------------------------

mortgage_trend_plot <- ggplot(combined_data, aes(x = Year, y = Average_Mortgage_Rate)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Average 30-Year Mortgage Rate Over Time",
    x = "Year",
    y = "Average Mortgage Rate (%)"
  )

print(mortgage_trend_plot)

ggsave("mortgage_trend_plot.png", mortgage_trend_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Normalized trends
# ------------------------------------------------------------

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

normalized_data <- combined_data %>%
  mutate(
    HPI_Normalized = min_max(Yearly_Average_HPI),
    Income_Normalized = min_max(Median_Household_Income),
    Mortgage_Rate_Normalized = min_max(Average_Mortgage_Rate)
  ) %>%
  select(Year, HPI_Normalized, Income_Normalized, Mortgage_Rate_Normalized) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Variable",
    values_to = "Normalized_Value"
  )

normalized_trends_plot <- ggplot(normalized_data, aes(x = Year, y = Normalized_Value, color = Variable)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Normalized Trends: HPI, Income, and Mortgage Rates",
    subtitle = "All variables are scaled between 0 and 1",
    x = "Year",
    y = "Normalized Value",
    color = "Variable"
  )

print(normalized_trends_plot)

ggsave("normalized_trends_plot.png", normalized_trends_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Scatter Plot: Income vs HPI
# ------------------------------------------------------------

income_vs_hpi_plot <- ggplot(combined_data, aes(x = Median_Household_Income, y = Yearly_Average_HPI)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Median Income vs. Housing Price Index",
    x = "Median Household Income ($)",
    y = "Yearly Average HPI"
  )

print(income_vs_hpi_plot)

ggsave("income_vs_hpi_scatter.png", income_vs_hpi_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Scatter Plot: Mortgage Rate vs HPI
# ------------------------------------------------------------

mortgage_vs_hpi_plot <- ggplot(combined_data, aes(x = Average_Mortgage_Rate, y = Yearly_Average_HPI)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Mortgage Rate vs. Housing Price Index",
    x = "Average 30-Year Mortgage Rate (%)",
    y = "Yearly Average HPI"
  )

print(mortgage_vs_hpi_plot)

ggsave("mortgage_vs_hpi_scatter.png", mortgage_vs_hpi_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Dual-axis graph: Income vs HPI over time
# ------------------------------------------------------------

scale_factor_income <- max(combined_data$Median_Household_Income, na.rm = TRUE) /
  max(combined_data$Yearly_Average_HPI, na.rm = TRUE)

income_hpi_dual_axis_plot <- ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Yearly_Average_HPI, color = "Housing Index"), linewidth = 1) +
  geom_line(aes(y = Median_Household_Income / scale_factor_income, color = "Median Income"), linewidth = 1) +
  scale_y_continuous(
    name = "House Price Index",
    sec.axis = sec_axis(~ . * scale_factor_income, name = "Income ($)")
  ) +
  scale_color_manual(
    values = c("Housing Index" = "red", "Median Income" = "blue")
  ) +
  theme_minimal() +
  labs(
    title = "California: Income vs. Housing Prices",
    x = "Year",
    color = ""
  )

print(income_hpi_dual_axis_plot)

ggsave("income_hpi_dual_axis_plot.png", income_hpi_dual_axis_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Dual-axis graph: Mortgage Rate vs HPI over time
# ------------------------------------------------------------

scale_factor_mortgage <- max(combined_data$Yearly_Average_HPI, na.rm = TRUE) /
  max(combined_data$Average_Mortgage_Rate, na.rm = TRUE)

mortgage_hpi_dual_axis_plot <- ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Yearly_Average_HPI, color = "Housing Index"), linewidth = 1) +
  geom_line(aes(y = Average_Mortgage_Rate * scale_factor_mortgage, color = "Mortgage Rate"), linewidth = 1) +
  scale_y_continuous(
    name = "House Price Index",
    sec.axis = sec_axis(~ . / scale_factor_mortgage, name = "Mortgage Rate (%)")
  ) +
  scale_color_manual(
    values = c("Housing Index" = "red", "Mortgage Rate" = "darkgreen")
  ) +
  theme_minimal() +
  labs(
    title = "California: Mortgage Rates vs. Housing Prices",
    x = "Year",
    color = ""
  )

print(mortgage_hpi_dual_axis_plot)

ggsave("mortgage_hpi_dual_axis_plot.png", mortgage_hpi_dual_axis_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Correlation Matrix
# ------------------------------------------------------------

correlation_data <- combined_data %>%
  select(
    Yearly_Average_HPI,
    Median_Household_Income,
    Average_Mortgage_Rate,
    HPI_Range
  )

cor_matrix <- cor(correlation_data, use = "complete.obs")

print(round(cor_matrix, 3))

# Save correlation matrix as CSV
write.csv(round(cor_matrix, 3), "correlation_matrix.csv")

# ------------------------------------------------------------
# Correlation Heatmap
# ------------------------------------------------------------

cor_df <- as.data.frame(as.table(cor_matrix))

correlation_heatmap <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 4) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  labs(
    title = "Correlation Matrix Heatmap",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(correlation_heatmap)

ggsave("correlation_heatmap.png", correlation_heatmap, width = 8, height = 5)

# ------------------------------------------------------------
# Linear Regression and R-squared
# ------------------------------------------------------------

model_income <- lm(
  Yearly_Average_HPI ~ Median_Household_Income,
  data = combined_data
)

model_mortgage <- lm(
  Yearly_Average_HPI ~ Average_Mortgage_Rate,
  data = combined_data
)

model_full <- lm(
  Yearly_Average_HPI ~ Median_Household_Income + Average_Mortgage_Rate,
  data = combined_data
)

r2_table <- tibble(
  Model = c(
    "HPI predicted by Median Income",
    "HPI predicted by Mortgage Rate",
    "HPI predicted by Median Income and Mortgage Rate"
  ),
  R_squared = c(
    summary(model_income)$r.squared,
    summary(model_mortgage)$r.squared,
    summary(model_full)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(model_income)$adj.r.squared,
    summary(model_mortgage)$adj.r.squared,
    summary(model_full)$adj.r.squared
  )
)

print(r2_table)

write.csv(r2_table, "r_squared_results.csv", row.names = FALSE)

print(summary(model_full))

# ------------------------------------------------------------
# Actual vs Predicted HPI
# ------------------------------------------------------------

combined_data <- combined_data %>%
  mutate(
    Predicted_HPI = predict(model_full, newdata = combined_data),
    Residual = Yearly_Average_HPI - Predicted_HPI
  )

actual_vs_predicted_plot <- ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Yearly_Average_HPI, color = "Actual HPI"), linewidth = 1) +
  geom_line(aes(y = Predicted_HPI, color = "Predicted HPI"), linewidth = 1, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Actual HPI vs. Predicted HPI",
    x = "Year",
    y = "House Price Index",
    color = ""
  )

print(actual_vs_predicted_plot)

ggsave("actual_vs_predicted_hpi.png", actual_vs_predicted_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------

residual_plot <- ggplot(combined_data, aes(x = Year, y = Residual)) +
  geom_point(color = "purple") +
  geom_line(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Residual Plot: Model Error Over Time",
    x = "Year",
    y = "Residual"
  )

print(residual_plot)

ggsave("residual_plot.png", residual_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Error Metrics
# ------------------------------------------------------------

rmse <- sqrt(mean(combined_data$Residual^2))
mae <- mean(abs(combined_data$Residual))

error_table <- tibble(
  Metric = c("RMSE", "MAE"),
  Value = c(rmse, mae)
)

print(error_table)

write.csv(error_table, "model_error_metrics.csv", row.names = FALSE)

# ------------------------------------------------------------
# HPI Range
# ------------------------------------------------------------

hpi_range_plot <- ggplot(combined_data, aes(x = Year, y = HPI_Range)) +
  geom_line(linewidth = 1, color = "orange") +
  geom_point(color = "orange") +
  theme_minimal() +
  labs(
    title = "Yearly HPI Range",
    subtitle = "Highest Quarterly HPI minus Lowest Quarterly HPI",
    x = "Year",
    y = "HPI Range"
  )

print(hpi_range_plot)

ggsave("hpi_range_plot.png", hpi_range_plot, width = 8, height = 5)

# ------------------------------------------------------------
# Written Summary
# ------------------------------------------------------------

cat("\nStep 2 Data Analysis Summary\n\n")
cat("This analysis visualized housing price trends, mortgage rate trends, and income trends over time.\n")
cat("The scatter plots compare housing prices against income and mortgage rates.\n")
cat("The correlation matrix measures how strongly the variables are related to each other.\n")
cat("The linear regression models use R-squared values to show how much of the housing price trend is explained by income and mortgage rates.\n")
cat("The residual plot, RMSE, MAE, and HPI range help show how much noise or unexplained variation exists in the data.\n")
