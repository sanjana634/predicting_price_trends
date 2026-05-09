import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score
import warnings

warnings.filterwarnings('ignore')

hpi_df = pd.read_csv('CASTHPI_yearly_cleaned.csv')
hpi_df_clean = hpi_df[['Year', 'Yearly_Average_HPI']]

income_df = pd.read_csv('cleaned_income_data.csv')
income_df_clean = income_df.rename(columns={'Date': 'Year'})

# Load and clean Mortgage data (first rows are not entries )
mortgage_df = pd.read_csv('cleaned_mortgage_rates_annual_avg.csv')
mortgage_df_clean = mortgage_df.iloc[3:].copy()
mortgage_df_clean.columns = ['Year', 'avg_mortgage_rate', 'min_mortgage_rate', 'max_mortgage_rate', 'weekly_count']

# Convert mortgage columns to numeric
cols_to_fix = ['Year', 'avg_mortgage_rate', 'min_mortgage_rate', 'max_mortgage_rate']
for col in cols_to_fix:
    mortgage_df_clean[col] = pd.to_numeric(mortgage_df_clean[col])

# 2. MERGE DATASETS
# Join all three datasets on the 'Year' column
merged_df = hpi_df_clean.merge(income_df_clean, on='Year') \
                       .merge(mortgage_df_clean[['Year', 'avg_mortgage_rate']], on='Year')

# 3. PREPARE MODEL DATA
X = merged_df[['Median_Household_Income', 'avg_mortgage_rate']]
y = merged_df['Yearly_Average_HPI']

# Split into Training (80%) and Testing (20%) sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# 4. TRAIN RANDOM FOREST MODEL
param_grid = {
    'n_estimators': [50, 100],
    'max_depth': [None, 3, 5],
    'min_samples_split': [2, 5, 7],
    'max_features': [1, 2]
}

rf = RandomForestRegressor(random_state=42)
gsrf = GridSearchCV(estimator=rf, param_grid=param_grid, 
                           cv=5, n_jobs=-1, scoring='r2')

gsrf.fit(X_train, y_train)

# 5. EVALUATE MODEL
y_pred = gsrf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)

best_rf = gsrf.best_estimator_

print(f"Best Parameters: {gsrf.best_params_}")

# Predict using the optimized model
y_pred = best_rf.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print(f"Mean Squared Error: {mse:.2f}")
print(f"R-squared Score: {r2:.4f}")

# Print Feature Importance from the BEST model
importances = best_rf.feature_importances_
for name, imp in zip(X.columns, importances):
    print(f"Feature: {name}, Importance: {imp:.4f}")

print(f"Training R2: {best_rf.score(X_train, y_train):.4f}")
print(f"Test R2: {best_rf.score(X_test, y_test):.4f}")

# 6. VISUALIZATIONS
# Plot 1: Actual vs Predicted (Using optimized predictions)
plt.figure(figsize=(10, 6))
plt.scatter(y_test, y_pred, color='blue', alpha=0.6, label='Test Data')
plt.plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], 'r--', lw=2, label='Perfect Prediction')
plt.xlabel('Actual HPI')
plt.ylabel('Predicted HPI')
plt.title(f'Actual vs Predicted CASTHPI (R2: {r2:.4f})')
plt.legend()
plt.savefig('hpi_actual_vs_predicted.png', dpi=300, bbox_inches='tight')
plt.grid(True)


# Plot 2: Feature Importance (Using optimized importances)
plt.figure(figsize=(8, 5))
plt.bar(X.columns, importances, color='teal')
plt.ylabel('Importance Score')
plt.title('Feature Importances (Optimized Random Forest)')
plt.savefig('hpi_feature_importance.png', dpi=300, bbox_inches='tight')


# Plot 3: Historical Trend (Predicting on whole dataset with best model)
y_all_pred = best_rf.predict(X)
plt.figure(figsize=(12, 6))
plt.plot(merged_df['Year'], merged_df['Yearly_Average_HPI'], label='Actual HPI', marker='o', color='red')
plt.plot(merged_df['Year'], y_all_pred, label='GS Optimized Prediction - Random Forest', linestyle='--', marker='x', color='blue')
plt.xlabel('Year')
plt.ylabel('CASTHPI')
plt.title('Historical Trend: Actual vs Optimized Random Forest')
plt.legend()
plt.grid(True)
plt.savefig('hpi_historical_trend.png', dpi=300, bbox_inches='tight')
plt.show()
