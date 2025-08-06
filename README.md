# Airbnb-price-rediction-bristol
# Airbnb Price Prediction - Bristol

This project focuses on predicting **Airbnb listing prices in Bristol, UK** using real-world data from [Inside Airbnb](http://insideairbnb.com/). The workflow involves comprehensive **data preprocessing, feature engineering, model training**, and **evaluation using both linear and non-linear approaches** (LASSO Regression and Gradient Boosting).

> **Note:** This project was completed as part of an academic assignment with a strict word limit. As a result, explanations in the final report are intentionally concise. This README provides a more thorough and expanded overview of the process and methodology for anyone interested in the technical implementation.

---

## Objectives

- Predict the price of Airbnb listings based on listing attributes, host information, location, reviews, amenities, etc.
- Apply **data-driven preprocessing techniques** to prepare the dataset.
- Implement **LASSO Regression** to handle high dimensionality and multicollinearity.
- Explore **Gradient Boosting** to capture complex, non-linear interactions.
- Evaluate performance and limitations of both models, especially for high-end listings.

---

## Dataset

- **Source:** [Inside Airbnb – Bristol (Dec 25, 2024)](https://data.insideairbnb.com/united-kingdom/england/bristol/)
- **Size:** ~2,700 listings
- **Features:** 75 columns including numerical, categorical, textual, date, and JSON-formatted data.

Key data groups:
- Listing features: `property_type`, `room_type`, `accommodates`, etc.
- Host data: `host_response_rate`, `host_since`, `host_is_superhost`, etc.
- Availability: `minimum_nights`, `availability_365`, etc.
- Reviews: `review_scores_rating`, `number_of_reviews`, etc.
- Amenities: parsed and one-hot encoded from JSON strings.
- Geolocation: `latitude`, `longitude`, transformed via K-means clustering.

---

## Data Preprocessing & Feature Engineering

The raw dataset required substantial cleaning and transformation:

- **Handling missing values** using mean/median/mode imputation and introducing "Unknown" categories when needed.
- **Price normalization** and outlier removal (top 5% trimmed).
- **Ordinal and one-hot encoding** for categorical variables.
- **Amenity extraction**: only those present in more than 10% of listings were kept to avoid overfitting.
- **Location clustering**: applied K-Means to latitude and longitude to convert coordinates into meaningful geographic zones.
- **Binary conversion**: transformed boolean fields (e.g., `superhost`, `instant_bookable`) into 0/1 format.
- **Interaction terms**: review variables multiplied by a `has_reviews` flag to ensure validity.

---

##  Modeling Approaches

### 1. **LASSO Regression (Linear Model)**

- Chosen for its ability to perform **automatic feature selection** through L1 regularization.
- Applied 10-fold **cross-validation** to determine the optimal `lambda`.
- Included interaction terms and removed redundant features to reduce multicollinearity.
- Performance:
  - **Train RMSE:** ~32.6
  - **Test RMSE:** ~32.0
  - **Test R²:** ~0.70

Limitations:
- Residuals increase with price.
- Difficulty capturing complex relationships with high-end listings.

---

### 2. **Gradient Boosting (Tree-Based Model)**

- Implemented with **XGBoost** using caret for cross-validation and hyperparameter tuning.
- Capable of handling non-linearity and feature interactions automatically.
- Training done on a 1,000-row sample due to resource constraints.

Performance:
- **Train RMSE:** significantly lower due to overfitting
- **Test RMSE:** ~33–35 (slightly worse than LASSO)
- No major performance gain on unseen data.

---

##  Visualizations

- **Price distribution** before and after outlier removal.
- **Boxplots** of continuous variables to visualize skew and detect outliers.
- **Predicted vs Actual** plots for both models.
- **Residual histograms** to assess error distribution.
- **Feature importance** from LASSO coefficients and XGBoost gain scores.

---

## Key Takeaways

- Feature engineering and careful preprocessing are **critical** to model performance.
- **LASSO performed comparably** to Gradient Boosting, but struggled with upper-end prices.
- Gradient Boosting introduced **overfitting** and didn’t significantly outperform simpler models.
- For real-world application, further steps like **log transformation**, **ensemble stacking**, or **price segmentation** could improve accuracy.

---

## References

- Wang, D. & Nicolau, J. (2017). *Price determinants of sharing economy-based accommodation rental.*
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An Introduction to Statistical Learning.*
- Inside Airbnb: [https://insideairbnb.com](https://insideairbnb.com)

---

## Technologies Used

- **Language:** R
- **Libraries:** `tidyverse`, `caret`, `glmnet`, `xgboost`, `fastDummies`, `jsonlite`, `ggplot2`, `DescTools`
- **Data Source:** Inside Airbnb (https://insideairbnb.com/)

---

##  Author

**Enric Fernández Sala**  
Bachelor's Student | International Business Economics
[LinkedIn](www.linkedin.com/in/enric-fernandez-sala) | [GitHub](https://github.com/yourusername)


