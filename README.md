# Analyzing The Impact of Customer Demographics on Product Category Preferences

**Overview**
In this project, I tried to investigate the relationship between customer demographics such as the income level, education level, age and their product category preferences. The goal is to identfy key demographic factors influencing purchasing behavior to improve personalized marketing strategies.


**Dataset**
The study utilized a subset of the Kaggle Retail Sales Customer Behavior Analysis dataset, narrowing down 1 million rows and 78 columns to 1,000 rows and 10 columns for analysis. Key variables include:

Age (Numerical)
Gender (Categorical)
Income Bracket (Categorical)
Loyalty Program Membership (Categorical)
Education Level (Categorical)
Product Category (Target Variable: Groceries, Furniture, Toys, Electronics, Clothing)

**Methodology**
1. Data Preprocessing:

Addressed missing values and skewed distributions with log transformations.
Encoded categorical variables using label and one-hot encoding.
Performed multicollinearity analysis (VIF) and feature scaling.

2. Modeling Techniques:

Multinomial Logistic Regression: Provided baseline predictions.
Lasso Regression: Improved feature selection by shrinking irrelevant coefficients to zero.
Random Forest: Leveraged ensemble learning to handle complex interactions.

3. Evaluation Metrics:

Model accuracy, feature importance, and ROC analysis.

**Results**
Key Predictors: Income Bracket, Age Group, Education Level, Gender, and Loyalty Program Membership.
Model Performance:
Multinomial Logistic Regression: 21% accuracy.
Lasso Regression: 23% accuracy.
Random Forest: 23% accuracy.
ROC analysis showed limited predictive separability using demographic data alone.

**Conclusion**
Demographic data alone provides limited predictive power for product category preferences. The study highlights the need to integrate behavioral and transactional data for improved customer segmentation and predictive performance.

References
Kaggle Dataset
Statistical Learning Techniques: Friedman, Hastie, Tibshirani
Random Forests: Breiman, L.

