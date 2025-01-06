## Loading the necessary libraries
library(dplyr)
library(ggplot2)

##Loading the dataset
data_retail <-read.csv("sampled_data.csv")

#Basic data exploration(overview)
summary(data_retail)
str(data_retail)

#Checking for missing values in each column
colSums(is.na(data_retail))


#To visualize missing values, using the 'naniar' package
install.packages('naniar')

library(naniar)
gg_miss_var(data_retail)

##No missing values detected

##Checking for duplicates
duplicates <- data_retail[duplicated(data_retail),]

#Count number of duplicate rows
num_duplicates <- nrow(duplicates)

#Display results
num_duplicates
duplicates

##No duplicates detected

## selecting the demographics variables to be used for our analysis.
# Load necessary library
library(dplyr)

# Selecting only the required demographic variables
data_retail_var <- data_retail %>% 
  select(marital_status, occupation, age, gender, income_bracket, 
         membership_years, number_of_children, loyalty_program, 
         education_level,product_category)

# Check the updated data to confirm
head(data_retail_var)

#Checking the updated data
glimpse(data_retail_var)

#checking the summary of the data
summary(data_retail_var)

####  Data Preprocessing  ######
##Creating age groups
#Ages 18–29 -- "Young Adults."
#Ages 30–44 -- "Adults."
#Ages 45–59 -- "Middle Age."
#Ages 60–79 -- "Seniors."
data_retail_var$age_group <- cut(
  data_retail_var$age,
  breaks = c(18, 30, 45, 60, 80), # Defines the boundaries of each age group.
  labels = c("Young Adults", "Adults", "Middle Age", "Seniors"),
  right = FALSE  # Indicates that intervals should be left-inclusive.
)

#Viewing the agegroup
data_retail_var$age_group

head(data_retail_var) 


#####.  Data Exploration and Visualization.   #######

#Visualizing the age_group


# Install and load ggplot2 package
install.packages("ggplot2")


library(ggplot2)

## Age Group ##
# Create a bar plot with refined aesthetics of the Age Group
ggplot(data_retail_var, aes(x = age_group)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Age Group Distribution",
       x = "Age Group",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars



## Marital Status ##
# Bar plot with refined aesthetics of the Marital_Status
ggplot(data_retail_var, aes(x = marital_status)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Marital Status Distribution",
       x = "Marital Status",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars




## Occupation Distribution##
# Bar plot with refined aesthetics of the Occupation
ggplot(data_retail_var, aes(x = occupation)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Occupation Distribution",
       x = "Occupation",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars


## Gender Distribution##
# Bar plot with refined aesthetics of the Gender Distribution
ggplot(data_retail_var, aes(x = gender)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars


##Income Bracket##
# Bar plot with refined aesthetics of the Income Bracket Distribution
ggplot(data_retail_var, aes(x = income_bracket)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Income Bracket Distribution",
       x = "Income Bracket",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars



##Loyalty Program##
# Bar plot with refined aesthetics of the Loyalty Program Distribution
ggplot(data_retail_var, aes(x = loyalty_program)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Loyalty Program Distribution",
       x = "Loyalty Program",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars


##Education Level##
# Bar plot with refined aesthetics of the Education Level Distribution
ggplot(data_retail_var, aes(x = education_level)) +
  geom_bar(fill = "#FF9800", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Education Level Distribution",
       x = "Education Level",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars



##Product Category Bar Graph##
# Bar plot with refined aesthetics of the Product Category Distribution
ggplot(data_retail_var, aes(x = product_category)) +
  geom_bar(fill = "#FF9", color = "black", width = 0.7) +  # Updated color and bar width
  labs(title = "Product Category Distribution",
       x = "Product Category",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Set base font size for readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")  # Add count labels above bars




##Product Category Pie chart##
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Summarize the data to get counts of each product category
category_summary <- data_retail_var %>%
  count(product_category) %>%
  mutate(percentage = round(100 * n / sum(n), 1),  # Calculate percentages
         labels = paste(product_category, percentage, "%"))  # Create labels with category and percentage

# Define colors for the pie chart
colors <- brewer.pal(n = nrow(category_summary), name = "Pastel1")  # Adjusts colors based on categories

# Plot pie chart
pie(
  category_summary$n,
  labels = category_summary$labels,
  main = "Product Category Distribution",
  col = colors,
  border = "black"
)


# Chi-square test for categorical variables"
categorical_vars <- c("income_bracket", "education_level", "age_group", "marital_status", 
                    "loyalty_program",  "occupation")

chi_square_results <- lapply(categorical_vars, function(var) {
  chisq.test(table(data_retail_var[[var]], data_retail_var$product_category))
})

# Display results
names(chi_square_results) <- categorical_vars
chi_square_results




##For the numerical variables

# ANOVA test for numeric variables
numeric_vars <- c("membership_years", "number_of_children" )

anova_results <- lapply(numeric_vars, function(var) {
  summary(aov(data_retail_var[[var]] ~ data_retail_var$product_category))
})

# Display results
names(anova_results) <- numeric_vars
anova_results


### Normality test #######
### Normality check for Age Variable###
# Load necessary libraries
library(ggplot2)
library(car)  # for Q-Q plot

# Histogram for Age Distribution
ggplot(data_retail_var, aes(x = age)) +
  geom_histogram(bins = 20, color = "black", fill = "orange") +
  ggtitle("Age Distribution Histogram") +
  xlab("Age") +
  ylab("Frequency")

# Q-Q Plot for Age
library(car)
qqPlot(data_retail_var$age, main = "Q-Q Plot of Age", pch = 19, col = "blue", line = "quartiles")

### Seems not normal, proceeding to normalize by taking log transformation#

# Check the first few rows to confirm the normalization
head(data_retail_var)
str(data_retail_var)

#### Log transformation of Age ####
data_retail_var$age_log <- log(data_retail_var$age)

# Histogram for Log Transformed Age
ggplot(data_retail_var, aes(x = age_log)) +
  geom_histogram(bins = 20, color = "black", fill = "orange") +
  ggtitle("Histogram of Log Transformed Age") +
  xlab("Log Transformed Age") +
  ylab("Frequency")

# Q-Q Plot for Log Transformed Age
library(car)
qqPlot(data_retail_var$age_log, main = "Q-Q Plot of Log Transformed Age", pch = 19, col = "blue", line = "quartiles")

# Shapiro-Wilk Test for Log Transformed Age
shapiro.test(data_retail_var$age_log)



### Normality check for Membership years Variable###
library(ggplot2)
library(car)  # for Q-Q plot

# Histogram for Age Distribution
ggplot(data_retail_var, aes(x = membership_years)) +
  geom_histogram(bins = 20, color = "black", fill = "orange") +
  ggtitle("Membership Years Distribution Histogram") +
  xlab("Membership Years") +
  ylab("Frequency")

# Q-Q Plot for Mambership years
library(car)
qqPlot(data_retail_var$age, main = "Q-Q Plot of Mambership Years", 
       pch = 19, col = "blue", line = "quartiles")

# Shapiro-Wilk Test for Mambership Years
shapiro.test(data_retail_var$membership_years)


#### Log transformation of Membership years ####
## Adding 1 to avoid log(0) if there are any zero values
data_retail_var$membership_years_log <- log(data_retail_var$membership_years + 1) 


# Histogram for membership_years_log Distribution
ggplot(data_retail_var, aes(x = membership_years_log)) +
  geom_histogram(bins = 20, color = "black", fill = "orange") +
  ggtitle("Log_Membership Years Distribution Histogram") +
  xlab("Log_Membership Years") +
  ylab("Frequency")


# Q-Q Plot for Log Transformed Membership years
library(car)
qqPlot(data_retail_var$membership_years_log, main = "Q-Q Plot of Log Transformed Membership years", pch = 19, col = "blue", line = "quartiles")

# Shapiro-Wilk Test for Log Transformed Age
shapiro.test(data_retail_var$membership_years_log)



### Normality check for number_of_children Variable###
library(ggplot2)
library(car)  # for Q-Q plot

# Histogram for number_of_children Distribution
ggplot(data_retail_var, aes(x = number_of_children)) +
  geom_histogram(bins = 20, color = "black", fill = "orange") +
  ggtitle("Number of children Distribution Histogram") +
  xlab("Number of children") +
  ylab("Frequency")

# Q-Q Plot for Number of children
library(car)
qqPlot(data_retail_var$number_of_children, main = "Q-Q Plot of Number of children", 
       pch = 19, col = "blue", line = "quartiles")

# Shapiro-Wilk Test for Number of children
shapiro.test(data_retail_var$number_of_children)


#### Log transformation of Number_of_children ####
## Adding 1 to avoid log(0) if there are any zero values
data_retail_var$number_of_children_log <- log(data_retail_var$number_of_children + 1) 


# Histogram for number_of_children_log Distribution
ggplot(data_retail_var, aes(x = number_of_children_log)) +
  geom_histogram(bins = 20, color = "black", fill = "orange") +
  ggtitle("Log_number_of_children Distribution Histogram") +
  xlab("Log_number_of_children Years") +
  ylab("Frequency")


# Q-Q Plot for Log number_of_children
library(car)
qqPlot(data_retail_var$membership_years_log, main = "Q-Q Plot of Log number_of_children", pch = 19, col = "blue", line = "quartiles")

# Shapiro-Wilk Test for Log number_of_children
shapiro.test(data_retail_var$membership_years_log)


head(data_retail_var)



#### Encoding ordinal categorical variables ####

# Define the ordered factor for income_bracket
data_retail_var$income_bracket <- factor(data_retail_var$income_bracket, 
                              levels = c("Low", "Medium", "High"), 
                              ordered = TRUE)

# Encode income_bracket as a numeric variable based on the order
data_retail_var$income_bracket_encoded <- as.numeric(data_retail_var$income_bracket)

# Define the ordered factor for education_level
data_retail_var$education_level <- factor(data_retail_var$education_level, 
                               levels = c("High School", "Bachelor's", "Master's", "PhD"), 
                               ordered = TRUE)

# Encode education_level as a numeric variable based on the order
data_retail_var$education_level_encoded <- as.numeric(data_retail_var$education_level)


# Define the ordered factor for age_group
data_retail_var$age_group <- factor(data_retail_var$age_group, 
                               levels = c("Young Adults", "Adults", "Middle Age", "Seniors"), 
                               ordered = TRUE)

# Encode age_group as a numeric variable based on the order
data_retail_var$age_group_encoded <- as.numeric(data_retail_var$age_group)




# Define all categorical variables with their possible levels
# marital_status
data_retail_var$marital_status <- factor(data_retail_var$marital_status, 
                              levels = c("Single", "Married", "Divorced"))

# occupation
data_retail_var$occupation <- factor(data_retail_var$occupation, 
                          levels = c("Employed", "Unemployed", "Retired", "Self-Employed"))

# gender
data_retail_var$gender <- factor(data_retail_var$gender, 
                      levels = c("Male", "Female", "Other"))

# loyalty_program
data_retail_var$loyalty_program <- factor(data_retail_var$loyalty_program, 
                               levels = c("No", "Yes"))

# product_category
data_retail_var$product_category <- factor(data_retail_var$product_category, 
                                levels = c("Groceries", "Toys", "Clothing", "Furniture", "Electronics"))


###
# Install fastDummies package if not already installed
install.packages("fastDummies")

# Load the package
library(fastDummies)

# One-hot encode the specified categorical variables
data_retail_var <- dummy_cols(data_retail_var, 
                              select_columns = c("marital_status", "occupation", "gender", "loyalty_program"),
                              remove_first_dummy = TRUE,   # Avoids multicollinearity by removing the first level
                              remove_selected_columns = TRUE)  # Removes original categorical columns


# View the first few rows to verify the encoding
head(data_retail_var)




# Dropping the original categorical columns
data_retail_var <- data_retail_var[, !(names(data_retail_var) %in% c("age","age_log", "income_bracket","education_level",
                                                                     "membership_years","age_group", "number_of_children"))]


head(data_retail_var)




summary(data_retail_var)

##### VIF #####

# Rename columns to avoid special characters
names(data_retail_var) <- make.names(names(data_retail_var), unique = TRUE)

install.packages("car")
library(car)

# Remove the target variable to focus on predictors
predictors_only <- data_retail_var[, !colnames(data_retail_var) %in% "product_category"]


# Fit a linear model using all predictor variables
vif_model <- lm(as.formula(paste("membership_years_log ~", paste(colnames(predictors_only), collapse = " + "))), 
                data = predictors_only)

# Calculate VIF
vif_values <- vif(vif_model)

# Display VIF values
print(vif_values)


#########

colnames(data_retail_var)
# Load necessary libraries
library(lattice)
library(nnet)    # for multinom function
library(caret)   # for data splitting and evaluation


# Set a seed for reproducibility
set.seed(123)

# Split the data into training (80%) and testing (20%) sets
trainIndex <- createDataPartition(data_retail_var$product_category, p = 0.8, list = FALSE)
train_data <- data_retail_var[trainIndex, ]
test_data <- data_retail_var[-trainIndex, ]

# Fit the multinomial logistic regression model with the correct column name
model <- multinom(product_category ~ membership_years_log + number_of_children_log + 
                    income_bracket_encoded + education_level_encoded + 
                    age_group_encoded + marital_status_Married + 
                    marital_status_Divorced + occupation_Unemployed + 
                    occupation_Retired + `occupation_Self.Employed` + 
                    gender_Female + gender_Other + loyalty_program_Yes, 
                  data = train_data)

# Summary of the model
summary(model)



# Make predictions on the test data
predicted_categories <- predict(model, newdata = test_data)

# Evaluate the model's accuracy
confusion_matrix <- table(Predicted = predicted_categories, Actual = test_data$product_category)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))





### Lasso regression ###

# Load necessary library
library(glmnet)

# Convert product_category to a factor if it's not already
data_retail_var$product_category <- as.factor(data_retail_var$product_category)

# Prepare the predictor matrix and response variable
X <- model.matrix(product_category ~ membership_years_log + number_of_children_log + 
                    income_bracket_encoded + education_level_encoded + 
                    age_group_encoded + marital_status_Married + 
                    marital_status_Divorced + occupation_Unemployed + 
                    occupation_Retired + `occupation_Self.Employed` + 
                    gender_Female + gender_Other + loyalty_program_Yes, 
                  data = train_data)[,-1]  # Remove the intercept column

y <- train_data$product_category

# Fit the regularized model using cv.glmnet for cross-validation
# alpha = 1 for Lasso, alpha = 0 for Ridge
cv_model <- cv.glmnet(X, y, family = "multinomial", alpha = 1)  # Lasso regression in this case

# View the optimal lambda (regularization parameter)
optimal_lambda <- cv_model$lambda.min
print(paste("Optimal Lambda:", optimal_lambda))

# Fit the final model using the optimal lambda
final_model <- glmnet(X, y, family = "multinomial", alpha = 1, lambda = optimal_lambda)



# Make predictions on the test set
X_test <- model.matrix(product_category ~ membership_years_log + number_of_children_log + 
                         income_bracket_encoded + education_level_encoded + 
                         age_group_encoded + marital_status_Married + 
                         marital_status_Divorced + occupation_Unemployed + 
                         occupation_Retired + `occupation_Self.Employed` + 
                         gender_Female + gender_Other + loyalty_program_Yes, 
                       data = test_data)[,-1]

predicted_categories <- predict(final_model, newx = X_test, s = "lambda.min", type = "class")

# Evaluate model accuracy
confusion_matrix <- table(Predicted = predicted_categories, Actual = test_data$product_category)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))



###Model Selection##
# Load necessary libraries
library(readr)
library(MASS)


str(data_retail_var)

library(nnet)

# Fit a full multinomial logistic regression model
full_model <- multinom(product_category ~ ., data = data_retail_var)

# Load glmnet
library(glmnet)

# Prepare the data for glmnet
# Convert categorical variables to dummy variables
X <- model.matrix(~ . - 1, data = data_retail_var)  # remove intercept with -1
y <- as.factor(data_retail_var$product_category)

# Fit a Lasso model using cross-validation to find the best lambda
cv_model <- cv.glmnet(X, y, family = "multinomial", alpha = 1)  # alpha=1 for Lasso

# Get the best lambda
optimal_lambda <- cv_model$lambda.min

# Fit the final model with optimal lambda
final_model <- glmnet(X, y, family = "multinomial", lambda = optimal_lambda)

# View non-zero coefficients to see selected features
coef(final_model, s = optimal_lambda)


library(MASS)  # For stepAIC

library(nnet)
# Null model (only intercept)
null_model <- multinom(product_category ~ 1, data = data_retail_var)

# Full model with all predictors
full_model <- multinom(product_category ~ ., data = data_retail_var)

forward_model <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), 
                         direction = "forward", trace = FALSE)


backward_model <- stepAIC(full_model, direction = "backward", trace = FALSE)



stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)


# Calculate AIC for each model
aic_values <- c(
  forward_model = AIC(forward_model),
  backward_model = AIC(backward_model),
  stepwise_model = AIC(stepwise_model),
  full_model = AIC(full_model)
)

# Calculate BIC for each model
bic_values <- c(
  forward_model = BIC(forward_model),
  backward_model = BIC(backward_model),
  stepwise_model = BIC(stepwise_model),
  full_model = BIC(full_model)
)

# Print AIC and BIC values for comparison
print("AIC values for each model:")
print(aic_values)

print("BIC values for each model:")
print(bic_values)





##Confusion Matrix visualization
# Assuming `predicted_categories` contains the predictions and `test_data$product_category` has the actual categories
library(ggplot2)

# Create the confusion matrix as a table
confusion_matrix <- table(Predicted = predicted_categories, Actual = test_data$product_category)

# Convert the table into a data frame for ggplot
confusion_df <- as.data.frame(confusion_matrix)
colnames(confusion_df) <- c("Predicted", "Actual", "Count")

# Plot the confusion matrix as a heatmap
ggplot(confusion_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "orange") +
  geom_text(aes(label = Count), color = "black") +
  theme_minimal() +
  labs(title = "Confusion Matrix Heatmap", fill = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####Feature importance plot ###


# Convert the list of non-zero coefficients to a vector
non_zero_coef_vector <- unlist(non_zero_coef)

# Create the data frame with feature names and coefficients
non_zero_coef_df <- data.frame(
  Feature = names(non_zero_coef_vector),
  Coefficient = as.numeric(non_zero_coef_vector)
)

# View the data frame to ensure it is correct
head(non_zero_coef_df)


library(ggplot2)
ggplot(non_zero_coef_df, aes(x = reorder(Feature, abs(Coefficient)), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Feature Importance (Lasso Coefficients)", x = "Feature", y = "Coefficient") +
  theme_minimal()

#

### Non Linear model ####
# Load necessary library
library(randomForest)

# Convert the target variable to a factor (if not already)
train_data$product_category <- as.factor(train_data$product_category)
test_data$product_category <- as.factor(test_data$product_category)

colnames(train_data)

# Rename the column in both training and test data
colnames(train_data)[colnames(train_data) == "occupation_Self-Employed"] <- "occupation_SelfEmployed"
colnames(test_data)[colnames(test_data) == "occupation_Self-Employed"] <- "occupation_SelfEmployed"

# Fit the Random Forest model on the training data
rf_model <- randomForest(product_category ~ membership_years_log + number_of_children_log + 
                           income_bracket_encoded + education_level_encoded + 
                           age_group_encoded + marital_status_Married + 
                           marital_status_Divorced + occupation_Unemployed + 
                           occupation_Retired + `occupation_Self.Employed` + 
                           gender_Female + gender_Other + loyalty_program_Yes, 
                         data = train_data, ntree = 500, mtry = 3)

# Print model summary
print(rf_model)



# Make predictions on the test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model's accuracy
confusion_matrix_rf <- table(Predicted = rf_predictions, Actual = test_data$product_category)
accuracy_rf <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)

# Print confusion matrix and accuracy
print(confusion_matrix_rf)
print(paste("Accuracy:", round(accuracy_rf, 4)))


## Confusion matrix heatmap for the random Forest ##
# Load necessary libraries
library(ggplot2)

# Assuming confusion_matrix_rf is your confusion matrix in table format, convert it to a data frame
confusion_df_rf <- as.data.frame(confusion_matrix_rf)
colnames(confusion_df_rf) <- c("Predicted", "Actual", "Count")

# Plot the confusion matrix as a heatmap with enhanced visuals
ggplot(confusion_df_rf, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Count), color = "white", fontface = "bold", size = 4) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Random Forest)",
    fill = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )




##  Feature Importance (Random Forest)
# Load necessary library
library(randomForest)

# Fit the Random Forest model (if not done already)
rf_model <- randomForest(product_category ~ membership_years_log + number_of_children_log + 
                           income_bracket_encoded + education_level_encoded + 
                           age_group_encoded + marital_status_Married + 
                           marital_status_Divorced + occupation_Unemployed + 
                           occupation_Retired + occupation_Self.Employed + 
                           gender_Female + gender_Other + loyalty_program_Yes, 
                         data = train_data, ntree = 500, mtry = 3)

# Calculate variable importance
importance(rf_model)

# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance Plot (Random Forest)")

###More appealing visual for the feature impoprtance ###
# Load necessary libraries
library(randomForest)
library(ggplot2)

# Calculate variable importance
importance_values <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_values), 
                            Importance = importance_values[, 1])

# Create a ggplot variable importance plot
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue") +
  coord_flip() +
  labs(title = "Variable Importance Plot (Random Forest)", 
       x = "Features", 
       y = "Mean Decrease in Gini") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))


##ROC Curve
# Load necessary libraries
library(pROC)
library(ggplot2)

# Get predicted probabilities for each class
predicted_probabilities <- predict(rf_model, newdata = test_data, type = "prob")

# Convert the actual product category to binary for each class (One-vs-Rest)
test_labels <- model.matrix(~ test_data$product_category - 1)

# Generate and plot ROC curves for each class
roc_curves <- lapply(1:ncol(test_labels), function(i) {
  roc(test_labels[, i], predicted_probabilities[, i], plot = FALSE)
})

# Plot ROC curves
plot(roc_curves[[1]], col = "red", main = "One-vs-Rest ROC Curves for Each Product Category")
for(i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, add = TRUE)
}

# Add legend
legend("bottomright", legend = colnames(test_labels), col = 1:length(roc_curves), lwd = 2)














