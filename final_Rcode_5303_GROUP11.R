##Importing libraries
library(caret)
library(MASS)
library(glmnet)
library(janitor)
library(ggplot2)
library(corrplot)
library(tidyr)
library(reshape2)
library(dplyr)
library(car)
library(gridExtra)
library(pROC)
options(max.print = 10000000)

######################################################################
#Importing Dataset

df_data_main <- read.csv('c:/Users/Giri/Downloads/in-vehicle-coupon-recommendation.csv')
df_data = df_data_main
summary(df_data)
head(df_data)

#Description of the dataset
str(df_data)

# Rename the column passanger to passenger
df_data <- df_data %>% rename(passenger = passanger)


####################################################################
#replace the space in the column names with '_' and column names with lower case

df_data <- df_data %>%
  clean_names()

names(df_data)

######################################################################
#Listing different classes in features
sapply(df_data, function(x) table(x))

#checking null , na , empty cells in the data set
sapply(df_data, function(x) sum(x == ""))
sapply(df_data, function(x) sum(is.na(x)))
sapply(df_data, function(x) sum(is.null(x)))

#######################################################################

#summary of categorical and numerical columns
categorical_col <- sapply(df_data, is.factor) | sapply(df_data, is.character)
categorical_col <- names(df_data)[categorical_col]
numerical_col <- sapply(df_data, is.numeric)
numerical_col <- names(df_data)[numerical_col]
print(categorical_col)
print(numerical_col)

summary(df_data[,categorical_col])
summary(df_data[,numerical_col])

##Listing out the classes and its respective counts for categorical and numerical column

cat_value_counts<- lapply(df_data[,categorical_col], table)
cat_value_counts

num_value_counts<- lapply(df_data[,numerical_col], table)
num_value_counts

######################################Exploratory Data Analysis (EDA)##########################################################

#Creating function for bivariate analysis
percent_value_counts <- function(df, feature, target) {
  df_summary <- df %>%
    group_by_at(vars(feature)) %>%
    summarise(
      Total_Count = n(),
      Accepted = sum(get(target) == 1, na.rm = TRUE),
      Rejected = sum(get(target) == 0, na.rm = TRUE)
    ) %>%
    mutate(
      Total_Percent = round((Total_Count / sum(Total_Count)) * 100, 3),
      Percent_Accepted = round((Accepted / Total_Count) * 100, 3),
      Percent_Rejected = round((Rejected / Total_Count) * 100, 3)
    )
  return(df_summary)
}

bivariate_analysis <- function(df, feature, target) {
  df_EDA <- percent_value_counts(df, feature, target)
  df_EDA <- df_EDA %>%
    mutate(
      Total_Label = paste0("(", Total_Percent, "%)"),
      Accepted_Label = paste0("(", Percent_Accepted, "%)")
    )
  
  #Creating bar plots
  plot <- ggplot(data = df_EDA) +
    geom_bar(aes_string(x = feature, y = "Total_Count"), stat = "identity", fill = "grey", alpha = 0.7) +
    geom_bar(aes_string(x = feature, y = "Accepted"), stat = "identity", fill = "blue", alpha = 0.7) +
    geom_text(aes_string(x = feature, y = "Total_Count", label = "Total_Label"), 
              vjust = -0.5, size = 3, color = "black") +  
    geom_text(aes_string(x = feature, y = "Accepted", label = "Accepted_Label"), 
              vjust = -0.5, size = 3, color = "black") + 
    labs(
      title = paste("Accepted Coupons with respect to", feature),
      x = feature,
      y = "Coupon Counts"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot)
  return(df_EDA)
}
#############################################################################

#Calculating the percentages for classes in target variable (Y)
y_table <- table(df_data$y)
y_percentage <- prop.table(y_table) * 100

#Creating bar plots
barplot_heights <- barplot(y_percentage, 
                           main = "Percentage of Each Class in y", 
                           col = c("skyblue", "lightcoral"), 
                           names.arg = c("0", "1"), 
                           ylim = c(0, 100),
                           ylab = "Percentage",
                           xlab = "Class")

#Adding the percentage values for the respective bar plots
text(barplot_heights, 
     y = y_percentage + 3,  
     labels = paste0(round(y_percentage, 1), "%"), 
     col = "black", 
     cex = 1)

###############################################################################

#Bivariate analysis for direction same
#The no. of people who accepted the coupon with respect to direction
feature_column_dir <- "direction_same"  #categorical feature to analyze
target_column <- "y"                    #target column
df_analysis_direction_same <- bivariate_analysis(df_data, feature_column_dir, target_column)
print(df_analysis_direction_same) # 78% of them are direction opposite in that 56% percent are accepted . Similarly 
                                  # 21% of them are direction same in that 58% percent are accepted

#Bivariate analysis for coupons
#Different types of coupons accepted and the no. of people in each coupon
feature_column_coupon <- "coupon" 
df_analysis_coupon <- bivariate_analysis(df_data, feature_column_coupon, target_column)
print(df_analysis_coupon)


#Bivariate analysis for education
#The no. of coupons accepted with respect to education
feature_column_edu <- "education"  
df_analysis_education <- bivariate_analysis(df_data, feature_column_edu, target_column)
print(df_analysis_education)

#Bivariate analysis for destination
#The no. of coupons accepted with respect to destination
feature_column_destination <- "destination" 
df_analysis_destination <- bivariate_analysis(df_data, feature_column_destination, target_column)
print(df_analysis_destination)

#Bivariate analysis for passenger
#The no. of coupons accepted with respect to passenger
feature_column_passenger <- "passenger"  
df_analysis_passenger <- bivariate_analysis(df_data, feature_column_passenger, target_column)
print(df_analysis_passenger)

#Bivariate analysis for weather
#The no. of coupons accepted with respect to weather
feature_column_weather <- "weather"  
df_analysis_weather <- bivariate_analysis(df_data, feature_column_weather, target_column)
print(df_analysis_weather)

#Bivariate analysis for temperature
#The no. of coupons accepted with respect to temperature
feature_column_temperature <- "temperature"  
df_analysis_temperature <- bivariate_analysis(df_data, feature_column_temperature, target_column)
print(df_analysis_temperature)

#Bivariate analysis for time
#The no. of coupons accepted with respect to time
feature_column_time <- "time"  
df_analysis_time <- bivariate_analysis(df_data, feature_column_time, target_column)
print(df_analysis_time)

#Bivariate analysis for maritalStatus
#The no. of coupons accepted with respect to maritalStatus
feature_column_maritalStatus <- "marital_status"
df_analysis_maritalstatus <- bivariate_analysis(df_data, feature_column_maritalStatus, target_column)
print(df_analysis_maritalstatus)


########################################## Data Pre-processing ################################

#Dropping columns "car" and "direction opp"
## Dropping 'car' column as it has significant missing values
## Dropping 'direction opposite' column due to redundancy

df_data_dummy <- df_data[ , !(names(df_data) %in% c("car","direction_opp"))]
dim(df_data_dummy)
head(df_data_dummy)


########################################Handling the missing values.##################################
#Mode imputation to handle missing values

get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

df_data_dummy[df_data_dummy == ""] <- NA

columns_to_impute <- c("bar", "coffee_house","carry_away","restaurant_less_than20","restaurant20to50")

for (col in columns_to_impute) {
  mode_value <- get_mode(df_data_dummy[[col]])
  df_data_dummy[[col]][is.na(df_data_dummy[[col]])] <- mode_value
}

#dropping duplicate entries after mode imputation

df_data_dummy <- df_data_dummy[!duplicated(df_data_dummy), ]
dim(df_data_dummy)

sapply(df_data_dummy, function(x) sum(x == ""))
sapply(df_data_dummy, function(x) sum(is.na(x)))
sapply(df_data_dummy, function(x) sum(is.null(x)))

################################################################################################################

####Feature engineering

#For columns destination and passenger

df_data_dummy$destination_passenger <- paste(df_data_dummy$destination, df_data_dummy$passenger, sep = "_")
head(df_data_dummy$destination_passenger)
length(df_data_dummy$destination_passenger)

#For columns Temperature and Weather

df_data_dummy$weather_temperature <- paste(df_data_dummy$weather, df_data_dummy$temperature, sep = "_")
head(df_data_dummy$weather_temperature)
length(df_data_dummy$weather_temperature)

#For columns Marital Status and Children

df_data_dummy$maritalstatus_children <- paste(df_data_dummy$marital_status, df_data_dummy$has_children, sep = "_")
head(df_data_dummy$maritalstatus_children)
length(df_data_dummy$maritalstatus_children)

#For columns to_coupon_geq5min to_coupon_geq15min to_coupon_geq25min

df_data_dummy <- df_data_dummy %>%
  mutate(
    to_coupon = case_when(
      to_coupon_geq25min == 1 ~ 2,                                 # Condition 3: Greater than 25 minutes -> 2
      to_coupon_geq15min == 1 & to_coupon_geq25min == 0 ~ 1,       # Condition 2: Between 15 and 25 minutes -> 1
      to_coupon_geq5min == 1 & to_coupon_geq15min == 0 ~ 0,        # Condition 1: Less than 15 minutes -> 0
      TRUE ~ NA_real_                                              # In case of missing or unexpected values
    )
  )
table(df_data_dummy$to_coupon)


#Dropping the columns which are used for feature engineering 

df_data_dummy <- df_data_dummy[ , !(names(df_data_dummy) %in% c("marital_status", "has_children",
                                                                "destination", "passenger","weather", "temperature","to_coupon_geq5min","to_coupon_geq15min","to_coupon_geq25min"))]


#Lisitng unique values in age columns

sapply(df_data_dummy['age'], unique)

####Categorize the age into age groups

age_group <- character(length(df_data_dummy$age))
print(length(age_group))
length(age_group) == length(df_data_dummy$age)

for (i in 1:length(df_data_dummy$age)) {
  if (df_data_dummy$age[i] < 21 | df_data_dummy$age[i] == 'below21') {
    age_group[i] <- "Teenagers"
  } else if (df_data_dummy$age[i] >= 21 && df_data_dummy$age[i] <= 35) {
    age_group[i] <- "Young Adults"
  } else if (df_data_dummy$age[i] >= 36 && df_data_dummy$age[i] <= 50) {
    age_group[i] <- "Middle-Aged Adults"
  } else if (df_data_dummy$age[i] == '50plus') {
    age_group[i] <- "Seniors"
  }
}

df_data_dummy$age <- age_group
head(df_data_dummy$age)

#listing out the unique value counts in the column age
table(df_data_dummy$age)

##### Categorize the income into groups

#Listing out the unique values in the income feature.
table(df_data_dummy$income)

income_group <- character(length(df_data_dummy$income))
print(length(income_group))
length(income_group) == length(df_data_dummy$income)

for (i in 1:length(df_data_dummy$income)) {
  if (df_data_dummy$income[i] == 'Less than $12500' | 
      df_data_dummy$income[i] == '$12500 - $24999' | 
      df_data_dummy$income[i] == '$25000 - $37499') {
    income_group[i] <- "Low_income"
  } else if (df_data_dummy$income[i] == '$37500 - $49999' | 
             df_data_dummy$income[i] == '$50000 - $62499' | 
             df_data_dummy$income[i] == '$62500 - $74999') {
    income_group[i] <- "Medium_income"
  } else if (df_data_dummy$income[i] == '$75000 - $87499' | 
             df_data_dummy$income[i] == '$87500 - $99999' | 
             df_data_dummy$income[i] == '$100000 or More') {
    income_group[i] <- "High_income"
  }
}


df_data_dummy$income <- income_group

#Listing out the classes in the income after feature engineering.
table(df_data_dummy$income)


###Listing out the unique values in occupation feature.
table(df_data_dummy$occupation)

####Categorize the occupation_list into groups

occupation_group <- character(length(df_data_dummy$occupation))
print(length(occupation_group))
length(occupation_group) == length(df_data_dummy$occupation)

for (i in 1:length(df_data_dummy$occupation)) {
  if (df_data_dummy$occupation[i] == 'Installation Maintenance & Repair' | 
      df_data_dummy$occupation[i] == 'Transportation & Material Moving' | 
      df_data_dummy$occupation[i] == 'Food Preparation & Serving Related' | 
      df_data_dummy$occupation[i] == 'Building & Grounds Cleaning & Maintenance') {
    occupation_group[i] <- "Labour"
  } else if (df_data_dummy$occupation[i] == 'Architecture & Engineering' | 
             df_data_dummy$occupation[i] == 'Education & Training & Library' | 
             df_data_dummy$occupation[i] == 'Healthcare Practitioners & Technical' | 
             df_data_dummy$occupation[i] == 'Management' | 
             df_data_dummy$occupation[i] == 'Arts Design Entertainment Sports & Media' | 
             df_data_dummy$occupation[i] == 'Computer & Mathematical' | 
             df_data_dummy$occupation[i] == 'Legal' | 
             df_data_dummy$occupation[i] == 'Business & Financial' | 
             df_data_dummy$occupation[i] == 'Farming Fishing & Forestry') {
    occupation_group[i] <- "Professionals"
  } else if (df_data_dummy$occupation[i] == 'Retired') {
    occupation_group[i] <- "Retired"
  } else if (df_data_dummy$occupation[i] == 'Sales & Related' | 
             df_data_dummy$occupation[i] == 'Personal Care & Service' | 
             df_data_dummy$occupation[i] == 'Protective Service') {
    occupation_group[i] <- "Service and sales"
  } else if (df_data_dummy$occupation[i] == 'Student') {
    occupation_group[i] <- "Student"
  } else if (df_data_dummy$occupation[i] == 'Healthcare Support' | 
             df_data_dummy$occupation[i] == 'Life Physical Social Science' | 
             df_data_dummy$occupation[i] == 'Community & Social Services' | 
             df_data_dummy$occupation[i] == 'Construction & Extraction' | 
             df_data_dummy$occupation[i] == 'Office & Administrative Support' | 
             df_data_dummy$occupation[i] == 'Production Occupations') {
    occupation_group[i] <- "Technicians"
  } else if (df_data_dummy$occupation[i] == 'Unemployed') {
    occupation_group[i] <- "Unemployed"
  } else occupation_group[i] <- "Others"
}


df_data_dummy$occupation <- occupation_group
head(df_data_dummy$occupation)

#Listing out the classes in the occupation_list after feature engineering.
table(df_data_dummy$occupation)

names(df_data_dummy)
dim(df_data_dummy)



############################################### Analyzing the features after feature engineering ######################

#Multivariate analysis after feature engineering.

# Calculate counts group by coupon,weather_temperature
df_data_dummy_summary <- df_data_dummy %>%
  group_by(coupon, weather_temperature, y) %>%
  summarise(count = n(), .groups = "drop")

#Bar plot of Weather_Temperature, Y, and Coupon
ggplot(df_data_dummy_summary, aes(x = factor(coupon), y = count, fill = factor(weather_temperature))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  labs(title = "Grouped Bar Plot of Weather_Temperature, Y, and Coupon", 
       x = "Coupon", 
       y = "Count", 
       fill = "Weather Temperature") +
  theme_minimal() +
  facet_wrap(vars(y))

# Calculate counts group by age and coupon
df_data_dummy_summary <- df_data_dummy %>%
  group_by(coupon, age, y) %>%  
  summarise(count = n(), .groups = "drop")

#Grouped Bar Plot of Age, Y, and Coupon
ggplot(df_data_dummy_summary, aes(x = factor(coupon), y = count, fill = factor(age))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  labs(title = "Grouped Bar Plot of Age, Y, and Coupon", 
       x = "Coupon", 
       y = "Count", 
       fill = "Age") +
  theme_minimal() +
  facet_wrap(vars(y))

# Calculate counts group by coupon, to_coupon
df_data_dummy_summary <- df_data_dummy %>%
  group_by(coupon, to_coupon, y) %>%
  summarise(count = n(), .groups = "drop")

ggplot(df_data_dummy_summary, aes(x = factor(coupon), y = count, fill = factor(to_coupon))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(
    values = c("red", "green", "yellow"),  
    labels = c("Less than 5 minutes", "15-25 minutes", "More than 25 minutes"), 
    name = "Travel Time to Coupon"
  ) +
  labs(
    title = "Grouped Bar Plot of Coupon, Y, and to_Coupon",
    x = "Coupon",
    y = "Count"
  ) +
  theme_minimal() +
  facet_wrap(vars(y))



###############################################
#Correlation via one hot encoding method 

#Separating the categorical and numerical columns

categorical_cols <- names(df_data_dummy)[sapply(df_data_dummy, function(x) is.factor(x) | is.character(x))]
numeric_cols <- names(df_data_dummy)[sapply(df_data_dummy, function(x) is.numeric(x))]
length(categorical_cols)
length(numeric_cols)

#encoding categorical column
df_categorical_encoded <- df_data_dummy %>%
  select(all_of(categorical_cols)) %>%
  mutate_if(is.character, as.factor) 

#converting the encoded data 
df_categorical_encoded <- model.matrix(~ . - 1, data = df_categorical_encoded) %>% 
  as.data.frame() 

head(df_categorical_encoded)
dim(df_categorical_encoded)

#combining encoded categorical and numerical columns
df_data_dummy_encoded <- cbind(df_data_dummy[numeric_cols], df_categorical_encoded)
dim(df_data_dummy_encoded)

#Dropping duplicates after one hot encoding
df_data_dummy_encoded <- df_data_dummy_encoded[!duplicated(df_data_dummy_encoded), ]
dim(df_data_dummy_encoded)

#generating correlation matrix
cor_matrix <- cor(df_data_dummy_encoded, use = "complete.obs")

cor_matrix_melted <- melt(cor_matrix)
head(cor_matrix_melted)


# Find pairs of highly correlated variables with threshold above 0.5
threshold <- 0.5
highly_correlated <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)

# printing the indices of the highly correlated pairs
print(highly_correlated)

# Extracting the variables
correlated_var_names <- data.frame(
  Var1 = rownames(cor_matrix)[highly_correlated[, 1]],
  Var2 = colnames(cor_matrix)[highly_correlated[, 2]],
  Correlation = cor_matrix[highly_correlated]
)

# Remove duplicate pairs (present in the upper triangle of the matrix)
correlated_var_names <- correlated_var_names[correlated_var_names$Var1 < correlated_var_names$Var2, ]

# Getting only high correlated pairs.
print(correlated_var_names)

#Dropping highly correlated variables 

df_data_dummy_encoded <- df_data_dummy_encoded[ ,!(names(df_data_dummy_encoded) %in% c("time7AM"))]

names(df_data_dummy_encoded)

################################################################################################################################
###################################### Building Model part ##########################################

##Building the logistic regression model for the large data set data

model_coupon_dummy <- glm(y ~ ., data=df_data_dummy_encoded, family = binomial)
summary(model_coupon_dummy)

#post modeling we found singularity issue with the variable "destination_passengerWork_Alone"

alias(model_coupon_dummy)
alias(model_coupon_dummy)$Complete

#Handling singularity issue by dropping column destination_passengerWork_Alone as it can expressed linearly by other variables.
df_data_dummy_encoded <- df_data_dummy_encoded[ ,!(names(df_data_dummy_encoded) %in% c("destination_passengerWork_Alone"))]

model_coupon_dummy <- glm(y ~ ., data=df_data_dummy_encoded, family = binomial)
summary(model_coupon_dummy)

#Selecting the best features and to tackle multi colinearity issue with VIF
#calculating the vif values

vif_values <- vif(model_coupon_dummy)
vif_values

#Printing features with has VIF Value greater than 5
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)
high_vif_vars <- vif_df[vif_values > 5,]
print(high_vif_vars)

#Printing the features which needs to be dropped which has VIF value greater than 5
Features_drop_vif <- high_vif_vars[,1] 
print(Features_drop_vif)

#Dropping features which has high VIF Value
df_data_dummy_encoded <- df_data_dummy_encoded[ , !(names(df_data_dummy_encoded) %in% Features_drop_vif)]
dim(df_data_dummy_encoded)
names(df_data_dummy_encoded)

#rerunning model with final features
model_coupon_dummy <- glm(y ~ ., data=df_data_dummy_encoded, family = binomial)
summary(model_coupon_dummy)

#Multi-collinearity issue and singularity issue has been fixed

############################################################################################################################

################################### Building models for random sample data ##############################################################

set.seed(10)
df_data_dummy_encoded_sample <- df_data_dummy_encoded[sample(1:nrow(df_data_dummy_encoded), 1000, replace = FALSE), ]

dim(df_data_dummy_encoded_sample)

#Using population data splitting the data set into train (70%) and test (30%)
set.seed(1025)
trainIndex2 <- createDataPartition(df_data_dummy_encoded_sample$y, p = .7, 
                                   list = FALSE)

train_samp <- df_data_dummy_encoded_sample[trainIndex2, ]
test_samp <- df_data_dummy_encoded_sample[-trainIndex2, ]

dim(train_samp)
dim(test_samp)

#Building logistic model for train_samp sample data set

model_coupon_samp <- glm(y ~ ., data = train_samp, family = binomial)
summary(model_coupon_samp)
coefficients(model_coupon_samp)

#Selecting the significant features and retraining the model

summary(model_coupon_samp)$coefficients[, 4] <= 0.05
significant_vars_log <- names(coef(model_coupon_samp))[summary(model_coupon_samp)$coefficients[, 4] <= 0.05]

significant_vars_log <- significant_vars_log[significant_vars_log != "(Intercept)"]
significant_vars_log

formula_log <- as.formula(paste("y ~", paste(significant_vars_log, collapse = "+")))
formula_log

#Retrain the model with significant features

model_coupon_samp <- glm(formula_log, data = train_samp, family = binomial)
summary(model_coupon_samp)


#Calculating the training accuracy by predicting the target values in train_samp data
pred_samp_train <- predict(model_coupon_samp, newdata = train_samp, type = "response")
pred_class_samp_train <- ifelse(pred_samp_train > 0.5, 1, 0)
pred_class_samp_train <- as.factor(pred_class_samp_train)
head(pred_class_samp_train)

train_samp$y <- factor(train_samp$y, levels = c(0, 1))

conf_log_train <- confusionMatrix(pred_class_samp_train, train_samp$y)
print(conf_log_train)


#Calculating the testing accuracy by predicting the target values in test_samp data
pred_samp_test <- predict(model_coupon_samp, newdata = test_samp, type = "response")
pred_class_samp_test <- ifelse(pred_samp_test > 0.5, 1, 0)
pred_class_samp_test <- as.factor(pred_class_samp_test)
head(pred_class_samp_test)

test_samp$y <- factor(test_samp$y, levels = c(0, 1))

conf_log_test <- confusionMatrix(pred_class_samp_test, test_samp$y)
print(conf_log_test)

##Fetching top 20 features from model_coupon_samp

# Extract coefficients
coefficients <- coef(model_coupon_samp)
 
# Convert to a data frame for better visualization
feature_importance <- data.frame(
  Feature = names(coefficients),
  Coefficient = coefficients,
  Odds_Ratio = exp(coefficients)
)
 
# Sort by absolute coefficient values
feature_importance <- feature_importance[order(abs(feature_importance$Coefficient), decreasing = TRUE), ]
 
# Printing features which has high importance
print(feature_importance)
 
# Plotting top 20 features and their coefficients in graph
library(ggplot2)
feature_importance <- feature_importance[order(abs(feature_importance$Coefficient), decreasing = TRUE), ]
top_features <- head(feature_importance, 20)
 
ggplot(top_features, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top Features by Coefficient", x = "Feature", y = "Coefficient")

####################################LDA model for sample data####################################################

lda_samp <- lda(y ~ ., data = train_samp)
summary(lda_samp)

#Calculating the training accuracy by predicting the target values in train_samp data
pred_lda_samp_train <- predict(lda_samp, newdata = train_samp)
pred_lda_samp_train <- pred_lda_samp_train$class

lda_conf_samp_train <- confusionMatrix(pred_lda_samp_train, as.factor(train_samp$y))
print(lda_conf_samp_train)

##Calculating the testing accuracy by predicting the target values in test_samp data
pred_lda_samp <- predict(lda_samp, newdata = test_samp)
pred_lda_samp <- pred_lda_samp$class

lda_conf_samp <- confusionMatrix(pred_lda_samp, as.factor(test_samp$y))
print(lda_conf_samp)


######################################QDA model for sample data ###########################################################

qda_samp <- qda(y ~ ., data = train_samp)
summary(qda_samp)

#Calculating the training accuracy by predicting the target values in train_samp data
pred_qda_samp_train <- predict(qda_samp, newdata = train_samp)
pred_qda_samp_train <- pred_qda_samp_train$class

qda_conf_samp_train <- confusionMatrix(pred_qda_samp_train, as.factor(train_samp$y))
print(qda_conf_samp_train)


#Calculating the testing accuracy by predicting the target values in test_samp data
pred_qda_samp <- predict(qda_samp, newdata = test_samp)
pred_qda_samp <- pred_qda_samp$class

qda_conf_samp <- confusionMatrix(pred_qda_samp, as.factor(test_samp$y))
print(qda_conf_samp)

######################################ROC CURVE#####################################################################################
# Function to plot ROC curves for multiple models
plot_roc_curves <- function(predictions, actual, model_names, auc_values) {
  roc_curves <- list()
  
  # Generating the ROC curves
  for (i in seq_along(predictions)) {
    roc_curves[[i]] <- roc(actual, predictions[[i]], levels = c(0, 1), direction = "<")
  }
  
  # Create a modified model name with AUC value for the legend
  model_names_with_auc <- paste0(model_names, " (AUC: ", round(auc_values, 3), ")")
  
  # Plotting ROC curves
  roc_data <- do.call(rbind, lapply(seq_along(roc_curves), function(i) {
    data.frame(
      TPR = roc_curves[[i]]$sensitivities,
      FPR = 1 - roc_curves[[i]]$specificities,
      Model = model_names_with_auc[i]
    )
  }))
  
  ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
    geom_line(size = 1.2) +
    labs(
      title = "ROC Curves for Logistic Regression and LDA Models",
      x = "False Positive Rate (FPR)",
      y = "True Positive Rate (TPR)"
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
}

# Logistic Regression probabilities
logistic_prob <- predict(model_coupon_samp, newdata = test_samp, type = "response")

# LDA probabilities
lda_prob <- predict(lda_samp, newdata = test_samp)$posterior[, 2] # Probabilities for class 1

# Compute AUC for Logistic Regression
roc_logistic <- roc(as.numeric(test_samp$y) - 1, logistic_prob, levels = c(0, 1), direction = "<")
auc_logistic <- auc(roc_logistic)

# Compute AUC for LDA
roc_lda <- roc(as.numeric(test_samp$y) - 1, lda_prob, levels = c(0, 1), direction = "<")
auc_lda <- auc(roc_lda)

# Combine ROC for all models
plot_roc_curves(
  predictions = list(logistic_prob, lda_prob),
  actual = as.numeric(test_samp$y) - 1, # Convert factor to binary (0, 1)
  model_names = c("Logistic Regression", "LDA"),
  auc_values = c(auc_logistic, auc_lda)
)

# Display AUC values
cat("AUC for Logistic Regression:", auc_logistic, "\n")
cat("AUC for LDA:", auc_lda, "\n")


##################################### Building Logistic regression, LDA , QDA models for larger data set #####################################

#Using Larger data set splitting the data set into train (70%) and test (30%)
set.seed(123)
trainIndex1 <- createDataPartition(df_data_dummy_encoded$y, p = .7, 
                                   list = FALSE)

train_pop <- df_data_dummy_encoded[trainIndex1, ]
test_pop <- df_data_dummy_encoded[-trainIndex1, ]

dim(train_pop)
dim(test_pop)

#Building logistic model for train_pop data set

model_coupon_pop <- glm(y ~ ., data = train_pop, family = binomial)
summary(model_coupon_pop)
coefficients(model_coupon_pop)

#Selecting the significant features and rerunning the model

summary(model_coupon_pop)$coefficients[, 4] <= 0.05
significant_vars_log <- names(coef(model_coupon_pop))[summary(model_coupon_pop)$coefficients[, 4] <= 0.05]

significant_vars_log <- significant_vars_log[significant_vars_log != "(Intercept)"]
significant_vars_log

formula_log <- as.formula(paste("y ~", paste(significant_vars_log, collapse = "+")))
formula_log

model_coupon_pop <- glm(formula_log, data = train_pop, family = binomial)
summary(model_coupon_pop)

#Predicting the target variable using model_coupon_pop with training data set.

pred_pop_train1 <- predict(model_coupon_pop, newdata = train_pop, type = "response")
pred_class_pop_train1 <- ifelse(pred_pop_train1 > 0.5, 1, 0)
pred_class_pop_train1 <- as.factor(pred_class_pop_train1)
head(pred_class_pop_train1)


#Predict the target variable using model_coupon_pop with testing data set.

pred_pop_train <- predict(model_coupon_pop, newdata = test_pop, type = "response")
pred_class_pop_train <- ifelse(pred_pop_train > 0.5, 1, 0)
pred_class_pop_train <- as.factor(pred_class_pop_train)
head(pred_class_pop_train)


test_pop$y <- factor(test_pop$y, levels = c(0, 1))
train_pop$y <- factor(train_pop$y, levels = c(0, 1))

# Generating the confusion for both testing and training dataset.

conf_log_pop <- confusionMatrix(pred_class_pop_train, test_pop$y)
conf_log_train_pop <- confusionMatrix(pred_class_pop_train1, train_pop$y)
print(conf_log_pop)
print(conf_log_train_pop)

# Extracting coefficients
coefficients <- coef(model_coupon_dummy)

# Convert to a data frame for better visualization
feature_importance <- data.frame(
  Feature = names(coefficients),
  Coefficient = coefficients,
  Odds_Ratio = exp(coefficients)  # Calculate Odds Ratios
)

# Sort by absolute coefficient values
feature_importance <- feature_importance[order(abs(feature_importance$Coefficient), decreasing = TRUE), ]

# Printing feature importance
print(feature_importance)

# Plot top features
library(ggplot2)
feature_importance <- feature_importance[order(abs(feature_importance$Coefficient), decreasing = TRUE), ]
top_features <- head(feature_importance, 20)

ggplot(top_features, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top Features by Coefficient", x = "Feature", y = "Coefficient")

############################### LDA for larger data set ######################
lda_pop <- lda(y ~ ., data = train_pop)
coefficients(lda_pop)


##Predicting values for training data set using lda_pop model
pred_lda_pop_train <- predict(lda_pop, newdata = train_pop)
pred_lda_pop_train
head(pred_lda_pop_train)
pred_lda_pop_train <- pred_lda_pop_train$class

##Predicting values for testing data set using lda_pop model

pred_lda_pop <- predict(lda_pop, newdata = test_pop)
pred_lda_pop
head(pred_lda_pop)
pred_lda_pop <- pred_lda_pop$class

#Generating confusion matrix for testing and training data set

lda_conf_pop <- confusionMatrix(pred_lda_pop, as.factor(test_pop$y))
print(lda_conf_pop)

lda_conf_pop_train <- confusionMatrix(pred_lda_pop_train, as.factor(train_pop$y))
print(lda_conf_pop_train)


###################################QDA for Larger data set ##################################################

qda_pop <- qda(y ~ ., data = train_pop)
summary(qda_pop)
coefficients(qda_pop)

#Predicting values for training data set

pred_qda_pop1 <- predict(qda_pop, newdata = train_pop)
pred_qda_pop1
head(pred_qda_pop1)
pred_qda_pop1 <- pred_qda_pop1$class

#Predicting values for testing data set

pred_qda_pop <- predict(qda_pop, newdata = test_pop)
pred_qda_pop
head(pred_qda_pop)
pred_qda_pop <- pred_qda_pop$class

qda_conf_pop <- confusionMatrix(pred_qda_pop, as.factor(test_pop$y))
print(qda_conf_pop)
qda_conf_pop1 <- confusionMatrix(pred_qda_pop1, as.factor(train_pop$y))
print(qda_conf_pop1)









