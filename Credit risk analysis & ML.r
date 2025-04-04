
#%%
#Importing libraries

library("dplyr")
library("tidyverse")
library("Hmisc")



#Importing data from desktop
data <- read.csv("C:/Users/vertt/Desktop/credit_risk_dataset.csv")

# Displaying the first rows, summary and number of rows
head(data)

summary(data)
nrow(data)

df <- data

#Replacing emp length values with 0 if not defined
df$person_emp_length[is.na(df$person_emp_length)] <- 0

summary(df)
# Something fishy going on in the person_age and person_emp length
# The maximum age in the dataset is 144
#and someone claims to have 123 years of experience.

# Cleaning those up and filling NA values with corresponding interest rates

loans_int <- df %>%
  group_by(loan_intent) %>%
  summarise(mean_int = mean(loan_int_rate, na.rm = TRUE))


df <- df %>%
  mutate(loan_int_rate = ifelse(loan_int_rate == "Invalid Number", NA, loan_int_rate),
         loan_int_rate = as.numeric(loan_int_rate)) %>%
  left_join(loans_int, by = "loan_intent") %>%
  mutate(loan_int_rate = ifelse(is.na(loan_int_rate), mean_int, loan_int_rate)) %>%
  select(-mean_int)

summary(df)

nrow(df)

sorted <- df[order(df$person_age, decreasing = TRUE), ]

df <- subset(df, person_age <= 84)


max_emp_length <-  df %>%
  arrange(desc(person_emp_length)) %>%
  slice(1:3)
print("Max emplength ")
print(max_emp_length)

df <- subset(df, person_emp_length < 50)


summary(df)
nrow(df)


# Dataset now seems to be clean and any unsual values are deleted.

y_dist <- df %>%
  count(loan_status)

y_dist$weight <- y_dist$n / sum(y_dist$n)

print(y_dist)

#The distribution still seems to be rather unbalanced, lets see how it performs.

loan_dist <- df %>%
  group_by(loan_intent) %>%
  summarise(count = n())

loan_dist <- loan_dist %>%
  arrange(desc(count))

ggplot(loan_dist, aes(x = reorder(loan_intent, -count), count)) +
  geom_bar(stat = "identity") +
  coord_flip()

#Most loans are for education and medical

ggplot(df, aes(x = person_age)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "#e9efe9", alpha = 0.9)

# Most loanees are quite young, just over 20
# Who are the defaulted individiuals?

def_df <- df %>% filter(loan_status == 1)
nondef_df <- df %>% filter(loan_status == 0)
summary(def_df)
summary(nondef_df)

# There seems not to be impact on age as both, defaulted and non-defaulted, seem to have same mean and median age.
# Income and employment length seem to be lower on defaulted versus the non-defaulted group.
# Also the loan amount, interest rate and loan % of the individuals income are higher on defaulted group on mean and median. 
# There is a slight difference in credit history between the groups, it can be assumed that the credit history is not important metric for defaulted group.


# Machine learning part

# feature engineering

# Define age and income groups
df$age_group <- cut(df$person_age,
                    breaks = c(20, 26, 36, 46, 56, 66, Inf),
                    labels = c("20-25", "26-35", "36-45", "46-55", "56-65", "67+"),
                    right = FALSE,
                    include.lowest = TRUE)

df$income_group <- cut(df$person_income,
                         breaks = c(0, 25000, 50000, 75000, 100000, Inf),
                         labels = c("low", "low-middle", "middle", "high-middle", "high"),
                         right = TRUE)

df$cb_person_default_on_file <- ifelse(df$cb_person_default_on_file == "Y", 1, 0)

#Creating numberic categories for ML purposes

print(unique(df$person_home_ownership))
print(unique(df$loan_grade))
print(unique(df$loan_intent))
print(unique(df$age_group))

df <- df %>%
  mutate(person_home_ownership_num = recode(person_home_ownership,
                                   "OWN" = 1,
                                   "MORTGAGE" = 2,
                                   "RENT" = 3,
                                   "OTHER" = 4))

df <- df %>%
  mutate(loan_intent_num = recode(loan_intent,
                                   "EDUCATION" = 1,
                                   "MEDICAL" = 2,
                                   "VENTURE" = 3,
                                   "PERSONAL" = 4,
                                   "HOMEIMPROVEMENT" = 5,
                                   "DEBTCONSOLIDATION" = 6))

df <- df %>%
  mutate(loan_grade_num = recode(loan_grade,
                                   "A" = 1,
                                   "B" = 2,
                                   "C" = 3.
                                   "D" = 4,
                                   "E" = 5,
                                   "F" = 6,
                                   "G" = 7))
df <- df %>%
  mutate(income_group_num = recode(income_group,
                                  "low"= 1,
                                  "low-middle" = 2,
                                  "middle" = 3, 
                                  "high-middle" = 4, 
                                  "high" = 5))

df <- df %>%
  mutate(age_group_num = recode(age_group,
                                "20-25" = 1,
                                "26-35" = 2,
                                "36-45" = 3, 
                                "46-55" = 4, 
                                "56-65" = 5.
                                "67-" = 6))

df <- df %>% 
  select(-person_home_ownership, -loan_intent, -loan_grade, -income_group, -age_group)


summary(df)
#Applying logistic regression

df <- df %>%
  select(loan_status, everything())

library(caTools)
library(caret)
set.seed(42)

sample_split <- sample.split(Y = df$loan_status, SplitRatio = 0.7)
train_set <- subset(x = df, sample_split == TRUE)
test_set <- subset(x = df, sample_split == FALSE)

logistic <- glm(loan_status ~ ., data = train_set, family = "binomial")


probs <- predict(logistic, newdata = test_set, type = "response")
pred <- ifelse(probs > 0.5, 1, 0)

reg <- confusionMatrix(data = factor(pred), 
                reference = factor(test_set$loan_status), positive = "1")
reg

# Moderate performance, based on confusion matrix, this model has statistically significant predictive power.
# It seems that the model cannot find the true positives as the sensitivity/recall ratio is 0.48. This may be due the imbalanced dataset (Y-value distribution).
# However, it seems that the negatives are clear from the dataset as specificity is aroun 0.95. 
# Precision/Pos pred value in this model is ok, around 0.73. This indicates that 75% of the labeled positives, were actually positives.

# Lets do some parameter optimization, it may improve the model performance!
library(glmnet)

train_set$loan_status <- as.factor(train_set$loan_status)

# Define cross-validation settings
ctrl <- trainControl(method = "cv", number = 10)

# Define a grid of hyperparameters for tuning:
# - 'alpha' controls the elastic net mixing (0 = ridge, 1 = lasso)
# - 'lambda' is the regularization parameter
grid <- expand.grid(alpha = c(0, 0.25, 0.5, 0.75, 1),
                    lambda = 10^seq(-4, 0, length = 15))

# Train the logistic regression model using glmnet
log_model <- train(loan_status ~ .,
                   data = train_set,
                   method = "glmnet",
                   family = "binomial",
                   trControl = ctrl,
                   tuneGrid = grid)

# View the results: best parameters and performance metrics
print(log_model)

best_params <- log_model$bestTune
print(best_params)

probs <- predict(log_model, newdata = test_set, type = "prob")[, 2]  # Get probabilities
preds <- ifelse(probs > 0.5, 1, 0)  # Convert to binary prediction

reg2 <- confusionMatrix(factor(preds), factor(test_set$loan_status), positive = "1")

reg2
#  With parameter optimization, the model's performance didn't improve. Lets move another model.


# Neural net with ROC parameter optimization (Aims for highest ROC value)

library(nnet)
library(MLmetrics)
# Ensure the target variable is a factor for classification
# Reassign the factor levels to valid names
train_set$loan_status <- factor(train_set$loan_status, 
                                levels = c("0", "1"), 
                                labels = c("No", "Yes"))
# Set up cross-validation settings
ctrlnn <- trainControl(method = "cv", 
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary
)
# Define a grid of tuning parameters:
# - size: number of neurons in the hidden layer
# - decay: weight decay parameter for regularization
# Grid smaller for NN parameter optimization as the time to train the model is much longer than with LR.
grid1 <- expand.grid(size = c(3, 5, 7, 9, 11),
                    decay = c(0.1, 0.01, 0.001, 0.0001))

# Train the neural network model
set.seed(42)
nn_model <- train(loan_status ~ .,
                  data = train_set,
                  method = "nnet",
                  preProcess = c("center", "scale"),  # Neural nets perform better with scaled inputs
                  trControl = ctrlnn,
                  tuneGrid = grid1,
                  metric = "ROC",  # Optimize based on ROC
                  maxit = 200,     # Maximum iterations for convergence
                  trace = TRUE)    # Maximum number of iterations

# View the tuning results and best model
print(nn_model)

probs <- predict(nn_model, newdata = test_set, type = "prob")[, 2]  # Get probabilities
preds <- ifelse(probs > 0.5, 1, 0)  # Convert to binary prediction, the threshold can be adjusted, this may or may not improve the model performance.

conf_nn <- confusionMatrix(factor(preds), factor(test_set$loan_status), positive = "1")
conf_nn

# Neural net socres meaningful P value (< 2.2e-16), indicating that this model is also statistically significant.
# Sensitivity/recall ratio is much higher (0.61), which means that the model can find the true positive values better than LR.
# Specificity is improved to 0.97, which is indicates that NN finds negative values better than LR.
# Also the precision metric is quite good (0.85). 85% of all positive labeled instances were true positive.

# It seems that neural network precict better defaults than LR. 
# One interesting aspect would be inspecting permutation importances for both models. By this the relevant factors for defaulting can be found.

