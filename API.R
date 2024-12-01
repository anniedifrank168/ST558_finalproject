# Load the necessary libraries 
library(plumber)
library(tidymodels)
library(yardstick)

# load data, do preprocessing steps
diabetes <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
diabetes <- diabetes %>% 
  mutate(Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes")),
         Smoker = factor(Smoker, levels = c(0, 1), labels = c("No", "Yes")),
         Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
         Age = factor(Age, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13), labels = c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 or older")),
         Education = factor(Education, levels = c(1,2,3,4,5,6), labels = c("never attended school or only kindergarten","elementary","some high school","high school graduate","some college or technical school","college graduate")),
         Income = factor(Income, levels = c(1,2,3,4,5,6,7,8), labels = c("less than 10k","less than 15k", "less than 20k", "less than 25k","less than 35k","less than 50k","less than 75k","75 or more"))
  ) %>% 
  select(c(Diabetes_binary, Smoker, Sex, Age, Education, Income, MentHlth))

#fit model- define recipe
tree_rec <- recipe(Diabetes_binary ~., data = diab_train) %>% 
  step_dummy(Age,Income,Sex,Education,Smoker) %>%
  step_normalize(MentHlth)

#fit model to entire dataset, using the best parameters as defined in the modeling section 
best_model <- rand_forest()