
# Load the necessary libraries 
library(plumber)
library(tidymodels)
library(yardstick)
library(tidyverse)

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
tree_rec <- recipe(Diabetes_binary ~ ., data = diabetes) %>%
  step_dummy(all_nominal_predictors(),-all_outcomes()) %>%
  step_normalize(all_numeric_predictors(),-all_outcomes()) %>%
  prep(training = diabetes, retain = TRUE)

# preprocess the data 
processed_data <- bake(tree_rec, new_data = diabetes)


#fit model to entire dataset, using the best parameters as defined in the modeling section 
best_model <- rand_forest(mtry = 6) %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  fit(Diabetes_binary ~., data = processed_data)

#Define the API ------
#* Best Model API


# Pred endpoint ------
#* Make predictions based on inputs
#* @param Age categorical predictor 1 (default:mode)
#* @param Income categorical predictor 2 (default:mode)
#* @param Sex categorical predictor 3 (default:mode)
#* @param Education categorical predictor 4 (default:mode)
#* @param Smoker categorical predictor 5 (default:mode)
#* @param MentHlth numeric predictor 6 (default:mean)
#* @get /pred
function(Age = as.character(get_mode(diabetes$Age)),
         Income = as.character(get_mode(diabetes$Income)),
         Sex = as.character(get_mode(diabetes$Sex)),
         Education = as.character(get_mode(diabetes$Education)),
         Smoker = as.character(get_mode(diabetes$Smoker)),
         MentHlth = mean(diabetes$MentHlth, na.rm = TRUE)) {
  
  
  predictors <- tibble(
    Age = factor(Age, levels = levels(diabetes$Age)),
    Income = factor(Income, levels = levels(diabetes$Income)),
    Sex = factor(Sex, levels = levels(diabetes$Sex)),
    Education = factor(Education, levels = levels(diabetes$Education)),
    Smoker = factor(Smoker, levels = levels(diabetes$Smoker)),
    MentHlth = as.numeric(MentHlth)
  )
  
  #preprocess input data with the recipe
  processed_input <- bake(tree_rec, new_data = predictors)
  
  #predict with the best model 
  prediction <- predict(best_model, new_data = processed_input)
  
  list(prediction = prediction)
}

#helper function to find the mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x,ux)))]
}

# Example function calls for /pred:
# Example 1: /pred?Age=50-54&Income=less%20than%2010k&Sex=Female&Education=college%20graduate&Smoker=Yes&Menthlth=0
# Example 2: /pred?Age=65-69&Income=75%20or%20more&Sex=Male&Education=high%20school%20graduate&Smoker=No&Menthlth=5
# Example 3: /pred?Age=70-74&Income=less%20than%2010k&Sex=Male&Education=elementary&Smoker=Yes&Menthlth=10

# Info endpoint ------
#*provide info about the API 
#*@get /info 
function() {
  list(
    message = "API created by Annie DiFrank",
    github_page = ""
  )
}

#Confusion endpoint ------
#*plot the confusion matrix 
#* @serializer png
#* @get /confusion
function() {
  #generate predictions for the training data 
  predictions <- predict(best_model, new_data = processed_data, type = "class")
  
  #combine predictions with true values 
  results<- bind_cols(
    truth = processed_data$Diabetes_binary,
    pred = predictions$.pred_class
  )
  
  #Create confusion matrix plot 
  cm <- conf_mat(results, truth, pred)
  autoplot(cm)
}


#curl "http://localhost:8000/pred?Age=45-64&Income=75%20or%20more&Sex=Male&Education=elementary&Smoker=No&MentHlth=3"