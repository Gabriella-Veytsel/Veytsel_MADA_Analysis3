
#Load packages
library(tidyverse)
library(tidymodels)
library(skimr) #for variable summaries
library(rsample) #for splitting data
library(yardstick) #roc_curve() and roc_auc()

#path to data: note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")
processed_data <- readRDS(data_location) #load data.
glimpse(processed_data)

#It is important that our outcome variable for training a logistic regression 
  #is a factor

#Randomly split data into training set and testing set
#*****************************************************

#The training data will be used to fit the model
#The testing set will be used to measure model performance

#Fix the random numbers by setting a seed
#this enables the analysis to be reproducible when random #s are used
set.seed(222) 
#Put 3/4 of the data into the training set
data_split <- initial_split(processed_data, prop = 3/4)
#Create data frames for the two sets
train_data <- training(data_split)
test_data <- testing(data_split)

#Fit categorical outcome of interest to all predictors
#*****************************************************

#Categorical outcome of interest = Nausea
#Recipe () has two arguments: a formula and the data
processed_cat_rec <- recipe(Nausea ~ ., data = train_data)

#Build a model specification using the parsnip package
logistic_mod <- logistic_reg() %>%
  set_engine("glm")

#Model workflow pairs a model and recipe together
processed_cat_workflow <- 
  workflow() %>%
  add_model(logistic_mod) %>%
  add_recipe(processed_cat_rec)

processed_cat_workflow

processed_cat_fit <-
  processed_cat_workflow %>%
  fit(data = train_data)

#Extract the fitted model object and use tidy() to get a tidy tibble 
  #of model coefficients
processed_cat_fit %>%
  extract_fit_parsnip() %>%
  tidy()
#~~~~~~~~~~~~~~~
#Model Evaluaton:
#Look at predictions, ROC and ROC-AUC for my data
#ROC-AUC is how good the model is at distinguishing 
  #between patients with nausea and patients without nausea
#************************************************

#Use the trained workflow to predict with the test data
#Returns predicted class
predict(processed_cat_fit, test_data)
#Can these warnings be ignored?

#Returns predicted class probabiliies
processed_cat_aug <- augment(processed_cat_fit, test_data)
processed_cat_aug

#Create the ROC curve
processed_cat_aug %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot()

#Estimate the area under the curve
processed_cat_aug %>%
  roc_auc(truth = Nausea, .pred_No) #0.724 - model is useful
#Was it correct to use .pred_No instead of .pred_Yes
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Out of curiousity, apply training data instead
#Use the trained workflow to predict with the training data
#Returns predicted class
predict(processed_cat_fit, train_data)

#Returns predicted class probabiliies
processed_cat_aug_train <- augment(processed_cat_fit, train_data)
processed_cat_aug_train

#Create the ROC curve
processed_cat_aug_train %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot()

#Estimate the area under the curve
processed_cat_aug_train %>%
  roc_auc(truth = Nausea, .pred_No) #0.787 - has better performance, as expected


#Alternatiely, fit only the main predictor (RunnyNose)
#*****************************************************

#Set up a new recipe
processed_cat_main_rec <- recipe(Nausea ~ RunnyNose, data = train_data)

#Set up a new workflow
#Model workflow pairs a model and recipe together
processed_cat_main_workflow <- 
  workflow() %>%
  add_model(logistic_mod) %>%
  add_recipe(processed_cat_main_rec) #new recipe

processed_cat_main_workflow

processed_cat_main_fit <-
  processed_cat_main_workflow %>%
  fit(data = train_data)

#Evaluate performace
#Use the trained workflow to predict with the test data
#Returns predicted class
predict(processed_cat_main_fit, test_data)
#No warnings this time

#Returns predicted class probabiliies
processed_cat_main_aug <- augment(processed_cat_main_fit, test_data)
processed_cat_main_aug

#Create the ROC curve
processed_cat_main_aug %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot() 

#Estimate the area under the curve
processed_cat_main_aug %>%
  roc_auc(truth = Nausea, .pred_No) #0.466 - model is no good
#Was it correct to use .pred_No instead of .pred_Yes

#Out of curiousity, apply training data instead
#Use the trained workflow to predict with the training data
#Returns predicted class
predict(processed_cat_main_fit, train_data)

#Returns predicted class probabiliies
processed_cat_main_aug_train <- augment(processed_cat_main_fit, train_data)
processed_cat_main_aug_train

#Create the ROC curve
processed_cat_main_aug_train %>%
  roc_curve(truth = Nausea, .pred_No) %>%
  autoplot()

#Estimate the area under the curve
processed_cat_main_aug_train %>%
  roc_auc(truth = Nausea, .pred_No) #0.519 - has better performance, as expected

####################################################################
#Amanda Glatter

#Continuous outcome of interest = BodyTemp
#Recipe () has two arguments: a formula and the data
bodytemp_recipe <- recipe(BodyTemp ~ ., data = train_data)

#Build a model specification using the parsnip package
linear_model <- linear_reg() %>%
  set_engine("lm")

#Model workflow pairs a model and recipe together
cont_workflow <- 
  workflow() %>%
  add_model(linear_model) %>%
  add_recipe(bodytemp_recipe)

cont_workflow

cont_fit <-
  cont_workflow %>%
  fit(data = train_data)

#Extract the fitted model object and use tidy() to get a tidy tibble 
#of model coefficients
cont_fit %>%
  extract_fit_parsnip() %>%
  tidy()

#We'll now start to evaluate the model using RMSE.

#We use RMSE on BodyTemp against all predictors and use the train data.
#Make predictions.

predict(cont_fit, train_data)

#Returns predicted class probabiliies
cont_aug_train <- augment(cont_fit, train_data)
cont_aug_train

#RMSE
train_rmse <- cont_aug_train %>%
  rmse(truth = BodyTemp, .pred)

train_rmse #The value is 1.11.

#Now we repeat this on the test data.
#We use RMSE on BodyTemp against all predictors and use the test data.
#Make predictions.

predict(cont_fit, test_data)

#Returns predicted class probabiliies
cont_aug_test <- augment(cont_fit, test_data)
cont_aug_test

#RMSE
test_rmse <- cont_aug_test %>%
  rmse(truth = BodyTemp, .pred)

test_rmse #The value is 1.15.

#Now we have to make a recipe for Body Temp and Runny nose.
bodytemp_runnynose_recipe <- recipe(BodyTemp ~ RunnyNose, data = train_data)

#Model workflow pairs a model and recipe together (runny nose and body temp)
cont_workflow2 <- 
  workflow() %>%
  add_model(linear_model) %>%
  add_recipe(bodytemp_runnynose_recipe)

cont_fit2 <-
  cont_workflow2 %>%
  fit(data = train_data)

#Extract the fitted model object and use tidy() to get a tidy tibble 
#of model coefficients
cont_fit2 %>%
  extract_fit_parsnip() %>%
  tidy()

#make predictions
predict(cont_fit2, train_data)

#Returns predicted class probabiliies
cont_aug_train2 <- augment(cont_fit2, train_data)
cont_aug_train2

#We use RMSE on BodyTemp with predictor RunnyNose using test data.

#RMSE
train_rmse2 <- cont_aug_train2 %>%
  rmse(truth = BodyTemp, .pred)

train_rmse2 #The value is 1.21.

#Now we repeat this on the test data.
#We use RMSE on BodyTemp against Runnt nose and use the test data.
#Make predictions.

predict(cont_fit2, test_data)

#Returns predicted class probabiliies
cont_aug_test2 <- augment(cont_fit2, test_data)
cont_aug_test2

#RMSE
test_rmse2 <- cont_aug_test2 %>%
  rmse(truth = BodyTemp, .pred)

test_rmse2 #The value is 1.13.
