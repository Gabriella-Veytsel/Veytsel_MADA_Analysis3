###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(broom) #glance function
library(tidyverse) #tidyr: tidy output
library(tidymodels) #linear and logistic reg
library(broom.mixed)
library(dotwhisker)

#path to data:: note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load clean data. 
mydata <- readRDS(data_location)

#RunnyNose is our main predictor of interest

#Fit a linear model to the continuous outcome using only the main predictor of interest.
lm_mod <- linear_reg() %>%
  set_engine("lm")

lm_fit <- lm_mod %>%
  fit(BodyTemp ~ RunnyNose, data = mydata)

lm_fit_summary <- tidy(lm_fit) #tidy output
lm_fit_summary

#Fit another linear model to the continuous outcome using all predictors of interest.
mydata_subset <- mydata %>%
  select(c(BodyTemp, SwollenLymphNodes, NasalCongestion, Sneeze, Fatigue, 
           SubjectiveFever, Pharyngitis))  ##All predictors of interest

lm_fit_all <- lm_mod %>%
  fit(BodyTemp ~ ., data = mydata_subset)

lm_fit_all_summary <- tidy(lm_fit_all) #tidy output
lm_fit_all_summary

#Compare the model results for the model with just the main predictor and all predictors.
glance(lm_fit) #adjusted r-squared: 0.0110; AIC: 2329, BIC: 2343
glance(lm_fit_all) #adjusted r-squared: 0.0862; AIC: 2277, BIC: 2313
#Adding additional predictors only slightly improves the model fit: 
  #slightly higher r-squared, but still vary low
  #slightly lower AIC and BIC

#Fit a logistic model to the categorical outcome using only the main predictor of interest.
logistic_mod <- logistic_reg() %>%
  set_engine("glm")

log_fit <- logistic_mod %>%
  fit(Nausea ~ RunnyNose, data = mydata)

log_fit_summary <- tidy(log_fit)
log_fit_summary

#Fit another logistic model to the categorical outcome using all (important) predictors of interest.
mydata_subset_nausea <- mydata %>%
  select(c(ChillsSweats, Fatigue, SubjectiveFever, Headache, 
           Weakness, Myalgia, AbPain, Diarrhea, Vomit, BodyTemp, Nausea))
         
log_fit_all <- logistic_mod %>%
  fit(Nausea ~ ., data = mydata_subset_nausea)

log_fit_all_summary <- tidy(log_fit_all)
log_fit_all_summary

#Compare the model results for the categorical model with just the main predictor and all predictors.
glance(log_fit) #deviance: 945
glance(log_fit_all) #deviance: 783

#the smaller the deviance, the better the fit, so adding additional predictors improves the model fit
#also, the AIC and BIC are lower

