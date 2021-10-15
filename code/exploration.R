#Exploration 

#load packages
library(here)
library(tidyverse)
library(ggplot2)
library(table1)

#Data Dictionary
#BodyTemp: not in data dictionary
#SwollenLymphNodes: Did the patient report swollen/tender lymph nodes (adenopathy)?
#Fatigue: Did the patient report fatigue?
#Nausea: Did the patient report nausea as a symptom?

#path to data: note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
processed_data <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(processed_data)

#main continuous outcome of interest is Body temperature
#main categorical outcome is Nausea
#We want to see if the other symptoms are correlated with (predict) those outcomes

#For each (important) variable, produce and print some numerical output 
table1 <- table1(~ Nausea + BodyTemp + SwollenLymphNodes + NasalCongestion +
                   Sneeze + Fatigue + SubjectiveFever + Pharyngitis + ChillsSweats +
                   Weakness + Myalgia, data = processed_data, overall="Total")
table1

#Relationship between predictors of interest and outcome BodyTemp?
#Predictors of interest:  SwollenLymphNodes, NasalCongestion, Sneeze, Fatigue, SubjectiveFever, Pharyngitis

#Plot Body Temperature: main continuous outcome of interest
ggplot(processed_data, aes(x=BodyTemp)) + 
  geom_density(color="darkblue", fill="lightblue") +
  ggtitle("Body Temperature, Density Plot")
summary(processed_data$BodyTemp)
#right skewed distibution (Median [Min, Max]: 98.5 [97.2, 103])

#Plot Nausea: main categorical outcome of interest
ggplot(processed_data, aes(x=Nausea)) + geom_bar(fill="lightblue") + 
  ggtitle("Nausea, Bar Plot") 
#255 (34.9%) reported nausea

#Plot NasalCongestion: predictor of interest
ggplot(processed_data, aes(x=NasalCongestion)) + geom_bar(fill="lightblue3") +
  ggtitle("Nasal Congestion, Bar Plot")
#563 (77.1%) reported nasal congestion

#Plot Sneeze: predictor of interest
ggplot(processed_data, aes(x=Sneeze)) + geom_bar(fill="lightblue3") +
  ggtitle("Sneeze, Bar Plot")
#391 (53.6%) reported Sneeze

#Plot SubjectiveFever: predictor of interest
ggplot(processed_data, aes(SubjectiveFever)) + geom_bar(fill="lightblue3") +
  ggtitle("Subjective Fever, Bar Plot")
#500 (68.5%) reported Subjective Fever

#Plot SubjectiveFever: predictor of interest
ggplot(processed_data, aes(SubjectiveFever)) + geom_bar(fill="lightblue3") +
  ggtitle("Subjective Fever, Bar Plot")
#500 (68.5%) reported Subjective Fever

#Plot Pharyngitis: predictor of interest
ggplot(processed_data, aes(x=Pharyngitis)) + geom_bar(fill="lightblue3") +
  ggtitle("Pharyngitis, Bar Plot")
#611 (83.7%) reported sore throat

#Plot Weakness: predictor of interest
ggplot(processed_data, aes(x=Weakness)) + geom_bar(fill="lightblue3") +
  ggtitle("Weakness, Bar Plot")
#120 (16.4%) reported severe weakness, 
#338 (46.3%) reported moderate weakness,
#223 (30.5%) reported mild weakness
#49 (6.7%) reported no weakness

#Plot Myalgia: predictor of interest
ggplot(processed_data, aes(x=Myalgia)) + geom_bar(fill="lightblue3") +
  ggtitle("Myalgia, Bar Plot")
#113 (15.5%) reported severe myalgia, 
#325 (44.5%) reported moderate myalgia,
#213 (29.2%) reported mild myalgia
#79 (10.8%) reported no myalgia

#Plot Swollen Lymph Nodes: predictor of interest
ggplot(processed_data, aes(x=SwollenLymphNodes)) + geom_bar(fill="lightblue3") +
  ggtitle("Swollen Lymph Nodes, Bar Plot")
#312 (42.7%) reported swollen lymph nodes

#Plot Fatigue vs. Body Temperature
ggplot(processed_data, aes(x = Fatigue, y = BodyTemp)) +
  geom_boxplot() + 
  ggtitle("Fatigue vs. Body Temperature, Boxplot")

#Median body temp is a little bit higher for individuals with fatigue 
#Probably not a significant difference
FatigueYes <- processed_data %>%
  filter(Fatigue == "Yes")
summary(FatigueYes$BodyTemp) #median = 98.6, IQR = 98.2 - 99.3

FatigueNo <- processed_data %>%
  filter(Fatigue == "No")
summary(FatigueNo$BodyTemp) #median = 98.3, IQR = 98.1 - 99


#Plot SwollenLymphNodes vs. Body Temperature
ggplot(processed_data, aes(x = SwollenLymphNodes, y = BodyTemp)) +
  geom_boxplot() + 
  ggtitle("Swollen Lymph Nodes vs. Body Temperature, Boxplot")

#Median body temp is a little bit higher for individuals without swollen lymph nodes
#Probably not a significant difference
SwollenLymphNodesYes <- processed_data %>%
  filter(SwollenLymphNodes == "Yes")
summary(SwollenLymphNodesYes$BodyTemp) #median = 98.5, IQR = 98.2 - 99.2

SwollenLymphNodesNo <- processed_data %>%
  filter(SwollenLymphNodes == "No")
summary(SwollenLymphNodesNo$BodyTemp) #median = 98.6, IQR = 98.2 - 99.3
