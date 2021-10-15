###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyr)

#SympAct_Any_Pos.Rda contains data for all influenza patients regardless of diagnosis method.

#path to data: note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

#load data. 
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#Remove all variables that have Score or Total or FluA or FluB or Dxname or Activity in their name.
names(rawdata)
processed_data <- rawdata %>% 
  select(-contains(c("Score", "Total", "FluA", "FluB", "Dxname", "Activity")))
names(processed_data)

#Also remove the variable Unique.Visit. 
processed_data <- processed_data %>%
  select(-c(Unique.Visit))
names(processed_data)

#You should be left with 32 variables coding for presence or absence of some symptom. 
#Only one, temperature, is continuous. 
#A few have multiple categories.
glimpse(processed_data)

#Remove any NA observations, there aren't many.
length(which(is.na(processed_data))) #5 observations with missing data
processed_data <- processed_data %>%
  drop_na() #735 observations -> 730

#you should end up with 730 observations and 32 variables.
glimpse(processed_data)

# save data as RDS: location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processed_data, file = save_data_location)


