
#Loading the required packages
library(tidyverse)
library(haven)
library(broom)
library(knitr)
library(bestNormalize)

#Importing required data sets
sec2a <- read_dta("Raw Data/glss4_new/sec2a.dta") #Education Data
sec1 <- read_dta("Raw Data/glss4_new/sec1.dta") #Household size
sec8b <- read_dta("Raw Data/glss4_new/sec8b.dta") #Land area
agg2 <- read_dta("Raw Data/glss4_new/aggregates/agg2.dta") #Profit
sec0a <- read_dta("Raw Data/glss4_new/sec0a.dta") #Enumeration Area IDs
cs2 <- read_dta("Raw Data/glss4_new/community/cs2.dta") #Community questionnaire - infra section
cs5b <- read_dta("Raw Data/glss4_new/community/cs5b.dta") #Community questionnaire - agriculture

#Calculating the average years in education 
education_data <- group_by(sec2a, clust, nh) %>% 
  summarise(education_years = mean(s2aq2, na.rm = TRUE))

#Calculating the number of household members
household_size <- group_by(sec1, clust, nh) %>% 
  summarise(household_size = n())

#1. Selecting units of measures in acres, poles, ropes
#2. Converting the ropes to acres
#3. Calculating the total land size in acres for every household 
land_area <- filter(sec8b, s8bq4b %in% c(1, 2, 3)) %>% 
  mutate(land_size_acres = ifelse(s8bq4b == 3, s8bq4a/9, s8bq4a)) %>% 
  group_by(clust, nh) %>% 
  summarise(total_land = sum(land_size_acres, na.rm = TRUE))

#Rename the agri1c column to agricultural_profit 
#Select relevant variables
#AGRI1C= CRPINC1 + CRPINC2 + ROOTINC + INCOTHAG +TRCRPINC +HOMEPROC -EXPLAND - EXCROP - EXLIV - EXPFDPR1 - EXPFDPR2 (from files SUBAGG13 to SUBAGG16, SUBAGG26, SUBAGG22 to SUBAGG25 respectively)
profit <- mutate(agg2, agricultural_profit = agri1c) %>% 
  select(clust, nh, agricultural_profit)

#Find questions who have more impact on profit from the questionnaire

#Create local characteristics variable
#There are duplicated for some enumerator area levels
#Identify the duplicated values
duplicated_eanum_values <- count(cs2, eanum) %>% 
  filter(n>1) %>% 
  pull(eanum)

#Excluding the duplicated values from cs2
#Recoding road_access labeling "2" (no) to 0
cs2_clean <- filter(cs2, !eanum %in% duplicated_eanum_values) %>% 
  mutate(road_access = ifelse(s2q4 == 2, 0, s2q4)) %>% 
  select(eanum, road_access)

#Excluding the duplicated values from cs5b
#Recoding extension_visit labeling "2" (no) to 0
cs5b_clean <- filter(cs5b, !eanum %in% duplicated_eanum_values) %>% 
  mutate(extension_visit = ifelse(s5bq7 == 2, 0, s5bq7)) %>% 
  select(eanum, extension_visit)

#Joining the cleaned cs2 and cs5b data sets
community_data <- left_join(cs2_clean, cs5b_clean, by = "eanum")

#Getting enumerator area ID for the corresponding clust-nh unit
#Adding the data to the community data
household_community <- select(sec0a, eanum, clust, nh) %>% 
  left_join(community_data, by = "eanum")

#Joining all data sets: education, household_size, land_area and household_community
#Calculating profit per area unit
final_data <- left_join(profit, education_data, by = c("clust", "nh")) %>% 
  left_join(household_size, by = c("clust", "nh")) %>% 
  left_join(land_area, by = c("clust", "nh")) %>% 
  left_join(household_community, by = c("clust", "nh")) %>% 
  mutate(profit_per_acre = agricultural_profit/total_land)

#Fitting the first regression model, agricultural profit as a function of education_years,
#household_size, road_access, extension_visit
reg_model_1 <- lm(agricultural_profit~education_years+household_size+road_access+extension_visit,
                  data = final_data)

#Printing summary table
summary(reg_model_1)
kable(tidy(summary(reg_model_1)), digits = 3)

#Creating diagnostic plots 
plot(reg_model_1, which = 1) #Residuals vs. fitted
plot(reg_model_1, which = 2) #Normal qqplot
ggplot()+geom_histogram(aes(resid(reg_model_1))) #Histogram of residuals 
ggplot(final_data, aes(agricultural_profit))+geom_histogram() #Histogram of outcome variable
plot(reg_model_1, which = 3) #Constant variance plot (scale-location)