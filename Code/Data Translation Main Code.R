
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

