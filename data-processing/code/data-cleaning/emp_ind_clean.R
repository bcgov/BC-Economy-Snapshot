
#### Data Cleaning
# This code will import the data from StatCan
# and clean and tidy it

# Load in relevant libraries 
library(statcanR)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(formattable)
library(here)

# Download Employment data from StatCan
emp_ind <- statcan_download_data("14-10-0355-02", "eng")

# Clean data
emp_ind <- emp_ind %>% 
  
  # Convert date and extract year
  mutate(date = ymd(REF_DATE)) %>%
  
  # Filter the data based on relevant criteria
  filter(
    Statistics  == "Estimate",
    `Data type` == "Seasonally adjusted",
    date        >= as.Date("2010-01-01")
  ) %>%
  
  # Select and rename variables (e.g., GEO is renamed to geo)
  select(date, geo = GEO, naics = `North American Industry Classification System (NAICS)`, value = VALUE) %>%
  
  # Convert 'value' from thousands to actual numbers
  mutate(value = as.numeric(value) * 1000) %>%
  
  # Remove numbers and square brackets at the end of naics
  mutate(naics = gsub("\\[.*?\\]", "", naics)) %>%
  mutate(naics = trimws(naics))                %>%  # Remove any trailing or leading whitespace
  
  # Create a hierarchical categorical variable
  mutate(parent_sector = case_when(
    naics == "Total employed, all industries" ~ "all industries",
    naics == "Goods-producing sector" ~ "business sector industries",
    naics %in% c("Agriculture", "Forestry, fishing, mining, quarrying, oil and gas", 
                 "Utilities", "Construction", "Manufacturing") ~ "goods producing",
    naics == "Services-producing sector" ~ "business sector industries",
    naics %in% c("Wholesale and retail trade", "Transportation and warehousing", 
                 "Finance, insurance, real estate, rental and leasing", 
                 "Professional, scientific and technical services", 
                 "Business, building and other support services", 
                 "Educational services", "Health care and social assistance", 
                 "Information, culture and recreation", 
                 "Accommodation and food services", 
                 "Other services (except public administration)") ~ "service producing",
    naics == "Public administration" ~ "government sector",
    TRUE ~ NA_character_  # Default case for unclassified naics
  )) %>%
  
  # Reorder the variables
  select(date, geo, parent_sector, naics, value, everything()) %>%
  
  # Sort the data by geo and date
  arrange(geo, date)

# Send cleaned data to folder on GitHub
write.csv(emp_ind, here("data-processing", "data", "emp_ind.csv"), row.names = FALSE)
