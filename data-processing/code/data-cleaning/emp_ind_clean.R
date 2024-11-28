
# emp_ind_clean.R - Consolidated Version with Fixes and Enhanced Comments

# Load Libraries
library(statcanR)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(formattable)
library(here)

# Download Employment data directly from StatCan
emp_data <- statcan_download_data("14-10-0355-02", "eng")
  
# Convert date format from text to Date object
emp_data <- emp_data %>% 
  mutate(date = ymd(REF_DATE))  
    
# Filter the data to only include relevant rows (Estimates and Seasonally Adjusted data)
emp_data <- emp_data %>%
  filter(
   Statistics  == "Estimate",  # Use only 'Estimate' type rows
   `Data type` == "Seasonally adjusted",  # Use only seasonally adjusted data
   date        >= as.Date("2010-01-01")  # Filter to start from January 2010
  ) 
    
# Select and rename columns for better readability
emp_data <- emp_data  %>%
 select(
  date, 
  geo   = GEO,  # Rename column 'GEO' to 'geo'
  naics = `North American Industry Classification System (NAICS)`,  # Rename to 'naics'
  value = VALUE  # Rename employment value column
 ) 
    
# Convert 'value' from thousands to actual values
emp_data <- emp_data %>%
 mutate(value = as.numeric(value) * 1000) 
  
# Remove numbers and square brackets from 'naics' descriptions
emp_data <- emp_data %>%
 mutate(naics = gsub("\\[.*?\\]", "", naics)) %>% 
 mutate(naics = trimws(naics))  # Remove any trailing or leading whitespace
  
# Categorize sectors into broader categories
emp_data <- emp_data %>%
 mutate(parent_sector = case_when(
  
 naics == "Total employed, all industries" ~ "all industries",
 
 naics == "Goods-producing sector"         ~ "total goods",
 
 naics %in% c("Agriculture", 
              "Forestry, fishing, mining, quarrying, oil and gas", 
              "Utilities", 
              "Construction", 
              "Manufacturing") ~ "goods",
 
      naics == "Services-producing sector" ~ "total services",
 
      naics %in% c("Wholesale and retail trade", 
                   "Transportation and warehousing", 
                   "Finance, insurance, real estate, rental and leasing", 
                   "Professional, scientific and technical services", 
                   "Business, building and other support services", 
                   "Educational services", 
                   "Health care and social assistance", 
                   "Information, culture and recreation", 
                   "Accommodation and food services", 
                   "Other services (except public administration)",
                   "Public administration") ~ "services",
      TRUE ~ NA_character_  # If not classified, set as NA
    )) 
    
# Reorder the columns to improve readability
emp_data <- emp_data %>%  
  select(date, geo, parent_sector, naics, value, everything())


# Ensure parent_sector is character type and remove NA value before filtering
emp_data <- emp_data %>%
  mutate(parent_sector = as.character(parent_sector)) %>%
  filter(!is.na(parent_sector))

# Create dummy variable for observations with "goods" or "services" only
emp_data <- emp_data %>%
  mutate(sect_dummy = if_else(parent_sector %in% c("goods", "services"), 1, 0))

# Create a variable that examines the growth from the same month last year (Jan 24 vs. Jan 23)
emp_data <- emp_data %>% 
  arrange(geo, naics, date) %>%
  group_by(geo, naics) %>% 
  # Calculate job gains or losses compared to a lagged 12 month period
  mutate(
    emp_gr_num_12m = value - lag(value, 12), # Numerical change 12 months ago
    emp_gr_per_12m = ((value - lag(value, 12))/(lag(value, 12))) * 100 # Percentage change 12 months ago
  ) %>%
  ungroup() 

# Job Gains and Job Losses dummy (growth in employment numbers from t-12) 
emp_data <- emp_data %>% 
  mutate(
    job_gain_dummy = ifelse(emp_gr_num_12m > 0, 1, 0), # 1 if jobs increased from t-12 months ago
    job_loss_dummy = ifelse(emp_gr_num_12m < 0, 1, 0)  # 1 for job losses from t-12 months ago
  )


#### TEMP CODE 

# Find latest month in the data
latest_month <- max(emp_data$date)

# Extract month and year from the latest month
latest_month_value <- month(latest_month)
latest_year_value  <-  year(latest_month)

# Create dummy mark latest month and same month in previous years 
emp_data <- emp_data |> 
  mutate(
    t_12_dummy = if_else(
      month(date) == latest_month_value & year(date) <= latest_year_value, 1, 0
    )
  )

### END TEMP CODE 

## Save the dataset for future use
write.csv(emp_data, here("data-processing/data/emp_ind_final.csv"), row.names = FALSE)



