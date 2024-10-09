
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

# Download Employment data from Statcan
emp_ind <- statcan_download_data("14-10-0355-02", "eng")


# Clean data
emp_ind <- emp_ind %>%
 
   # Convert date and extract year
  mutate(date = ymd(REF_DATE),
         year = year(date)) %>%
 
   # Filter the data based on relevant criteria
  filter(
    Statistics  == "Estimate",
    `Data type` == "Seasonally adjusted",
    date        >= as.Date("2010-01-01")
  ) %>%
 
   # Select and rename variables
  select(date, year, geo = GEO, naics = `North American Industry Classification System (NAICS)`, value = VALUE) %>%
 
   # Convert 'value' from thousands to actual numbers
  mutate(value = as.numeric(value) * 1000) %>%
 
   # Sort the data by geo and date
  arrange(geo, date)

emp_ind <- emp_ind %>%
  arrange(geo, date)

# Send cleaned data to folder on GitHub
write.csv(emp_ind, "C:/Users/DOLAWLOR/BC-Economy-Snapshot/data-processing/data/emp_ind.csv", row.names = FALSE)


### IGNORE - Notes to remember to check and compare data to published sources like BC stats on employment
# Browse BC data and compare to previous data downloaded to ensure no mistake. 
# historical data should be the same. 
# df_bc <- emp_ind %>%
#   filter(GEO    == "British Columbia", 
#         naics  == "Total employed, all industries") %>%
#  select(date, year, GEO, naics, value)  # Select only relevant columns

# df_avg_year <- df_bc %>%
#  group_by(year) %>%  # Assuming the year column is named 'Year'
#  summarize(average_employment = mean(value, na.rm = TRUE))  # Taking the average of th

# df_avg_year %>%
#  mutate(average_employment = comma(average_employment)) %>%
#  head()

# df_avg_year <- df_avg_year %>%
#  mutate(average_employment_thousands = average_employment / 1000)

## Labour force keep employment, full time and part time employment. 
## Get different dataset to examine sectors. 
## Keep the variable name as VALUE














