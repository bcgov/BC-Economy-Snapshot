
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
df1 <- statcan_download_data("14-10-0355-02", "eng")

# Put the date variable into date format (not string) and then remove REF_DATE
df1      <- df1 %>% mutate(date = ymd(REF_DATE)) %>% select(-REF_DATE)
df1$year <- year(df1$date)


# Keep obs with certain characteristics
df1 <- df1 %>%
  filter(
    Statistics   == "Estimate",            # Filter for Statistic type
    `Data type`  == "Seasonally adjusted", # Filter seasonally adjusted 
    date         >= as.Date("2010-01-01")  # Filter for dates 2023 onwards
  )

# Reduce size of the dataset so keep only relevant variables. 
df1 <- df1 %>%
  select(date, year, GEO, `North American Industry Classification System (NAICS)`, VALUE)

# Rename vars 
df1 <- df1 %>%
  rename(naics = `North American Industry Classification System (NAICS)`,
         value = VALUE)

# Convert 'value' from thousands to actual numbers
df1 <- df1 %>%
  mutate(value = as.numeric(value) * 1000)

# Sort df1 by GEO (alphabetically) and date (chronologically)
df1 <- df1 %>%
  arrange(GEO, date)


# Browse BC data and compare to previous data downloaded to ensure no mistake. 
# historical data should be the same. 
# df_bc <- df1 %>%
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

