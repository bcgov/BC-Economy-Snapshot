
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
df1 <- statcan_download_data("14-10-0287-03", "eng")

# Put the date variable into date format (not string)
df1$date <- ymd(df1$REF_DATE)

# Keep obs with certain characteristics
df1 <- df1 %>%
  filter(
    GEO                            == "British Columbia",    # Filter for British Columbia in GEO
    Sex                            == "Both sexes",          # Filter for both sexes
    `Age group`                    == "15 years and over",   # Filter for age group
    `Statistics`                   == "Estimate",            # Filter for Statistic type
    `Labour force characteristics` == "Employment",          # Filter for variable employment
    `Data type`                    == "Seasonally adjusted", # Filter seasonally adjusted 
    date                           >= as.Date("2023-01-01")  # Filter for dates 2023 onwards
    )

## Keep relevant variables
df1 <- df1 %>%
  rename(emp = VALUE)

df2 <- df1 %>%
  select(emp, date) %>%  # Select and reorder variables
  arrange(date)

## format value of emp for ease of viewing
df2$emp <- number(df2$emp, accuracy = 1, big.mark = ",")







rm(df2) 


