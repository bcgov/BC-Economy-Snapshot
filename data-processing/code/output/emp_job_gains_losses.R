

# emp_job_gains_losses.R

# Load libraries
library(dplyr)
library(here)


# Load data
emp_data <- read.csv(here("data-processing/data/emp_ind.csv"), header = TRUE)

# Step 1: Calculate Employment Growth Over 12 Month ---- 
emp_data <- emp_data           |>
  mutate(date = as.Date(date)) |>
  arrange(geo, naics, date)    |>
  group_by(geo, naics)         |>
  mutate(
    employment_growth_num = value - lag(value, 12), #Difference from 12 months ago
    employment_growth_perc = (employment_growth_num / lag(value, 12)) * 100 # Percentage change 
  ) |>
  ungroup()

# Step 2: Calculate Job gains/ losses for each sector ---- 
# Separate the dataset into two for gains and losses

# Job gains
job_gains <- emp_data |>
  filter  (!is.na(employment_growth_num) & employment_growth_num > 0) |>
  group_by(geo, parent_sector)                                        |>
  arrange (desc(employment_growth_num))                               |>
  
  mutate(
    share_of_total = (employment_growth_num / sum(employment_growth_num)) * 100,
    growth_rate = employment_growth_perc
  ) |>
  ungroup() 

# Job losses
job_losses <- emp_data |>
  filter(!is.na(employment_growth_num) & employment_growth_num < 0) |> 
  group_by(geo, parent_sector)                                      |>
  arrange(desc(abs(employment_growth_num)))                         |>
  
  mutate(
    share_of_total = (abs(employment_growth_num) / sum(abs(employment_growth_num))) * 100, 
    growth_rate = employment_growth_perc
  ) |>
  ungroup()

# Save datasets to be accessed later to turn into dashboard. 
write.csv(job_gains,  here("data-processing/data/emp_job_gains.csv") , row.names = FALSE)
write.csv(job_losses, here("data-processing/data/emp_job_losses.csv"), row.names = FALSE)


