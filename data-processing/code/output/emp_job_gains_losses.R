

# emp_job_gains_losses.R

# Load libraries
library(dplyr)
library(here)


# Load data
emp_data <- read.csv(here("data-processing/data/emp_ind.csv"), header = TRUE)


# Generalized function for Calculating Job Gains and Losses
calculate_job_changes <- function(emp_data, lag_period, threshold = 0) {
  emp_data <- emp_data           |>
    mutate(date = as.Date(date)) |>
    arrange(geo, naics, date)    |>
    group_by(geo, naics)         |>
    mutate(
      emp_gr_num = value - lag(value, lag_period),
      emp_gr_per = (emp_gr_num / lag(value, lag_period)) * 100
    ) |>
    ungroup()
  
  # Filter for to take sub-sectors and not totals
  filtered)data <- emp_data |> 
    fitler(grpl("goods|services", parent_sector, ignore.case = TRUE))
 
  # Separate job gains and losses
  filtered_data <- emp_data |>
    filter(!is.na(emp_gr_num) & if (threshold >= 0) 
      { emp_gr_num > threshold } else { emp_gr_num < threshold }) |>
    group_by(geo, parent_sector)   |>
    arrange(desc(abs(emp_gr_num))) |>
    mutate(
      share_of_total = (abs(emp_gr_num) / sum(abs(emp_gr_num))) * 100,
      growth_rate = emp_gr_per
    ) |>
    ungroup()
  
  return(filtered_data)
  
}

# Create Job Gains and Job Losses Data for Various Time Periods
job_gains  <- calculate_job_changes(emp_data, lag_period = 12, threshold =  0)
job_losses <- calculate_job_changes(emp_data, lag_period = 12, threshold = -1)

job_gains_1m  <- calculate_job_changes(emp_data, lag_period = 1, threshold =  0)
job_losses_1m <- calculate_job_changes(emp_data, lag_period = 1, threshold = -1)


# Save datasets to be accessed later to turn into dashboard. 
write.csv(job_gains, here("data-processing/data/emp_job_gains.csv"), row.names = FALSE)
write.csv(job_losses, here("data-processing/data/emp_job_losses.csv"), row.names = FALSE)
write.csv(job_gains_1m, here("data-processing/data/emp_job_gains_1m.csv"), row.names = FALSE)
write.csv(job_losses_1m, here("data-processing/data/emp_job_losses_1m.csv"), row.names = FALSE)

