# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at 
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# Loading data----
    load_emp_ind <- function() {
      emp_data   <- read.csv(here("data-processing/data/emp_ind.csv")       , header = TRUE)
      job_gains  <- read.csv(here("data-processing/data/emp_job_gains.csv") , header = TRUE)
      job_losses <- read.csv(here("data-processing/data/emp_job_losses.csv"), header = TRUE)
      
      # Ensure no missing values
      emp_data   <- na.omit(emp_data)
      job_gains  <- na.omit(job_gains)
      job_losses <- na.omit(job_losses)
      
      # Return all the datasets in a named list
      
      return(list(emp_data = emp_data, job_gain = job_gains, job_losses = job_losses))
      
    }
    

# Employment Industry Dash----
    ## line plot ----
    
    employment_emp_ind_lineplot_data <- function(df){
      df |>
        filter(geo   == "British Columbia",
               naics == "Total employed, all industries") |>
        select(date, value)
      
    }
    
    employment_emp_ind_render_lineplot <- function(df, input){
      dash_lineplot(employment_emp_ind_lineplot_data, df, input, "Persons")}
    
    ## table ----
    employment_emp_ind_table_data <- function(df, year, geo, parent){
       df |>
        filter(
          date          == max(df$date),
          geo           == geo,
          parent_sector == parent)
    }
    
    employment_emp_ind_render_table <- function(df, input){
      data <- employment_emp_ind_table_data(df, input$employment_emp_ind_table_geo, input$employment_emp_ind_table_parent)
      DT::datatable(data, options = list(dom = 't'), escape = FALSE, rownames = FALSE, 
                    caption = htmltools::tags$caption("",
                                                      style = "font-family: Arial; font-size: 12px;"))
    }
    

    
    
    
