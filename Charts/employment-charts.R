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
  emp_data      <- read.csv(here("data-processing/data/emp_ind_final.csv"), header = TRUE)
    
  # Ensure no missing values
  emp_data      <- na.omit(emp_data)
  
  # Return all the datasets in a named list
  return(emp_data) 
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
      data <- employment_emp_ind_table_data(df, 
                                            input$employment_emp_ind_table_geo, 
                                            input$employment_emp_ind_table_parent)
      
      DT::datatable(data, 
                    options  = list(dom = 't'), 
                    escape   = FALSE, 
                    rownames = FALSE, 
                    caption  = htmltools::tags$caption("",
                                                      style = "font-family: Arial; font-size: 12px;"))
    }
    
    ## waterfall----
    employment_emp_ind_waterfall_data <- function(df, inputdate, inputgeo) {
     df2 <- df |>
        group_by(geo, naics) |>
        arrange(date) |>
        mutate(
          change = value - lag(value), 
          percentagevalue = ifelse(lag(value) == 0 | is.na(lag(value)), NA, (change / lag(value)) * 100)
        ) |>
        ungroup() |>
        select(
          date,
          geo,
          parent_sector,
          naics,
          value,
          change,
          percentagevalue
        )

     df3 <- df2 |> 
        filter(
          date == inputdate,
          geo == inputgeo,
          parent_sector %in% c("services", "goods")
        ) |>
        arrange(desc(change)) |>
        mutate(
          naics = factor(naics, levels = unique(naics[order(-change)]))
        )

     return(df3)

    }
    
    
    employment_emp_ind_render_waterfall <- function(df, input){
      df3 <- employment_emp_ind_waterfall_data(df, input$employment_emp_ind_waterfall_date, input$employment_emp_ind_waterfall_geo)
      # df3 <- employment_emp_ind_waterfall_data(df, max(df$date), "Alberta")
      
      p   <- plot_ly(
        df3,
        type = "waterfall",
        x = ~naics,
        y = ~change,
        # text = text_labels,
        # textposition = "outside",
        connector = list(line = list(color = "rgb(63, 63, 63)"))
      )
      return(p)
      }

    
    
    
