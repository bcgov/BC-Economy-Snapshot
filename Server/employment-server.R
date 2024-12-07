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

# employment ----

# Homepage----

server_employment_home <- function(emp_data,
                                   output, input, session) {
  
  plot_and_triangle(data               = emp_data, 
                    plot_data_func     = employment_emp_ind_lineplot_data, 
                    plot_output_id     = "employment_homepage_worm_emp_ind", 
                    button_input_id    = "employment_emp_ind_homepage_button",
                    tab_name           = "emp_ind", 
                    triangle_output_id = "employment_homepage_triangle_emp_ind", 
                    output, 
                    input, 
                    session)
                  }


## emp_ind----
employment_emp_ind_server <- function(emp_data, 
                                      output, 
                                      input){
  
  output$Exesum_employment_emp_ind_main <- renderUI(Exesum_employment_emp_ind_main)

  
  ### Executive Summary----
  output$Exesum_employmet_emp_ind <- renderUI({
    HTML("<h4>Summary of Employment by Industry</h4>")
    })
  
  
  ### Job Gains Table---- 
  output$job_gains_table <- DT::renderDataTable({
    
    # Filter data for job gains using the dummy variable
    latest_month <- max(emp_data$date) 
    
    # Extract the latest month and comparison period for dynamic naming
    latest_month_value <- format(as.Date(latest_month), "%b %y") # e.g., "Oct 24"
    latest_month_comp  <- format(as.Date(latest_month) %m-% months(12), "%b %y") # e.g., "Oct 23"
    
    # Create a title dynamically
    output$job_gains_header <- renderText({
      paste("Job Gains: Comparing", latest_month_value, "to", latest_month_comp)
    })
    
    # Filter emp_data to only include rows from the latest month
    filtered_gains_df <- emp_data |> 
      filter(date           == latest_month, 
             geo            == "British Columbia", 
             job_gain_dummy == 1,
             parent_sector %in% c("goods", "services")) |> 
      arrange(parent_sector, desc(abs(emp_gr_num_12m))) |> # Sort by parent_sector and then by largest job gains
      ungroup()
    
    # Calculate share of total job gains and growth rate
    filtered_gains_df <- filtered_gains_df |>
      mutate(
        share_of_total = round((abs(emp_gr_num_12m) / sum(abs(emp_gr_num_12m))) * 100, 0),
        growth_rate    = round(emp_gr_per_12m, 1)
      )
    
    # Add the summary row at the end 
    summary_row <- filtered_gains_df |> 
      summarise(
        parent_sector  = "Total Job Gains",
        naics          = "",
        value          = sum(as.numeric(value)         , na.rm = TRUE),
        emp_gr_num_12m = sum(as.numeric(emp_gr_num_12m), na.rm = TRUE), 
        share_of_total = round(sum(share_of_total      , na.rm = TRUE), 1), # Keep as numeric, do not format as character here
        growth_rate    = round(mean(growth_rate        , na.rm = TRUE), 1)  # Keep as numeric, do not format as character here
      )
    
    # Combine the filtered gains data with the summary row
    formatted_gains_df <- bind_rows(filtered_gains_df, summary_row)
    
    # Format columns for better readability (including the summary row)
    formatted_gains_df <- formatted_gains_df |> 
      mutate(
        value          = format(as.numeric(value)                , big.mark = ",", scientific = FALSE),
        emp_gr_num_12m = format(as.numeric(emp_gr_num_12m)       , big.mark = ",", scientific = FALSE),
        share_of_total = paste0(format(as.numeric(share_of_total), big.mark = ",", scientific = FALSE, trim = TRUE), "%"),
        growth_rate    = paste0(format(as.numeric(growth_rate)   , big.mark = ",", scientific = FALSE), "%")
      )
    
    # Rename and reorder columns 
    formatted_gains_df <- formatted_gains_df |> 
      rename(
        "Category"                                                                     = parent_sector,
        "Sector"                                                                       = naics,
        !!paste0("Employment (", latest_month_value, ")")                              := value,
        !!paste0("Job Gains (", latest_month_value, " vs. ", latest_month_comp, ")")   := emp_gr_num_12m,
        "Share of Total"                                                               = share_of_total,
        !!paste0("Growth Rate (", latest_month_value, " vs. ", latest_month_comp, ")") := growth_rate
      ) |>
      select(Category, 
             Sector, 
             !!paste0("Employment (", latest_month_value, ")"),
             !!paste0("Job Gains (", latest_month_value, " vs. ", latest_month_comp, ")"), 
             `Share of Total`,
             !!paste0("Growth Rate (", latest_month_value, " vs. ", latest_month_comp, ")"))
    
    # Render the job gains table and order the columns 
    DT::datatable(formatted_gains_df,
                  options  = list(dom = 't'),
                  escape   = FALSE, 
                  rownames = FALSE)
  })
  
 ### Job Losses Table----
output$job_losses_table <- DT::renderDataTable({
  
  # Filter data for job losses using the dummy variable
  latest_month <- max(emp_data$date) 
  
  # Extract the latest month and comparison period for dynamic naming
  latest_month_value <- format(as.Date(latest_month), "%b %y") # e.g., "Oct 24"
  latest_month_comp  <- format(as.Date(latest_month) %m-% months(12), "%b %y") # e.g., "Oct 23"
  
  # Create overall title dynamically (updates automatically to most recent month)
  output$job_losses_header <- renderText({
    paste("Job Losses: Comparing", latest_month_value, "to", latest_month_comp)
  })
  
  
  # Filter emp_data to only include rows from the latest month
  filtered_losses_df <- emp_data |> 
    filter(date           == latest_month, 
           geo            == "British Columbia", 
           job_loss_dummy == 1,
           parent_sector %in% c("goods", "services")) |> 
    arrange(parent_sector, desc(abs(emp_gr_num_12m))) |> # Sort by parent_sector and then by largest losses 
    ungroup()


  # Calculate share of total job losses
  filtered_losses_df <- filtered_losses_df |> 
    mutate(
      share_of_total = round((abs(emp_gr_num_12m) / sum(abs(emp_gr_num_12m))) * 100, 1),
      growth_rate    = round(emp_gr_per_12m, 1)
    )

  # Add the summary row at the end
  summary_row <- filtered_losses_df |> 
    summarise(
      parent_sector  = "Total Job Losses",
      naics          = "",
      value          = sum(as.numeric(value)         , na.rm = TRUE),
      emp_gr_num_12m = sum(as.numeric(emp_gr_num_12m), na.rm = TRUE), 
      share_of_total = round(sum(share_of_total      , na.rm = TRUE), 1), # Keep as numeric, do not format as character here
      growth_rate    = round(mean(growth_rate        , na.rm = TRUE), 1)  # Keep as numeric, do not format as character here
    )
  
  # Combine the filtered losses data with the summary row
  formatted_losses_df <- bind_rows(filtered_losses_df, summary_row)
  
  # Format columns for better readability
  formatted_losses_df <- formatted_losses_df |> 
    mutate(
      value          = format(as.numeric(value)           , big.mark = ",", scientific = FALSE),
      emp_gr_num_12m = format(as.numeric(emp_gr_num_12m)  , big.mark = ",", scientific = FALSE),
      share_of_total = paste0(format(round(share_of_total), big.mark = ",", scientific = FALSE), "%"),
      growth_rate    = paste0(format(round(growth_rate, 1), big.mark = ",", scientific = FALSE), "%")
    )
  
  
  
  # Rename and reorder columns 
  formatted_losses_df <- formatted_losses_df |> 
    rename(
      "Category"                                                                     =  parent_sector,
      "Sector"                                                                       =  naics,
      !!paste0("Employment (", latest_month_value, ")")                              := value,
      !!paste0("Job Losses (", latest_month_value, " vs. ", latest_month_comp, ")")  := emp_gr_num_12m,
      "Share of Total"                                                               =  share_of_total,
      !!paste0("Growth Rate (", latest_month_value, " vs. ", latest_month_comp, ")") := growth_rate,
    ) |>
    select(Category, 
           Sector, 
           !!paste0("Employment (", latest_month_value, ")"),
           !!paste0("Job Losses (", latest_month_value, " vs. ", latest_month_comp, ")"), 
           `Share of Total`,
           !!paste0("Growth Rate (", latest_month_value, " vs. ", latest_month_comp, ")"))
           
  # Render the job losses table and order the columns 
  DT::datatable(formatted_losses_df,
                options  = list(dom = 't', scrolly = "500px"),
                escape   = FALSE, 
                rownames = FALSE)
  })


  ### Line Plot----
  output$employment_emp_ind_lineplot <- renderPlotly({
    p1 <- employment_emp_ind_render_lineplot(emp_data, input)
    p1
  })
  
  output$employment_emp_ind_lineplot_dwnbtt <- downloadHandler(
    filename = "Employment_Industry.csv",
    content = function(file) {
      df <- employment_emp_ind_lineplot_data(emp_data)
      write.csv(df, file)
    })
  
  output$employment_emp_ind_map <- renderLeaflet({
    p1 <- employment_emp_ind_render_map(emp_data,input)
    
    p1
  })

  
  ### waterfall----
                                                         
  output$Exesum_employment_emp_ind_waterfall <- renderUI(Exesum_employment_emp_ind_waterfall)
  
  output$employment_emp_ind_waterfall <- renderPlotly({
    p1 <- employment_emp_ind_render_waterfall(emp_data, input)
    p1
    })
  
  output$employment_emp_ind_waterfall_dwnbtt <- downloadHandler(
    filename = "Employment_waterfall.csv",
    content = function(file){
      df <- employment_emp_ind_waterfall_data(emp_data, input$employment_emp_ind_waterfall_data, input$employment_emp_ind_waterfall_geo)
      write.csv(df, file)
    }
  ) 
  
  }
    


