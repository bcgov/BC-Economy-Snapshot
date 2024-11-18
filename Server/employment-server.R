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

server_employment_home <- function(emp_ind_df,
                                   output, input, session) {
  
  plot_and_triangle(data               = emp_ind_df, 
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
employment_emp_ind_server <- function(Exesum_employment_emp_ind_main, 
                                      Exesum_employment_emp_ind, 
                                      loaded_data, 
                                      output, input){
  
  output$Exesum_employment_emp_ind_main <- renderUI(Exesum_employment_emp_ind_main)

  
  ### Executive Summary----
  output$Exesum_employmet_emp_ind <- renderUI(Exesum_employment_emp_ind)
  
  ### Job Gains Table---- 
  output$job_gains_table <- DT::renderDataTable({
    job_gains_df <- loaded_data$job_gains
    DT::datatable(
                  job_gains_df, 
                  options  = list(dom = 't'),
                  escape   = FALSE,
                  rownames = FALSE)
  })
  
 ### Job Losses Table----
  output$job_losses_table <- DT::renderDataTable({
    job_losses_df <- loaded_data$job_losses
    DT::datatable(job_losses_df, 
                  options  = list(dom = 't'),
                  escape   = FALSE,
                  rownames = FALSE)
  })

  
  ### Line Plot----
  output$employment_emp_ind_lineplot <- renderPlotly({
    p1 <- employment_emp_ind_render_lineplot(loaded_data$emp_data, input)
    p1
  })
  
  output$employment_emp_ind_lineplot_dwnbtt <- downloadHandler(
    filename = "Employment_Industry.csv",
    content = function(file) {
      df <- employment_emp_ind_lineplot_data(loaded_data$emp_data)
      write.csv(df, file)
    })
  
  output$employment_emp_ind_map <- renderLeaflet({
    p1 <- employment_emp_ind_render_map(loaded_data$emp_data,input)
    
    p1
  })
}
    
