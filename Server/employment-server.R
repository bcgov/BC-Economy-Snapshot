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
  
  plot_and_triangle(emp_ind_df, employment_emp_ind_lineplot_data, "employment_homepage_worm_emp_ind", "employment_emp_ind_homepage_button","emp_ind", "employment_homepage_triangle_emp_ind", output, input, session)
                
  
}

## emp_ind----
employment_emp_ind_server <- function(Exesum_employmnet_emp_ind_main, Exesum_employmnet_emp_ind, emp_ind_df, output, input){
  output$Exesum_employment_emp_ind_main <- renderUI(Exesum_employment_emp_ind_main)

  ### Executive Summary----
  output$Exesum_employmet_emp_ind <- renderUI(Exesum_employmnet_emp_ind)
  ### Line Plot----
  output$employmnet_emp_ind_lineplot <- renderPlotly({
    p1 <- employmnet_emp_ind_render_lineplot(emp_ind_df, input)
    p1
  })
  
  output$employmnet_emp_ind_lineplot_dwnbtt <- downloadHandler(
    filename = "Employment_Industry.csv",
    content = function(file) {
      df <- employment_emp_ind_lineplot_data(emp_ind_df)
      write.csv(df, file)
    })
  
  output$employmnet_emp_ind_map <- renderLeaflet({
    p1 <- employmnet_emp_ind_render_map(emp_ind_df,input)
    
    p1
  })
}
    
