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

### Home ----
ui_employment_home <- function(df1) {
  tabItem(tabName = "employment_home",
          fluidPage(
            # CSS Code ----
            tags$head(
              tags$style(HTML("
      .main-title {
        font-weight: bold;
        color: black;
        font-size: 40px;
        text-align: center;
        padding:0px;
        margin-top: -30px;
        margin-bottom: 0px;
      }
      .custom-box {
        background-color: #f2f2f2;
        border-left: 16px solid #FEB70D;
        padding: 10px;
        margin-bottom: 20px; /* Space between rows of boxes */
        height: 220px;       /* Increased height to accommodate labels */
        position: relative;  /* Positioning context for absolute positioning */
      }
      .custom-title {
        font-weight: bold;
        text-align: center;
        margin-top:0px;
        margin-bottom: 30px; /* Space below the title */
        cursor: pointer;     /* Change cursor to pointer on hover */
        color: black; 
        text-decoration: none; /* Remove default underline */
        line-height: 1.5;
        word-wrap: break-word; /* Ensure title is responsive and wraps */
      }
      .custom-title:hover {
        text-decoration: underline; /* Underline on hover */
      }
      .indicator-content {
        display: flex;
        flex-direction: row;         /* Align items in a row */
        align-items: center;         /* Align items in the center vertically */
        justify-content: flex-start; /* Align items to the left */
        height: 100%;                /* Occupy full height */
        position: relative;          /* Positioning context for absolute positioning */
        bottom:50px;
        left:25px;
      }
      .trend-triangle {
        width: 15px;                          /* Smaller width for the triangle */
        height: 15px;                         /* Smaller height for the triangle */
        border-left: 10px solid transparent;
        border-right: 10px solid transparent;
        margin-left: 5px;                     /* Space to the left of the triangle */
        position: absolute;                   /* Positioning context within .indicator-content */
        top: 35%;                             /* Adjust vertical position to center */
        transform: translateY(-50%);          /* Center vertically */
        right: 35px;                          /* Adjust right position */
      }
      .green-triangle {
        border-bottom: 15px solid #4EA72E;    /* Green color for upward trend */
      }
      .yellow-triangle {
        right:30px;
        border-left: 15px solid #FEB70D;      /* Yellow color for neutral trend */
        border-top: 10px solid transparent;
        border-bottom: 10px solid transparent;
      }
      .red-triangle {
        border-top: 15px solid #E97132;       /* Red color for downward trend */
      }
      .year-label {
        font-weight: bold;
        font-size: 18px;             /* Larger font size for the year */
        position: absolute;          /* Positioning context within .indicator-content */
        top: 35%;                    /* Adjust vertical position to center */
        transform: translateY(-50%); /* Center vertically */
        right: 60px;                 /* Adjust right position */
      }
      .plot-container {
        width: 90%;         /* Adjust width of plot container */
        position: relative; /* Ensure position is relative for child elements */
      }
      .action-button {
        border: none;     /* Remove border */
        background: none; /* No background color */
        width: 100%;      /* Full width */
        padding: 0;       /* Remove padding */
      }
      .action-button:hover {
        background: none; /* No background color on hover */
      }
    "))
            ),
            #----
            # div(
            #   actionButton("employment_to_homepage", label = "Employment and Job Market", class = "main-title")
            # ),
            div(style = "height: 10px;"),  # Space between title and first row of boxes
            fluidRow(
              wormchart_ui(df = df1,
                           button   = "employment_emp_ind_homepage_button",
                           title    = "Industry Employment",
                           worm     = "employment_homepage_worm_emp_ind",
                           triangle = "employment_homepage_triangle_emp_ind")
            ),
            div(style = "height: 20px;"),  # Space between first and second rows of boxes
            fluidRow(
              # Add content for the second row here
            )
          )
  )
}


### emp_ind ----

ui_job_gains_table <- function(chart, df1) {
  column(9,
         DT::dataTableOutput(chart, height = "calc(100vh - 460px)"),
         # Source
         fluidRow(
           style = "background-color: #f2f2f2; 
                    padding-left:     80px; 
                    padding-right:    40px; 
                    margin-right:     0px; 
                    margin-left:      0px; 
                    margin-bottom:    0px; 
                    height:           12px; 
                    font-size:        12px;",
           "Source: Employment Data - Job Gains"
         ),
         # Inputs (if needed)
         fluidRow(
           style = "background-color: #f2f2f2; 
                    margin-right:     0px; 
                    margin-left:      0px; 
                    margin-top:       0px; 
                    margin-left:      0px;",
           column(4),
           column(4),
           column(2),
           column(2, style = "background-color: #f2f2f2; 
                              height:           20px; 
                              padding-top:      40px; 
                              display:          flex; 
                              justify-content:  center; 
                              align-items:      center;",
                  
              downloadButton("job_gains_table_dwnbtt", 
                              label = NULL, 
                              class = "btn-custom-black", 
                              icon = icon("cloud-download-alt")))
         )
  )
}
ui_job_losses_table <- function(chart, df1) {
  column(9,
         DT::dataTableOutput(chart, height = "calc(100vh - 460px)"),
         # Source
         fluidRow(
           style = "background-color: #f2f2f2; 
                    padding-left: 80px; 
                    padding-right: 40px; 
                    margin-right: 0px; 
                    margin-left: 0px; 
                    margin-bottom: 0px; 
                    height: 12px; 
                    font-size: 12px;",
           "Source: Employment Data - Job Losses"
         ),
         # Inputs (if needed)
         fluidRow(
           style = "background-color: #f2f2f2; 
                    margin-right: 0px; 
                    margin-left:  0px; 
                    margin-top:   0px; 
                    margin-left:  0px;",
           column(4),
           column(4),
           column(2),
           column(2, style = "background-color: #f2f2f2; 
                              height:              20px; 
                              padding-top:         40px; 
                              display:             flex; 
                              justify-content:   center; 
                              align-items:       center;",
                  downloadButton("job_losses_table_dwnbtt", 
                                  label = NULL, 
                                  class = "btn-custom-black", 
                                  icon = icon("cloud-download-alt")))
         )
  )
}
ui_employment_emp_ind_feature_table <- function(chart, df1){
  column(9,
         DT::dataTableOutput(chart ,height = "calc(100vh - 460px)" ),
         # Source
         fluidRow(style = "background-color: #f2f2f2; 
                           padding-left:     80px; 
                           padding-right:    40px; 
                           margin-right:     0px;
                           margin-left:      0px; 
                           margin-buttom:    0px; 
                           height:           12px; 
                           font-size:        12px;", 
                           "Source: Statistics Canada, Table 36-10-0480-01"),
         # inputs
         fluidRow(style = "background-color: #f2f2f2;
                           margin-right:     0px; 
                           margin-left:      0px;
                           margin-top:       0px; 
                           margin-left:      0px;",
                  
                  column(4, div(class = "upward-dropdown", 
                                selectInput("employment_emp_ind_table_geo", "", 
                                            choices = unique(df1$geo), 
                                            selected = "British Columbia" ))),
                  
                  column(4, div(class = "upward-dropdown", 
                                selectInput("employment_emp_ind_table_parent", "", 
                                            choices = unique(df1$parent_secor), 
                                            selected = "business sector industries"))),
                  ),
         fluidRow(style = "background-color: #f2f2f2;
                           margin-right:     0px; 
                           margin-left:      0px;
                           margin-top:       0px; 
                           margin-left:      0px;",
                  column(4),
                  column(4), 
                  column(2),
                  column(2, style = "background-color: #f2f2f2; 
                                     height:              20px; 
                                     padding-top:         40px; 
                                     display:             flex; 
                                     justify-content:   center; 
                                     align-items:       center;", 
                         downloadButton("employment_emp_ind_table_dwnbtt", 
                                        label = NULL, 
                                        class = "btn-custom-black", 
                                        icon = icon("cloud-download-alt")))),
  )
}
ui_employment_emp_ind_feature_waterfall <- function(chart, df1) {
  column(9,
         plotlyOutput(chart ,height = "calc(100vh - 460px)" ),
         # Source
         fluidRow(style = "background-color: #f2f2f2; 
                           padding-left:     80px; 
                           padding-right:    40px; 
                           margin-right:     0px;
                           margin-left:      0px; 
                           margin-buttom:    0px; 
                           height:           12px; 
                           font-size:        12px;", 
                  "Source: Statistics Canada, Table 36-10-0480-01"),
         # inputs
         fluidRow(style = "background-color: #f2f2f2;
                           margin-right:     0px; 
                           margin-left:      0px;
                           margin-top:       0px; 
                           margin-left:      0px;",
                  
                  column(4, div(class = "upward-dropdown", 
                                selectInput("employment_emp_ind_waterfall_date", "", 
                                            choices = unique(df1$date), 
                                            selected = max(df1$date) ))),
                  
                  column(4, div(class = "upward-dropdown", 
                                selectInput("employment_emp_ind_waterfall_geo", "", 
                                            choices = unique(df1$geo), 
                                            selected = "British Columbia" ))),
                  
                  column(2),
                  column(2, style = "background-color: #f2f2f2; 
                                     height:              20px; 
                                     padding-top:         40px; 
                                     display:             flex; 
                                     justify-content:   center; 
                                     align-items:       center;", 
                         downloadButton("employment_emp_ind_waterfall_dwnbtt", 
                                        label = NULL, 
                                        class = "btn-custom-black", 
                                        icon = icon("cloud-download-alt")))
         )
  )
}
ui_employment_emp_ind <- function(df1) {
  tabItem(tabName = "emp_ind",
          
          ui_main_chart(title       = "Industry Employment",
                        chart_name  = "employment_emp_ind_lineplot",
                        button_name = "employment_emp_ind_lineplot_dwnbtt",
                        source      = "Statistics Canada, Table 36-10-0480-01",
                        summary     = "Exesum_employment_emp_ind_main"),
          fluidRow(
            h3("Employment Industry Deep-dive", style = "text-align: center;"),
            tabsetPanel(    #Donough
              
              tabPanel("table",
                       feature_tab(df1,
                                   tab_name          = "table",
                                   title             = "title",
                                   tab_feature_chart = ui_employment_emp_ind_feature_table,
                                   chart             = "employment_emp_ind_table")
                       #) DONOUGH: this one was extra, shoule be dropped
              ),
              
              # Job Gains Tab 
              tabPanel("Job Gains",
                       
                       feature_tab(df1,
                                   tab_name          = "job_gains",
                                   title             = "Job Gains Summary",
                                   tab_feature_chart = ui_job_gains_table,
                                   chart             = "job_gains_table")
              ),
              
              # Job Losses Tab
              tabPanel("Job Losses",
                       
                       feature_tab(df1,
                                   tab_name          = "job_losses",
                                   title             = "Job Losses Summary",
                                   tab_feature_chart = ui_job_losses_table,
                                   chart             = "job_losses_table")
              ),
              # waterfall
              tabPanel("waterfall",
                       feature_tab(df1,
                                   tab_name          = "waterfall",
                                   title             = "Waterfall",
                                   tab_feature_chart = ui_employment_emp_ind_feature_waterfall,
                                   chart             = "employment_emp_ind_waterfall",
                                   summary           = "Exesum_employment_emp_ind_waterfall")
              ),
              
              # Scroll buttons for navigation
              tags$script(HTML("
            $(document).on('click', '#go_to_main_chart', function() {
              $('html, body').animate({scrollTop: $('.scroll-section:eq(0)').offset().top}, 800);
            });

            $(document).on('click', '#go_to_deep_dive', function() {
              $('html, body').animate({scrollTop: $(document).height()}, 800);
            });
          "))
            )))
}
