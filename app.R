
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

## load libraries ----
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(here)

library(plotly)
library(leaflet)
library(sf)
library(gridExtra)
library(waffle)
library(ggplot2)
library(htmlwidgets)
library(ggrepel)


options(scipen = 999999999)  


source(here("Charts/employment-charts.R"))
source(here("Charts/standard-charts.R"))
source(here("UI/home_ui.R"))
source(here("UI/employment-ui.R"))
source(here("Server/employment-server.R"))
source(here("data-processing/texts/Executive_summaries.R"))


# Loading data ----
loaded_data   <- load_emp_ind()

# Extract individual datasets from loaded dataset
  emp_ind_df <- loaded_data$emp_data
  job_gains  <- loaded_data$job_gains
  job_losses <- loaded_data$job_losses 

# UI ----
ui <- function() {
  # Include CSS----
  header_content <- tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
    .navbar.navbar-default.navbar-static-top {
      position: fixed;
      top: 60px; /* Adjust the position to be 100px lower */
      width: 100%;
      z-index: 999; /* Ensure it stays on top */
    }
    
    body {
      padding-top: 160px; /* Adjust the body padding to avoid content being hidden under the navbar */
    }
  "))
  )
  
  # Custom header----
  custom_header <- tags$header(
    class = "header",
    style = "background-color:#003366; border-bottom:2px solid #fcba19;
             padding:0 0px 0 0px; display:flex; height:60px; width:100%; 
             justify-content:space-between; align-items:center;
             position:fixed; top:0; left:0; z-index:1000;",
    tags$div(
      class = "banner",
      style = "display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px;",
      a(
        href = "https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
        img(
          src = 'https://raw.githubusercontent.com/bcgov/BC-Economic-Development/main/bc_logo.svg',
          title = "StrongerBC",
          height = "30px",
          alt = "British Columbia - StrongerBC"
        )
      ),
      h1(
        "BC Economy Snapshot",
        style = "font-weight:400; color:white; margin: 5px 5px 0 18px;"
      ),
      h2(
        "Work in progress, subject to change!",
        style = "font-size: 16px; color: white; margin: 0 5px 5px 18px;"
      )
    ),
    tags$div(
      style = "margin-right:10px;",
      a(
        href = "https://mehdinaji.shinyapps.io/BC-Economy-Snapshot/",
        class = "btn btn-primary",
        style = "color:white; background-color:#fcba19; border:none;",
        "BC Economy Snapshot"
      )
    )
  )
  
  # Custom footer----
  custom_footer <- column(
    width = 12,
    style = "background-color:#003366; border-top:2px solid #fcba19;",
    tags$footer(
      class = "footer",
      tags$div(
        class = "container",
        style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
        tags$ul(
          style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
          tags$li(
            a(
              href = "https://www2.gov.bc.ca/gov/content/home",
              "Home",
              style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"
            )
          ),
          tags$li(
            a(
              href = "https://www2.gov.bc.ca/gov/content/home/disclaimer",
              "Disclaimer",
              style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"
            )
          )
        )
      )
    )
  )
  
  # Build the navbarPage----
  ui <- tagList(
    tags$style(HTML("
    .navbar-default {
      background-color: #3C8DBC; /* Background color */
      border-color:     #3C8DBC; /* Border color */
    }
    .navbar-default .navbar-nav > li > a {
      color: #fff;               /* Link color */
    }
    .navbar-default .navbar-brand {
      color: #fff;               /* Brand text color */
    }
    .navbar-default .navbar-nav > li > a:hover {
      background-color: #fcba19; /* Hover color */
    }
  ")),
    header_content,
    custom_header,
    #----
    navbarPage(
      title = NULL,
      id = "tabs",
      # Home tab
      tabPanel("Home", ui_home()),
      # Employment
        navbarMenu(
          "Employment",
          tabPanel(
            "Employment Overview",
            ui_employment_home(
              employment_emp_ind_lineplot_data(emp_ind_df)
            )
          ),
            tabPanel("\xE2\x96\xB6 Employment Industry", ui_employment_emp_ind(emp_ind_df)),
            # tabPanel("\xE2\x96\xB6 Unemployment Rate", ui_m6_VAEX(df_m6_VAEX_1)),
            # tabPanel("\xE2\x96\xB6 Non-residential Investment", ui_m6_nRinv(df_m6_nRinv_1)),
            # tabPanel("\xE2\x96\xB6 Labour Productivity", ui_m6_LP(df_m6_LP_1)),
            # tabPanel("\xE2\x96\xB6 Export", ui_m6_EXP(df_m6_EXP_1, df_m6_EXP_3))
        )
      ),
    custom_footer
  )
  
  return(ui)
}



# Server----
server <- function(input, output, session) {
  server_employment_home(loaded_data$emp_data, output, input, session)
  employment_emp_ind_server( Exesum_employment_emp_ind_main, 
                             Exesum_employment_emp_ind, 
                             loaded_data, 
                             output, 
                             input)
  }



shinyApp(ui, server)
