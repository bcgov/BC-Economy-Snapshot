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


ui_home <- function(){
  ## Body ----
  ##CAEDF8
  ##D9F2D0
  style1 <- "background-color:white; height: 350px; padding: 4px; border-radius: 10px; border: 5px solid #ecf0f5;font-size: 12px;"
  style2 <- "background-color:#337AB7; color:white;"
  style3 <- "background-color:white; height: 350px; padding: 4px; border-radius: 10px; border: 5px solid #ecf0f5;font-size: 12px;"
  
tabItem(tabName = "home",
        fluidRow(
          column(width = 4,
                 style = style1, 
                 h4(style = "text-align: center;",
                    strong("Employment:"), br(), "Explore Employment Metrics and Job Market Trends", br()),
                 div(
                   style = "display: flex; 
                            justify-content: center; 
                            align-items: center;",
                   actionButton("employment_button", 
                                label = "", 
                   style = "width: 250px; 
                            height: 250px; 
                            background-image: url('Images/emp.png'); 
                            background-size: cover; 
                            border: none;"
                    )
                  )
            ),
            column(width = 4, 
                 style = style1,
                 h4(style = "text-align: center;",
                    strong("Placeholder"), br(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", br()),
                 div(
                   style = "display: flex; 
                            justify-content: center; 
                            align-items: center;",
                   actionButton("button2", 
                                label = "", 
                                style = "width: 250px; 
                            height: 250px; 
                            background-image: ; 
                            background-size: cover; 
                            border: none;"
                   )
                  )
            ),
            column(width = 4,
                 style = style1,
                 h4(style = "text-align: center;",
                    strong("Placeholder"), br(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
                 div(
                   style = "display: flex; 
                            justify-content: center; 
                            align-items: center;",
                   actionButton("button3", 
                                label = "", 
                                style = "width: 250px; 
                            height: 250px; 
                            background-image: ; 
                            background-size: cover; 
                            border: none;"
                   )
                )
              )),
        fluidRow(
          column(width =4,
                 style=style3,
                 h4(style = "text-align: center;",strong("Placeholder"), br(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
                 div(
                   style = "display: flex; 
                            justify-content: center; 
                            align-items: center;",
                   actionButton("button4", 
                                label = "", 
                                style = "width: 250px; 
                            height: 250px; 
                            background-image: ; 
                            background-size: cover; 
                            border: none;"            
                  )
                  )
           ),
           column (width=4,
                  style=style3,
                  h4(style = "text-align: center;",strong("Placeholder"), br(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
                  div(
                    style = "display: flex; 
                            justify-content: center; 
                            align-items: center;",
                    actionButton("button5", 
                                 label = "", 
                                 style = "width: 250px; 
                            height: 250px; 
                            background-image: ; 
                            background-size: cover; 
                            border: none;"                 
                    )
                  )
            ),
            column (width=4,
                  style=style3,
                  h4(style = "text-align: center;",
                     strong("Placeholder"), br(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
                  div(
                    style = "display: flex; 
                            justify-content: center; 
                            align-items: center;",
                    actionButton("button6", 
                                 label = "", 
                                 style = "width: 250px; 
                            height: 250px; 
                            background-image: ; 
                            background-size: cover; 
                            border: none;"
                    )
                  )
              )
        ))}
