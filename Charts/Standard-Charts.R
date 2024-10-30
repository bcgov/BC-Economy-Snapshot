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


# Main Chart UI----
ui_main_chart <- function(title, chart_name, button_name, source, summary){
  fluidPage(
    style = "height: 100vh; display: flex; flex-direction: column; justify-content: space-between; padding-bottom: 120px;",
    
    fluidRow(
      style = "background-color: white; color:black; margin-top: -10px; margin-buttom:0px; height: 100vh;",
      column(9,
             fluidRow(
               style = "background-color: #003366; color:white; display: flex; flex-direction: column; ",

                          fluidRow(
                            h1(title, style = "margin-left: 45px; font-family: 'Century Gothic'; font-size: 40px; font-weight: bold; flex-shrink: 0;")
                          ),

                          fluidRow(
                            style = "flex-grow: 1; display: flex; justify-content: center; align-items: center;height: calc(100vh - 280px);margin-left: 45px; margin-right: 45px;",
                            plotlyOutput(chart_name, height = "100%")
                          ),

                          fluidRow(
                            style = "background-color: #003366; height: 20px; display: flex; align-items: center;margin-left: 45px; margin-right: 45px;",

                            column(7,
                                   paste0("Source:", source),
                                   style = "margin-left: 45px;"
                            ),

                            tags$style(HTML(".btn-custom {
                                       background-color: transparent;
                                       border: none;
                                       color: white;
                                     }
                                     .btn-custom .fa-cloud-download-alt {
                                       color: white;
                                     }
                                   ")),
                            column(3,
                                   actionButton("go_to_deep_dive", 
                                                label = tagList(icon("angle-double-down"), ""),                                                
                                                class = "scroll-button", 
                                                style = "background-color: #003366; height: 18px; text-align: left; color: white; margin-top: 0px; margin-bottom: 0px; padding : 0px;"),
                            ),

                            column(2,
                                   downloadButton(button_name, label = NULL, class = "btn-custom", icon = icon("cloud-download-alt"))
                            )
                          )
             )
                   ),

                   column(3,
                          style = "height: 100%; display: flex; align-items: center;",
                          div(
                            style = "width: 100%; display: flex; justify-content: center;",
                            div(
                              style = "background-color: #f0f0f0; border-radius: 15px; padding: 20px; width: 100%;",
                              h3("Summary"),
                              uiOutput(summary)
                            )
                          )
      
      )
    )
      
      
    
  )
}


# boxes in mission pages ----
                                                                                     # imp_ind", "employment_homepage_triangle_emp_ind", output, input, session)
plot_and_triangle <- function(data, plot_data_func, plot_output_id, button_input_id, tab_name, triangle_output_id, output, input, session) {
  output[[plot_output_id]] <- renderPlot({
    wormchart(plot_data_func(data))
  })
  
  observeEvent(input[[button_input_id]], {
    updateTabItems(session, "tabs", selected = tab_name)
  })
  
  output[[triangle_output_id]] <- renderUI({
    dff <- plot_data_func(data)
    dff <- dff[order(dff$date), ]
    last_value <- dff$value[nrow(dff)]
    second_last_value <- dff$value[nrow(dff) - 1]
    Sign <- sign(last_value - second_last_value)
    div(class = get_triangle_class(Sign))
  })
}

# Line Plot ----

dash_lineplot <- function(data_func, df, input, y_label = "") {
  df1 <- data_func(df)
  df1$date <- as.Date(df1$date)
  
  tickvals <- seq(from = max(df1$date) - 60, to = max(df1$date), by = 5)
  
  p1 <- plot_ly(data = df1, x = ~date, y = ~value, type = 'scatter', mode = 'lines',
                line = list(color = "#FEB70D", width = 4)) %>%
    layout(
      xaxis = list(
        title = "",
        tickvals = tickvals,
        ticktext = tickvals,
        rangeslider = list(
          visible = TRUE,
          thickness = 0.02,
          bgcolor = "white"
        ),
        tickfont = list(color = "white"),
        showgrid = FALSE,
        gridcolor = "white"
      ),
      yaxis = list(
        title = "$ per hour",
        titlefont = list(color = "white"),
        tickfont = list(color = "white"),
        gridcolor = "white"
      ),
      annotations = list(
        list(
          x = df1$date[1],
          y = df1$value[1],
          text = paste(format(df1$value[1], big.mark = ",")),
          showarrow = TRUE,
          arrowcolor = "white",
          font = list(color = "white")
        ),
        list(
          x = df1$date[length(df1$date)],
          y = df1$value[length(df1$value)],
          text = paste(
            format(df1$value[length(df1$value)], big.mark = ","),
            "\n",
            "<span style='font-size:7px;'>(", df1$date[length(df1$value)], ")</span>"
          ),
          showarrow = TRUE,
          arrowcolor = "white",
          font = list(color = "white")
        )
      ),
      plot_bgcolor = '#003366',
      paper_bgcolor = '#003366'
    )
  p1
  
  validate(need(nrow(df1) > 0, "The data for this set of inputs is inadequate. To obtain a proper visualization, please adjust the inputs in the sidebar."))
  
  return(p1)
}


# Mission Homepage UI----
# # mission-homepage-ui("m6_home", "Fostering Innovation Across the Economy")
# mission-homepage-ui <- function(tabname, maintitle,  ){
#   tabItem(tabName = tabname,
#     fluidPage(
#     ## design----  
#     tags$head(
#       tags$style(HTML("
#         .main-title {
#           font-weight: bold;
#           color: black;
#           font-size: 40px;
#           text-align: center;
#           margin-top: 20px;
#           margin-bottom: 20px;
#         }
#         .custom-box {
#           background-color: #f2f2f2;
#           border-left: 16px solid #FEB70D;
#           padding: 10px;
#           margin-bottom: 20px; /* Space between rows of boxes */
#           height: 220px; /* Increased height to accommodate labels */
#           position: relative; /* Positioning context for absolute positioning */
#         }
#         .custom-title {
#           font-weight: bold;
#           text-align: center;
#           margin-bottom: 30px; /* Space below the title */
#           cursor: pointer; /* Change cursor to pointer on hover */
#           color: black; 
#           text-decoration: none; /* Remove default underline */
#           line-height: 1.5;
#         }
#         .custom-title:hover {
#           text-decoration: underline; /* Underline on hover */
#         }
#         .indicator-content {
#           display: flex;
#           flex-direction: row; /* Align items in a row */
#           align-items: center; /* Align items in the center vertically */
#           justify-content: flex-start; /* Align items to the left */
#           height: 100%; /* Occupy full height */
#           position: relative; /* Positioning context for absolute positioning */
#           bottom:50px;
#           left:25px;
#         }
#         .trend-triangle {
#           width: 15px; /* Smaller width for the triangle */
#           height: 15px; /* Smaller height for the triangle */
#           border-left: 10px solid transparent;
#           border-right: 10px solid transparent;
#           margin-left: 5px; /* Space to the left of the triangle */
#           position: absolute; /* Positioning context within .indicator-content */
#           top: 35%; /* Adjust vertical position to center */
#           transform: translateY(-50%); /* Center vertically */
#           right: 35px; /* Adjust right position */
#         }
#         .green-triangle {
#           border-bottom: 15px solid #4EA72E; /* Green color for upward trend */
#         }
#         .yellow-triangle {
#           right:30px;
#           border-left: 15px solid #FEB70D; /* Yellow color for neutral trend */
#           border-top: 10px solid transparent;
#           border-bottom: 10px solid transparent;
#         }
#         .red-triangle {
#           border-top: 15px solid #E97132; /* Red color for downward trend */
#         }
#         .year-label {
#           font-weight: bold;
#           font-size: 18px; /* Larger font size for the year */
#           position: absolute; /* Positioning context within .indicator-content */
#           top: 35%; /* Adjust vertical position to center */
#           transform: translateY(-50%); /* Center vertically */
#           right: 60px; /* Adjust right position */
#         }
#         .plot-container {
#           width: 90%; /* Adjust width of plot container */
#           position: relative; /* Ensure position is relative for child elements */
#         }
#       "))
#     ),
#     ## ui----
#     div(
#       class = "main-title", maintitle
#     ),
#     div(style = "height: 20px;"),  
#     fluidRow(
#       column(width = 4,
#              div(
#                class = "custom-box",
#                tags$a(href = "https://example.com/indicator1", class = "custom-title",
#                       h4("Private Sector Investment in Innovation", class = "custom-title")),
#                div(class = "indicator-content",
#                    plotOutput("line_chart1", height = "100px", width = "70%"),  # Plot output directly in the box
#                    div(class = "year-label", "2023"),
#                    div(class = "trend-triangle green-triangle")
#                )
#              )
#       ),
#       column(width = 4,
#              div(
#                class = "custom-box",
#                tags$a(href = "https://example.com/indicator2", class = "custom-title",
#                       h4("Labour Productivity", class = "custom-title")),
#                div(class = "indicator-content",
#                    plotOutput("line_chart2", height = "150px", width = "70%"),  # Plot output directly in the box
#                    div(class = "year-label", "2023"),
#                    div(class = "trend-triangle yellow-triangle")
#                )
#              )
#       ),
#       column(width = 4,
#              div(
#                class = "custom-box",
#                tags$a(href = "https://example.com/indicator3", class = "custom-title",
#                       h4("Value-Added Goods and Services Exports", class = "custom-title")),
#                div(class = "indicator-content",
#                    plotOutput("line_chart3", height = "130px", width = "70%"),  # Plot output directly in the box
#                    div(class = "year-label", "2023"),
#                    div(class = "trend-triangle green-triangle")
#                )
#              )
#       )
#     ),
#     div(style = "height: 20px;"),  # Space between first and second rows of boxes
#     fluidRow(
#       column(width = 4,
#              div(
#                class = "custom-box",
#                tags$a(href = "https://example.com/indicator4", class = "custom-title",
#                       h4("Non-residential Investments", class = "custom-title")),
#                div(class = "indicator-content",
#                    plotOutput("line_chart4", height = "150px", width = "70%"),  # Plot output directly in the box
#                    div(class = "year-label", "2023"),
#                    div(class = "trend-triangle green-triangle")
#                )
#              )
#       ),
#       column(width = 4,
#              div(
#                class = "custom-box",
#                tags$a(href = "https://example.com/indicator5", class = "custom-title",
#                       h4("Exports Share", class = "custom-title")),
#                div(class = "indicator-content",
#                    plotOutput("line_chart5", height = "150px", width = "70%"),  # Plot output directly in the box
#                    div(class = "year-label", "2023"),
#                    div(class = "trend-triangle red-triangle")
#                )
#              )
#       )
#     )
#     )
#   )}

# wormcharts
wormchart <- function(data, label) {
  
  add_data_labels <- function(data, variable) {
    data$label <- ifelse(data$date == max(data$date), round(data[[variable]], 2), NA)
    data
  }
  plot_data <- add_data_labels(data, "value")
  ggplot(plot_data, aes(x = date, y = value, group = 1)) +
    geom_line(color = "#003366", size = 1.5) +
    geom_point(data = subset(plot_data, !is.na(label)), aes(x = date, y = value), color = "#003366", size = 3) +
    geom_text(data = subset(plot_data, !is.na(label)), aes(label = label),
              hjust = 1.2, vjust = -0.5, size = 8, color =  "#FEB70D",
              family = "Aptos(Body)"
              ) +  
    geom_segment(data = subset(plot_data, !is.na(label)), aes(x = date, xend = date, y = value, yend = label), color = "#003366", size = 0.5) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#f2f2f2", color = NA),
      plot.margin = margin(0, 0, 0, 0, "cm")
    )
  
}
  


# ---- 

wormchart_ui <- function(df, button,  title, worm, triangle){
  column(width = 4,
         actionButton(
           inputId = button,
           div(
             class = "custom-box",
             h4(title , class = "custom-title"),
             div(class = "indicator-content",
                 plotOutput(worm, height = "100px", width = "70%"), 
                 div(class = "year-label", max(df$date)),
                 uiOutput(triangle)
             )
           
         )
  ))}



get_growth_arrow <- function(value) {
  if (value > 0) {
    return(HTML("<span style='color:#2ecc71;font-size:36px;'>&#9650;</span>"))  # Upward triangle
  } else if (value < 0) {
    return(HTML("<span style='color: #e74c3c;font-size:36px; '>&#9660;</span>"))  # Downward triangle
  } else {
    return(HTML("<span style='color:#f1c40f; font-size:36px; '>&#8213;</span>"))  
  }
}

Extract_Status <- function(df, input){
  most_recent_value <- df$VALUE[nrow(df)]
  most_recent_year <- df$Year[nrow(df)]
  previous <- df$VALUE[nrow(df) - 1]
  growth <- round((most_recent_value - previous) / previous * 100,1)
  abs_growth <- abs(growth)
  
  HTML(paste(HTML(paste("<span style='font-size: larger;'><b>" , get_growth_arrow(growth), "</b></span>")),
             HTML(paste("<span style='font-size: larger;'><i>", most_recent_value, "</i></span>")),
             HTML(paste("<span style='font-size: large;'><i>", input, "</i></span>")),
             HTML(paste("<span style='font-size: small;'><i>", "in", most_recent_year, "</i></span>"))
             # HTML(
             #   paste(
             #     HTML(paste("<span style='font-size: larger;'><b>", most_recent_value," ", "</b></span>")),
             #     HTML(paste("<span style='font-size:  large;'><b>", input, "</b></span>" )),
             #     HTML(paste("<span style='font-size: larger;'><b>", get_growth_arrow(growth))),
             #     HTML(paste("<span style='font-size: small;'><i>", "in", most_recent_year, "</i></span>"))
  ))
}






get_triangle_class <- function(Sign) {
  if (Sign == -1) {
    return("trend-triangle red-triangle")
  } else if (Sign == 1) {
    return("trend-triangle green-triangle")
  } else {
    return("trend-triangle yellow-triangle")
  }
}




# # ui_indicatorpage_generalcss <- function(){
#   tags$head(
#     tags$style(HTML("
#     .chart-container {
#       height: 400px;
#       background-color: #f0f0f0;
#     }
#     .content-container {
#       display: flex;
#       position: relative;
#     }
#     .fixed-box {
#       position: absolute;
#       top: 0px;
#       right: 20px;
#       width: 400px;
#       background-color: #003366;
#       padding: 0px;
#       /*box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);*/
#       z-index: 1;
#       transition: top 0.3s;
#     }
#     .scrollable-boxes {
#       padding: 10px;
#       width: calc(100% - 440px); /* Adjust the width to make space for the fixed box */
#       margin-right: 20px;
#       background-color: #e9ecef;
#     }
#     .selectize-input, .selectize-dropdown {
#       background-color: #003366 !important;
#       color: white !important;
#       border-color: #00336 !important;
#       text-align: center; /* Center text horizontally */
#       padding-left: 0 !important; /* Minimize left padding */
#       padding-right: 0 !important; /* Minimize right padding */
#     }
#     .selectize-dropdown-content .option {
#       color: white !important;
#       text-align: center; /* Center text horizontally */
#     }
#     .selectize-input::after {
#       display: none !important;
#     }
# 
#     .blue-dropdown .selectize-input, .blue-dropdown .selectize-dropdown {
#       background-color: #003366 !important;
#       color: white !important;
#       padding: 5px;
#       font-size: 10px;
#       border-color: #004e93 !important;
#       text-align: center; /* Center text horizontally */
#       padding-left: 0 !important; /* Minimize left padding */
#       padding-right: 0 !important; /* Minimize right padding */
#     }
#     .blue-dropdown .selectize-dropdown-content .option {
#       color: white !important;
#       text-align: center; /* Center text horizontally */
# 
# 
#     }
#     .grey-dropdown .selectize-input, 
#     .grey-dropdown .selectize-dropdown {
#       top: auto !important;
#       bottom: 100% !important;
# 
#       background-color: #f2f2f2 !important;
#       color: black !important;
#       border-color: #cccccc !important;
#       text-align: center; /* Center text horizontally */
#       padding-left: 0 !important; /* Minimize left padding */
#       padding-right: 0 !important; /* Minimize right padding */
#     }
#     .grey-dropdown .selectize-dropdown-content .option {
#       color: black !important;
#       text-align: center; /* Center text horizontally */
#     }
#     
# 
# .grey-dropdown .selectize-dropdown {
#       top: auto !important;
#       bottom: 100% !important;
#       margin-bottom: 5px !important;
#     }
# 
# 
# 
#     .btn-custom {
#       background-color: transparent;
#       border: none;
#     }
# 
#     .btn-custom .fa-cloud-download-alt {
#       color: white;
#     }
#     .btn-custom-black {
#       background-color: transparent;
#       border: none;
#       color: black;
#     }
#     .btn-custom-black .fa-cloud-download-alt {
#       color: black;
#     }
#   "))  ,
#   tags$script(HTML("
#   document.addEventListener('scroll', function() {
#     const fixedBox = document.querySelector('.fixed-box');
#     const contentContainer = document.querySelector('.content-container');
#     const containerRect = contentContainer.getBoundingClientRect();
#     const middleScreen = window.innerHeight / 2;
# 
#     if (containerRect.top < middleScreen - fixedBox.offsetHeight / 2) {
#       fixedBox.style.position = 'fixed';
#       fixedBox.style.top = middleScreen - fixedBox.offsetHeight / 2 + 'px';
#       fixedBox.style.right = (window.innerWidth - contentContainer.offsetWidth) / 2 + 20 + 'px';
#     } else {
#       fixedBox.style.position = 'absolute';
#       fixedBox.style.top = '20px';
#       fixedBox.style.right = '20px';
#     }
#   });
# "))
#   
#   
#     
#     
#     
#     
#   
#   
#   
#             ) }


# Map chart----
mapchart <- function(df_map, input) {
  # Load the Canada map (assuming the function load_canada_map() is available)
  canada_map <- load_canada_map()
  
  # Merge data based on province names
  canada_map <- merge(canada_map, df_map, by.x = "prov_name_en", by.y = "GEO", all.x = TRUE)
  
  # Define the color palette from light yellow to dark yellow
  library(RColorBrewer)
  colors <- colorRampPalette(c("#FFFCE6", "#D4AF37"))(n = 20)
  
  # Create color palette based on the value range
  pal <- colorNumeric(palette = colors, domain = range(canada_map$VALUE, na.rm = TRUE))
  
  # Create the leaflet map focused on Canada
  p2 <- leaflet(data = canada_map) %>%
    # Add default map tiles
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    
    # Add polygons with province borders and color fill based on values
    addPolygons(
      fillColor = ~pal(VALUE),
      fillOpacity = 0.8,
      color = "#003366",   # Province border color
      weight = 1,
      popup = ~paste0("<b>", prov_name_en, "</b><br>Value: ", round(VALUE, 2))
    ) %>%
    
    # Add a legend for the values
    addLegend(
      pal = pal,
      values = ~VALUE,
      opacity = 0.7,
      title = "Values by Province",
      position = "bottomright"
    ) %>%
    
    # Restrict the view to Canada by specifying latitude and longitude bounds
    setView(lng = -106.3468, lat = 56.1304, zoom = 4) %>%
    
    # Limit the map to focus only on Canada
    setMaxBounds(lng1 = -141, lat1 = 83, lng2 = -52, lat2 = 40)
  
  return(p2)
}

# mapchart <- function(df_map, input){
#   canada_map <- load_canada_map()
#   
#   merged_df <- merge(canada_map, df_map, by.x="prov_name_en", by.y="GEO", all.x = TRUE)
#   canada_map <- merged_df
#   
#   # Create a color palette
#   # pal <- colorNumeric(palette = "YlOrBr", domain = canada_map$VALUE)
#   library(RColorBrewer)
#   
#   # Define the color palette from light yellow to dark yellow
#   colors <- colorRampPalette(c("#FFFCE6", "#D4AF37"))(n = 20)
#   
#   # Create a colorNumeric function with the defined colors and your data range
#   pal <- colorNumeric(
#     palette = colors,
#     domain = c(min(canada_map$VALUE, na.rm = TRUE), max(canada_map$VALUE, na.rm = TRUE))
#   )
#   
#   # Create the leaflet map
#   p2 <- leaflet(data = canada_map) %>%
#     addTiles() %>%  # Add default OpenStreetMap tiles
#     addPolygons(fillColor = ~pal(VALUE),
#                 fillOpacity = 0.8,
#                 color = "#003366",  # Border color for provinces
#                 weight = 1,  # Border weight
#                 popup = ~paste0("<b>", prov_name_en, "</b><br>Value: ", round(VALUE, 2))) %>%
#     addLegend(pal = pal, values = ~VALUE, opacity = 0.7, title = "Values by Province", position = "bottomright")%>%
#       # Add a white background by adding a blank tile layer
#       addProviderTiles("Stamen.TonerLite") %>%
#       addPolygons(fillColor = ~pal(canada_map$VALUE),
#                   fillOpacity = 0.5,
#                   color = "#003366",
#                   weight = 1,
#                   popup = ~paste0("<b>", prov_name_en, "</b><br>Value: ", round(canada_map$VALUE, 2))) %>%
#       # Remove zoom controls
#       leaflet::addControl(html = "", position = "topright", className = "leaflet-control-zoom") %>%
#       leaflet::addControl(html = "", position = "topleft", className = "leaflet-control-zoom")
#   
#   # p2 <- leaflet(data = canada_map,
#   #               options = leafletOptions(minZoom = 1.6, maxZoom = 1.6, dragging = FALSE, zoomControl = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE, boxZoom = FALSE, attributionControl = FALSE)) %>%
#   #   addTiles()%>%
#   #   # Add a white background by adding a blank tile layer
#   #   addProviderTiles("Stamen.TonerLite") %>%
#   #   addPolygons(fillColor = ~pal(canada_map$VALUE),
#   #               fillOpacity = 0.5,
#   #               color = "#003366",
#   #               weight = 1,
#   #               popup = ~paste0("<b>", prov_name_en, "</b><br>Value: ", round(canada_map$VALUE, 2))) %>%
#   #   # Remove zoom controls
#   #   leaflet::addControl(html = "", position = "topright", className = "leaflet-control-zoom") %>%
#   #   leaflet::addControl(html = "", position = "topleft", className = "leaflet-control-zoom")
# 
#   validate(need(nrow(df_map) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
# 
#   # Specify the size of the leaflet map
#   # p2 <- p2 %>% htmlwidgets::onRender("
#   #   function(el, x) {
#   #     el.style.width = '300px';
#   #     el.style.height = '250px';
#   #     el.style.backgroundColor = 'rgb(0, 51, 102)';
#   # 
#   #     // Remove zoom controls
#   #     var zoomControl = document.getElementsByClassName('leaflet-control-zoom')[0];
#   #     if (zoomControl) {
#   #       zoomControl.parentNode.removeChild(zoomControl);
#   #     }
#   # 
#   #     var css = '.custom-legend .legend-scale { font-size: 5px; } .custom-legend .legend-labels { font-size: 5px; padding: 4px; }';
#   #     var style = document.createElement('style');
#   #     if (style.styleSheet) {
#   #       style.styleSheet.cssText = css;
#   #     } else {
#   #       style.appendChild(document.createTextNode(css));
#   #     }
#   #     document.head.appendChild(style);
#   #   }
#   # ")
# # 
# #   return(p2)
# # }




go_to_button <- function(name1, label1, name2, label2) {
  style1 <- "color: white; background-color: #003366; height: 30px; width: 120px; font-size: 20px; margin:10px; "
  fluidRow(
    column(9),
    column(1, actionButton(name1, label1, style = style1)),
    column(1, actionButton(name2, label2, style = style1 )),
    column(1)
)}


feature_tab <- function(df, tab_name, title, tab_feature_chart, chart){  
  tabPanel(tab_name, 
           fluidPage(
             style = "background-color: white; padding-left: 20px; padding-top: 0px; margin: 0px;",
             fluidRow(
               column(8, h3(title )),
               column(1, actionButton("go_to_main_chart", 
                                      label = tagList(icon("angle-double-up"), ""), 
                                      class = "scroll-button", 
                                      style = "background-color: white; color: #003366; margin-bottom: 0px; text-align: right; padding-right: 0px;"),
                      column(3))),
             fluidRow(
               style = "display: flex; flex-direction: row; align-items: stretch; height: calc(100vh - 300px);",
               
               tab_feature_chart(chart, df),
               column(3,
                      div(style = "background-color: #003366; color: white; padding: 15px; border-radius: 5px; font-size: 14px; height: 95%;",
                          h4("Highlights"),
                          p("Here is a summary or insight related to the Labour Productivity growth rate.")
                      )
               )
             )))}

