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
      data <- read.csv(here("data-processing/data/emp_ind.csv"), header = TRUE)
      data <- na.omit(data)
      return(data)
    }
    

# RnD Dash ----
    ## Line plot----
    m6_RnD_lineplot_data <- function(df, geo, funder, performer, science_type, prices) {
      df |>
        filter(Year >= 2000,
               GEO == "British Columbia",
               Funder == " business enterprise sector",
               Performer == " total, all sectors",
               Science.type == "Total sciences",
               Prices == "Current prices")
    }
    
    m6_RnD_render_lineplot <- function(df, input){
      dash_lineplot(m6_RnD_lineplot_data, df, input, "$ Millions")}
    
    ## Bar plot----
    
    m6_RnD_barplot_data <- function(df, year){
      df |> 
        filter (Year == year,
                GEO %in% c("British Columbia", 
                           "Ontario", 
                           "Quebec", 
                           "Alberta", 
                           "Canada", 
                           "France", 
                           "Germany", 
                           "Italy", 
                           "Japan", 
                           "United Kingdom", 
                           "United States")
        )|>
        arrange(VALUE) %>%
        mutate(GEO = factor(GEO, levels = GEO),
               color = case_when(
                 GEO == "Ontario" ~ "lightblue1" ,
                 GEO == "Quebec" ~ "lightskyblue1" ,
                 GEO == "Alberta" ~ "lightskyblue2" ,
                 GEO == "British Columbia" ~ "steelblue3",
                 GEO == "Canada" ~ "royalblue4",
                 GEO == "United States" ~ "#00868B",
                 TRUE ~ "darkgrey"))
    }
    
    m6_RnD_render_barplot <- function(df, input){
      df2 <- m6_RnD_barplot_data(df, input$m6_RnD_barplot_year)
      df2$formatted_VALUE <- sprintf("<b>%.1f%%</b>", df2$VALUE)
      df2$adjusted_VALUE <- df2$VALUE + 0.1
      p2 <- df2 |> 
        plot_ly(x = ~VALUE,y=~GEO, color=~GEO, type = 'bar',
                colors = ~color, showlegend = FALSE)  |>
        add_text(x = ~adjusted_VALUE,text = ~formatted_VALUE, textposition = 'outside')
      validate(need(nrow(df2) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
      return(p2)
    }
    
    ## Table ----
    
    m6_RnD_table_data <- function(df, year , funder, performer, science_type, prices){
        df |>
          arrange(GEO, Funder, Performer, Science.type, Prices, Year) |>
          filter(Year %in% c(as.numeric(as.character(year)) -5, 
                             as.numeric(as.character(year))-3, 
                             as.numeric(as.character(year)))) |>
          group_by(GEO, Funder, Performer, Science.type, Prices) |>
          mutate(Maxyear = year,
                 GR5 = ifelse(all(c(as.numeric(as.character(year))-5, 
                                    as.numeric(as.character(year))) %in% Year), 
                              (VALUE[Year == as.numeric(as.character(year))] / VALUE[Year == as.numeric(as.character(year))-5]) - 1, NA),
                 GR3 = ifelse(all(c(as.numeric(as.character(year))-3, 
                                    as.numeric(as.character(year))) %in% Year), 
                              (VALUE[Year == as.numeric(as.character(year))] / VALUE[Year == as.numeric(as.character(year))-3]) - 1, NA))|>
          select(GEO, Funder, Performer, Science.type, Prices, GR5, GR3, Maxyear, Year)|>
          distinct(.keep_all = TRUE) |>
          filter (GEO %in% c("British Columbia", "Ontario", "Quebec", "Alberta", "Canada"),
                  Year == as.numeric(as.character(year)),
                  Funder == funder,
                  Performer == performer,
                  Science.type == science_type,
                  Prices == prices)|>
          ungroup()|>
          select (GEO, GR3, GR5, Maxyear) |>
          mutate(GR5 = paste0(round(GR5*100,1),"%"),
                 GR3 = paste0(round(GR3*100,1),"%"))|>
          mutate(GEO = factor(GEO, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Canada")))|>
          arrange(GEO)|>
          rename(Region = GEO) |>
          rename_with(~paste0("3-year growth <br>(", as.numeric(as.character(year))-3, "-", as.numeric(as.character(year)) ,")") , GR3)|>
          rename_with(~paste0("5-year growth <br>(", as.numeric(as.character(year))-5, "-", as.numeric(as.character(year)) ,")") , GR5)|>
          select(-Maxyear)
        
      }
    
    
    
    m6_RnD_render_table <- function(df, input){
      data <- m6_RnD_table_data(df, input$m6_RnD_table_year, input$m6_RnD_table_funder, input$m6_RnD_table_performer, input$m6_RnD_table_science_type, input$m6_RnD_table_prices )
      DT::datatable(data, options = list(dom = 't'), escape = FALSE, rownames = FALSE)
    }
    
    ## Sankey Plot----
    m6_RnD_sankey_data <- function(df, year , geo, science_type){
      df |>
        filter(Year == year,
               GEO == geo,
               Science.type == science_type,
               Prices == "Current prices",
               Funder != " total, all sectors",
               Performer != " total, all sectors") |>
        mutate(Funder = paste(Funder, "(F)", sep = " "),
               Performer = paste(Performer, "(P)", sep = " "))
    }
    
    m6_RnD_render_sankey <- function(df, input){
      df1 <- m6_RnD_sankey_data(df, input$m6_RnD_sankey_year, input$m6_RnD_sankey_geo, input$m6_RnD_sankey_science_type)
      nodes <- data.frame(name = c(as.character(df1$Funder), as.character(df1$Performer)))
      nodes <- unique(nodes)
      links <- data.frame(source = match(df1$Funder, nodes$name) - 1,
                          target = match(df1$Performer, nodes$name) - 1,
                          value = df1$VALUE)
      
      
      node_colors <- c(                      " total, all sectors (F)" = "grey",
                                             " federal government sector (F)" = "#8DB6CD", 
                                             " provincial governments sector (F)" = "#00EEEE", 
                                             " provincial research organizations sector (F)" = "#edbf33",
                                             " business enterprise sector (F)" = "#4F94CD",
                                             " higher education sector (F)" = "#27408B", 
                                             " private non-profit sector (F)" = "#27aeef", 
                                             " foreign sector (F)" = "#BFEFFF",
                                             " total, all sectors (P)" = "grey",
                                             " federal government sector (P)" = "#8DB6CD", 
                                             " provincial governments sector (P)" = "#00EEEE", 
                                             " provincial research organizations sector (P)" = "#edbf33",
                                             " business enterprise sector (P)" = "#4F94CD",
                                             " higher education sector (P)" = "#27408B", 
                                             " private non-profit sector (P)" = "#27aeef")
      
      
      nodes$color <- node_colors[nodes$name]
      
      links$source_name <- nodes$name[links$source + 1]
      links$target_name <- nodes$name[links$target + 1]
      
      fig <- plot_ly(
        type = "sankey",
        domain = list(
          x = c(0,1),
          y = c(0,1)
        ),
        orientation = "h",
        valueformat = "$,.0f",
        valuesuffix = "M",
        node = list(
          label = nodes$name,
          color = nodes$color,
          pad = 15,
          thickness = 30,
          line = list(
            color = "grey",
            width = 0.5
          ),
          hovertemplate = '%{label}<br>Total Value: %{value}<extra></extra>'        ),
        
        
        link = list(
          source = links$source,
          target = links$target,
          value = links$value
        ))
      
       fig <- fig |> layout(
         plot_bgcolor = '#F2F2F2',
         paper_bgcolor = '#F2F2F2', 
        font = list(size = 14, color = "black", weight = "bold"),
        hoverlabel = list(font = list(size = 14, color = "lightgrey")),
        updatemenus = list(font = list(color = "black") 
        )
      )
    
      validate(need(nrow(df1) > 0, "The data for this set of inputs is inadequate. To obtain a proper visualization, please adjust the 'Year',  'Price type', or 'Region' selection in the sidebar."))
      
      return(fig)
    }
    ## RnD Map ----
    
    m6_RnD_map_data <- function(df, year){
      df |>
        filter(
          Year == year
          ) |>
        select(
          GEO, VALUE
        )}
    
    m6_RnD_render_map <- function(df, input){
      df_map <- m6_RnD_map_data(df, input$m6_RnD_map_year)
      mapchart(df_map, input)
    }
    
    
# VAEX Dash----
    ## Line Plot ----
    m6_VAEX_lineplot_data <- function(df){
      df |>
        filter(GEO == "British Columbia",
               Industry == "Total industries",
               Value.added.exports.variable == "Value added exports" )
    }
    
    m6_VAEX_render_lineplot <- function(df, input){
      dash_lineplot(m6_VAEX_lineplot_data, df, input, "$ Billions")}
    
    ## Bar Plot ----
    m6_VAEX_bar_data <- function(df, year, industry){
      df |> 
        filter (Year == year,
                Industry == industry,
                Value.added.exports.variable == "Value added exports")|>
        mutate (EXP_GDP = 100 * VALUE / (GDP*1000),
                VALUE = Value.added.exports.variable)
    }
    
    m6_VAEX_render_barplot <- function(df, input){
      df2 <- m6_VAEX_bar_data(df, input$m6_VAEX_barplot_year, input$m6_VAEX_barplot_industry)
      df2$GEO <- reorder(df2$GEO, df2$EXP_GDP)
      df2$formatted_VALUE <- sprintf("%.2f%%", df2$EXP_GDP)
      df2$adjusted_VALUE <- df2$EXP_GDP + max(df2$EXP_GDP) / 10
      p2 <- df2 |>
        plot_ly(x = ~EXP_GDP, y=~GEO, color=~GEO, type = 'bar',
                showlegend = FALSE)  |>
        add_text(x = ~adjusted_VALUE,text = ~formatted_VALUE, textposition = 'outside') 
      
      validate(need(nrow(df2) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
      return(p2)
    }
    
    ## Pie Chart ----
    
    m6_VAEX_pie_data <- function(df, geo, year){
      df |>
        filter(Industry != "Total industries",
               GEO == geo,
               Year == year,
               Value.added.exports.variable == "Value added in exports, within-province" )
    }
    
    m6_VAEX_render_pie <- function(df, input){
      df1 <- m6_VAEX_pie_data(df, input$m6_VAEX_pie_geo, input$m6_VAEX_pie_year)
      sorted_data <- df1 |>
        arrange(desc(VALUE))
      
      # Create a pie chart
      fig <- plot_ly(sorted_data, labels = ~Industry, values = ~VALUE, type = 'pie', textposition = 'inside')
      fig <- fig |> layout(showlegend = FALSE)
      
      return(fig)
    }
    
# Non residential Investment Dash----
    ## line plot ----
    m6_nRinv_lineplot_data <- function(df){
      df |>
        filter(Year >= 2000,
               GEO == "British Columbia",
               Estimates == "Non-residential Investment",
               Prices == "Chained (2017) dollars") |>
        mutate(VALUE = Estimate_per_GDP)
    }
    
    m6_nRinv_render_lineplot <- function(df, input){
      dash_lineplot(m6_nRinv_lineplot_data, df, input, "Percentage")}
    
    ## lines plot data----
    m6_nRinv_lines_data <- function(df, geo, prices){
      df |>
        filter(GEO == geo,
               Prices == prices,
               Estimates %in% c(
                 "Business gross fixed capital formation",
                 "General governments final consumption expenditure",
                 "Non-residential structures, machinery and equipment",
                 "Residential structures"
               ))
    }
    
    m6_nRinv_render_lines <- function(df, input){
      dflines <- m6_nRinv_lines_data(df, input$m6_nRinv_lines_geo, input$m6_nRinv_lines_prices)
      plines <- dflines |>
        plot_ly(x = ~Year, y = ~VALUE, color = ~Estimates , type = 'scatter', mode = 'lines')|>
        layout(yaxis = list(title = ""),
               xaxis = list(title = ""),
               legend = list(y = -0.3, x=0))
      
      validate(need(nrow(dflines) > 0, "The data for this set of inputs is inadequate. To obtain a proper visualization, please adjust the inputs in the sidebar."))
      
      plines <- ggplotly(plines)
      return(plines)
    }
    
    ## Bar plot----
    m6_nRinv_barplot_data <- function(df, year){
      df |> 
        filter (Year == year,
                GEO %in% c("British Columbia", 
                           "Ontario", 
                           "Quebec", 
                           "Alberta", 
                           "Canada"),
                Estimates %in% c("Intellectual property products",
                                 "Non-residential structures",
                                 "Machinery and equipment"))
    }
    
    m6_nRinv_render_barplot <- function(df, input){
      
      df2 <- m6_nRinv_barplot_data(df, input$m6_nRinv_barplot_year)

      p2 <- df2 |> 
        plot_ly(x = ~VALUE, y = ~GEO, color = ~Estimates, type = 'bar', 
                orientation = 'h', stackgroup = 'group', text = ~VALUE, 
                width = 800, height = 600) |>
        layout(yaxis = list(title = ""),
               xaxis = list(title = ""),
               bargap = 0.2,
               barmode = "stack",
               barnorm = "percent",
               legend = list(y = -0.3, x = 0, orientation = 'h'))
      
            # p2 <- df2 |> 
      #   plot_ly(x = ~VALUE,y=~GEO, color=~Estimates, type = 'bar', 
      #           orientation = 'h', stackgroup = 'group', text = ~VALUE) |>
      #   layout(yaxis = list(title = ""),
      #          xaxis = list(title = ""),
      #          bargap = 0.2,
      #          barmode = "stack",
      #          barnorm = "percent",
      #          legend = list(y = -0.3, x=0, orientation = 'h'),
      #          autosize = TRUE)
      
      validate(need(nrow(df2) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
      return(p2)
    }
    
# Labour Productivity Dash----
    ## line plot ----
    m6_LP_lineplot_data <- function(df){
      df |>
        filter(Year >= 2000,
               GEO == "British Columbia",
               Industry == "All industries",
               measure == "Labour productivity")
    }
    
    m6_LP_render_lineplot <- function(df, input){
      dash_lineplot(m6_LP_lineplot_data, df, input, "$ per hour")}
    
    ## lines plot ----
    m6_LP_lines_data <- function(df, geo, labourtype ){
      
      short_titles <- c(
        "Business sector industries" = "Business sector",
        "Energy sector" = "Energy sector",
        "Information and communication sector" = "Info & Comm sector",
        "Finance and insurance, and holding companies [BS5B]" = "Finance & Insurance sector",
        "Industrial production" = "Industrial Production",
        "Non-business sector industries" = "Non-Business sector"
      )
      
      # Filter and rename the industries
      df |>
        filter(
          GEO == geo,
          measure == labourtype,
          Industry %in% names(short_titles)
        ) |>
        mutate(Industry = short_titles[Industry])
    }
    
    m6_LP_render_lines <- function(df, input){
      df2 <- m6_LP_lines_data(df, input$m6_LP_lines_geo, input$m6_LP_lines_labourtype)
      
      # Create an empty plotly object
      p <- plot_ly()
      
      # Loop through each industry to add traces
      for(industry in unique(df2$Industry)){
        p <- p |> add_trace(
          data = df2[df2$Industry == industry, ],
          x = ~Year,
          y = ~Y1_growth,
          type = 'scatter',
          mode = 'lines',
          color = ~Industry,
          visible = ifelse(industry %in% c("Energy sector", "Info & Comm sector"), TRUE, "legendonly")
        )
      }
      
      # Add layout configurations
      p <- p |> layout(
        plot_bgcolor = '#F2F2F2',
        paper_bgcolor = '#F2F2F2',
        legend = list(
          orientation = 'h',  
          x = 0.5,            
          xanchor = 'center', 
          y = -0.1 ),
        xaxis = list(
          title = ""  
        ),
        yaxis = list(
          title = ""  
        ),
        margin = list(
          l = 100,     
          r = 50,
          t = 20,
          b = 0
        )
      )
      p
    }
    

    ## Treemap ----
    m6_LP_treemap_data <- function(df, geo, year){
      df |>
        filter(GEO == geo,
               Year == year,
               measure == "Total number of jobs",
               parent != "NONE") |>
        group_by(parent) |>
        mutate(total_value = sum(VALUE))|>
        ungroup()|>
        mutate(per_val = (VALUE / total_value) * parent_val)
    }
    m6_LP_render_treemap <- function(df, input) {
      df2 <- m6_LP_treemap_data(df, input$m6_LP_treemap_geo, input$m6_LP_treemap_year)
      
      # Define colors for parents
      parent_colors <- c("#F2F2F2", 
                         "green",
                         "#32cd32", 
                         "lightgreen", #cc
                         "lightgreen", 
                         "lightgreen",
                         "lightgreen", 
                         "lightgreen",
                         "#32cd32",  #fff
                         "lightgreen",
                         "lightgreen",
                         "lightgreen", 
                         "lightgreen",
                         "lightgreen", 
                         "lightgreen",
                         "lightgreen",
                         "lightgreen", 
                         "lightgreen",
                         "lightgreen", 
                         "lightgreen",
                         "lightgreen", 
                         "lightgreen",
                         "lightgreen",
                         "lightgreen", 
                         "#ed872d", # keep this
                         "#efcc00", #dark orange
                         "#efcc00", 
                         "#fff8dc", 
                         "#fff8dc",
                         "#fff8dc",
                         "#fff8dc",
                         "#fff8dc", 
                         "#fff8dc",
                         "#fff8dc", 
                         "#fff8dc",
                         "#fff8dc", 
                         "#fff8dc",
                         "#fff8dc",  
                         "#fff8dc",
                         "#fff8dc")
      
      p <- df2 |>
        plot_ly(
          type = 'treemap',
          branchvalues = "total",
          labels = ~Industry,
          parents = ~parent,
          values = ~per_val,
          marker = list(
            colors = parent_colors
            
          )
        )
      
      # Add layout configurations
      p <- p |> layout(
        plot_bgcolor = '#F2F2F2',
        paper_bgcolor = '#F2F2F2',
        legend = list(
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.1
        ),
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 50,
          r = 30,
          t = 20,
          b = 15
        )
      )
      
      validate(need(nrow(df2) > 0, "The data for this set of inputs is inadequate. To obtain a proper visualization, please adjust the inputs in the sidebar."))
      
      return(p)
    }
    
    
    # m6_LP_render_treemap <- function(df, input) {
    #   df2 <- m6_LP_treemap_data(df, input$m6_LP_treemap_geo, input$m6_LP_treemap_year)
    #   colors <- c("#FE9900", "#7DDA58", "#CC6CE7", "#060270", "#DFC57B")
    #   p <- df2 |>
    #     plot_ly(
    #       type = 'treemap',
    #       branchvalues = "total",
    #       labels = ~Industry,
    #       parents = ~parent,
    #       values = ~per_val,
    #       colors = colors
    #     )
    #   
    #   # Add layout configurations
    #   p <- p |> layout(
    #     plot_bgcolor = '#F2F2F2',
    #     paper_bgcolor = '#F2F2F2',
    #     legend = list(
    #       orientation = 'h',
    #       x = 0.5,
    #       xanchor = 'center',
    #       y = -0.1 ),
    #     xaxis = list(
    #       title = ""
    #     ),
    #     yaxis = list(
    #       title = ""
    #     ),
    #     height = 290,
    #     margin = list(
    #       l = 50,
    #       r = 30,
    #       t = 20,
    #       b = 15
    #     )
    #   )
    #   
    #   validate(need(nrow(df2) > 0, "The data for this set of inputs is inadequate. To obtain a proper visualization, please adjust the inputs in the sidebar."))
    #   
    #   return(p)
    # }
    
    ## table----
    m6_LP_table_data <- function(df, year, labourtype, industry){
       df |>
        filter(
          Year == year,
          measure == labourtype,
          Industry == industry,
          GEO %in% c("British Columbia", "Ontario", "Quebec", "Alberta", "Canada"))|>
        select (GEO, Y1_growth, Y3_growht, Y5_growth) |>
        mutate(GEO = factor(GEO, levels = c("British Columbia", "Ontario", "Quebec", "Alberta", "Canada")))|>
               # Y1_growth = paste0(round(Y1_growth,1),"%"),
               # Y3_growht = paste0(round(Y3_growht,1),"%"),
               # Y5_growth = paste0(round(Y5_growth,1),"%"))|>
        arrange(GEO)|>
        rename(Region = GEO) |>
        rename_with(~paste0("one-year growth <br>(", as.numeric(as.character(year))-1, "-", as.numeric(as.character(year)) ,")") , Y1_growth)|>
        rename_with(~paste0("3-year growth <br>(", as.numeric(as.character(year))-3, "-", as.numeric(as.character(year)) ,")") , Y3_growht)|>
        rename_with(~paste0("5-year growth <br>(", as.numeric(as.character(year))-5, "-", as.numeric(as.character(year)) ,")") , Y5_growth)
    }
    
    m6_LP_render_table <- function(df, input){
      data <- m6_LP_table_data(df, input$m6_LP_table_year, input$m6_LP_table_labourtype, input$m6_LP_table_industry)
      DT::datatable(data, options = list(dom = 't'), escape = FALSE, rownames = FALSE, 
                    caption = htmltools::tags$caption("",
                                                      style = "font-family: Arial; font-size: 12px;"))
    }
    
    m6_LP_render_growthsectors <- function(df, input){
      data <- m6_LP_table_data(df, input$m6_LP_table_year, input$m6_LP_table_labourtype, input$m6_LP_table_industry)
      data_long <- reshape2::melt(data, id.vars = "Region")

      # Create an empty plotly object
      p2 <- plot_ly()
      p2 <- data_long |> 
        plot_ly(x = ~value,y=~Region, name=~variable, type = 'bar', 
                orientation = 'h', 
                visible = ~ifelse(variable == unique(variable)[1], "legendonly", TRUE),
                text = ~paste(round(value,1),"%")
                ) |>
        layout(yaxis = list(title = ""),
               xaxis = list(title = ""),
               bargap = 0.1,
        legend = list(y = 0, x=0, orientation = 'h'),
        autosize = TRUE)

        
      
      # Add layout configurations
      p2 <- p2 |> layout(
        plot_bgcolor = '#F2F2F2',
        paper_bgcolor = '#F2F2F2',
        legend = list(
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = -0.1
        ),
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 50,
          r = 30,
          t = 20,
          b = 15
        )
      )
      
      validate(need(nrow(data) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
      return(p2)
    }

    
    ## map ----
    m6_LP_map_data <- function(df, year, labourtype, industry){
      df |>
        filter(
          GEO != "Canada",
          Year == year,
          measure == labourtype,
          Industry == industry
        ) |>
        select(
          GEO, measure, Industry, VALUE
        )}
    
    m6_LP_render_map <- function(df, input){
          df_map <- m6_LP_map_data(df, input$m6_LP_map_year, input$m6_LP_map_labourtype, input$m6_LP_map_industry)
          mapchart(df_map, input)
    }
    
  #   m6_LP_render_map <- function(df, input){
  #     df_map <- m6_LP_map_data(df, input$m6_LP_map_year, input$m6_LP_map_labourtype, input$m6_LP_map_industry)
  #     canada_map <- load_canada_map()
  #     merged_df <- merge(canada_map, df_map, by.x="prov_name_en", by.y="GEO", all.x = TRUE)
  #     canada_map$VALUE <- merged_df$VALUE
  #     
  #     # Create a color palette
  #     # pal <- colorNumeric(palette = "YlOrBr", domain = canada_map$VALUE)
  #     library(RColorBrewer)
  #     
  #     # Define the color palette from light yellow to dark yellow
  #     colors <- colorRampPalette(c("#FFFCE6", "#D4AF37"))(n = 20)
  # 
  #     # Create a colorNumeric function with the defined colors and your data range
  #     pal <- colorNumeric(
  #       palette = colors,
  #       domain = c(min(canada_map$VALUE, na.rm = TRUE), max(canada_map$VALUE, na.rm = TRUE))
  #     )
  # 
  #     
  #     p2 <- leaflet(data = canada_map, 
  #                   options = leafletOptions(minZoom = 1.6, maxZoom = 1.6, dragging = FALSE, zoomControl = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE, boxZoom = FALSE, attributionControl = FALSE)) %>%
  #       # Add a white background by adding a blank tile layer
  #       addProviderTiles("Stamen.TonerLite") %>%
  #       addPolygons(fillColor = ~pal(canada_map$VALUE),
  #                   fillOpacity = 0.8,
  #                   color = "#003366",
  #                   weight = 1,
  #                   popup = ~paste0("<b>", prov_name_en, "</b><br>Value: ", round(canada_map$VALUE, 2))) %>%
  #       # Remove zoom controls
  #       leaflet::addControl(html = "", position = "topright", className = "leaflet-control-zoom") %>%
  #       leaflet::addControl(html = "", position = "topleft", className = "leaflet-control-zoom")
  #     
  #     validate(need(nrow(df_map) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
  #     
  #     # Specify the size of the leaflet map
  #     p2 <- p2 %>% htmlwidgets::onRender("
  #   function(el, x) {
  #     el.style.width = '300px'; 
  #     el.style.height = '250px'; 
  #     el.style.backgroundColor = 'rgb(0, 51, 102)';
  # 
  #     // Remove zoom controls
  #     var zoomControl = document.getElementsByClassName('leaflet-control-zoom')[0];
  #     if (zoomControl) {
  #       zoomControl.parentNode.removeChild(zoomControl);
  #     }
  # 
  #     var css = '.custom-legend .legend-scale { font-size: 5px; } .custom-legend .legend-labels { font-size: 5px; padding: 4px; }';
  #     var style = document.createElement('style');
  #     if (style.styleSheet) {
  #       style.styleSheet.cssText = css;
  #     } else {
  #       style.appendChild(document.createTextNode(css));
  #     }
  #     document.head.appendChild(style);
  #   }
  # ")
  #     
  #     return(p2)
  #   }
    

  #   m6_LP_render_map <- function(df, input){
  #     df_map <- m6_LP_map_data(df, input$m6_LP_map_year, input$m6_LP_map_labourtype, input$m6_LP_map_industry)
  #     canada_map <- load_canada_map()
  #     merged_df <- merge(canada_map, df_map, by.x="prov_name_en", by.y="GEO", all.x = TRUE)
  #     canada_map$VALUE <- merged_df$VALUE
  #     # Create a color palette
  #     pal <- colorNumeric(palette = "YlGnBu", domain = canada_map$VALUE)
  # 
  #     p2 <- leaflet(data = canada_map, 
  #                   options = leafletOptions(minZoom = 1.6, maxZoom = 1.6, dragging = FALSE, zoomControl = FALSE, scrollWheelZoom = FALSE, doubleClickZoom = FALSE, boxZoom = FALSE, attributionControl = FALSE)) %>%
  #       # setView(lng = -95, lat = 60, zoom = 2) %>%
  #       addProviderTiles(providers$CartoDB.Positron) %>%
  #       addPolygons(fillColor = ~pal(canada_map$VALUE),
  #                   fillOpacity = 0.8,
  #                   color = "#BDBDC3",
  #                   weight = 1,
  #                   popup = ~paste0("<b>", prov_name_en, "</b><br>Value: ", round(canada_map$VALUE, 2))) |>
  #       # leaflet::addLegend(pal = pal,
  #       #                    values = canada_map$VALUE,
  #       #                    # title = "Value",
  #       #                    labFormat = labelFormat(suffix = ""),
  #       #                    position = "topleft",
  #       #                    className = "custom-legend")%>%
  #       # Remove zoom controls
  #       leaflet::addControl(html = "", position = "topright", className = "leaflet-control-zoom") %>%
  #       leaflet::addControl(html = "", position = "topleft", className = "leaflet-control-zoom")
  # 
  #     validate(need(nrow(df_map) > 0, "The data for this year is inadequate. To obtain a proper visualization, please modify the year selection in the sidebar."))
  #     
  #     # Specify the size of the leaflet map
  #     p2 <- p2 %>% htmlwidgets::onRender("
  #   function(el, x) {
  #     el.style.width = '230px';
  #     el.style.height = '250px';
  #     // Remove zoom controls
  #     var zoomControl = document.getElementsByClassName('leaflet-control-zoom')[0];
  #     zoomControl.parentNode.removeChild(zoomControl);
  # 
  #     var css = '.custom-legend .legend-scale { font-size: 5px; } .custom-legend .legend-labels { font-size: 5px; padding: 4px; }';
  #     var style = document.createElement('style');
  #     if (style.styleSheet) {
  #       style.styleSheet.cssText = css;
  #     } else {
  #       style.appendChild(document.createTextNode(css));
  #     }
  #     document.head.appendChild(style);
  #   }
  # ")
  #     
  #     p2
  #   }
    
# Export Dash----
    ## line plot ----
    m6_EXP_lineplot_data <- function(df){
      df |>
        filter(Year >= 2000,
               GEO == "British Columbia",
               EXP_type == "Share of Canadian Export") |>
        mutate(VALUE = round(as.numeric(str_sub(VALUE, end=-2)),1))
    }
    
    m6_EXP_render_lineplot <- function(df, input){
      dash_lineplot(m6_EXP_lineplot_data, df, input, "Percentage")}
    
    ## Heat Map----
    m6_EXP_heatmap_data <- function(df){
      return(df)
    }
    
    m6_EXP_render_heatmap <- function(df, input){
      df1 <- m6_EXP_heatmap_data(df)
      df1$Value <- as.numeric(sub("%", "", df1$Value))
      
      p1 <- plot_ly(
        data = df1,
        x = ~GEO,
        y = ~Sector,
        z = ~Value,
        type = "heatmap",
        colorscale = "Plasma" )|>
        layout(title = "Exports as a share of total Canadian exports in 2023 ")
      p1
    }
    ##Stacked Bar Chart ----
    m6_EXP_stackbar_data <- function(df, year){
      df |>
        filter(Year == year)
    }
    
    m6_EXP_render_stackbar <- function(df, input){
      df1 <- m6_EXP_stackbar_data(df, input$m6_EXP_stackbar_year)
      
      p1 <- plot_ly(data = df1, 
                    y = ~GEO, 
                    x = ~Value,
                    color = ~Type,
                    type='bar',
                    orientation = 'h') |> 
        layout(barmode = 'stack',
               legend = list(x = 0, y = -0.1, orientation = 'h'))
      p1
    }
    ##Bubble  chart----
    m6_EXP_bubble_data <- function(df){
      df 
    }
    
    m6_EXP_render_bubble <- function(df, input){
      data <- m6_EXP_bubble_data(df)
      data$GDP <- as.numeric(data$GDP)
      data$growth_change <- as.numeric(data$growth_change)
      data$growth <- as.numeric(data$growth)
      data$exports <- as.numeric(data$exports)
      
      p1 <- plot_ly(
        data,
        x = ~exports,
        y = ~growth,
        size = ~GDP,
        color = ~growth_change,
        type = "scatter",
        mode = "markers",
        marker = list(sizemode = "diameter"),
        text = ~GEO) %>%
        layout(
          xaxis = list(title = "Export Level"),
          yaxis = list(title = "Export Growth"),
          showlegend = TRUE
        )
      
      p1
    }
