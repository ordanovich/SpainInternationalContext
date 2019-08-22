library(Cairo)
library(dplyr)
library(eurostat)
library(ggplot2)
library(ggstatsplot)
library(ggthemes)
library(highcharter)
library(htmltools)
library(leaflet)
library(magrittr)
library(maptools)
library(OECD)
library(paletteer)
library(plotly)
library(RColorBrewer)
library(scales)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(viridis)
library(wbstats)
library(WHO)
library(chorddiag)
library(parsetR)
library(flipPlots)

source("global.R")

shinyServer(function(input, output, session) { 
  
  options(shiny.usecairo=T)
  
  ## EUROSTAT OUTPUTS ------
  
  output$eurostat_search_result <- renderUI({
    
    dd1 <-  reactive({search_eurostat(input$eurostat_search_term,
                                      type = "dataset")})
    
    selectInput(inputId="eurostat_search_result",
                label="Select dataset",
                choices = dd1()$title,
                selected= "Infant mortality by NUTS2 region")
    
  })
  
  dd2 <- reactive({
    search_eurostat(input$eurostat_search_term,
                    type = "dataset") %>%
      filter(title == input$eurostat_search_result) %>%
      select(code) %>%
      distinct()
  })
  
  
  eurostat.data <- reactive({
    
    
    as.data.frame(get_eurostat(id = as.character(dd2()$code)))%>%
      label_eurostat(fix_duplicated=T, code = "geo") %>%
      mutate_if(is.factor, as.character)
  })
  
  
  output$eurostat_unit <- renderUI({
    
    selectInput(inputId="eurostat_unit",
                label = "Unit of measure",
                choices = unique(eurostat.data()$unit))
  })
  
  output$eurostat_param1 <- renderUI({
    
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    if (length(nn) > 0 )
      selectInput(inputId="eurostat_param1",
                  label = "Vertical facet",
                  choices = nn,
                  selected = nn[1])
    else (p(""))
    
  })
  
  output$eurostat_param2 <- renderUI({
    
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    if (length(nn) > 1)
      selectInput(inputId="eurostat_param2",
                  label = "Horizontal facet",
                  choices = nn,
                  selected=nn[2])
    else (p(""))
    
  })
  
  
  
  
  data_0_int <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    tt %>% group_by(geo_code,
                    `Geopolitical entity (reporting)`,
                    `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`
    ) %>%
      summarize(Values = mean(Values, na.rm = T)) %>%
      set_colnames(c("geo_code",
                     "geopolitical_entity",
                     "period",
                     "values")) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>% st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER"))
    
  })
  
  output$eurostat_ts_0_palette_pkg <- renderUI({
    
    
    palettes_d_names %>% filter(length >= length(unique(data_0_int()$CNTR_CODE))) -> pkgs_available0
    
    selectInput(inputId="eurostat_ts_0_palette_pkg",
                label = "Package for palette",
                choices=unique(pkgs_available0$package))
    
  })
  
  output$eurostat_ts_0_palette <- renderUI({
    
    
    palettes_d_names %>% filter(length >= length(unique(data_0_int()$CNTR_CODE)) &
                                  package == input$eurostat_ts_0_palette_pkg) -> palettes_available0
    
    selectInput(inputId="eurostat_ts_0_palette",
                label = "Palette to use",
                choices=unique(palettes_available0$palette))
    
  })
  
  output$eurostat_ts_0 <- renderPlotly({
    
    
    sp <- ggplot(data_0_int() %>% filter(LEVL_CODE == input$eurostat_nuts_data0),
                 
                 aes(x=period,
                     
                     y = values,
                     
                     col = CNTR_CODE,
                     
                     shape = SPAIN,
                     
                     text = paste(geopolitical_entity,
                                  ': ',
                                  round(values,2)))
    ) +
      
      geom_point(alpha = .8) +
      
      scale_x_date(expand = c(0.01,0.01)) +
      
      scale_color_paletteer_d(UQ(as.name(input$eurostat_ts_0_palette_pkg)),
                              UQ(as.name(input$eurostat_ts_0_palette)))
    
    hide_line <- switch(input$regline,
                        "Add" = "#00FFFF",
                        "Hide" = "#00FFFF00")
    
    
    theme <- switch(input$theme,
                    theme_base = ggthemes::theme_base(),
                    theme_calc = ggthemes::theme_calc(),
                    theme_clean = ggthemes::theme_clean(),
                    theme_economist = ggthemes::theme_economist(),
                    theme_economist_white = ggthemes::theme_economist_white(),
                    theme_excel = ggthemes::theme_excel(),
                    theme_excel_new = ggthemes::theme_excel_new(),
                    theme_few = ggthemes::theme_few(),
                    theme_fivethirtyeight = ggthemes::theme_fivethirtyeight(),
                    theme_foundation = ggthemes::theme_foundation(),
                    theme_gdocs = ggthemes::theme_gdocs(),
                    theme_hc = ggthemes::theme_hc(),
                    theme_igray = ggthemes::theme_igray(),
                    theme_map = ggthemes::theme_map(),
                    theme_pander = ggthemes::theme_pander(),
                    theme_par = ggthemes::theme_par(),
                    theme_solarized = ggthemes::theme_solarized(),
                    theme_solarized_2 = ggthemes::theme_solarized_2(),
                    theme_solid = ggthemes::theme_solid(),
                    theme_stata = ggthemes::theme_stata(),
                    theme_tufte = ggthemes::theme_tufte(),
                    theme_wsj = ggthemes::theme_wsj()
    )
    
    sp <- sp +
      geom_smooth(data = data_0_int() %>% filter(LEVL_CODE == input$eurostat_nuts_data0 &
                                                   SPAIN == "SPAIN") %>%
                    group_by(period, SPAIN) %>%
                    summarize(values = mean(values, na.rm = T)),
                  aes(x=as.Date(period),
                      
                      y = values,
                      
                      text = ""),
                  
                  method = input$method,
                  
                  col = hide_line,
                  
                  se = F,
                  
                  lwd = 1,
                  
                  lty = "dotted",
                  
                  span = input$span,
                  
                  level = input$conf,
                  
                  alpha = .6
      )+
      theme + theme(legend.position="none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    plot.margin=unit(c(.5, .5, .5, .5),"cm")
      )
    
    
    ggplotly(tooltip = "text", width = input$width,
             height = input$height)
    
  })
  
  data_1_int <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    
    
    tt %>% group_by(geo_code,
                    `Geopolitical entity (reporting)`,
                    `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`
                    , UQ(as.name(input$eurostat_param1))
    ) %>%
      summarize(Values = mean(Values, na.rm = T)) %>%
      set_colnames(c("geo_code",
                     "geopolitical_entity",
                     "period",
                     "strat1",
                     "values")) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>% st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER"))
  })
  
  output$eurostat_ts_1_palette_pkg <- renderUI({
    
    
    
    palettes_d_names %>% filter(length >= length(unique(data_1_int()$CNTR_CODE))) -> pkgs_available1
    
    selectInput(inputId="eurostat_ts_1_palette_pkg",
                label = "Package for palette",
                choices=unique(pkgs_available1$package))
    
  })
  
  
  
  
  output$eurostat_ts_1_palette <- renderUI({
    
    palettes_d_names %>% filter(length >= length(unique(data_1_int()$CNTR_CODE)) &
                                  package == input$eurostat_ts_1_palette_pkg) -> palettes_available1
    
    selectInput(inputId="eurostat_ts_1_palette",
                label = "Palette to use",
                choices=unique(palettes_available1$palette))
    
  })
  
  output$eurostat_ts_1 <- renderPlotly({
    
    
    
    sp <- ggplot(data_1_int() %>% filter(LEVL_CODE == input$eurostat_nuts_data1),
                 
                 aes(x=period,
                     
                     y = values,
                     
                     col = CNTR_CODE,
                     
                     shape = SPAIN,
                     
                     text = paste(geopolitical_entity,
                                  " - ",
                                  strat1,
                                  ': ', round(values,2)))
    )  +
      
      geom_point(alpha = .8)+
      
      
      
      facet_grid(strat1~.,
                 scales = input$scales) +
      
      scale_x_date(expand = c(0.01,0.01)) +
      
      scale_color_paletteer_d(UQ(as.name(input$eurostat_ts_1_palette_pkg)),
                              UQ(as.name(input$eurostat_ts_1_palette)))
    
    hide_line <- switch(input$regline,
                        "Add" = "#00FFFF",
                        "Hide" = "#00FFFF00")
    
    
    theme <- switch(input$theme,
                    theme_base = ggthemes::theme_base(),
                    theme_calc = ggthemes::theme_calc(),
                    theme_clean = ggthemes::theme_clean(),
                    theme_economist = ggthemes::theme_economist(),
                    theme_economist_white = ggthemes::theme_economist_white(),
                    theme_excel = ggthemes::theme_excel(),
                    theme_excel_new = ggthemes::theme_excel_new(),
                    theme_few = ggthemes::theme_few(),
                    theme_fivethirtyeight = ggthemes::theme_fivethirtyeight(),
                    theme_foundation = ggthemes::theme_foundation(),
                    theme_gdocs = ggthemes::theme_gdocs(),
                    theme_hc = ggthemes::theme_hc(),
                    theme_igray = ggthemes::theme_igray(),
                    theme_map = ggthemes::theme_map(),
                    theme_pander = ggthemes::theme_pander(),
                    theme_par = ggthemes::theme_par(),
                    theme_solarized = ggthemes::theme_solarized(),
                    theme_solarized_2 = ggthemes::theme_solarized_2(),
                    theme_solid = ggthemes::theme_solid(),
                    theme_stata = ggthemes::theme_stata(),
                    theme_tufte = ggthemes::theme_tufte(),
                    theme_wsj = ggthemes::theme_wsj()
    )
    
    sp <- sp +
      geom_smooth(data = data_1_int() %>% filter(LEVL_CODE == input$eurostat_nuts_data1 &
                                                   SPAIN == "SPAIN") %>%
                    group_by(period, SPAIN) %>%
                    summarize(values = mean(values, na.rm = T)),
                  aes(x=as.Date(period),
                      
                      y = values,
                      
                      text = ""),
                  
                  method = input$method,
                  
                  col = hide_line,
                  
                  se = F,
                  
                  lwd = 1,
                  
                  lty = "dotted",
                  
                  span = input$span,
                  
                  level = input$conf,
                  
                  alpha = .6
      )+
      theme + theme(legend.position="none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    plot.margin=unit(c(.5, .5, .5, .5),"cm")
      )
    
    
    ggplotly(tooltip = "text", width = input$width,
             height = input$height)
  })
  
  data_2_int <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    
    
    tt %>% group_by(geo_code,
                    `Geopolitical entity (reporting)`,
                    `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`
                    , UQ(as.name(input$eurostat_param1))
                    , UQ(as.name(input$eurostat_param2))
    ) %>%
      summarize(Values = mean(Values, na.rm = T)) %>%
      set_colnames(c("geo_code",
                     "geopolitical_entity",
                     "period",
                     "strat1",
                     "strat2",
                     "values")) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>% st_drop_geometry(),
            by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER"))
    
    
    
  })
  output$eurostat_ts_2_palette_pkg <- renderUI({
    
    palettes_d_names %>% filter(length >= length(unique(data_2_int()$CNTR_CODE))) -> pkgs_available2
    
    selectInput(inputId="eurostat_ts_2_palette_pkg",
                label = "Package for palette",
                choices=unique(pkgs_available2$package))
    
  })
  
  output$eurostat_ts_2_palette <- renderUI({
    
    
    palettes_d_names %>% filter(length >= length(unique(data_2_int()$CNTR_CODE)) &
                                  package == input$eurostat_ts_2_palette_pkg) -> palettes_available2
    
    selectInput(inputId="eurostat_ts_2_palette",
                label = "Palette to use",
                choices=unique(palettes_available2$palette))
    
  })
  
  output$eurostat_ts_2 <- renderPlotly({
    
    
    sp <- ggplot(data_2_int() %>% filter(LEVL_CODE == input$eurostat_nuts_data2),
                 
                 aes(x=period,
                     
                     y = values,
                     
                     col = CNTR_CODE,
                     
                     shape = SPAIN,
                     
                     text = paste(geopolitical_entity,
                                  " - ",
                                  strat1,
                                  " - ",
                                  strat2,
                                  ': ', round(values,2)))
    ) +
      
      geom_point(alpha = .8)+
      
      scale_color_paletteer_d(UQ(as.name(input$eurostat_ts_2_palette_pkg)),
                              UQ(as.name(input$eurostat_ts_2_palette)))+
      
      facet_grid(strat2~strat1,
                 scales = input$scales
                 # ,labeller = label_wrap_gen(25)
      ) +
      
      scale_x_date(expand = c(0.1,0.1))+
      
      scale_y_continuous(breaks = trans_breaks(identity, identity, n = 5))
    
    hide_line <- switch(input$regline,
                        "Add" = "#00FFFF",
                        "Hide" = "#00FFFF00")
    
    
    theme <- switch(input$theme,
                    theme_base = ggthemes::theme_base(),
                    theme_calc = ggthemes::theme_calc(),
                    theme_clean = ggthemes::theme_clean(),
                    theme_economist = ggthemes::theme_economist(),
                    theme_economist_white = ggthemes::theme_economist_white(),
                    theme_excel = ggthemes::theme_excel(),
                    theme_excel_new = ggthemes::theme_excel_new(),
                    theme_few = ggthemes::theme_few(),
                    theme_fivethirtyeight = ggthemes::theme_fivethirtyeight(),
                    theme_foundation = ggthemes::theme_foundation(),
                    theme_gdocs = ggthemes::theme_gdocs(),
                    theme_hc = ggthemes::theme_hc(),
                    theme_igray = ggthemes::theme_igray(),
                    theme_map = ggthemes::theme_map(),
                    theme_pander = ggthemes::theme_pander(),
                    theme_par = ggthemes::theme_par(),
                    theme_solarized = ggthemes::theme_solarized(),
                    theme_solarized_2 = ggthemes::theme_solarized_2(),
                    theme_solid = ggthemes::theme_solid(),
                    theme_stata = ggthemes::theme_stata(),
                    theme_tufte = ggthemes::theme_tufte(),
                    theme_wsj = ggthemes::theme_wsj()
    )
    
    sp <- sp +
      geom_smooth(data = data_2_int() %>% filter(LEVL_CODE == input$eurostat_nuts_data2 &
                                                   SPAIN == "SPAIN") %>%
                    group_by(period, SPAIN) %>%
                    summarize(values = mean(values, na.rm = T)),
                  aes(x=as.Date(period),
                      
                      y = values,
                      
                      text = ""),
                  
                  method = input$method,
                  
                  col = hide_line,
                  
                  se = F,
                  
                  lwd = 1,
                  
                  lty = "dotted",
                  
                  span = input$span,
                  
                  level = input$conf,
                  
                  alpha = .6
      )+
      theme + theme(legend.position="none",
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    plot.margin=unit(c(.5, .5, .5, .5),"cm")
      )
    
    
    ggplotly(tooltip = "text", width = input$width,
             height = input$height)
    
  })
  
  output$eurostat_ts_palette_pkg <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    
    if (length(nn) == 0) { uiOutput("eurostat_ts_0_palette_pkg")}
    else if (length(nn) == 1) { uiOutput("eurostat_ts_1_palette_pkg")}
    else if (length(nn) > 1) { uiOutput("eurostat_ts_2_palette_pkg")}
    
    
    
    
  })
  
  output$eurostat_ts_palette <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    
    if (length(nn) == 0) { uiOutput("eurostat_ts_0_palette")}
    else if (length(nn) == 1) { uiOutput("eurostat_ts_1_palette")}
    else if (length(nn) > 1) { uiOutput("eurostat_ts_2_palette")}
    
    
    
    
  })
  
  output$eurostat_ts <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    
    if (length(nn) == 0) { plotlyOutput("eurostat_ts_0")%>% withSpinner(color="#0dc5c1")}
    else if (length(nn) == 1) {plotlyOutput("eurostat_ts_1")%>% withSpinner(color="#0dc5c1")}
    else if (length(nn) > 1) {plotlyOutput("eurostat_ts_2")%>% withSpinner(color="#0dc5c1") }
    
    
    
    
  })
  
  
  output$eurostat_nuts_data0 <- renderUI({
    
    radioButtons(inputId="eurostat_nuts_data0",
                 label="NUTS",
                 choices = unique(data_0_int()$LEVL_CODE),
                 selected = max(unique(data_0_int()$LEVL_CODE)),
                 inline=T)
    
    
  }) 
  
  
  output$eurostat_nuts_data1 <- renderUI({
    
    radioButtons(inputId="eurostat_nuts_data1",
                 label="NUTS",
                 choices = unique(data_1_int()$LEVL_CODE),
                 selected = max(unique(data_1_int()$LEVL_CODE)),
                 inline=T)
    
    
  }) 
  
  output$eurostat_nuts_data2 <- renderUI({
    
    radioButtons(inputId="eurostat_nuts_data2",
                 label="NUTS",
                 choices = unique(data_2_int()$LEVL_CODE),
                 selected = max(unique(data_2_int()$LEVL_CODE)),
                 inline=T)
    
    
  }) 
  
  output$eurostat_nuts_unique <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    
    if (length(nn) == 0) {htmlOutput("eurostat_nuts_data0")}
    else if (length(nn) == 1) {htmlOutput("eurostat_nuts_data1")}
    else if (length(nn) > 1) {htmlOutput("eurostat_nuts_data2")}
    
    
    
    
  })
  
  
  
  eurostat_heatmap_data_data0 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
              st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      filter(SPAIN == "SPAIN" & LEVL_CODE == input$eurostat_nuts_data0) %>%
      group_by(geo, time = lubridate::year(time)) %>%
      summarize(values = mean(values, na.rm = T)) %>%
      data.table::dcast(geo~time) %>% set_rownames(.$geo) %>%
      select(-geo) %>%
      as.matrix()
    
    
  })
  
  
  output$eurostat_heatmap_data0 <- renderHighchart({
    
    hchart(eurostat_heatmap_data_data0()) %>% hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = paste0("<b>",paste0(input$eurostat_search_result, " (", input$eurostat_unit, ") at NUTS", input$eurostat_nuts_data0, " level"), "</b>"),
               margin = 20, align = "left",
               style = list(color = "#2b908f", useHTML = TRUE)) %>%
      hc_subtitle(text = "Time series sourced from the <i>Eurostat</i> database",
                  align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
    
  })
  
  eurostat_heatmap_data_data1 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
              st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      filter(SPAIN == "SPAIN" & LEVL_CODE == input$eurostat_nuts_data1) %>%
      group_by(geo, time = lubridate::year(time)) %>%
      summarize(values = mean(values, na.rm = T)) %>%
      data.table::dcast(geo~time) %>% set_rownames(.$geo) %>%
      select(-geo) %>%
      as.matrix()
    
    
  })
  
  
  output$eurostat_heatmap_data1 <- renderHighchart({
    
    hchart(eurostat_heatmap_data_data1()) %>% hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = paste0("<b>",paste0(input$eurostat_search_result, " (", input$eurostat_unit, ") at NUTS", input$eurostat_nuts_data1, " level"), "</b>"),
               margin = 20, align = "left",
               style = list(color = "#2b908f", useHTML = TRUE)) %>%
      hc_subtitle(text = "Time series sourced from the <i>Eurostat</i> database",
                  align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
    
  })
  
  eurostat_heatmap_data_data2 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
              st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      filter(SPAIN == "SPAIN" & LEVL_CODE == input$eurostat_nuts_data2) %>%
      group_by(geo, time = lubridate::year(time)) %>%
      summarize(values = mean(values, na.rm = T)) %>%
      data.table::dcast(geo~time) %>% set_rownames(.$geo) %>%
      select(-geo) %>%
      as.matrix()
    
    
  })
  
  
  output$eurostat_heatmap_data2 <- renderHighchart({
    
    hchart(eurostat_heatmap_data_data2()) %>% hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = paste0("<b>",paste0(input$eurostat_search_result, " (", input$eurostat_unit, ") at NUTS", input$eurostat_nuts_data2, " level"), "</b>"),
               margin = 20, align = "left",
               style = list(color = "#2b908f", useHTML = TRUE)) %>%
      hc_subtitle(text = "Time series sourced from the <i>Eurostat</i> database",
                  align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
    
  })
  
  
  output$eurostat_heatmap <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    if (length(nn) == 0) {highchartOutput("eurostat_heatmap_data0", width = "90%")%>% withSpinner(color="#0dc5c1")}
    else if (length(nn) == 1) {highchartOutput("eurostat_heatmap_data1", width = "90%")%>% withSpinner(color="#0dc5c1")}
    else if (length(nn) > 1) {highchartOutput("eurostat_heatmap_data2", width = "90%")%>% withSpinner(color="#0dc5c1")}
    
    
    
    
  })
  
  
  
  eurostat_betweenstats_data0 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
              st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      filter(LEVL_CODE == input$eurostat_nuts_data0)
    
  })
  
  output$eurostat_betweenstats_regionsOther_data0 <- renderUI(
    
    selectizeInput(inputId = "eurostat_betweenstats_regionsOther_data0",
                   label = "Select units outside of Spain",
                   choices = unique(eurostat_betweenstats_data0()[eurostat_betweenstats_data0()$SPAIN == "OTHER" , "geo"]),
                   selected = sample(unique(eurostat_betweenstats_data0()[eurostat_betweenstats_data0()$SPAIN == "OTHER" , "geo"]),1),
                   multiple = T,
                   width = '200%')
  )
  
  output$eurostat_betweenstats_regionsSpain_data0 <- renderUI(
    
    selectizeInput(inputId = "eurostat_betweenstats_regionsSpain_data0",
                   label = "Select units inside Spain",
                   choices = unique(eurostat_betweenstats_data0()[eurostat_betweenstats_data0()$SPAIN == "SPAIN" , "geo"]),
                   selected = sample(unique(eurostat_betweenstats_data0()[eurostat_betweenstats_data0()$SPAIN == "SPAIN" , "geo"]),1),
                   multiple = T,
                   width = '200%')
  )
  
  eurostat_betweenstats_data1 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
              st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      filter(LEVL_CODE == input$eurostat_nuts_data1)
    
  })
  
  output$eurostat_betweenstats_regionsOther_data1 <- renderUI(
    
    selectizeInput(inputId = "eurostat_betweenstats_regionsOther_data1",
                   label = "Select units outside of Spain",
                   choices = unique(eurostat_betweenstats_data1()[eurostat_betweenstats_data1()$SPAIN == "OTHER" , "geo"]),
                   selected = sample(unique(eurostat_betweenstats_data1()[eurostat_betweenstats_data1()$SPAIN == "OTHER" , "geo"]),1),
                   multiple = T,
                   width = '200%')
  )      
  
  output$eurostat_betweenstats_regionsSpain_data1 <- renderUI(
    
    selectizeInput(inputId = "eurostat_betweenstats_regionsSpain_data1",
                   label = "Select units inside Spain",
                   choices = unique(eurostat_betweenstats_data1()[eurostat_betweenstats_data1()$SPAIN == "SPAIN" , "geo"]),
                   selected = sample(unique(eurostat_betweenstats_data1()[eurostat_betweenstats_data1()$SPAIN == "SPAIN" , "geo"]),1),
                   multiple = T,
                   width = '200%')
  )    
  eurostat_betweenstats_data2 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit) %>%
      merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
              st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      filter(LEVL_CODE == input$eurostat_nuts_data2)
    
  })
  
  output$eurostat_betweenstats_regionsOther_data2 <- renderUI(
    
    selectizeInput(inputId = "eurostat_betweenstats_regionsOther_data2",
                   label = "Select units outside of Spain",
                   choices = unique(eurostat_betweenstats_data2()[eurostat_betweenstats_data2()$SPAIN == "OTHER" , "geo"]),
                   selected = sample(unique(eurostat_betweenstats_data2()[eurostat_betweenstats_data2()$SPAIN == "OTHER" , "geo"]),1),
                   multiple = T,
                   width = '200%')
  )       
  
  output$eurostat_betweenstats_regionsSpain_data2 <- renderUI(
    
    selectizeInput(inputId = "eurostat_betweenstats_regionsSpain_data2",
                   label = "Select units inside Spain",
                   choices = unique(eurostat_betweenstats_data2()[eurostat_betweenstats_data2()$SPAIN == "SPAIN" , "geo"]),
                   selected = sample(unique(eurostat_betweenstats_data2()[eurostat_betweenstats_data2()$SPAIN == "SPAIN" , "geo"]),1),
                   multiple = T,
                   width = '200%')
  )    
  
  
  output$eurostat_betweenstats_regionsOther <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    if (length(nn) == 0) {htmlOutput("eurostat_betweenstats_regionsOther_data0")}
    else if (length(nn) == 1) {htmlOutput("eurostat_betweenstats_regionsOther_data1")}
    else if (length(nn) > 1) {htmlOutput("eurostat_betweenstats_regionsOther_data2")}
    
    
  }) 
  
  
  output$eurostat_betweenstats_regionsSpain <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    if (length(nn) == 0) {htmlOutput("eurostat_betweenstats_regionsSpain_data0")}
    else if (length(nn) == 1) {htmlOutput("eurostat_betweenstats_regionsSpain_data1")}
    else if (length(nn) > 1) {htmlOutput("eurostat_betweenstats_regionsSpain_data2")}
    
    
  }) 
  
  
  
  
  plotInput_eurostat <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    if (length(nn) == 0) {
      
      dd <- eurostat_betweenstats_data0()  %>% 
        filter(geo %in% append(input$eurostat_betweenstats_regionsOther_data0, input$eurostat_betweenstats_regionsSpain_data0)) %>%
        mutate(time = lubridate::year(time)) %>% 
        select(geo,time,values) %>% 
        set_colnames(c("geo", "date", "value"))
      
      
      
      dd$cntr <-
        base::factor(
          x = dd$geo,
          levels = unique(dd$geo)
        )
      
      if(input$eurostat_betweestats_tagOutliers == FALSE){
        
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = FALSE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = paste0("Comparison between administrative units at NUTS", input$eurostat_nuts_data0, " level"),
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )}
      else if(input$eurostat_betweestats_tagOutliers == TRUE){
        
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = TRUE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = paste0("Comparison between administrative units at NUTS", input$eurostat_nuts_data0, " level"),
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )}
      
      
      
      
    }
    
    else if (length(nn) == 1) {
      
      dd <- eurostat_betweenstats_data1()  %>% 
        filter(geo %in% append(input$eurostat_betweenstats_regionsOther_data1, input$eurostat_betweenstats_regionsSpain_data1)) %>%
        mutate(time = lubridate::year(time)) %>% 
        select(geo,time,values) %>% 
        set_colnames(c("geo", "date", "value"))        
      
      
      dd$cntr <-
        base::factor(
          x = dd$geo,
          levels = unique(dd$geo)
        )
      
      if(input$eurostat_betweestats_tagOutliers == FALSE){
        
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = FALSE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = paste0("Comparison between administrative units at NUTS", input$eurostat_nuts_data1, " level"),
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )}
      else if(input$eurostat_betweestats_tagOutliers == TRUE){
        
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = TRUE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = paste0("Spain compared to the geogrpaphic units at NUTS", input$eurostat_nuts_data1, " level"),
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )
      }
      
      
      
    }
    
    else if (length(nn) > 1) {
      
      dd <- eurostat_betweenstats_data2()  %>% 
        filter(geo %in% append(input$eurostat_betweenstats_regionsOther_data2, input$eurostat_betweenstats_regionsSpain_data2)) %>%
        mutate(time = lubridate::year(time)) %>% 
        select(geo,time,values) %>% 
        set_colnames(c("geo", "date", "value"))        
      
      
      dd$cntr <-
        base::factor(
          x = dd$geo,
          levels = unique(dd$geo)
        )
      
      if(input$eurostat_betweestats_tagOutliers == FALSE){
        
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = FALSE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = paste0("Spain compared to the geogrpaphic units at NUTS", input$eurostat_nuts_data2, " level"),
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )}
      else if(input$eurostat_betweestats_tagOutliers == TRUE){
        
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = TRUE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = paste0("Spain compared to the geogrpaphic units at NUTS", input$eurostat_nuts_data2, " level"),
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )}
      
      
      
      
    }
    
    
  })
  
  output$eurostat_betweenstats <- renderPlot({
    
    print(plotInput_eurostat())
    
  })
  
  output$eurostat_downloadPlot <- downloadHandler(
    filename = function() { paste(gsub(" ", "_", tolower(unique(input$eurostat_search_term))), '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 15, height = 10, res = 300, units = "in")
      ggsave(file, plot = plotInput_eurostat(), device = device)
    }
  )
  
  
  output$res <- renderPrint(input$myColor)
  
  eurostat_data_map_nonsp_data0 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    
    tt %>% merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
                   st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      group_by(LEVL_CODE,
               geo_code,
               `Geopolitical entity (reporting)`,
               `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`
      ) %>%
      summarize(Values = mean(Values, na.rm = T)) %>%
      set_colnames(c("LEVL_CODE",
                     "geo_code",
                     "geopolitical_entity",
                     "period",
                     "values")) %>%
      filter(LEVL_CODE == input$eurostat_nuts_data0)
    
    
  })
  
  
  
  output$eurostat_map_time_data0 <- renderUI({
    
    tt <- unique(lubridate::year(eurostat_data_map_nonsp_data0()$period))
    
    sliderInput("eurostat_map_time_data0",
                "Select a period",
                min = min(tt),
                max = max(tt),
                value = max(tt),
                sep = "",
                width = '200%')
  })
  
  eurostat_data_map_sp_data0 <- reactive({
    
    merge(nuts %>% select(-LEVL_CODE),
          eurostat_data_map_nonsp_data0(),
          by.x = "NUTS_ID",
          by.y = "geo_code") %>%
      filter(!is.na(values)) %>%
      filter(lubridate::year(period) == input$eurostat_map_time_data0)
  })
  
  
  
  
  eurostat_data_map_nonsp_data1 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    
    tt %>% merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
                   st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      group_by(LEVL_CODE,
               geo_code,
               `Geopolitical entity (reporting)`,
               `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`
      ) %>%
      summarize(Values = mean(Values, na.rm = T)) %>%
      set_colnames(c("LEVL_CODE",
                     "geo_code",
                     "geopolitical_entity",
                     "period",
                     "values")) %>%
      filter(LEVL_CODE == input$eurostat_nuts_data1)
    
    
  })
  
  
  
  output$eurostat_map_time_data1 <- renderUI({
    
    tt <- unique(lubridate::year(eurostat_data_map_nonsp_data1()$period))
    
    sliderInput("eurostat_map_time_data1",
                "Select a period",
                min = min(tt),
                max = max(tt),
                value = max(tt),
                sep = "",
                width = '200%')
  })
  
  eurostat_data_map_sp_data1 <- reactive({
    
    merge(nuts %>% select(-LEVL_CODE),
          eurostat_data_map_nonsp_data1(),
          by.x = "NUTS_ID",
          by.y = "geo_code") %>%
      filter(!is.na(values)) %>%
      filter(lubridate::year(period) == input$eurostat_map_time_data1)
  })
  
  
  
  
  
  eurostat_data_map_nonsp_data2 <- reactive({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    
    tt %>% merge(nuts[,c("NUTS_ID", "CNTR_CODE", "LEVL_CODE")] %>%
                   st_drop_geometry(), by.x = "geo_code", by.y = "NUTS_ID")  %>%
      mutate(SPAIN = ifelse(CNTR_CODE == "ES", "SPAIN", "OTHER")) %>%
      group_by(LEVL_CODE,
               geo_code,
               `Geopolitical entity (reporting)`,
               `Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)`
      ) %>%
      summarize(Values = mean(Values, na.rm = T)) %>%
      set_colnames(c("LEVL_CODE",
                     "geo_code",
                     "geopolitical_entity",
                     "period",
                     "values")) %>%
      filter(LEVL_CODE == input$eurostat_nuts_data2)
    
    
  })
  
  
  
  output$eurostat_map_time_data2 <- renderUI({
    
    tt <- unique(lubridate::year(eurostat_data_map_nonsp_data2()$period))
    
    sliderInput("eurostat_map_time_data2",
                "Select a period",
                min = min(tt),
                max = max(tt),
                value = max(tt),
                sep = "",
                width = '200%')
  })
  
  eurostat_data_map_sp_data2 <- reactive({
    
    merge(nuts %>% select(-LEVL_CODE),
          eurostat_data_map_nonsp_data2(),
          by.x = "NUTS_ID",
          by.y = "geo_code") %>%
      filter(!is.na(values)) %>%
      filter(lubridate::year(period) == input$eurostat_map_time_data2)
  })
  
  
  output$eurostat_map_data0 <- renderLeaflet({
    
    qpal <- colorQuantile(c(input$myColor_eu_high,
                            input$myColor_eu_mid,
                            input$myColor_eu_low),
                          eurostat_data_map_sp_data0()$values,
                          n = as.numeric(input$eurostat_map_bins),
                          na.color = "#808080",
                          alpha = FALSE,
                          reverse = TRUE)
    
    
    
    leaflet(st_transform(eurostat_data_map_sp_data0() %>% mutate(lbl = paste0(geopolitical_entity, ":", values)), crs = 4326) %>% rmapshaper::ms_simplify(.)) %>%
      setView(2, 45, 4) %>%
      addProviderTiles(input$eurostat_map_providerTiles,
                       options = providerTileOptions(opacity = .8))%>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = .7,
                  
                  fillColor = ~qpal(values),
                  highlight = highlightOptions(bringToFront = TRUE),
                  label = ~lbl,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  output$eurostat_map_data1 <- renderLeaflet({
    
    
    
    qpal <- colorQuantile(c(input$myColor_eu_high,
                            input$myColor_eu_mid,
                            input$myColor_eu_low),
                          eurostat_data_map_sp_data1()$values,
                          n = as.numeric(input$eurostat_map_bins),
                          na.color = "#808080",
                          alpha = FALSE,
                          reverse = TRUE)
    
    
    
    leaflet(st_transform(eurostat_data_map_sp_data1()%>% mutate(lbl = paste0(geopolitical_entity, ":", values)), crs = 4326) %>% rmapshaper::ms_simplify(.)) %>%
      setView(2, 45, 4) %>%
      addProviderTiles(input$eurostat_map_providerTiles,
                       options = providerTileOptions(opacity = .8))%>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = .7,
                  
                  fillColor = ~qpal(values),
                  highlight = highlightOptions(bringToFront = TRUE),
                  label = ~lbl,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  output$eurostat_map_data2 <- renderLeaflet({
    
    
    
    qpal <- colorQuantile(c(input$myColor_eu_high,
                            input$myColor_eu_mid,
                            input$myColor_eu_low),
                          eurostat_data_map_sp_data2()$values,
                          n = as.numeric(input$eurostat_map_bins),
                          na.color = "#808080",
                          alpha = FALSE,
                          reverse = TRUE)
    
    
    
    leaflet(st_transform(eurostat_data_map_sp_data2()%>% mutate(lbl = paste0(geopolitical_entity, ":", values)), crs = 4326) %>% rmapshaper::ms_simplify(.)) %>%
      setView(2, 45, 4) %>%
      addProviderTiles(input$eurostat_map_providerTiles,
                       options = providerTileOptions(opacity = .8))%>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = .7,
                  
                  fillColor = ~qpal(values),
                  highlight = highlightOptions(bringToFront = TRUE),
                  label = ~lbl,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  
  
  
  output$eurostat_map_time <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    if (length(nn) == 0) {htmlOutput("eurostat_map_time_data0")}
    else if (length(nn) == 1) {htmlOutput("eurostat_map_time_data1")}
    else if (length(nn) > 1) {htmlOutput("eurostat_map_time_data2")}
    
    
    
    
  })
  
  
  output$eurostat_map <- renderUI({
    
    eurostat.data() %>%
      filter(unit == input$eurostat_unit) %>%
      select (-unit)-> tt
    
    
    lbl <- label_eurostat_vars(names(tt)) %>% na.omit()
    names(tt) <- append(append("geo_code", lbl), "Values")
    
    names(tt[!(names(tt) %in% c("geo_code"
                                ,"Geopolitical entity (reporting)"
                                ,"Period of time (a=annual, q=quarterly, m=monthly, d=daily, c=cumulated from January)"
                                ,"Values" ))]) -> nn
    
    
    if (length(nn) == 0) {leafletOutput("eurostat_map_data0")%>% withSpinner(color="#0dc5c1")}
    else if (length(nn) == 1) {leafletOutput("eurostat_map_data1")%>% withSpinner(color="#0dc5c1")}
    else if (length(nn) > 1) {leafletOutput("eurostat_map_data2")%>% withSpinner(color="#0dc5c1")}
    
    
    
    
  })
  
  
  ## WORLDBANK OUTPUTS ----
  
  output$wb_lang_display <-renderUI(
    
    if(input$wb_update == "Latest update") {
      
      htmlOutput("wb_lang")
    }
    
  )
  
  output$wb_lang <- renderUI({
    
    selectInput(inputId = "wb_lang",
                label = "Language in which to return the results",
                c("English" = "en",
                  "Spanish" = "es",
                  "French" = "fr",
                  "Arabic" = "ar",
                  "Mandarin" = "zh"),
                selected = "en")
    
  })
  
  
  
  wb_cachelist_choice <- reactive({
    
    wbcache(lang = input$wb_lang)
    
  })
  
  ff <- reactive({
    
    if(input$wb_update == "Latest update") {
      
      lapply(wb_cachelist_choice(), function(df) cbind(names(df))) %>%
        plyr::ldply(data.frame) %>%
        as.data.frame() %>%
        mutate_if(is.factor, as.character) %>%
        set_colnames(c("fieldGroup", "fieldName"))
    }
    
    else if(input$wb_update == "Cached") {
      
      lapply(wb_cachelist, function(df) cbind(names(df))) %>%
        plyr::ldply(data.frame) %>%
        as.data.frame() %>%
        mutate_if(is.factor, as.character) %>%
        set_colnames(c("fieldGroup", "fieldName"))
    }
    
    
    
  }) 
  
  output$wb_field_group_choice <- renderUI({
    
    selectInput(inputId = "wb_field_group_choice",
                label = "Category",
                choices = unique(ff()$fieldGroup),
                selected = "indicators"
    )
    
    
  })
  
  output$wb_field_choice <- renderUI({
    
    ff_sel <- ff() %>% filter(fieldGroup == input$wb_field_group_choice)
    
    selectInput(inputId = "wb_field_choice",
                label = "Field",
                choices = ff_sel$fieldName,
                selected = "indicatorDesc"
    )
  })
  
  
  data_wb_search <- reactive({
    
    if(input$wb_update == "Latest update") {wbsearch(pattern = input$wb_search_term, 
                                                     fields = input$wb_field_choice,
                                                     cache = wb_cachelist_choice())}
    
    
    else if(input$wb_update == "Cached") {wbsearch(pattern = input$wb_search_term, 
                                                   fields = input$wb_field_choice,
                                                   cache = wb_cachelist)}
    
    
  })
  
  output$wb_indicator <- renderUI({
    
    selectInput(inputId = "wb_indicator",
                label = "Select an indicator",
                choices = data_wb_search()$indicator,
                selected = data_wb_search()$indicator[4]
    )
    
  })
  
  
  wb_data <- reactive({
    
    wbstats::wb(
      country = append("ES", wbcountries()[wbcountries()$country %in% input$wb_countries, "iso2c"])
      , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
      , startdate = as.numeric(input$wb_startdate)
      , enddate = as.numeric(input$wb_enddate)
      # , mrv = as.numeric(input$wb_mrv)
      , freq = input$wb_freq
      , POSIXct = TRUE
    )
    
    
  })
  
  cols <- reactive({
    
    append(
      
      paletteer_d(package = "ggsci",
                  palette = "default_igv",
                  n = nrow(wb_data() %>% 
                             filter(country != "Spain") %>%
                             select(country) %>% 
                             distinct()
                  )
      ),
      
      c(input$wb_ts_color_for_spain,input$wb_ts_color_for_spain))
    
  })
  
  
  output$wb_ts <- renderHighchart({
    
    
    hc <- highchart(type = "stock") %>%
      
      hc_add_series_df(name = "Other",
                       wb_data() %>% filter(country != "Spain"),
                       "line",
                       x=date_ct,
                       y=value,
                       group = country) %>%
      
      hc_add_series_df(name = "Spain",
                       wb_data() %>% filter(country == "Spain"),
                       "line",
                       x=date_ct,
                       y=value,
                       group = country) %>%
      
      hc_add_series_df(name = "Spain",
                       wb_data() %>% filter(country == "Spain"),
                       "scatter",
                       symbol='cross',
                       x=date_ct,
                       y=value,
                       group = country) %>%
      
      hc_colors(cols()) %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(reversed = TRUE) %>%
      hc_chart(inverted = TRUE) %>%
      hc_tooltip(pointFormat = "{point.country}:{point.value:.2f}")%>%
      hc_title(text = paste0("<b>", unique(wb_data()$indicator), "</b>"),
               margin = 20, align = "left",
               style = list(color = "#2b908f", useHTML = TRUE)) %>%
      hc_subtitle(text = "Time series sourced from the <i>World Bank</i> database",
                  align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
    
    if (input$wb_ts_theme != FALSE) {
      theme <- switch(input$wb_ts_theme,
                      null = hc_theme_null(),
                      db = hc_theme_db(),
                      darkunica = hc_theme_darkunica(),
                      gridlight = hc_theme_gridlight(),
                      sandsignika = hc_theme_sandsignika(),
                      fivethirtyeight = hc_theme_538(),
                      economist = hc_theme_economist(),
                      chalk = hc_theme_chalk(),
                      handdrwran = hc_theme_handdrawn()
      )
      
      hc <- hc %>% hc_add_theme(theme)
      
    }
    
    hc %>%
      hc_exporting(enabled = TRUE,
                   filename = paste0("World bank Time Series graph"))
    
  })
  
  output$wb_betweenstats_regions <- renderUI({
    
    wbcountries() %>% filter(region == "Aggregates" | country == "Spain") %>% select(country) -> xx
    
    selectizeInput(inputId = "wb_betweenstats_regions",
                   label = "Select units of interest to add to the graph",
                   choices = xx$country,
                   selected = append("Spain", sample(xx$country, 9)),
                   multiple = T,
                   width = '100%')
    
  })
  
  output$wb_betweenstats_countries <- renderUI({
    
    wbcountries() %>% filter(region != "Aggregates") %>% select(country) -> xx
    
    selectizeInput(inputId = "wb_betweenstats_countries",
                   label = "Select units of interest to add to the graph",
                   choices = xx$country,
                   selected = unique(append("Spain", sample(xx$country, 9))),
                   multiple = T,
                   width = '100%')
    
  })
  
  output$wb_betweenstats_selected_unit <- renderUI({
    
    
    
    if (input$wb_betweenstats_level == "Regions") {htmlOutput("wb_betweenstats_regions")}
    else if (input$wb_betweenstats_level == "Countries") {htmlOutput("wb_betweenstats_countries")}
    
    
  })
  
  plotInput_wb <- reactive({
    
    
    
    
    if (input$wb_betweenstats_level == "Regions") {
      
      dd <- wb_data_all() %>%
        merge(wbcountries() %>% select(country, region), by = "country") %>%
        filter(country %in% input$wb_betweenstats_regions) %>% 
        filter(!is.na(value))
      
      dd$cntr <-
        base::factor(
          x = dd$country,
          levels = unique(dd$country)
        )
      
      if(input$wb_betweestats_tagOutliers == TRUE){
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = TRUE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = "Spain compared to the Regional Aggregates",
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )
      }
      else if(input$wb_betweestats_tagOutliers == FALSE){
        p <- ggbetweenstats(
          data = dd,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = FALSE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = "Spain compared to the Regional Aggregates",
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE
        )
      }
      
      
    }
    
    else if (input$wb_betweenstats_level == "Countries") {
      
      dd <- wb_data_all() %>%
        merge(wbcountries() %>% select(country, region), by = "country") %>% 
        filter(country %in% input$wb_betweenstats_countries) %>% 
        filter(!is.na(value))
      
      dd$cntr <-
        base::factor(
          x = dd$country,
          levels = unique(dd$country)
        )
      
      if(input$wb_betweestats_tagOutliers == TRUE){
        
        p <- ggbetweenstats(
          data = dd ,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = TRUE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = "Spain compared to the Countries of Choice",
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE)
        
      }
      else if(input$wb_betweestats_tagOutliers == FALSE){
        
        p <- ggbetweenstats(
          data = dd ,
          x = cntr,
          y = value,
          notch = FALSE, # show notched box plot
          mean.plotting = TRUE, # whether mean for each group is to be displayed
          mean.ci = TRUE, # whether to display confidence interval for means
          mean.label.size = 5, # size of the label for mean
          type = "p", # which type of test is to be run
          k = 3, # number of decimal places for statistical results
          outlier.tagging = FALSE, # whether outliers need to be tagged
          outlier.label = date, # variable to be used for the outlier tag
          outlier.label.color = "darkgreen", # changing the color for the text label
          xlab = "Country", # label for the x-axis variable
          ylab = "Attribute value", # label for the y-axis variable
          title = "Spain compared to the Countries of Choice",
          ggtheme = hrbrthemes::theme_ipsum_tw(base_size = 12,
                                               axis_title_size = 10), # choosing a different theme
          ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
          # package = "wesanderson", # package from which color palette is to be taken
          # palette = "Darjeeling1", # choosing a different color palette
          messages = FALSE)
        
      }
      
      
    }
    
    
    
  })
  
  output$wb_betweenstats <- renderPlot({
    
    print(plotInput_wb())
    
  })
  
  output$wb_downloadPlot <- downloadHandler(
    filename = function() { paste(gsub(" ", "_", unique(wb_data_all()$indicator)), '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 15, height = 10, res = 300, units = "in")
      ggsave(file, plot = plotInput_wb(), device = device)
    }
  )
  
  wb_data_all <- reactive({
    
    wb(
      country = "all"
      , indicator = data_wb_search()[data_wb_search()$indicator == input$wb_indicator, "indicatorID"]
      , startdate = as.numeric(input$wb_startdate)
      , enddate = as.numeric(input$wb_enddate)
      # , mrv = as.numeric(input$wb_mrv)
      , freq = input$wb_freq
      , POSIXct = TRUE
    )
    
  })
  
  output$wb_map_time <- renderUI({
    
    selectInput("wb_map_time",
                "Select a period",
                choices = unique(wb_data_all()$date_ct),
                selected = max(unique(wb_data_all()$date_ct)))
  })
  
  wb_map_data <- reactive({
    
    wrld_simpl %>%
      select(ISO3) %>%
      merge(wb_data_all() %>% filter(date_ct == input$wb_map_time),
            by.x = "ISO3",
            by.y = "iso3c") %>%
      select(country, date_ct, value)
    
    
  })
  
  
  output$wb_map <- renderLeaflet({
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/> Value: %s",
      wb_map_data()$country,
      round(wb_map_data()$value),1) %>% 
      lapply(HTML)
    
    qpal <- colorQuantile(c(input$myColor_wb_high,
                            input$myColor_wb_mid,
                            input$myColor_wb_low),
                          wb_map_data()$values,
                          n = as.numeric(input$wb_map_bins),
                          na.color = "#808080",
                          alpha = FALSE,
                          reverse = TRUE)
    
    
    leaflet()%>% 
      setView(0, 45, 4) %>%
      addProviderTiles(input$wb_map_providerTiles,
                       options = providerTileOptions(opacity = .8)) %>% 
      
      addPolygons( data = st_transform(wb_map_data(), crs = 4326),
                   color = "#444444", weight = 1, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = .7,
                   fillColor = ~qpal(value),
                   highlight = highlightOptions(bringToFront = TRUE),
                   label = labels,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal",
                                  padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto") 
      ) 
    
    
  })
  
  
  ## WHO OTPUTS -------
  
  
  who_data <- reactive({
    
    ll <- WHO::get_codes() %>%
      filter(display == input$who_codes) %>% 
      select(label)
    
    WHO::get_data(ll$label) %>% mutate(value = clean_value(value))
    
  })
  
  # output$who_table <- renderTable(head(who_data(),10))
  
  output$who_regions_for_chordNparset <- renderUI({
    
    selectInput(inputId = "who_regions_for_chordNparset",
                label = "Select region for chord diagram",
                choices = unique(who_data()$region))
    
  })
  
  
  
  
  
  output$who_years_for_chord <- renderUI({
    
    who_data() %>% 
      filter(region == input$who_regions_for_chordNparset) %>%
      select(year) %>% 
      na.omit() -> yy
    
    sort(unique(yy$year)) -> yy
    
    
    sliderInput("who_years_for_chord",
                label = "Select year range",
                min = min(yy),
                max = max(yy),
                value = c(quantile(yy)[2],
                          quantile(yy)[4]),
                sep = ""
    )
    
  })
  
  output$who_years_for_parset <- renderUI({
    
    who_data() %>% 
      filter(region == input$who_regions_for_chordNparset) %>%
      select(year) %>% 
      na.omit() -> yy
    
    sort(unique(yy$year)) -> yy
    
    
    selectInput("who_years_for_parset",
                label = "Select year",
                choices = yy
    )
    
  })
  
  output$who_years <- renderUI({
    
    if(class(who_data()$value) == "numeric") {
      
      htmlOutput("who_years_for_chord")
      
    }
    
    else if(class(who_data()$value) == "character") {
      
      htmlOutput("who_years_for_parset")
      
    }
    
  })
  
  
  
  
  who_data_chord <- reactive({
    
    who_data() %>% 
      filter(region == input$who_regions_for_chordNparset) %>%
      select(country, year, value) %>%  
      filter(year %in% seq(input$who_years_for_chord[1],
                           input$who_years_for_chord[2],
                           1)) %>%
      data.table::dcast(year ~ country, fun.aggregate=mean, na.rm=TRUE) -> df_i
    
    rownames(df_i) <- df_i[,1]
    
    as.matrix(df_i[,-1])
    
  })
  
  output$who_chordPlot <- renderChorddiag({
    
    cols <- paletteer_d(package = "ggsci",
                        palette = "default_igv",
                        n = ncol(who_data_chord()))
    
    rows <- paletteer_c(package = "oompaBase",
                        palette = "greyscale",
                        n = nrow(who_data_chord()))
    
    groupColors <- append( rows, cols)
    
    chorddiag(who_data_chord(),
              type = "bipartite", 
              showTicks = F, groupnameFontsize = 14, 
              groupnamePadding = 10, margin = 90,
              groupColors = groupColors
              # palette = "Spectral"
    )
    
    
  })
  
  
  who_data_parset <- reactive({
    
    who_data() %>% 
      filter(region == input$who_regions_for_chordNparset & year == input$who_years_for_parset) %>%
      select(country, value) %>%
      group_by(country, variable = value) %>% 
      summarize(counts = n()) %>%
      ungroup() %>%
      arrange(desc(counts))
    
  })
  
  
  output$who_parsetPlot <- renderParset({
    
    parset(who_data_parset(),
           dimensions = c('country', 'variable'), 
           value = htmlwidgets::JS("function(d){return d.counts}"), 
           tension = 0.5, spacing = 50)
    
  })
  
  
  output$who_summaryPlots <- renderUI({
    
    if(class(who_data()$value) == "numeric") {
      
      chorddiagOutput("who_chordPlot", height = 600)%>% withSpinner(color="#0dc5c1")
      
    }
    
    else if(class(who_data()$value) == "character") {
      
      parsetOutput("who_parsetPlot", height = 700, width = '200%')%>% withSpinner(color="#0dc5c1")
      
    }
    
    
  })
  
  
  who_data_ts <- reactive({
    
    who_data() %>%
      dplyr::select(-c(gho, publishstate)) %>%
      reshape2::melt(id.vars = c("year", "value")) %>% 
      magrittr::set_colnames(c("year", "gho_value", "variable", "variable_value")) %>% 
      mutate_if(is.factor, as.character) %>% 
      filter(!is.na(variable_value)& !is.na(gho_value)) 
    
  })
  
  
  output$who_ts_variable <- renderUI({
    
    if(class(who_data()$value) == "numeric") {
      
      selectInput(inputId = "who_ts_variable",
                  label = "Select grouping variable",
                  choices = sort(unique(who_data_ts()$variable), decreasing = TRUE))
      
    }
    
    
  })
  
  output$who_ts_nonCountry <- renderHighchart({
    
    highchart(type = "stock") %>%
      
      hc_add_series_df(who_data_ts() %>%
                         filter(variable == input$who_ts_variable) %>% 
                         dplyr::group_by(year = as.Date(paste0(year, "-01-01")),
                                         variable_value) %>% 
                         summarize(value = mean(as.numeric(gho_value), na.rm = T)) %>%
                         filter(variable_value %in% input$who_ts_var_value),
                       "column", 
                       x=year,
                       y=value,
                       group = variable_value) %>%
      hc_rangeSelector(selected = 2) %>% 
      hc_add_theme(hc_theme_flat()) %>%
      hc_legend(enabled = T) %>% 
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = T)
      )%>%
      hc_tooltip(pointFormat = "{point.variable_value}:{point.value:.2f}")%>%
      hc_title(text = paste0("<b>", input$who_codes, "</b>"),
               margin = 20, align = "left",
               style = list(color = "#2b908f", useHTML = TRUE)) %>%
      hc_subtitle(text = "Data sourced from the <i>WHO</i> database",
                  align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
    
  })
  
  output$who_ts_Country <- renderHighchart({
    
    highchart(type = "stock") %>%
      
      hc_add_series_df(who_data_ts() %>%
                         filter(variable == input$who_ts_variable) %>% 
                         dplyr::group_by(year = as.Date(paste0(year, "-01-01")), variable_value) %>% 
                         summarize(value = mean(as.numeric(gho_value), na.rm = T))%>%
                         filter(variable_value %in% input$who_ts_var_value),
                       "line", 
                       x=year,
                       y=value,
                       group = variable_value) %>%
      hc_add_series_df(who_data_ts() %>%
                         filter(variable == input$who_ts_variable ) %>% 
                         dplyr::group_by(year = as.Date(paste0(year, "-01-01")), variable_value) %>% 
                         summarize(value = mean(as.numeric(gho_value), na.rm = T))%>%
                         filter(variable_value %in% input$who_ts_var_value),
                       "scatter", 
                       x=year,
                       y=value,
                       group = variable_value) %>%
      hc_rangeSelector(selected = 2) %>% 
      hc_add_theme(hc_theme_flat()) %>%
      hc_legend(enabled = F) %>% 
      hc_tooltip(pointFormat = "{point.variable_value}:{point.value:.2f}")%>%
      hc_title(text = paste0("<b>", input$who_codes, "</b>"),
               margin = 20, align = "left",
               style = list(color = "#2b908f", useHTML = TRUE)) %>%
      hc_subtitle(text = "Data sourced from the <i>WHO</i> database",
                  align = "left", style = list(color = "#90ed7d", fontWeight = "bold"))
    
  })
  
  
  output$who_ts_var_value <- renderUI({
    
    if(class(who_data()$value) == "numeric") {
      
      who_data_ts() %>%
        filter(variable == input$who_ts_variable ) %>% 
        select(variable_value) -> dd
      
      
      selectizeInput(inputId = "who_ts_var_value",
                     label = "Select variables",
                     choices = sort(unique(dd$variable_value)),
                     selected = sample(unique(dd$variable_value), 3),
                     multiple = T)
      
    }
    
  })
  
  
  
  
  output$who_ts <- renderUI({
    
    if(class(who_data()$value) == "numeric") {
      
      if(input$who_ts_variable != "country"){ highchartOutput("who_ts_nonCountry", height = 600)%>% withSpinner(color="#0dc5c1")}
      
      else if(input$who_ts_variable == "country"){highchartOutput("who_ts_Country", height = 600)%>% withSpinner(color="#0dc5c1")}
      
    }
    
  })
  
  
  ## OECD OUTPUT -------
  
  oecd_dataset_id <- reactive({
    
    dataset_list %>% 
      mutate_if(is.factor, as.character) %>% 
      filter(title == input$oecd_dataset) %>% 
      select(id) -> id; id$id
    
  })
  
  
  oecd_data_str <- reactive({
    
    get_data_structure(oecd_dataset_id())
    
  })
  
  oecd_time_range <- reactive({
    
    
    tt_range_TIME <- as.numeric(oecd_data_str()$TIME$id[nchar(oecd_data_str()$TIME$id) == 4])
    tt_range_YEA <- as.numeric(oecd_data_str()$YEA$id[nchar(oecd_data_str()$YEA$id) == 4])
    
    unique(append(tt_range_TIME, tt_range_YEA))
    
    
  })
  
  output$oecd_time_range <- renderUI({
    
    
    sliderInput(inputId = "oecd_time_range",
                label = "Select time period",
                min = min(oecd_time_range()),
                max = max(oecd_time_range()),
                value = c(round(quantile(oecd_time_range())[4],0),
                          round(quantile(oecd_time_range())[5],0)),
                sep = "")
    
  })
  
  oecd_data_raw <- reactive({
    
    if(input$oecd_quickSpain == "Enable"){
      
      OECD::get_dataset(
        
        dataset =  oecd_dataset_id(),
        filter = "ESP",
        start_time = input$oecd_time_range[1],
        end_time = input$oecd_time_range[2])
      
    }
    else if(input$oecd_quickSpain == "Disable"){
      
      OECD::get_dataset(
        
        dataset =  oecd_dataset_id(),
        start_time = input$oecd_time_range[1],
        end_time = input$oecd_time_range[2])
      
    }
    
    
  })
  
  
  oecd_data_clean <- reactive({
    
    
    
    dstruc_df <- lapply(names(oecd_data_str()),
                        function(current_name)
                          transform(oecd_data_str()[[current_name]],
                                    variable = current_name))
    
    dstruc_df <- lapply(dstruc_df, function(df) df %>% mutate_if(is.factor, as.character))
    dstruc_df <- data.table::rbindlist(dstruc_df[-1])
    
    df <- oecd_data_raw()
    
    nn_df <- names(df); nn_df[!(nn_df %in% c("obsTime", "obsValue"))] -> nn_df
    
    
    
    for (i in nn_df) {
      
      df  %>% merge(dstruc_df %>%
                      filter(variable == i) %>%
                      select(id, label),
                    by.x = i,
                    by.y = "id") -> df
      
    }
    
    
    names(df)[!(names(df) %in% append(nn_df, c("obsTime", "obsValue")))] <- oecd_data_str()$VAR_DESC[oecd_data_str()$VAR_DESC$id %in% nn_df, "description"]
    
    df[!(names(df) %in% nn_df)] -> df
    
    df$obsYear <- as.integer(substr(df$obsTime, start = 1, stop = 4))
    
    df
    
    
  })
  
  
  output$oecd_data_clean_vars_Spain <- renderUI({
    
    nn <- names(oecd_data_clean())
    nn <- nn[!(nn %in% c("obsTime", "obsYear", "obsValue"))]
    
    selectizeInput(inputId = "oecd_data_clean_vars_Spain",
                   label = "Select up to two grouping variables",
                   choices = sort(nn),
                   selected = sample(nn,2),
                   multiple = T)
  })
  
  output$oecd_data_clean_vars_Entire <- renderUI({
    
    nn <- names(oecd_data_clean())
    nn <- nn[!(nn %in% c("obsTime", "obsYear", "obsValue"))]
    
    selectizeInput(inputId = "oecd_data_clean_vars_Entire",
                   label = "Select one grouping variable",
                   choices = sort(nn),
                   selected = sample(nn,1),
                   multiple = F)
  })
  
  output$oecd_data_clean_vars <- renderUI({
    
    
    if(input$oecd_quickSpain == "Enable"){
      
      htmlOutput("oecd_data_clean_vars_Spain")
    }
    
    
    
    else if(input$oecd_quickSpain == "Disable"){
      
      htmlOutput("oecd_data_clean_vars_Entire")
    }
    
    
    
    
  })
  
  
  oecd_data_plot_Spain <- reactive({
    
    data.frame(Year = oecd_data_clean()$obsYear,
               Value = oecd_data_clean()$obsValue,
               Variable_1 = oecd_data_clean()[,input$oecd_data_clean_vars_Spain[1]],
               Variable_2 = oecd_data_clean()[,input$oecd_data_clean_vars_Spain[2]]) %>%
      # filter(vv_geo %in% input$oecd_countries_AnimPlot) %>% 
      mutate_if(is.factor, as.character)
    
  })
  
  
  output$oecd_AnimPlot_Spain <- renderPlotly({
    
    oecd_data_plot_Spain() %>%
      group_by(Year, Variable_1, Variable_2) %>%
      summarize(Value = mean(Value, na.rm = T)) %>% 
      plot_ly(x = ~Variable_1, 
              y = ~Value, 
              size = ~Value, 
              text = ~Variable_2, 
              hoverinfo = "text")  %>%
      add_markers(color = ~Variable_2, alpha = 0.2, alpha_stroke = 0.2, showlegend = F) %>%
      add_markers(color = ~Variable_2, frame = ~Year, ids = ~Variable_1, showlegend = F) %>%
      animation_opts(1000,  redraw = FALSE) %>%
      animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
      animation_slider(currentvalue = list(prefix = "Year ", font = list(color="grey")))
    
    
  })
  
  
  output$oecd_data_clean_GeoVar <- renderUI({
    
    nn <- names(oecd_data_clean())
    nn <- nn[!(nn %in% c("obsTime", "obsYear", "obsValue"))]
    
    if(input$oecd_quickSpain == "Disable"){
      
      selectInput(inputId = "oecd_data_clean_GeoVar",
                  label = "Variable corresponding to geographic units",
                  choices = sort(nn),
                  selected = "Country")
      
    }
    
  })
  
  
  output$oecd_countries_AnimPlot <- renderUI({
    
    if(input$oecd_quickSpain == "Disable"){
      
      
      selectizeInput(inputId = "oecd_countries_AnimPlot",
                     label = "Units to add to the plot",
                     choices = unique(oecd_data_clean()[,input$oecd_data_clean_GeoVar]),
                     selected = sample(unique(oecd_data_clean()[,input$oecd_data_clean_GeoVar]),5),
                     multiple = T)
    }
    
    
  })
  
  oecd_data_plot_Entire <- reactive({
    
    data.frame(Year = oecd_data_clean()$obsYear,
               Value = oecd_data_clean()$obsValue,
               Unit = oecd_data_clean()[,input$oecd_data_clean_GeoVar],
               Variable = oecd_data_clean()[,input$oecd_data_clean_vars_Entire]) %>%
      filter(Unit %in% input$oecd_countries_AnimPlot) %>% 
      mutate_if(is.factor, as.character)
    
  })
  
  
  output$oecd_AnimPlot_Entire <- renderPlotly({
    
    oecd_data_plot_Entire() %>%
      group_by(Year, Unit, Variable) %>%
      summarize(Value = mean(Value, na.rm = T)) %>% 
      plot_ly(x = ~Unit, 
              y = ~Value, 
              size = ~Value, 
              text = ~Variable, 
              hoverinfo = "text")  %>%
      add_markers(color = ~Variable, alpha = 0.2, alpha_stroke = 0.2, showlegend = F) %>%
      add_markers(color = ~Variable, frame = ~Year, ids = ~Unit, showlegend = F) %>%
      animation_opts(1000,  redraw = FALSE) %>%
      animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
      animation_slider(currentvalue = list(prefix = "Year ", font = list(color="grey")))
    
    
  })
  
  
  
  
  output$oecd_AnimPlot <- renderUI({
    
    if(input$oecd_quickSpain == "Enable"){
      
      plotlyOutput("oecd_AnimPlot_Spain", width = '90%') %>% withSpinner(color="#0dc5c1")
      
    }
    
    else if(input$oecd_quickSpain == "Disable"){
      
      plotlyOutput("oecd_AnimPlot_Entire", width = '90%') %>% withSpinner(color="#0dc5c1")
      
    }
    
    
  })
  
  
  
  output$oecd_heatmap_GeoVar <- renderUI({
    
    nn <- names(oecd_data_clean())
    nn <- nn[!(nn %in% c("obsTime", "obsYear", "obsValue"))]
    
    selectInput(inputId = "oecd_heatmap_GeoVar",
                label = "Geographic unit",
                choices = sort(nn),
                selected = sort(nn)[2])
  })
  
  output$oecd_heatmap_SelVar <- renderUI({
    
    nn <- names(oecd_data_clean())
    nn <- nn[!(nn %in% c("obsTime", "obsYear", "obsValue"))]
    
    selectInput(inputId = "oecd_heatmap_SelVar",
                label = "Grouping variable",
                choices = sort(nn),
                selected = sort(nn)[3])
  })
  
  
  
  output$oecd_heatmap_cntrs <- renderUI({
    
    
    selectInput(inputId = "oecd_heatmap_cntrs",
                label = "Render heatmap for:",
                choices = sort(unique(oecd_data_clean()[,input$oecd_heatmap_GeoVar]))
                
    )
    
  })
  
  
  oecd_data_heatmap <- reactive({
    
    if(input$oecd_replaceNA == "Yes"){
      
      data.frame(Year = oecd_data_clean()$obsYear,
                 Value = oecd_data_clean()$obsValue,
                 vv_geo = oecd_data_clean()[,input$oecd_heatmap_GeoVar],
                 vv = oecd_data_clean()[,input$oecd_heatmap_SelVar]) %>%
        filter(vv_geo %in% input$oecd_heatmap_cntrs) %>% 
        mutate_if(is.factor, as.character) %>% 
        group_by(Year, vv) %>%
        summarize(values = mean(Value, na.rm = T)) %>%
        ungroup() %>%
        data.table::dcast(vv~Year) %>% 
        set_rownames(.$vv) %>%
        select(-vv) %>%
        replace(., is.na(.),0) %>% 
        as.matrix()
      
    }
    
    else if(input$oecd_replaceNA == "No"){
      
      data.frame(Year = oecd_data_clean()$obsYear,
                 Value = oecd_data_clean()$obsValue,
                 vv_geo = oecd_data_clean()[,input$oecd_heatmap_GeoVar],
                 vv = oecd_data_clean()[,input$oecd_heatmap_SelVar]) %>%
        filter(vv_geo %in% input$oecd_heatmap_cntrs) %>% 
        mutate_if(is.factor, as.character) %>% 
        group_by(Year, vv) %>%
        summarize(values = mean(Value, na.rm = T)) %>%
        ungroup() %>%
        data.table::dcast(vv~Year) %>% 
        set_rownames(.$vv) %>%
        select(-vv) %>%
        na.omit()%>% 
        as.matrix()
      
    }
    
    
  })
  
  output$oecd_heatmaps <- renderHighchart({
    
    hchart(oecd_data_heatmap()) %>%
      hc_colorAxis(stops = color_stops(colors = viridis::viridis(10))) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE)
    
    
  })
  
  
})