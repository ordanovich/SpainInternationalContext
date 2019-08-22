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

# Define UI for application that plots random distributions 
shinyUI(dashboardPage(
  skin = "purple",
  
  dashboardHeader(
  
  title = p("Spain in International Context", style = "font-size:16px"),
  titleWidth = 250,
  
  # Dropdown menu for notifications
  dropdownMenu(type = "notifications", badgeStatus = "danger",
               
               notificationItem(icon = icon("user-graduate"),
                                status = "success", 
                                "Learn more about LONGPOP",
                                href = "http://longpop-itn.eu/"
               ),
               
               notificationItem(icon = icon("hospital"), status = "success",
                                "Visit the Web Portal for Health",
                                href = "http://193.146.75.235/sample-apps/final_apps/layout/"
               ),
               
               
               notificationItem(icon = icon("code"), status = "success",
                                "Access the source code",
                                href = "https://github.com/ordanovich/SpainInternationalContext"
               )
  )
),


dashboardSidebar(
  
  width = 250,
  
  sidebarMenu(style = "overflow.y: scroll;",
              # Setting id makes input$tabs give the tabName of currently-selected tab
              id = "tabs",
              
              menuItem("Eurostat", tabName = "eurostat", icon = icon("euro-sign"),
                       badgeLabel = "ropengov/eurostat", badgeColor =  "aqua"),
              
              fluidRow(
                
                column(width = 12,
                       
                       socialBox(
                         title = "Details",
                         collapsible = T,
                         closable =  F,
                         width = 12,
                         subtitle = "External links",
                         footer_padding = F,
                         src = "https://github.com/ordanovich/images/blob/master/logo_eu.png?raw=true",
                         comments = tagList(
                           boxComment(
                             src = "http://442r58kc8ke1y38f62ssb208-wpengine.netdna-ssl.com/wp-content/uploads/2014/04/shiny.png",
                             title = "Interactive app",
                             tags$a(href="http://193.146.75.235/sample-apps/final_apps/eurostat_download/", 
                                    p("Interface for bulk download", style = "color:purple"))
                           ),
                           boxComment(
                             src = "https://github.githubassets.com/images/modules/logos_page/Octocat.png",
                             title = "Reproducible code",
                             tags$a(href="https://github.com/ordanovich/downloadEUROSTAT#interactive-application-for-programmatic-data-retrieval-from-eurostat", 
                                    p("Retrieve, visualize & report", style = "color:purple"))
                           )
                           
                         )
                       )
                )
              ) ,
              
              menuItem("World Bank", tabName = "worldbank", icon = icon("piggy-bank"),
                       badgeLabel = "GIST-ORNL/wbstats", badgeColor= "maroon"),
              
              fluidRow(
                
                column(width = 12,
                       
                       socialBox(
                         title = "Details",
                         collapsible = T,
                         closable =  F,
                         width = 12,
                         subtitle = "External links",
                         footer_padding = F,
                         src = "https://github.com/ordanovich/images/blob/master/logo_wb.png?raw=true",
                         comments = tagList(
                           boxComment(
                             src = "http://442r58kc8ke1y38f62ssb208-wpengine.netdna-ssl.com/wp-content/uploads/2014/04/shiny.png",
                             title = "Interactive app",
                             tags$a(href="http://193.146.75.235/sample-apps/final_apps/worldbank_download/", 
                                    p("Interface for bulk download", style = "color:purple"))
                           ),
                           boxComment(
                             src = "https://github.githubassets.com/images/modules/logos_page/Octocat.png",
                             title = "Reproducible code",
                             tags$a(href="https://github.com/ordanovich/downloadWORLDBANK#interactive-application-for-programmatic-data-retrieval-from-worldbank", 
                                    p("Retrieve, visualize & report", style = "color:purple"))
                           )
                           
                         )
                       )
                )
              ) ,
              
              menuItem("WHO", tabName = "who", icon = icon("heartbeat"),
                       badgeLabel = "expersso/WHO", badgeColor= "yellow"),
              
              fluidRow(
                
                column(width = 12,
                       
                       socialBox(
                         title = "Details",
                         collapsible = T,
                         closable =  F,
                         width = 12,
                         subtitle = "External links",
                         src = "https://github.com/ordanovich/images/blob/master/logo_who.png?raw=true",
                         footer_padding = F,
                         comments = tagList(
                           boxComment(
                             src = "http://442r58kc8ke1y38f62ssb208-wpengine.netdna-ssl.com/wp-content/uploads/2014/04/shiny.png",
                             title = "Interactive app",
                             tags$a(href="http://193.146.75.235/sample-apps/final_apps/who_download/", 
                                    p("Interface for bulk download", style = "color:purple"))
                           ),
                           boxComment(
                             src = "https://github.githubassets.com/images/modules/logos_page/Octocat.png",
                             title = "Reproducible code",
                             tags$a(href="https://github.com/ordanovich/downloadWHO#interactive-application-for-programmatic-data-retrieval-from-whogho", 
                                    p("Retrieve, visualize & report", style = "color:purple"))
                           )
                           
                         )
                       )
                )
              ) ,
              
              menuItem("OECD", tabName = "oecd", icon = icon("users"),
                       badgeLabel = "expersso/OECD", badgeColor= "olive"),
              
              fluidRow(
                
                column(width = 12,
                       
                       socialBox(
                         title = "Details",
                         collapsible = T,
                         closable =  F,
                         width = 12,
                         subtitle = "External links",
                         src = "https://github.com/ordanovich/images/blob/master/logo_oecd.png?raw=true",
                         footer_padding = F,
                         comments = tagList(
                           boxComment(
                             src = "http://442r58kc8ke1y38f62ssb208-wpengine.netdna-ssl.com/wp-content/uploads/2014/04/shiny.png",
                             title = "Interactive app",
                             tags$a(href="http://193.146.75.235/sample-apps/final_apps/oecd_download/", 
                                    p("Interface for bulk download", style = "color:purple"))
                           ),
                           boxComment(
                             src = "https://github.githubassets.com/images/modules/logos_page/Octocat.png",
                             title = "Reproducible code",
                             tags$a(href="https://github.com/ordanovich/downloadWHO#interactive-application-for-programmatic-data-retrieval-from-whogho", 
                                    p("Retrieve, visualize & report", style = "color:purple"))
                           )
                           
                         )
                       )
                )
              )
  )
), 



dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tags$head(
    tags$style(
      HTML('.highcharts-chart{ spacingBottom: 0,
                                                               spacingTop: 0,
                                                               spacingLeft: 0,
                                                               spacingRight: 0,}')
    )
  ),
  
  tags$head(tags$style("
                                                       .plot-container.plotly {
                                                       max-height: 400px;
                                                       float: center;
                                                       max-width: 1200px;
                                                       overflow-y: auto;
                                                       overflow-x: auto;
                                                       }")),
  
  
  
  tabItems(
    
    ## EUROSTAT TAB ------
    
    tabItem("eurostat",
            
            
            
            fluidRow(
              
              ## UI EUROSTAT TS ----
              
              column(width = 12, align = "justify",
                     
                     gradientBox(
                       title = "Temporal series",
                       width = 12,
                       icon = "fa fa-chart-area",
                       gradientColor = "purple",
                       boxToolSize = "xs",
                       closable = TRUE,
                       
                       fluidRow(
                         column(width = 3,
                                
                                textInput(inputId="eurostat_search_term",
                                          label = "Search by keyword",
                                          value = "mortality",
                                          placeholder="mortality")),
                         
                         column(width = 6,
                                
                                htmlOutput("eurostat_search_result")),
                         
                         column(width = 3,
                                
                                htmlOutput("eurostat_unit"))),
                       
                       
                       fluidRow(
                         
                         column(width = 3,
                                
                                uiOutput("eurostat_nuts_unique")
                         ),
                         
                         column(width = 3,
                                
                                htmlOutput("eurostat_param1")),
                         
                         column(width = 3,
                                
                                htmlOutput("eurostat_param2"))
                       ),
                       
                       
                       footer = 
                         
                         tabsetPanel(type = "pills",
                                     
                                     tabPanel("Scatter plot", 
                                              
                                              fluidRow(
                                                
                                                
                                                fluidRow(column(width = 12,align = "center",
                                                                
                                                                
                                                                
                                                                column(width = 10,align = "center",
                                                                       
                                                                       fluidRow(column(width = 12,
                                                                                       uiOutput("eurostat_ts")
                                                                       ),
                                                                       
                                                                       
                                                                       column(width = 12,
                                                                              fluidRow(
                                                                                
                                                                                column(width = 3,
                                                                                       sliderInput(inputId = "width",
                                                                                                   label = "Plot width",
                                                                                                   min = 0,
                                                                                                   max = 3000,
                                                                                                   value = 1200,
                                                                                                   step = 100) 
                                                                                ),
                                                                                
                                                                                column(width = 3,
                                                                                       sliderInput(inputId = "height",
                                                                                                   label = "Plot height",
                                                                                                   min = 0,
                                                                                                   max = 3000,
                                                                                                   value = 400,
                                                                                                   step = 100) 
                                                                                ),
                                                                                
                                                                                column(width = 2,
                                                                                       selectInput("theme", label = "Theme",
                                                                                                   choices = ls("package:ggthemes")[grepl("theme_", ls("package:ggthemes"))],
                                                                                                   selected="theme_pander")
                                                                                ),
                                                                                
                                                                                column(width = 2,
                                                                                       uiOutput("eurostat_ts_palette_pkg")
                                                                                ),
                                                                                
                                                                                column(width = 2,
                                                                                       uiOutput("eurostat_ts_palette")
                                                                                )
                                                                                
                                                                              )
                                                                       ))),
                                                                column(width = 2,
                                                                       align = "center",
                                                                       
                                                                       fluidRow(column(width = 12,
                                                                                       
                                                                                       radioButtons("regline", label = "Trend line for Spain",
                                                                                                    choices = c("Add", "Hide"),
                                                                                                    inline=T )
                                                                       )),
                                                                       
                                                                       fluidRow(column(width = 12,
                                                                                       
                                                                                       selectInput("method", label = "Method for smoothed conditional means",
                                                                                                   choices = c("auto", "lm", "glm", "gam", "loess") )
                                                                       )),
                                                                       fluidRow(column(width = 12,
                                                                                       sliderInput("conf", label = "Confidence interval",
                                                                                                   min = .9,
                                                                                                   max = 1,
                                                                                                   value = .95,
                                                                                                   step = .01)
                                                                       )),
                                                                       
                                                                       fluidRow(column(width = 12,
                                                                                       sliderInput("span", label = "Smoothing factor",
                                                                                                   min = 0,
                                                                                                   max = 1,
                                                                                                   value = .5,
                                                                                                   step = .1)
                                                                       )),
                                                                       
                                                                       fluidRow(column(width = 12,
                                                                                       
                                                                                       selectInput("scales", label = "If facets enabled, scales are",
                                                                                                   choices = c("fixed", "free") )
                                                                       ))
                                                                       
                                                                       
                                                                )
                                                )
                                                
                                                )
                                              ) 
                                     ),
                                     
                                     tabPanel("Comparison statistics",
                                              
                                              fluidRow(
                                                
                                                column(width = 5,
                                                       align = "justify",
                                                       uiOutput("eurostat_betweenstats_regionsSpain")),
                                                
                                                column(width = 5,
                                                       align = "justify",
                                                       uiOutput("eurostat_betweenstats_regionsOther")),
                                                column(width = 2,
                                                       align = "center",
                                                       fluidRow(radioButtons(inputId = "eurostat_betweestats_tagOutliers",
                                                                             label = "Tag outliers",
                                                                             choices =  c("No" = FALSE,
                                                                                          "Yes" = TRUE),
                                                                             selected = TRUE,
                                                                             inline = T
                                                       )),
                                                       fluidRow(downloadButton('eurostat_downloadPlot', 'Download Plot'))
                                                       
                                                )
                                              ), 
                                              
                                              fluidRow(
                                                
                                                column(width = 12,
                                                       align = "center",
                                                       
                                                       plotOutput("eurostat_betweenstats", height = "400px")%>% withSpinner(color="#0dc5c1")
                                                )
                                              )
                                              
                                     )
                                     
                                     
                         )
                       
                       
                       
                       
                     )
                     ## here box ends
                     
                     
              )
            ),
            
            fluidRow(
              column(width = 12, align = "justify",
                     gradientBox(
                       title = "Aggregated value for Spain at selected administrative level",
                       width = 12,
                       icon = "fa fa-magic",
                       gradientColor = "purple", 
                       
                       boxToolSize = "xs", 
                       closable = TRUE,
                       
                       footer = 
                         
                         tabsetPanel(type = "pills",
                                     
                                     tabPanel("Heatmap (Spain only)",
                                              
                                              
                                              fluidRow(
                                                
                                                column(width = 12,
                                                       
                                                       fluidRow(column(width = 12,
                                                                       align = "center",
                                                                       uiOutput("eurostat_heatmap")
                                                       ))))),
                                     tabPanel("Overview map (entire area)",
                                              
                                              
                                              column(width = 12, 
                                                     align = "center",
                                                     
                                                     fluidRow(
                                                       column(width = 8,
                                                              uiOutput("eurostat_map_time")),
                                                       
                                                       column(width = 4)
                                                     ),
                                                     
                                                     
                                                     fluidRow(
                                                       
                                                       column(width = 8,
                                                              uiOutput("eurostat_map"),
                                                              dataTableOutput("test0")
                                                       ),
                                                       
                                                       column(width = 4,
                                                              
                                                              
                                                              sliderInput(inputId = "eurostat_map_bins",
                                                                          label = "Bins",
                                                                          min = 1,
                                                                          max = 10,
                                                                          value = 5),
                                                              
                                                              spectrumInput(
                                                                inputId = "myColor_eu_high",
                                                                label = "Maximum values",
                                                                choices = list(
                                                                  
                                                                  as.list(brewer.pal(n = 9, name = "PiYG")),
                                                                  as.list(brewer.pal(n = 9, name = "RdBu")),
                                                                  as.list(brewer.pal(n = 9, name = "PRGn")),
                                                                  as.list(brewer.pal(n = 9, name = "RdYlGn"))
                                                                ),
                                                                options = list(`toggle-palette-more-text` = "Show more")
                                                              ),
                                                              
                                                              spectrumInput(
                                                                inputId = "myColor_eu_mid",
                                                                label = "Mean values",
                                                                choices = list(
                                                                  as.list(brewer.pal(n = 8, name = "Pastel1")),
                                                                  as.list(brewer.pal(n = 8, name = "Paired")),
                                                                  as.list(brewer.pal(n = 8, name = "Accent")),
                                                                  as.list(brewer.pal(n = 8, name = "Set2"))
                                                                ),
                                                                options = list(`toggle-palette-more-text` = "Show more")
                                                              ),
                                                              
                                                              spectrumInput(
                                                                inputId = "myColor_eu_low",
                                                                label = "Minimum values",
                                                                choices = list(
                                                                  as.list(rev(brewer.pal(n = 9, name = "PiYG"))),
                                                                  as.list(rev(brewer.pal(n = 9, name = "RdBu"))),
                                                                  as.list(rev(brewer.pal(n = 9, name = "PRGn"))),
                                                                  as.list(rev(brewer.pal(n = 9, name = "RdYlGn")))
                                                                ),
                                                                options = list(`toggle-palette-more-text` = "Show more")
                                                              ) ,
                                                              
                                                              selectInput(inputId = "eurostat_map_providerTiles",
                                                                          label = "Select Basemap",
                                                                          choices = providers %>% purrr::keep(~ grepl('^Esri',.)),
                                                                          selected = "Esri.WorldGrayCanvas")
                                                              
                                                              
                                                       )
                                                     )
                                                     
                                                     
                                              )
                                              
                                              
                                              
                                              
                                              
                                              
                                     )
                         )
                     ))
              
              
            )
            ## UI EUROSTAT MAP ----
            
            
            
            
    ),
    
    ## WORLDBANK TAB ------
    
    tabItem("worldbank",
            
            fluidRow(
              
              ## UI WORLDBANK TS ----
              
              column(width = 12, align = "justify",
                     gradientBox(
                       title = "Temporal series",
                       width = 12,
                       icon = "fa fa-chart-area",
                       gradientColor = "purple",
                       boxToolSize = "xs", 
                       closable = TRUE,
                       
                       fluidRow(
                         
                         column(
                           
                           width = 12,
                           
                           fluidRow( 
                             
                             column(width = 3,
                                    
                                    radioButtons(inputId = "wb_update",
                                                 label = "Update",
                                                 choices = c("Cached", "Latest update"),
                                                 selected = "Cached",
                                                 inline = T)),
                             
                             column(width = 3,
                                    
                                    uiOutput("wb_lang_display")
                                    
                             )
                           ),
                           
                           fluidRow(column(width = 4,
                                           
                                           textInput(inputId = "wb_search_term",
                                                     label = "Search for a specific topic",
                                                     value = "pollution"),
                                           tags$code("for multiple choice use '|' e.g. 'health|pollution'")
                           ),
                           
                           column(width = 2, uiOutput("wb_field_group_choice")),
                           
                           column(width = 2, uiOutput("wb_field_choice")),
                           
                           column(width = 4,
                                  
                                  uiOutput("wb_indicator"))),
                           
                           fluidRow(
                             
                             column(width = 3,
                                    
                                    sliderInput(inputId = "wb_startdate",
                                                label = "Start year",
                                                min = 1960,
                                                max = as.numeric(lubridate::year(Sys.Date())),
                                                value = as.numeric(lubridate::year(Sys.Date()))-10,
                                                step = 1,
                                                sep="")),
                             column(width = 3,      
                                    sliderInput(inputId = "wb_enddate",
                                                label = "End year",
                                                min = 1960,
                                                max = as.numeric(lubridate::year(Sys.Date())),
                                                value = as.numeric(lubridate::year(Sys.Date())),
                                                step = 1,
                                                sep="")
                             ),
                             
                             column(width = 3,
                                    
                                    radioButtons(inputId = "wb_freq",
                                                 label = "Frequency",
                                                 choices = c("Yearly " = "Y",
                                                             "Monthly" = "M",
                                                             "Quarterly " = "Q"),
                                                 selected = "Y",
                                                 inline = T
                                    )
                                    
                             )
                             
                           )
                           
                           
                         )
                         
                       ),
                       
                       footer = fluidRow(
                         column(width = 12,
                                
                                fluidRow(
                                  
                                  column(width = 6,
                                         align = "center",
                                         
                                         fluidRow(column(width = 12,
                                                         selectizeInput(inputId = "wb_countries",
                                                                        label = "Countries to compare Spain with",
                                                                        choices = sort(wbcountries()$country)[sort(wbcountries()$country) != "Spain"],
                                                                        selected = c("Portugal", "China", "United States", "Burundi"),
                                                                        multiple = T),
                                                         
                                                         fluidRow(column(width = 12,
                                                                         highchartOutput("wb_ts", width = "99%", height = 400) %>% withSpinner(color="#0dc5c1")
                                                         )
                                                         ),
                                                         
                                                         tags$br(),
                                                         
                                                         fluidRow(
                                                           column(width = 2),
                                                           column(width = 4,
                                                                  align = "center",
                                                                  spectrumInput(
                                                                    inputId = "wb_ts_color_for_spain",
                                                                    label = "Color to highlight Spain",
                                                                    choices = list(
                                                                      as.list(c("#ffcf00", "#74b25d", "#28652c", "#a3efff", "#14a8c7"))
                                                                    ),
                                                                    options = list(`toggle-palette-more-text` = "Show more")
                                                                  )
                                                           ),
                                                           
                                                           column(width = 4,
                                                                  align = "center",
                                                                  selectInput("wb_ts_theme", label = "Select Theme",  width = "100%",
                                                                              choices = c(FALSE, "db", "fivethirtyeight", "economist",
                                                                                          "darkunica", "gridlight", "sandsignika",
                                                                                          "null", "handdrwran", "chalk")
                                                                  )
                                                           ),
                                                           column(width = 2))
                                                         
                                         ))),
                                  
                                  column(width = 6,
                                         align = "center",
                                         
                                         fluidRow(
                                           
                                           column(width = 4,
                                                  uiOutput("wb_map_time")
                                           ),
                                           
                                           column(width = 4,
                                                  
                                                  selectInput(inputId = "wb_map_providerTiles",
                                                              label = "Select Basemap",
                                                              choices = providers %>% purrr::keep(~ grepl('^Esri',.)),
                                                              selected = "Esri.WorldGrayCanvas")),
                                           
                                           column(width = 4,
                                                  sliderInput(inputId = "wb_map_bins",
                                                              label = "Bins",
                                                              min = 1,
                                                              max = 10,
                                                              value = 3)
                                           )
                                         ),
                                         
                                         
                                         
                                         fluidRow(
                                           
                                           column(width = 12,
                                                  
                                                  leafletOutput("wb_map", height = 400)%>% withSpinner(color="#0dc5c1")
                                                  
                                                  
                                           )),
                                         
                                         fluidRow(
                                           column(width = 4,
                                                  spectrumInput(
                                                    inputId = "myColor_wb_high",
                                                    label = "Maximum values",
                                                    choices = list(
                                                      
                                                      as.list(brewer.pal(n = 9, name = "PiYG")),
                                                      as.list(brewer.pal(n = 9, name = "RdBu")),
                                                      as.list(brewer.pal(n = 9, name = "PRGn")),
                                                      as.list(brewer.pal(n = 9, name = "RdYlGn"))
                                                    ),
                                                    options = list(`toggle-palette-more-text` = "Show more")
                                                  )),
                                           
                                           
                                           
                                           column(width = 4,
                                                  spectrumInput(
                                                    inputId = "myColor_wb_mid",
                                                    label = "Mean values",
                                                    choices = list(
                                                      as.list(brewer.pal(n = 8, name = "Pastel1")),
                                                      as.list(brewer.pal(n = 8, name = "Paired")),
                                                      as.list(brewer.pal(n = 8, name = "Accent")),
                                                      as.list(brewer.pal(n = 8, name = "Set2"))
                                                    ),
                                                    options = list(`toggle-palette-more-text` = "Show more")
                                                  )),
                                           
                                           column(width = 4,
                                                  spectrumInput(
                                                    inputId = "myColor_wb_low",
                                                    label = "Minimum values",
                                                    choices = list(
                                                      as.list(rev(brewer.pal(n = 9, name = "PiYG"))),
                                                      as.list(rev(brewer.pal(n = 9, name = "RdBu"))),
                                                      as.list(rev(brewer.pal(n = 9, name = "PRGn"))),
                                                      as.list(rev(brewer.pal(n = 9, name = "RdYlGn")))
                                                    ),
                                                    options = list(`toggle-palette-more-text` = "Show more")
                                                  ))
                                         )
                                  )
                                  
                                  
                                )
                                
                                
                         )
                       )
                     ) ## end of box
              )
            ), ## end of row with box
            
            fluidRow(
              column(width = 12,
                     align = "justify",
                     gradientBox(
                       title = "Box/Violin plots for group comparisons in between-subjects designs",
                       width = 12,
                       icon = "fa fa-chart-area",
                       gradientColor = "purple",
                       boxToolSize = "xs", 
                       closable = TRUE,
                       
                       
                       fluidRow(
                         
                         column(width = 12,
                                
                                column(width = 2,
                                       
                                       align = "justify",
                                       
                                       selectInput(inputId = "wb_betweenstats_level",
                                                   label = "Select level of dissagregation",
                                                   choices = c("Regions", "Countries"),
                                                   selected = "Regions")
                                ),
                                
                                column(width = 8,
                                       align = "justify",
                                       uiOutput("wb_betweenstats_selected_unit")
                                       
                                       
                                       
                                ),
                                
                                column(width = 2,
                                       align = "center",
                                       
                                       fluidRow(radioButtons(inputId = "wb_betweestats_tagOutliers",
                                                             label = "Tag outliers",
                                                             choices =  c("No" = FALSE,
                                                                          "Yes" = TRUE),
                                                             selected = TRUE,
                                                             inline = T
                                       )),
                                       
                                       fluidRow(downloadButton('wb_downloadPlot', 'Download Plot'))
                                )    
                         )
                         
                         
                       ),
                       footer = fluidRow(
                         
                         column(width = 12,
                                align = "center",
                                
                                plotOutput("wb_betweenstats", height = "600px")%>% withSpinner(color="#0dc5c1")
                         )
                       )
                     ))
            )
    ), 
    
    
    ## WHO TAB ------
    
    tabItem("who",
            
            fluidRow(
              
              ## UI WHO CHORD ----
              
              column(width = 12, align = "justify",
                     gradientBox(
                       title = "Temporal series",
                       width = 12,
                       icon = "fa fa-chart-area",
                       gradientColor = "purple",
                       boxToolSize = "xs", 
                       closable = TRUE,
                       collapsed = T,
                       collapsible = T,
                       fluidRow(
                         
                         column(width = 12,
                                align = "justify",
                                
                                fluidRow(
                                  
                                  column(width = 6,
                                         align = "justify",
                                         
                                         selectizeInput(inputId = "who_codes",
                                                        label = "Select table",
                                                        choices = sort(WHO::get_codes()$display),
                                                        selected = "Cholera case fatality rate")
                                  ),
                                  
                                  column(width = 3,
                                         htmlOutput("who_ts_variable"))),
                                
                                fluidRow(
                                  
                                  column(width = 3,
                                         align = "justify",
                                         
                                         htmlOutput("who_regions_for_chordNparset")
                                  ),
                                  
                                  
                                  column(width = 3,
                                         uiOutput("who_years")
                                  ),
                                  
                                  column(width = 3,
                                         htmlOutput("who_ts_var_value")
                                  )
                                )
                                
                         )
                         
                       ),
                       footer = fluidRow(
                         
                         
                         column(width = 12,
                                align = "justify",
                                
                                fluidRow(
                                  
                                  column(width = 6,
                                         
                                         uiOutput("who_summaryPlots")
                                         
                                  ),
                                  
                                  
                                  column(width = 6,
                                         
                                         uiOutput("who_ts")
                                         
                                  )
                                ))
                         
                         
                       ))
                     
              )
            )
            
            
            
            
    ),
    
    
    ## UI OECD TS ----
    
    tabItem("oecd",
            
            
            fluidRow(
              
              
              column(width = 12, align = "justify",
                     
                     gradientBox(
                       title = "Temporal series",
                       width = 12,
                       icon = "fa fa-chart-area",
                       gradientColor = "purple",
                       boxToolSize = "xs",
                       closable = TRUE,
                       
                       fluidRow(
                         column(width = 2,
                                
                                
                                
                                radioButtons(inputId = "oecd_quickSpain",
                                             label = "Quick search for Spain only",
                                             choices = c("Enable", "Disable"),
                                             selected = "Enable",
                                             inline = T)),
                         
                         column(width = 5,   
                                selectInput(inputId = "oecd_dataset",
                                            label = "Select table",
                                            choices = dataset_list$title,
                                            selected = "Public sector debt by instrument coverage"
                                )),
                         
                         column(width = 5,
                                htmlOutput("oecd_time_range")
                         )
                       ),
                       
                       footer = fluidRow(
                         
                         column(width = 12,
                                
                                fluidRow(
                                  
                                  column(width = 6,
                                         
                                         uiOutput("oecd_AnimPlot"),
                                         
                                         fluidRow(
                                           
                                           column(width = 12,
                                                  
                                                  column(width = 4,
                                                         htmlOutput("oecd_data_clean_vars")),
                                                  
                                                  column(width = 4,
                                                         htmlOutput("oecd_data_clean_GeoVar")),
                                                  
                                                  column(width = 4,
                                                         htmlOutput("oecd_countries_AnimPlot"))      
                                                  
                                           )
                                         )
                                         
                                  ),
                                  
                                  column(width = 6,
                                         
                                         highchartOutput("oecd_heatmaps")%>% withSpinner(color="#0dc5c1"),
                                         
                                         fluidRow(
                                           
                                           column(width = 12,
                                                  
                                                  column(width = 3,
                                                         htmlOutput("oecd_heatmap_GeoVar")),
                                                  
                                                  column(width = 3,
                                                         htmlOutput("oecd_heatmap_cntrs")),
                                                  
                                                  column(width = 3,
                                                         htmlOutput("oecd_heatmap_SelVar")),
                                                  
                                                  column(width = 3,
                                                         radioButtons(inputId = "oecd_replaceNA",
                                                                      label = "Replace null values with zeros",
                                                                      choices = c("Yes", "No"),
                                                                      selected = "No",
                                                                      inline = T)
                                                  )
                                                  
                                           )
                                         )
                                         
                                         
                                         
                                  )
                                )
                         )
                       )
                     )
              )
            )
    )
  )
)
))