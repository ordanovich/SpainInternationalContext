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

get_eurostat_toc() %>% filter(type %in% c("dataset", "table")) %>% distinct() %>% 
            mutate(table_w_code =  paste0(title, " (", code, ")")) -> toc

nuts <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = "all") %>% 
            select(-c(id, NUTS_NAME, FID))

data("wrld_simpl"); wrld_simpl%>% st_as_sf() -> wrld_simpl

dataset_list <- OECD::get_datasets()

startData <- NULL

letters_only <- function(x) !grepl("[^A-Za-z]", gsub('[[:punct:] ]+','', x))

numbers_only <- function(x) !grepl("\\D", gsub('[[:punct:] ]+','', x))

clean_value <- function(x){
            
            if(unique(letters_only(x)) == TRUE) {
                        
                        as.character(tolower(trimws(gsub('[[:punct:] ]+',' ', x))))
                        
            }
            
            else if(unique(numbers_only(x)) == TRUE) {
                        
                        as.numeric(gsub(' ', '', (gsub("\\[[^}]*\\]", "", x))))
                        
            }
            
}