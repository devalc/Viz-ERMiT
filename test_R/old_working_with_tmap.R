## --------------------------------------------------------------------------------------##
##
## Script name: app.R
##
## Purpose of the script: Prototype shiny application to visualize ERMiT runs integrated
## with the weppcloud results for the Portland runs
##
## Author: Chinmay Deval
##
## Created On: 2022-07-13
##
## Copyright (c) Chinmay Deval, 2022
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##  Notes:
##   
##
## --------------------------------------------------------------------------------------##

## --------------------------clear environment and console-------------------------------##
rm(list = ls())
cat("\014")

## ----------------------------------Load packages---------------------------------------##

library(shiny, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
library(shinycustomloader, quietly = TRUE)
library(bslib, quietly = TRUE)
library(sf, quietly = TRUE)
library(tmap, quietly = TRUE)


## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

## --------------------------------------------------------------------------------------##
## data

df = readRDS("./data/portland202203_with_ermit_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")
bbox <- st_bbox(df) %>% as.vector()

## ----------------------------------define UI------------------------------------------##
# 
ui <- navbarPage(title = div("Viz-WEPPcloud with ERMiT",
                             div(
                               tags$a(
                                 href = "https://forest.moscowfsl.wsu.edu/fswepp/",
                                 tags$img(
                                   src = 'FS.png',
                                   style = "position:fixed;right: 75px;top: 5px;padding-bottom:10px;",
                                   height = 70
                                 )
                               ),
                               tags$a(
                                 href = "https://www.uidaho.edu/",
                                 tags$img(
                                   src = 'UI.jpg',
                                   style = "position:fixed;right: 135px;top: 5px;padding-bottom:10px;",
                                   height = 70
                                 )
                               ),
                               tags$a(
                                 href = "https://github.com/devalc/Viz-WEPPcloud_with_ERMiT",
                                 tags$img(
                                   src = "GitHub-Mark.png",
                                   style = "position:fixed;right: 10px;top: 5px;padding-bottom:10px;",
                                   height = 70
                                 )
                               )
                             )),
                 
                 windowTitle = "Viz-WEPPcloud with ERMiT",
                 
                 id="nav",
                 theme = bslib::bs_theme(bootswatch = "darkly",font_scale = 1.2,
                                         bg = "#202123", fg = "#B8BCC2",
                                         primary = "#6D597A", secondary = "#EA80FC",
                                         base_font = font_google("Righteous")),
                 
                 
                 tabPanel("",
                          
                          div(class="outer",
                              
                              tags$head(
                                # Include custom CSS
                                includeCSS("www/styles.css"),
                                includeScript("gtag.js")
                              ),
                              
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", 
                                            class = "panel panel-default",
                                            fixed = TRUE, draggable = TRUE, 
                                            top = 140, left = 180, right = "auto",
                                            bottom = "auto", width = 330, height = "auto",
                                            
                                            pickerInput(
                                              inputId = "wat_sel",
                                              label = "Select watershed",
                                              choices = unique(df$Watershed),
                                              selected = unique(df$Watershed)[2],
                                              multiple = TRUE,
                                              options = list(
                                                `actions-box` = TRUE,
                                                `header` = "Select watershed ",
                                                `windowPadding` = 1,
                                                `width` = " css-width ",
                                                `size` = 6
                                              )
                                            ),
                                            
                                            pickerInput(
                                              inputId = "scen_sel",
                                              label = "Select scenario",
                                              choices = unique(df$Scenario),
                                              selected = unique(df$Scenario)[-1],
                                              options = list(
                                                `actions-box` = TRUE,
                                                `header` = "Select scenario ",
                                                `windowPadding` = 1,
                                                `width` = " css-width ",
                                                `size` = 6
                                              )
                                            ),
                                            
                                            pickerInput(
                                              inputId = "var_sel",
                                              label = "Select variable",
                                              choices = c("SdYd_kg_ha",
                                                          "sed_reduc_at_25probab",
                                                          "sed_reduc_at_50probab",
                                                          "sed_reduc_at_75probab"),
                                              selected = "sed_reduc_at_50probab",
                                              options = list(
                                                `actions-box` = TRUE,
                                                `header` = "Select variable ",
                                                `windowPadding` = 1,
                                                `width` = " css-width ",
                                                `size` = 6
                                              )
                                            )
                                            
                              )
                              
                          )
                          
                          
                 ) 
)


## ----------------------------------define server logic------------------------------------------##

server <- function(input, output, session) {
  
  df_subset <- reactive({
    req(input$wat_sel)
    req(input$scen_sel)
    df %>%
      dplyr::filter(Watershed %in% input$wat_sel) %>%
      dplyr::filter(Scenario %in% input$scen_sel)
  })
  
  
  output$map <- leaflet::renderLeaflet({
    req(df_subset())
    tm2 <-tm_basemap(leaflet::providers$Esri.WorldImagery,group = "WorldImagery") +
      tm_basemap(leaflet::providers$Esri.WorldTopoMap,group = "WorldTopoMap")  +
      tm_basemap(leaflet::providers$OpenStreetMap, group = "OpenStreetMap")+
      tm_basemap(leaflet::providers$Esri.WorldGrayCanvas,group = "WorldGrayCanvas") +
      tm_shape(df_subset(), name = "") +
      tmap::tm_polygons(input$var_sel,
                        id = "watershed",
                        palette = "viridis",
                        style = "kmeans",
                        midpoint = TRUE,
                        textNA = "No Erosion",
                        popup.vars = if (input$var_sel == "SdYd_kg_ha") {
                          c("Watershed", "Scenario","wepp_id", input$var_sel, "sed_reduc_at_25probab",
                            "sed_reduc_at_50probab", "sed_reduc_at_75probab")
                        }else{
                          c("Watershed", "Scenario","wepp_id", "SdYd_kg_ha", input$var_sel)}
      )+
      tmap::tm_layout(scale = 0.1,
                      title = "")
    
    tmap_leaflet(tm2,in.shiny = TRUE)  %>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE,
                 zoomAnimation = TRUE,position = "bottomright",height = 100)
  })
  
  
}


## --------------------------------------------------------------------------------------##
shinyApp(ui, server)