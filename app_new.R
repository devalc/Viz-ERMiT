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
library(ggthemes, quietly = TRUE)
library(bslib, quietly = TRUE)
library(sf, quietly = TRUE)
library(plotly, quietly = TRUE)
library(sfarrow, quietly = TRUE)

## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

## --------------------------------------------------------------------------------------##
## data

df = sfarrow::st_read_parquet("./data/portland202203_with_ermit_shps_subcatchments_wgs84_split_wshed_and_scen.parquet")
bbox <- st_bbox(df) %>% as.vector()

## --------------------------------------------------------------------------------------##
## Leaflet NA legend position fix 

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

## ----------------------------------define UI------------------------------------------##
# 
ui <- navbarPage(title = div("Viz-ERMiT",
                             
                             HTML(html_fix),
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
                 theme = bslib::bs_theme(bootswatch = "darkly",font_scale = 1.2,version =5,
                                         bg = "#202123", fg = "#B8BCC2",
                                         primary = "#6D597A", secondary = "#EA80FC",
                                         base_font = font_google("Righteous")),
                 
                 
                  
)


## ----------------------------------define server logic------------------------------------------##

server <- function(input, output, session) {
  
  
  
  
  
  
}


## --------------------------------------------------------------------------------------##
shinyApp(ui, server)