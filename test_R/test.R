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

## --------------------------------------------------------------------------------------##
## Leaflet NA legend position fix 

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

## ----------------------------------define UI------------------------------------------##
# 
ui <- navbarPage(title = div("Viz-WEPPcloud with ERMiT",
                             
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
                 theme = bslib::bs_theme(bootswatch = "darkly",font_scale = 1.2,
                                         bg = "#202123", fg = "#B8BCC2",
                                         primary = "#48A1D9", secondary = "#EA80FC",
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
    leaflet::leaflet(df)%>%
      leaflet::addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>%
      leaflet::addProviderTiles("Esri.WorldTopoMap",group = "WorldTopoMap") %>%
      leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") %>%
      leaflet::addProviderTiles("Esri.WorldGrayCanvas",group = "WorldGrayCanvas")%>%
      leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])%>%
      leaflet::addMiniMap(tiles = "Esri.WorldStreetMap",
                          toggleDisplay = TRUE,
                          zoomAnimation = TRUE,position = "bottomright",height = 100)%>%
      leaflet::addLayersControl(
        baseGroups =c("WorldImagery", "OpenStreetMap", "WorldTopoMap", "WorldGrayCanvas"),
        options = layersControlOptions(collapsed=FALSE)
      )%>% 
      addLegend(pal = colorpal(),
                values = ~get(unique(input$var_sel)),
                opacity = 0.7,
                title = ~as.character(input$var_sel),
                position = "bottomright",
                na.label = "No Erosion")
  })
  
  colorpal  <- reactive({
    req(df_subset())
    req(input$var_sel)
    colorNumeric("viridis", unique(df_subset()$input$var_sel)
    )
  })
  
 
  observe(leafletProxy("map", data = df_subset()) %>%
            leaflet::clearShapes() %>%
            leaflet::addPolygons(data = df_subset(),
                                 fillColor = ~colorpal()(get(input$var_sel)),
                                 weight = 0.7,
                                 opacity = 1,
                                 color = "white",
                                 dashArray = "3",
                                 fillOpacity = 0.7,
                                 highlightOptions = highlightOptions(weight = 2,
                                                                     color = "red",
                                                                     dashArray = "",
                                                                     fillOpacity = 0.7,
                                                                     bringToFront = TRUE),
                                 popup = paste0(
                                   "<strong>Watershed: </strong> ",
                                               df_subset()$Watershed,                                                  "<br>",
                                               "<br/>",
                                   "<strong>Scenario: </strong> ",
                                               df_subset()$Scenario,
                                               "<br/>",
                                   "<strong>wepp_id: </strong> ",
                                               df_subset()$wepp_id,
                                               "<br/>",
                                   "<strong>SdYd_kg_ha: </strong> ",
                                               df_subset()$SdYd_kg_ha,
                                               "<br/>",
                                   "<strong>sed_reduc_at_25probab: </strong> ",
                                               df_subset()$sed_reduc_at_25probab,
                                               "<br/>",
                                   "<strong>sed_reduc_at_50probab: </strong> ",
                                               df_subset()$sed_reduc_at_50probab,
                                               "<br/>",
                                   "<strong>sed_reduc_at_75probab: </strong> ",
                                               df_subset()$sed_reduc_at_75probab
                                 ),
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto")

                                 ))
}


## --------------------------------------------------------------------------------------##
shinyApp(ui, server)