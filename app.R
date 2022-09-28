## --------------------------------------------------------------------------------------##
##
## Script name: app.R
##
## Purpose of the script: Prototype shiny Dashboard for ERMiT runs integrated
## with the weppcloud results
##
## Author: Chinmay Deval
##
## Created On: 2022-09-13
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
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyhelper)
library(sf)
library(sfarrow)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(tidyverse)
library(sever)



## --------------------------------------------------------------------------------------##

ui <-
  dashboardPage(
    skin = "purple",
    scrollToTop = TRUE,
    title = "Viz-ERMiT",
    
    
    ## --------------------------------------------------------------------------------------##
    dbHeader <- dashboardHeader(
      title = "Viz-ERMiT",
      titleWidth = 350,
      tags$li(
        a(
          href = 'https://forest.moscowfsl.wsu.edu/fswepp/',
          img(
            src = 'FS.png',
            title = "US Forest Service",
            height = "30px"
          ),
          style = "padding-top:10px; padding-bottom:10px;"
        ),
        class = "dropdown"
      ),
      tags$li(
        a(
          href = 'https://www.uidaho.edu/',
          img(
            src = 'UI.jpg',
            title = "University of Idaho",
            height = "30px"
          ),
          style = "padding-top:10px; padding-bottom:10px;"
        ),
        class = "dropdown"
      ),
      tags$li(
        a(
          href = 'https://github.com/devalc/Viz-ERMiT',
          img(
            src = 'GitHub-Mark.png',
            title = "GitHub page of this tool",
            height = "30px"
          ),
          style = "padding-top:10px; padding-bottom:10px;"
        ),
        class = "dropdown"
      )
    ),
    ## --------------------------------------------------------------------------------------##
    dashboardSidebar(
      width = 350,
      minified = TRUE,
      useSever(),
      sidebarMenu(
        menuItem(
          "Select Data",
          icon = icon("database"),
          startExpanded = TRUE,
          shinyWidgets::prettyRadioButtons(
            inputId = "dataradio",
            label = "",
            choices = c("Default", "Upload"),
            inline = TRUE,
            shape = "curve",
            animation = "pulse"
          ),
          
          conditionalPanel(
            condition = "input.dataradio == 'Upload'",
            fileInput(
              "inputfile",
              label = "Uplaod GeoParquet file (*.parquet)",
              multiple = F,
              placeholder = "No file selected",
              accept = ".parquet"
            )
          ) 
        ),
        
        menuItemOutput("SelVars"),
        
        menuItem(
          "Cumulative Chart",
          icon = icon("chart-line"),
          br(),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput("cumulative", height = 350),type = 8)
        ),
        
        menuItem(
          "Filter Visuals",
          icon = icon("filter"),
          startExpanded = FALSE,
          sliderInput(
            "filt_area",
            "Filter by % Area:",
            min = 0,
            max = 100,
            value = 100
          )
        )
        
        
        
      )
    ),
    
    ## --------------------------------------------------------------------------------------##
    dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      shinycssloaders::withSpinner(
        leaflet::leafletOutput("map", width="100%", height="100%"), type = 8)
      
      )
  )

## --------------------------------------------------------------------------------------##
server <- function(input, output, session) {
  
  sever()
  
  data_df <- reactive({
    if (input$dataradio == 'Default') {
      data_df = sfarrow::st_read_parquet(
        "./data/portland202203_with_ermit_shps_subcatchments_wgs84_split_wshed_and_scen_MG.parquet"
      )
      
    } else
      if (input$dataradio == 'Upload') {
        req(input$inputfile)
        if (is.null(input$inputfile$datapath)) {
          return()
        }
        validate(need(
          grepl("parquet", input$inputfile) == TRUE,
          "Please provide a parquet file and try again!"
        ))
        data_df = sfarrow::st_read_parquet(input$inputfile$datapath)
        return(data_df)
      }
  })
  
  
  
output$SelVars <- renderMenu({
    sidebarMenu(if (input$dataradio == 'Default') {
      req(data_df())
      menuItem(
        "Select Variables",
        icon = icon("list"),
        startExpanded = FALSE,

        pickerInput(
          inputId = "wat_sel",
          label = "Select watershed",
          choices = unique(data_df()$Watershed),
          selected = unique(data_df()$Watershed)[2],
          multiple = FALSE,
        ),

        pickerInput(
          inputId = "scen_sel",
          label = "Select scenario",
          choices = unique(data_df()$Scenario),
          selected = unique(data_df()$Scenario)[-1],
          multiple = FALSE
        ),

        pickerInput(
          inputId = "var_sel",
          label = "Select variable",
          choices = c("SdYd_Mg",
                      "Sediment_Reduction_Mg"),
          selected = "SdYd_Mg",
          multiple = FALSE
        ),
        
        conditionalPanel(
          condition = "input.var_sel == 'Sediment_Reduction_Mg'",
          pickerInput(
          inputId = "probab_sel",
          label = "Select probability",
          choices = c("25%", "50%", "75%"),
          selected = "25%",
          multiple = FALSE,
        ))

      )
    } else
      if (input$dataradio == 'Upload') {
        menuItem(
          "Select Variables",
          icon = icon("list"),

          pickerInput(
            inputId = "wat_sel",
            label = "Select watershed",
            choices = unique(data_df()$Watershed),
            selected = unique(data_df()$Watershed)[2],
            multiple = FALSE),
          
          pickerInput(
            inputId = "scen_sel",
            label = "Select scenario",
            choices = unique(data_df()$Scenario),
            selected = unique(data_df()$Scenario)[-1],
            multiple = FALSE),
          
          pickerInput(
            inputId = "var_sel",
            label = "Select variable",
            choices = c("SdYd_Mg",
                        "Sediment_Reduction_Mg"),
            selected = "SdYd_Mg",
            multiple = FALSE),
          
          conditionalPanel(
            condition = "input.var_sel == 'Sediment_Reduction_Mg'",
            pickerInput(
              inputId = "probab_sel",
              label = "Select probability",
              choices = c("25%", "50%", "75%"),
              selected = "25%",
              multiple = FALSE,
            ))
          
        )
      })
  })
  
  
  bbox <- reactive({
    req(data_df())
    st_bbox(data_df()) %>% as.vector()
  }) 

  
  
  df_subset <- reactive({
    req(input$wat_sel)
    req(input$scen_sel)
    data_df() %>%
      dplyr::filter(Watershed %in% input$wat_sel) %>%
      dplyr::filter(Scenario %in% input$scen_sel) 
  })
  
  
  
  df_for_cplt <- reactive({
    req(df_subset())
    req(input$var_sel)
    req(input$filt_area)
    if (input$var_sel == "SdYd_Mg") {
      df_subset() %>%
          dplyr::select(Watershed, Scenario, wepp_id, area_ha_ , input$var_sel)%>%na.omit() %>%
          dplyr::arrange_at(.vars = input$var_sel, desc) %>%
          dplyr::mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *100,
                        !!paste("cum", input$var_sel, sep = "_") := cumsum(as.numeric(get(input$var_sel)))/sum(as.numeric(get(input$var_sel)))*100) %>%
          dplyr::filter(cumPercArea <= input$filt_area)

    }else
      if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "25%"){
        var_name = "sed_reduc_Mg_25probab"
        df_subset() %>%
          dplyr::select(Watershed, Scenario, wepp_id, area_ha_ , SdYd_Mg, sed_reduc_Mg_50probab, sed_reduc_Mg_75probab,  dplyr::all_of(var_name))%>%na.omit() %>%
          dplyr::arrange_at(.vars = var_name, desc) %>%
          dplyr::mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *100,
                        !!paste("cum", var_name, sep = "_") := cumsum(as.numeric(get(var_name)))/sum(as.numeric(get(var_name)))*100) %>%
          dplyr::filter(cumPercArea <= input$filt_area)
      }else
        if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "50%"){
          var_name = "sed_reduc_Mg_50probab"
          df_subset() %>%
            dplyr::select(Watershed, Scenario, wepp_id, area_ha_ , SdYd_Mg, sed_reduc_Mg_25probab, sed_reduc_Mg_75probab, dplyr::all_of(var_name))%>%na.omit() %>%
            dplyr::arrange_at(.vars = var_name, desc) %>%
            dplyr::mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *100,
                          !!paste("cum", var_name, sep = "_") := cumsum(as.numeric(get(var_name)))/sum(as.numeric(get(var_name)))*100) %>%
            dplyr::filter(cumPercArea <= input$filt_area)
        }else
          if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "75%"){
            var_name = "sed_reduc_Mg_75probab"
            df_subset() %>%
              dplyr::select(Watershed, Scenario, wepp_id, area_ha_ , SdYd_Mg, sed_reduc_Mg_25probab, sed_reduc_Mg_50probab, dplyr::all_of(var_name))%>%na.omit() %>%
              dplyr::arrange_at(.vars = var_name, desc) %>%
              dplyr::mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *100,
                            !!paste("cum", var_name, sep = "_") := cumsum(as.numeric(get(var_name)))/sum(as.numeric(get(var_name)))*100) %>%
              dplyr::filter(cumPercArea <= input$filt_area)}
  })
  
  output$cumulative <- renderPlotly({
    req(df_for_cplt())
    req(input$var_sel)
    req(input$probab_sel)
    p1 <- df_for_cplt()  %>% ggplot(aes(x = cumPercArea)) + 
      geom_line(aes(y = if (input$var_sel == "SdYd_Mg") {
        get(!!paste("cum", input$var_sel,sep = "_"))}else
          if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "25%"){
          get(!!paste("cum", "sed_reduc_Mg_25probab",sep = "_"))
        }else
          if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "50%"){
            get(!!paste("cum", "sed_reduc_Mg_50probab",sep = "_"))
          }else
            if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "75%"){
             get(!!paste("cum", "sed_reduc_Mg_75probab",sep = "_"))
            }), size = 0.5)+ 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y =  element_blank(),
            axis.text.x = element_blank())+
      ggthemes::theme_economist()+ ggthemes::scale_color_colorblind()+
      labs(x = "Percent of total hillslope area",
           y = "Cumulative Percent of total \nselected variable"
      )
    
  })
  
  
  colorpal <- reactive({
    req(df_for_cplt())
    req(input$var_sel)
    if (input$var_sel == "SdYd_Mg") {
      colorNumeric("viridis", unique(df_for_cplt()$input$var_sel))
      
      
    }else
      if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "25%"){
        var_name = "sed_reduc_Mg_25probab"
        colorNumeric("viridis", unique(df_for_cplt()$var_name))
      }else
        if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "50%"){
          var_name = "sed_reduc_Mg_50probab"
          colorNumeric("viridis", unique(df_for_cplt()$var_name))
        }else
          if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "75%"){
            var_name = "sed_reduc_Mg_75probab"
            colorNumeric("viridis", unique(df_for_cplt()$var_name))
          }
    })
  
  
  output$map <- renderLeaflet({
    req(bbox())
    req(df_for_cplt())
    basemap = leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "WorldTopoMap") %>%
      leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "WorldGrayCanvas") %>%
      leaflet::fitBounds(bbox()[1], bbox()[2], bbox()[3], bbox()[4]) %>%
      leaflet::addMiniMap(
        tiles = "Esri.WorldStreetMap",
        toggleDisplay = TRUE,
        zoomAnimation = TRUE,
        position = "bottomright",
        height = 100
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c(
          "WorldTopoMap",
          "WorldImagery",
          "OpenStreetMap",
          "WorldGrayCanvas"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) 
    
   
    
  })
  
  observe(leafletProxy("map", data = df_for_cplt()) %>%
            leaflet::clearShapes() %>%
            leaflet::addPolygons(data = df_for_cplt(),
                                 fillColor = ~colorpal()(
                                   if (input$var_sel == "SdYd_Mg") {
                                     get(input$var_sel)
                                     
                                   }else
                                     if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "25%"){
                                       var_name = "sed_reduc_Mg_25probab"
                                       get(var_name)
                                     }else
                                       if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "50%"){
                                         var_name = "sed_reduc_Mg_50probab"
                                         get(var_name)
                                       }else
                                         if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "75%"){
                                           var_name = "sed_reduc_Mg_75probab"
                                           get(var_name)
                                         }
                                   ),
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
                                   df_for_cplt()$Watershed,                                                  "<br>",
                                   "<br/>",
                                   "<strong>Scenario: </strong> ",
                                   df_for_cplt()$Scenario,
                                   "<br/>",
                                   "<strong>wepp_id: </strong> ",
                                   df_for_cplt()$wepp_id,
                                   "<br/>", 
                                   if (input$var_sel == "SdYd_Mg") {
                                     paste0("<strong>SdYd_Mg: </strong> ",
                                     round(df_for_cplt()$SdYd_Mg,2))
                                     }else{paste0(
                                   "<strong>SdYd_Mg: </strong> ",
                                   round(df_for_cplt()$SdYd_Mg,2),
                                   "<br/>",
                                   "<strong>sed_reduc_Mg_25probab: </strong> ",
                                   round(df_for_cplt()$sed_reduc_Mg_25probab,2),
                                   "<br/>",
                                   "<strong>sed_reduc_Mg_50probab: </strong> ",
                                   round(df_for_cplt()$sed_reduc_Mg_50probab,2),
                                   "<br/>",
                                   "<strong>sed_reduc_Mg_75probab: </strong> ",
                                   round(df_for_cplt()$sed_reduc_Mg_75probab,2),
                                   "<br/>"
                                 )}),
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto")
                                 )%>% 
            addLegend(pal = colorpal(),
                      values = ~get(if(input$var_sel == "SdYd_Mg"){
                        unique(input$var_sel)}else
                          if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "25%"){
                            var_name= "sed_reduc_Mg_25probab"
                          unique(var_name)}else
                            if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "50%"){
                              var_name= "sed_reduc_Mg_50probab"
                              unique(var_name)}else
                                if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "75%"){
                                  var_name= "sed_reduc_Mg_75probab"
                                  unique(var_name)}
                            ),
                      opacity = 0.7,
                      title = ~if(input$var_sel == "SdYd_Mg"){
                        as.character(input$var_sel)
                      }else
                        if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "25%"){
                          var_name= "sed_reduc_Mg_25probab"
                          as.character(var_name)}else
                            if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "50%"){
                              var_name= "sed_reduc_Mg_50probab"
                              as.character(var_name)}else
                                if(input$var_sel == "Sediment_Reduction_Mg" & input$probab_sel == "75%"){
                                  var_name= "sed_reduc_Mg_75probab"
                                  as.character(var_name)},
                      position = "bottomright",
                      na.label = "No Erosion")
  )
  
}

## --------------------------------------------------------------------------------------##
shinyApp(ui, server)

## --------------------------------------------------------------------------------------##