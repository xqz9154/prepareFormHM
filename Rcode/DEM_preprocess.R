# DEM preprocess
library(shiny)
library(pacman)
p_load(tidyverse, raster, sf, whitebox, tmap, stars, leaflet, leafem)

options(shiny.maxRequestSize = 100 * 1024 ^ 2)
#
ui <- fluidPage(navbarPage(
  "Hydrological Model Catchment Delineation",
  tabPanel("DEM preprocess", #download from source, clip using roi, reproject to roi
           sidebarLayout(
             sidebarPanel(
               # Add a file input for the user to select the DEM dataset
               fileInput(
                 "dem",
                 "Select DEM Dataset",
                 accept = c(".tif",
                            ".geotiff",
                            '.hgt'),
                 multiple = TRUE
               )
             ),
             mainPanel(leafletOutput("map"))
           ))
)) 
    

server <- function(input, output) {
  observeEvent(input$dem, {
    req(input$dem)
    # Initialize an empty list to store the raster objects
    rasters <- list()
    
    # Loop over each selected file
    for (i in seq_along(input$dem)) {
      # Check the file extension
      file_ext <- tools::file_ext(input$dem[[i]]$datapath)
      print(input$dem[[i]])
      # Read the file and add it to the rasters list
      if (file_ext == "tif") {
        rasters[[i]] <- raster(input$dem[[i]]$datapath)
      } else if (file_ext == "hgt") {
        rasters[[i]] <- raster(input$dem[[i]]$datapath)
      } else if (file_ext == "geotiff") {
        rasters[[i]] <- raster(input$dem[[i]]$datapath)
      }
    }
    
    # Display the rasters on the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        lapply(rasters, addRasterImage)
    })
  })
}

shinyApp(ui = ui, server = server)